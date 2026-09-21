source("R/config.R")
source("R/db.R")

library(sf)
library(dplyr)
library(readr)
library(lubridate)
library(cli)

BASE_QUEIMADAS <- "https://dataserver-coids.inpe.br/queimadas/queimadas/focos/csv/diario/America_Sul/"

# Colunas que o CSV do INPE normalmente traz, mas que nao sao essenciais.
# Se alguma faltar, entra como NA e o painel simplesmente nao mostra aquele campo.
COLUNAS_OPCIONAIS <- c("satelite", "data_hora_gmt", "bioma", "frp",
                       "risco_fogo", "numero_dias_sem_chuva")

baixar_focos_dia <- function(data) {
  url <- paste0(BASE_QUEIMADAS, "focos_diario_", format(data, "%Y%m%d"), ".csv")
  tryCatch(read_csv(url, show_col_types = FALSE), error = function(e) NULL)
}

garantir_colunas <- function(df) {
  for (nm in setdiff(COLUNAS_OPCIONAIS, names(df))) df[[nm]] <- NA
  df
}

# INPE usa valores negativos (ex.: -999) para "sem informacao".
valor_valido <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.finite(x) & x >= 0, x, NA_real_)
}

limpar_focos <- function(bruto, codigos) {
  bruto <- garantir_colunas(bruto)

  bruto |>
    filter(is.finite(lat), is.finite(lon), between(lat, -90, 90), between(lon, -180, 180)) |>
    mutate(municipio_id = as.character(municipio_id)) |>
    filter(municipio_id %in% codigos) |>
    mutate(
      satelite = as.character(satelite),
      referencia = !is.na(satelite) & satelite == SATELITE_REFERENCIA,
      data_hora_gmt = as.POSIXct(data_hora_gmt, tz = "UTC"),
      # Arredonda lat/lon (~100m de precisao) e deduplica por local no dia.
      # Isso evita contar o mesmo ponto de calor varias vezes so porque
      # um satelite geoestacionario (ex.: GOES-19) passa a cada 10 min,
      # ou porque satelites diferentes detectaram o mesmo foco.
      lat_r = round(lat, 3),
      lon_r = round(lon, 3)
    ) |>
    # Quando ha duplicata, fica o registro do satelite de referencia (se houver),
    # depois o mais antigo. A ordem NAO altera a contagem, so qual registro sobra.
    arrange(desc(referencia), data_hora_gmt) |>
    distinct(lat_r, lon_r, .keep_all = TRUE)
}

# dias_retroativos = 2 -> atualiza ontem E hoje.
# Motivo: o arquivo de "hoje" e parcial quando a Action roda (12:15 de Brasilia);
# no dia seguinte o arquivo de ontem ja esta completo e substitui o valor parcial.
# Para reprocessar um periodo maior de uma vez (ex.: 14 dias), rode localmente:
#   source("R/update_queimadas.R"); atualizar_queimadas(dias_retroativos = 14)
atualizar_queimadas <- function(dias_retroativos = 2) {
  mun_ride <- readRDS(path_municipios())
  base_mun <- mun_ride |>
    st_drop_geometry() |>
    transmute(municipio, uf, code_muni = as.character(code_muni))
  codigos <- base_mun$code_muni

  hoje <- as.Date(now(tzone = TZ_RIDE))
  datas <- hoje - rev(seq_len(dias_retroativos) - 1L) # do mais antigo ao mais recente
  agora <- as.POSIXct(Sys.time(), tz = "UTC")

  diarios <- list()
  pontos <- list()
  datas_ok <- as.Date(character())

  for (i in seq_along(datas)) {
    d <- datas[i]
    bruto <- baixar_focos_dia(d)
    if (is.null(bruto)) {
      # Arquivo ausente NAO vira "zero focos": o que ja estava no banco fica como esta.
      cli_alert_warning("BDQueimadas: arquivo de {d} indisponivel; dado desse dia mantido como estava.")
      next
    }

    focos <- limpar_focos(bruto, codigos)

    contagem <- focos |> count(municipio_id, name = "n_focos_queimadas")
    diario <- base_mun |>
      left_join(contagem, by = c("code_muni" = "municipio_id")) |>
      mutate(
        n_focos_queimadas = coalesce(as.integer(n_focos_queimadas), 0L),
        data_observacao = d,
        executado_em = agora
      ) |>
      select(data_observacao, municipio, uf, code_muni, n_focos_queimadas, executado_em)

    pts <- focos |>
      select(-any_of(c("municipio", "estado"))) |> # nomes do INPE; usamos os da RIDE
      left_join(base_mun, by = c("municipio_id" = "code_muni")) |>
      transmute(
        data_observacao = d,
        data_hora_gmt,
        lat = round(lat, 5),
        lon = round(lon, 5),
        satelite,
        referencia,
        code_muni = municipio_id,
        municipio,
        uf,
        bioma = as.character(bioma),
        frp = valor_valido(frp),
        risco_fogo = valor_valido(risco_fogo),
        dias_sem_chuva = valor_valido(numero_dias_sem_chuva),
        executado_em = agora
      )

    diarios[[length(diarios) + 1]] <- diario
    pontos[[length(pontos) + 1]] <- pts
    datas_ok <- c(datas_ok, d)
    cli_alert_success("Queimadas {d}: {sum(diario$n_focos_queimadas)} focos na RIDE ({sum(pts$referencia)} pelo satelite de referencia {SATELITE_REFERENCIA})")
  }

  if (!length(datas_ok)) stop("BDQueimadas sem arquivo disponivel para os ultimos ", dias_retroativos, " dia(s).")

  con <- abrir_db()
  on.exit(fechar_db(con), add = TRUE)
  criar_schema(con)
  # Contagem e pontos saem do mesmo processamento e sao gravados juntos.
  DBI::dbWithTransaction(con, {
    substituir_por_datas(con, "queimadas_diarias", datas_ok, bind_rows(diarios), "data_observacao")
    substituir_por_datas(con, "focos_pontos", datas_ok, bind_rows(pontos), "data_observacao")
  })

  invisible(bind_rows(diarios))
}
