source("R/config.R")
source("R/db.R")

library(sf)
library(dplyr)
library(readr)
library(lubridate)
library(cli)

BASE_QUEIMADAS <- "https://dataserver-coids.inpe.br/queimadas/queimadas/focos/csv/diario/America_Sul/"

baixar_focos_dia <- function(data) {
  url <- paste0(BASE_QUEIMADAS, "focos_diario_", format(data, "%Y%m%d"), ".csv")
  tryCatch(read_csv(url, show_col_types = FALSE), error = function(e) NULL)
}

atualizar_queimadas <- function() {
  mun_ride <- readRDS(path_municipios())
  codigos <- mun_ride |> st_drop_geometry() |> pull(code_muni) |> as.character()

  # Tenta hoje e, se o arquivo diario ainda nao foi publicado, ontem.
  data_obs <- as.Date(now(tzone = TZ_RIDE))
  bruto <- baixar_focos_dia(data_obs)
  if (is.null(bruto) || !nrow(bruto)) {
    data_obs <- data_obs - 1
    bruto <- baixar_focos_dia(data_obs)
  }
  if (is.null(bruto) || !nrow(bruto)) stop("BDQueimadas sem arquivo disponivel para hoje/ontem.")

  # Mantem a mesma regra do projeto anterior para permitir comparabilidade historica.
  focos <- bruto |>
    filter(satelite %in% c("AQUA_M-M", "AQUA_M-T")) |>
    filter(is.finite(lat), is.finite(lon), between(lat, -90, 90), between(lon, -180, 180)) |>
    mutate(municipio_id = as.character(municipio_id)) |>
    distinct(id, .keep_all = TRUE) |>
    filter(municipio_id %in% codigos)

  contagem <- focos |> count(municipio_id, name = "n_focos_queimadas")
  diario <- mun_ride |>
    st_drop_geometry() |>
    transmute(municipio, uf, code_muni = as.character(code_muni)) |>
    left_join(contagem, by = c("code_muni" = "municipio_id")) |>
    mutate(
      n_focos_queimadas = coalesce(as.integer(n_focos_queimadas), 0L),
      data_observacao = data_obs,
      executado_em = as.POSIXct(Sys.time(), tz = "UTC"),
      .before = 1
    ) |>
    select(data_observacao, municipio, uf, code_muni, n_focos_queimadas, executado_em)

  con <- abrir_db()
  on.exit(fechar_db(con), add = TRUE)
  criar_schema(con)
  upsert_por_datas(con, "queimadas_diarias", diario, "data_observacao")

  cli_alert_success("Queimadas atualizadas para {data_obs}: {sum(diario$n_focos_queimadas)} focos na RIDE")
  invisible(diario)
}
