source("R/config.R")
source("R/db.R")

library(ecmwfr)
library(terra)
library(sf)
library(exactextractr)
library(dplyr)
library(lubridate)
library(glue)
library(cli)

baixar_com_retry <- function(request, path, tentativas = 3) {
  ultimo_erro <- NULL
  for (i in seq_len(tentativas)) {
    out <- tryCatch(
      wf_request(request = request, transfer = TRUE, path = path),
      error = function(e) e
    )
    if (!inherits(out, "error")) return(invisible(out))
    ultimo_erro <- out
    cli_alert_warning("CAMS: tentativa {i}/{tentativas} falhou: {conditionMessage(out)}")
    if (i < tentativas) Sys.sleep(30)
  }
  stop("Falha ao baixar CAMS: ", conditionMessage(ultimo_erro))
}

resumir_matriz_por_dia <- function(mat, datas_locais, nomes, min_horas = 12) {
  dias <- sort(unique(as.character(datas_locais)))
  purrr::map_dfr(dias, function(dia) {
    idx <- which(as.character(datas_locais) == dia)
    if (length(idx) < min_horas) return(NULL)
    x <- mat[, idx, drop = FALSE]
    tibble::tibble(
      municipio = nomes$municipio,
      uf = nomes$uf,
      code_muni = as.character(nomes$code_muni),
      data_referencia = as.Date(dia),
      n_horas = length(idx),
      media = round(rowMeans(x, na.rm = TRUE), 2),
      maximo = round(apply(x, 1, max, na.rm = TRUE), 2),
      minimo = round(apply(x, 1, min, na.rm = TRUE), 2)
    )
  })
}

atualizar_cams <- function() {
  if (!nzchar(Sys.getenv("ecmwfr_PAT"))) {
    stop("Secret CAMS ausente. No GitHub, crie o secret ECMWFR_PAT com seu token do Copernicus ADS.")
  }

  mun_ride <- readRDS(path_municipios())
  nomes <- sf::st_drop_geometry(mun_ride) |>
    transmute(municipio, uf, code_muni = as.character(code_muni))

  bb <- st_bbox(mun_ride)
  area_cams <- c(bb[["ymax"]] + CAMS_MARGIN_DEG, bb[["xmin"]] - CAMS_MARGIN_DEG,
                 bb[["ymin"]] - CAMS_MARGIN_DEG, bb[["xmax"]] + CAMS_MARGIN_DEG)

  ciclo <- escolher_ciclo_diario()
  ciclo_dt <- ymd_hm(paste(ciclo$data, ciclo$hora), tz = "UTC")
  valid_utc <- ciclo_dt + hours(CAMS_LEAD_HOURS)
  valid_local <- with_tz(valid_utc, TZ_RIDE)
  datas_locais <- as.Date(valid_local, tz = TZ_RIDE)
  hoje_local <- as.Date(now(tzone = TZ_RIDE))

  # Exclui o pedaco do dia anterior gerado pela conversao UTC -> horario de Brasilia.
  manter <- datas_locais >= hoje_local
  datas_locais <- datas_locais[manter]
  leads <- CAMS_LEAD_HOURS[manter]

  tmp <- tempfile("cams_ride_")
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  fazer_request <- function(variavel, arquivo) list(
    dataset_short_name = CAMS_DATASET,
    variable = variavel,
    date = glue("{ciclo$data}/{ciclo$data}"),
    time = ciclo$hora,
    leadtime_hour = as.character(CAMS_LEAD_HOURS),
    type = "forecast",
    data_format = "netcdf",
    download_format = "unarchived",
    area = area_cams,
    target = arquivo
  )

  reqs <- list(
    pm25 = fazer_request("particulate_matter_2.5um", "cams_pm25.nc"),
    pm10 = fazer_request("particulate_matter_10um", "cams_pm10.nc"),
    temp = fazer_request("2m_temperature", "cams_temp.nc")
  )
  for (nm in names(reqs)) {
    cli_alert_info("Baixando CAMS: {nm} | ciclo {ciclo$data} {ciclo$hora} UTC")
    baixar_com_retry(reqs[[nm]], tmp)
  }

  pm25 <- rast(file.path(tmp, "cams_pm25.nc")) * 1e9
  pm10 <- rast(file.path(tmp, "cams_pm10.nc")) * 1e9
  temp <- rast(file.path(tmp, "cams_temp.nc")) - 273.15

  # Garante alinhamento entre as camadas baixadas e os lead times solicitados.
  n_esperado <- length(CAMS_LEAD_HOURS)
  if (nlyr(pm25) != n_esperado || nlyr(pm10) != n_esperado || nlyr(temp) != n_esperado) {
    stop("Numero inesperado de camadas CAMS. Esperado: ", n_esperado,
         "; recebido PM2.5/PM10/temp = ", paste(c(nlyr(pm25), nlyr(pm10), nlyr(temp)), collapse = "/"))
  }

  extrair <- function(rst) as.matrix(exact_extract(rst, mun_ride, "mean", progress = FALSE))[, manter, drop = FALSE]
  s25 <- extrair(pm25); s10 <- extrair(pm10); st <- extrair(temp)

  d25 <- resumir_matriz_por_dia(s25, datas_locais, nomes) |>
    select(data_referencia, municipio, uf, code_muni, n_horas, pm25_media = media, pm25_max = maximo)
  d10 <- resumir_matriz_por_dia(s10, datas_locais, nomes) |>
    select(data_referencia, municipio, code_muni, pm10_media = media, pm10_max = maximo)
  dtp <- resumir_matriz_por_dia(st, datas_locais, nomes) |>
    select(data_referencia, municipio, code_muni, temp_max = maximo, temp_min = minimo)

  diario <- d25 |>
    left_join(d10, by = c("data_referencia", "municipio", "code_muni")) |>
    left_join(dtp, by = c("data_referencia", "municipio", "code_muni")) |>
    mutate(
      ciclo_previsao_cams = as.POSIXct(ciclo_dt, tz = "UTC"),
      executado_em = as.POSIXct(Sys.time(), tz = "UTC"),
      .after = data_referencia
    ) |>
    select(data_referencia, ciclo_previsao_cams, municipio, uf, code_muni, n_horas,
           pm25_media, pm25_max, pm10_media, pm10_max, temp_max, temp_min, executado_em)

  con <- abrir_db()
  on.exit(fechar_db(con), add = TRUE)
  criar_schema(con)

  # Guarda cada rodada para auditoria/reprodutibilidade.
  DBI::dbExecute(con, "DELETE FROM cams_rodadas WHERE ciclo_previsao_cams = ?",
                 params = list(as.POSIXct(ciclo_dt, tz = "UTC")))
  DBI::dbAppendTable(con, "cams_rodadas", diario)

  # Snapshot diario: para datas ainda previstas, a rodada mais recente substitui a anterior.
  # Datas passadas nao aparecem em diario e portanto permanecem congeladas no historico.
  upsert_por_datas(con, "cams_diario", diario, "data_referencia")

  cli_alert_success("CAMS atualizado: {n_distinct(diario$data_referencia)} dias x {n_distinct(diario$code_muni)} territorios")
  invisible(diario)
}
