source("R/config.R")
source("R/db.R")

library(sf)
library(dplyr)
library(jsonlite)
library(lubridate)
library(purrr)
library(glue)
library(cli)

fmt_pt <- function(x, casas = 1) format(round(x, casas), decimal.mark = ",", nsmall = casas)
meses_pt <- c("jan","fev","mar","abr","mai","jun","jul","ago","set","out","nov","dez")

gerar_html <- function() {
  dir.create(path_docs(), recursive = TRUE, showWarnings = FALSE)
  con <- abrir_db(read_only = TRUE)
  on.exit(fechar_db(con), add = TRUE)

  mun_ride <- readRDS(path_municipios())
  tmp_geojson <- tempfile(fileext = ".geojson")
  st_write(mun_ride |> rename(name = municipio), tmp_geojson, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE)
  boundaries_json <- paste(readLines(tmp_geojson, encoding = "UTF-8", warn = FALSE), collapse = "\n")

  agua <- DBI::dbGetQuery(con, "SELECT municipio, uf, empresa, regiao, iag0001, iag0002, iag0003, intermitencia FROM abastecimento_hidrico ORDER BY municipio")
  agua_json <- toJSON(agua, auto_unbox = TRUE, na = "null", digits = NA)
  pior <- agua[which.min(agua$iag0001), , drop = FALSE]
  maior_interm <- agua[which.max(agua$intermitencia), , drop = FALSE]
  stat_pior <- if (nrow(pior)) glue('{pior$municipio} <span class="unit">{fmt_pt(pior$iag0001)}%</span>') else "s/ dado"
  stat_interm <- if (nrow(maior_interm)) glue('{maior_interm$municipio} <span class="unit">{fmt_pt(maior_interm$intermitencia, 0)}%</span>') else "s/ dado"

  clima <- DBI::dbGetQuery(con, "
    SELECT data_referencia, ciclo_previsao_cams, municipio, uf, code_muni,
           pm25_media, pm25_max, pm10_media, pm10_max, temp_max, temp_min
    FROM cams_diario ORDER BY data_referencia, municipio") |>
    mutate(data_referencia = as.Date(data_referencia))

  periods_clima <- if (nrow(clima)) {
    clima |> group_split(data_referencia) |>
      map(function(rows) {
        dia <- unique(rows$data_referencia)[1]
        list(
          id = paste0("c", format(dia, "%Y%m%d")),
          date = as.character(dia),
          dateLabel = glue("{sprintf('%02d', day(dia))}/{meses_pt[month(dia)]}/{year(dia)}"),
          cycle = as.character(max(rows$ciclo_previsao_cams, na.rm = TRUE)),
          data = rows |> select(municipio, uf, code_muni, pm25_media, pm25_max, pm10_media, pm10_max, temp_max, temp_min)
        )
      })
  } else list()

  queimadas <- DBI::dbGetQuery(con, "
    SELECT data_observacao, municipio, uf, code_muni, n_focos_queimadas
    FROM queimadas_diarias ORDER BY data_observacao, municipio") |>
    mutate(data_observacao = as.Date(data_observacao))

  periods_queimadas <- if (nrow(queimadas)) {
    queimadas |> group_split(data_observacao) |>
      map(function(rows) {
        dia <- unique(rows$data_observacao)[1]
        list(
          id = paste0("q", format(dia, "%Y%m%d")),
          date = as.character(dia),
          dateLabel = glue("{sprintf('%02d', day(dia))}/{meses_pt[month(dia)]}/{year(dia)}"),
          data = rows |> select(municipio, uf, code_muni, n_focos_queimadas)
        )
      })
  } else list()

  template <- paste(readLines(path_template(), encoding = "UTF-8", warn = FALSE), collapse = "\n")
  html <- template
  html <- sub("__AGUA_JSON__", as.character(toJSON(agua, auto_unbox = TRUE, na = "null", digits = NA)), html, fixed = TRUE)
  html <- sub("__PERIODS_CLIMA_JSON__", as.character(toJSON(periods_clima, auto_unbox = TRUE, na = "null", digits = NA)), html, fixed = TRUE)
  html <- sub("__PERIODS_QUEIMADAS_JSON__", as.character(toJSON(periods_queimadas, auto_unbox = TRUE, na = "null", digits = NA)), html, fixed = TRUE)
  html <- sub("__BOUNDARIES_JSON__", boundaries_json, html, fixed = TRUE)
  html <- sub("__STAT_PIOR_ATENDIMENTO__", stat_pior, html, fixed = TRUE)
  html <- sub("__STAT_MAIOR_INTERMITENCIA__", stat_interm, html, fixed = TRUE)
  html <- sub("__SUBTITLE__", "34 territórios · SINISA (estrutural), CAMS (diário) e BDQueimadas/INPE (diário) · atualização automática e histórico preservado", html, fixed = TRUE)

  sobrando <- regmatches(html, gregexpr("__[A-Z_]+__", html))[[1]]
  if (length(sobrando) > 0 && sobrando[1] != "") stop("Placeholder sem substituir: ", paste(unique(sobrando), collapse = ", "))

  out <- path_docs("index.html")
  writeLines(html, out, useBytes = TRUE)
  cli_alert_success("Painel gerado: {out}")
  invisible(out)
}
