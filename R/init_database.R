source("R/config.R")
source("R/db.R")

library(DBI)
library(duckdb)
library(readr)
library(dplyr)
library(lubridate)
library(cli)

inicializar_banco <- function() {
  con <- abrir_db()
  on.exit(fechar_db(con), add = TRUE)
  criar_schema(con)

  # Abastecimento e fixo/estrutural: substitui somente quando o CSV-fonte muda.
  agua <- read_delim(path_agua(), delim = ";", show_col_types = FALSE) |>
    transmute(
      municipio = as.character(municipio), uf = as.character(uf),
      empresa = as.character(empresa), regiao = as.character(regiao),
      lat = as.numeric(lat), lon = as.numeric(lon),
      iag0001 = as.numeric(iag0001), iag0002 = as.numeric(iag0002),
      iag0003 = as.numeric(iag0003), intermitencia = as.numeric(intermitencia),
      atualizado_em = as.POSIXct(Sys.time(), tz = "UTC")
    )
  substituir_tabela(con, "abastecimento_hidrico", agua)

  # Migra uma unica vez o historico antigo de clima (media do horizonte de 5 dias).
  legacy <- path_data("legacy", "historico_clima_5dias.csv")
  if (file.exists(legacy) && DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM cams_legacy_5dias")$n[1] == 0) {
    velho <- read_csv(legacy, show_col_types = FALSE, col_types = cols(
      data_execucao = col_date(), ciclo_previsao_cams = col_datetime(),
      municipio = col_character(), uf = col_character(), code_muni = col_character()
    ))
    DBI::dbAppendTable(con, "cams_legacy_5dias", velho)
    cli_alert_info("Historico antigo preservado em cams_legacy_5dias; ele nao e confundido com dados diarios.")
  }

  # Migra queimadas ja coletadas, pois esses dados ja sao diarios.
  old_fire <- path_data("legacy", "historico_queimadas.csv")
  if (file.exists(old_fire) && DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM queimadas_diarias")$n[1] == 0) {
    q <- read_csv(old_fire, show_col_types = FALSE, col_types = cols(
      data_observacao = col_date(), municipio = col_character(), uf = col_character(), code_muni = col_character()
    )) |>
      mutate(executado_em = as.POSIXct(Sys.time(), tz = "UTC")) |>
      select(data_observacao, municipio, uf, code_muni, n_focos_queimadas, executado_em)
    DBI::dbAppendTable(con, "queimadas_diarias", q)
  }

  cli_alert_success("Banco inicializado: {path_db()}")
  invisible(TRUE)
}
