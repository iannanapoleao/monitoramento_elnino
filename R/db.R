abrir_db <- function(read_only = FALSE) {
  dir.create(dirname(path_db()), recursive = TRUE, showWarnings = FALSE)
  DBI::dbConnect(duckdb::duckdb(), dbdir = path_db(), read_only = read_only)
}

fechar_db <- function(con) {
  try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
}

criar_schema <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS abastecimento_hidrico (
      municipio VARCHAR,
      uf VARCHAR,
      empresa VARCHAR,
      regiao VARCHAR,
      lat DOUBLE,
      lon DOUBLE,
      iag0001 DOUBLE,
      iag0002 DOUBLE,
      iag0003 DOUBLE,
      intermitencia DOUBLE,
      atualizado_em TIMESTAMP
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS cams_rodadas (
      ciclo_previsao_cams TIMESTAMP,
      data_referencia DATE,
      municipio VARCHAR,
      uf VARCHAR,
      code_muni VARCHAR,
      n_horas INTEGER,
      pm25_media DOUBLE,
      pm25_max DOUBLE,
      pm10_media DOUBLE,
      pm10_max DOUBLE,
      temp_max DOUBLE,
      temp_min DOUBLE,
      executado_em TIMESTAMP
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS cams_diario (
      data_referencia DATE,
      ciclo_previsao_cams TIMESTAMP,
      municipio VARCHAR,
      uf VARCHAR,
      code_muni VARCHAR,
      n_horas INTEGER,
      pm25_media DOUBLE,
      pm25_max DOUBLE,
      pm10_media DOUBLE,
      pm10_max DOUBLE,
      temp_max DOUBLE,
      temp_min DOUBLE,
      executado_em TIMESTAMP
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS queimadas_diarias (
      data_observacao DATE,
      municipio VARCHAR,
      uf VARCHAR,
      code_muni VARCHAR,
      n_focos_queimadas INTEGER,
      executado_em TIMESTAMP
    )")

  # Mantem o historico anterior sem misturar a antiga media de 5 dias
  # com a nova serie diaria.
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS cams_legacy_5dias (
      data_execucao DATE,
      ciclo_previsao_cams TIMESTAMP,
      municipio VARCHAR,
      uf VARCHAR,
      code_muni VARCHAR,
      pm25_media DOUBLE,
      pm25_max DOUBLE,
      pm10_media DOUBLE,
      pm10_max DOUBLE,
      temp_max DOUBLE,
      temp_min DOUBLE
    )")
}

substituir_tabela <- function(con, tabela, dados) {
  DBI::dbExecute(con, paste0("DELETE FROM ", tabela))
  if (nrow(dados)) DBI::dbAppendTable(con, tabela, dados)
}

upsert_por_datas <- function(con, tabela, dados, coluna_data) {
  if (!nrow(dados)) return(invisible(NULL))
  datas <- unique(as.Date(dados[[coluna_data]]))
  for (d in datas) {
    DBI::dbExecute(
      con,
      sprintf("DELETE FROM %s WHERE %s = ?", tabela, coluna_data),
      params = list(as.Date(d, origin = "1970-01-01"))
    )
  }
  DBI::dbAppendTable(con, tabela, dados)
  invisible(NULL)
}
