# Configuracao central do projeto.
# Todos os caminhos sao relativos ao repositorio: nada depende de C:/Users/...

project_root <- function() {
  # GitHub Actions e RStudio executam normalmente na raiz do repositorio.
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

path_data <- function(...) file.path(project_root(), "data", ...)
path_docs <- function(...) file.path(project_root(), "docs", ...)
path_template <- function() file.path(project_root(), "template_ride.html")
path_db <- function() path_data("ride_historico.duckdb")
path_municipios <- function() path_data("mun_ride_epsg4326.rds")
path_agua <- function() path_data("sinisa_agua.csv")

TZ_RIDE <- "America/Sao_Paulo"

# Queimadas (INPE/BDQueimadas)
# Satelite de referencia do Programa Queimadas: serve para comparar a serie
# historica ao longo dos anos. Aqui ele so MARCA os pontos (nao filtra a contagem).
SATELITE_REFERENCIA <- "AQUA_M-T"
# Quantos dias de pontos (focos) sao embutidos no HTML. O banco guarda tudo.
DIAS_PONTOS_NO_PAINEL <- 30
CAMS_DATASET <- "cams-global-atmospheric-composition-forecasts"
CAMS_MARGIN_DEG <- 0.5
CAMS_LEAD_HOURS <- 0:120

# Para uma rotina diaria, usar sempre o ciclo 00 UTC produz dias locais mais completos.
# O ciclo 00 UTC costuma estar disponivel varias horas depois; a Action roda depois disso.
escolher_ciclo_diario <- function(agora = lubridate::now(tzone = "UTC")) {
  agora <- lubridate::with_tz(agora, "UTC")
  data_utc <- lubridate::as_date(agora)
  inicio <- lubridate::as_datetime(data_utc, tz = "UTC")
  # Se rodar antes de 11 UTC, usa o ciclo 00 UTC do dia anterior.
  if (agora < inicio + lubridate::hours(11)) data_utc <- data_utc - 1
  list(data = data_utc, hora = "00:00")
}
