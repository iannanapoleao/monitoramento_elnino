options(timeout = 1800)

source("R/init_database.R")
source("R/update_cams.R")
source("R/update_queimadas.R")
source("R/generate_html.R")

cli::cli_h1("Painel RIDE - atualizacao diaria")
inicializar_banco()

# CAMS e queimadas sao independentes. Um erro nao apaga historico ja existente.
erros <- character()
tryCatch(atualizar_cams(), error = function(e) {
  erros <<- c(erros, paste("CAMS:", conditionMessage(e)))
  cli::cli_alert_danger(conditionMessage(e))
})
tryCatch(atualizar_queimadas(), error = function(e) {
  erros <<- c(erros, paste("Queimadas:", conditionMessage(e)))
  cli::cli_alert_danger(conditionMessage(e))
})

gerar_html()

if (length(erros)) {
  cli::cli_alert_warning("Painel foi regenerado com o ultimo historico valido, mas houve falhas: {paste(erros, collapse = ' | ')}")
  # Nao falha o job por indisponibilidade temporaria de uma fonte; o log registra o problema.
}
