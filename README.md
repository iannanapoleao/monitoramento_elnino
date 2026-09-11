# Painel RIDE-DF — histórico diário automático

Esta versão reorganiza o projeto para funcionar no mesmo princípio de **pipeline de dados + painel**, sem depender do computador pessoal para armazenar o histórico.

## O que fica preservado

- **Abastecimento hídrico (SINISA):** dado estrutural/fixo em tabela própria.
- **CAMS:** PM2,5, PM10 e temperatura agregados **por município e por dia de referência**.
- **Queimadas/INPE:** número diário de focos por município.
- **Histórico:** fica em `data/ride_historico.duckdb`, dentro do repositório GitHub.
- **Arquivos NetCDF brutos:** são temporários e apagados ao fim de cada execução.

O histórico antigo do projeto foi preservado em `data/legacy/`. A antiga série CAMS era uma média/máxima do horizonte inteiro de 5 dias; por isso ela é importada para `cams_legacy_5dias` e **não é misturada** com a nova série diária.

## Como funciona todos os dias

`GitHub Actions -> CAMS + INPE -> agregação RIDE -> DuckDB -> docs/index.html -> commit automático`

A Action roda diariamente às 12:15 (horário de Brasília), e também pode ser executada manualmente em **Actions > Atualizar Painel RIDE > Run workflow**.

## Configuração única no GitHub

1. Crie um repositório e envie esta pasta.
2. No repositório, abra **Settings > Secrets and variables > Actions**.
3. Crie o secret `ECMWFR_PAT` e cole o token pessoal do Copernicus/ADS. **Não coloque o token em nenhum arquivo do repositório.**
4. No portal do Copernicus ADS, confirme/aceite a licença do dataset `cams-global-atmospheric-composition-forecasts`.
5. Em **Settings > Pages**, publique a pasta `/docs` da branch principal.
6. Execute o workflow manualmente uma vez para testar.

## Banco DuckDB

Tabelas principais:

- `abastecimento_hidrico`
- `cams_diario` — fotografia mais recente para cada `data_referencia`; dias passados ficam congelados.
- `cams_rodadas` — guarda as diferentes rodadas do modelo para auditoria e comparação futura.
- `queimadas_diarias`
- `cams_legacy_5dias` — dados anteriores, mantidos separadamente.

### Por que existem `data_referencia` e `ciclo_previsao_cams`?

O CAMS é uma previsão. `data_referencia` é o dia ao qual o valor diário se refere. `ciclo_previsao_cams` registra qual rodada do modelo gerou aquela previsão. Isso evita confundir “dia analisado” com “momento em que o modelo foi executado”.

## Rodar localmente (opcional)

O computador pessoal não é necessário para a atualização diária. Rodar localmente serve apenas para desenvolvimento. Se desejar, defina `ecmwfr_PAT` no ambiente e use:

```r
Rscript run_daily.R
```

## Próxima expansão

A estrutura de `cams_diario` pode receber O3, NO2, SO2, CO e IQAr sem alterar a lógica de histórico. Recomenda-se primeiro validar esta versão diária com PM2,5/PM10/temperatura por alguns dias e depois adicionar os gases.
