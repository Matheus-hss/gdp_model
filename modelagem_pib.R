##################PROCESSO DE COLETA DE DADOS/TRATAMENTO - SIDRA_IBGE###########
#Contas Nacionais - Tabela 5932: Taxa de variação do volume trimestral
#PIB A PREÇOS DE MERCADO 1996 A 2024
###############################################################################
#carregando pacote
library(jsonlite)
library(dplyr)
library(lubridate)
library(tsibble)
library(feasts)
library(fabletools)
library(fable)
library(urca)
#------------------------------------------------------------------------------
#coletando dados
#link sidra IBGE PIB:https://apisidra.ibge.gov.br/values/t/5932/n1/all/v/6561/p/all/c11255/90707/d/v6561%201?formato=json
dados_brutos = jsonlite::fromJSON(
  "https://apisidra.ibge.gov.br/values/t/5932/n1/all/v/6561/p/all/c11255/90707/d/v6561%201?formato=json"
                                  )
#------------------------------------------------------------------------------
#tratamento dos dados
dados_limpos <- dados_brutos |> 
  dplyr::rename_with(~as.character(dados_brutos[1, ])) |> 
  dplyr::filter(dplyr::row_number() != 1) |> 
  dplyr::mutate(
    data = `Trimestre (Código)` |> lubridate::yq() |> tsibble::yearquarter(),
    pib = as.numeric(`Valor`),
    .keep = "none"
  ) |> 
  tsibble::as_tsibble(index = "data")
#------------------------------------------------------------------------------
#analise exploratória
#decomposição STL
dados_limpos|>
  fabletools::model(stl = feasts::STL(pib))|>
  fabletools::components()|>
  fabletools::autoplot()
#analise de autocorrelação
feasts::gg_tsdisplay(dados_limpos, y = pib, plot_type = "partial")
#------------------------------------------------------------------------------
#especificando e estimando modelo
#o modelo de séries temporais utilizado é o ARIMA
modelagem = fabletools::model(
  .data = dados_limpos,
  AR2 = fable::ARIMA(pib ~ 1 + pdq(2,0,0) + PDQ(0,0,0)),
  MA4 = fable::ARIMA(pib ~ 1 + pdq(0,0,4) + PDQ(0,0,0)),
  auto_arima = fable::ARIMA(pib)
)
#------------------------------------------------------------------------------
#Reportar estimativas dos modelos
estimativas = fabletools::tidy(modelagem)
#Critérios de informação
modelagem |> fabletools::glance() |> dplyr::arrange(AICc)
#Analise dos residuos
residuos = modelagem |> dplyr::select("auto_arima") |> 
  feasts::gg_tsresiduals()
View(residuos)
#Previsão
previsão = modelagem |> dplyr::select("auto_arima") |> 
  fabletools::forecast(h = 5)
View(previsão)
#Visualização da previsão do PIB a preços de mercado 
#para os proximos 5 periodos
previsão |> fabletools::autoplot(data = dados_limpos, show_gap = FALSE)


