# ---
library(rstudioapi)

setwd(dirname(getActiveDocumentContext()$path))
while (!file.exists("PDLDGLM_Geral.R")) setwd(dirname(getwd()))
source("PDLDGLM_Geral.R")
# ---

## SP – 2015 (d = 3, fd = 0.97, lag = 10, covar = Umid < p15, lags_covar = 0)
df_sp_2015 <- prepara_base("sao_paulo", ano = 2015)

ajustar_sp_2015 <- PDLDGLM_clima(
  Y = df_sp_2015$Casos_Resp,
  X = df_sp_2015$pm25,
  data = df_sp_2015$Data,
  lags = 10,
  d = 3,
  fd = 0.97,
  lag_covar = 0,
  covar = df_sp_2015$umid,
  perc = 0.15,
  lado = "abaixo"
)

ajustar_sp_2015$g1
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/São Paulo", "sp2015_mu.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")
ajustar_sp_2015$g2
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/São Paulo", "sp2015_beta.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")


## SP – 2019 (d = 2, fd = 0.97, lag = 10, covar = Temp > p90, lags_covar = 1)
df_sp_2019 <- prepara_base("sao_paulo", ano = 2019)

ajustar_sp_2019 <- PDLDGLM_clima(
  Y = df_sp_2019$Casos_Resp,
  X = df_sp_2019$pm25,
  data = df_sp_2019$Data,
  lags = 10,
  d = 2,
  fd = 0.97,
  lag_covar = 1,
  covar = df_sp_2019$temp,
  perc = 0.90,
  lado = "acima"
)

ajustar_sp_2019$g1
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/São Paulo", "sp2019_mu.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")
ajustar_sp_2019$g2
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/São Paulo", "sp2019_beta.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")

## SP – 2021 (d = 2, fd = 0.97, lag = 10, covar = Temp > p90, lags_covar = 1)
df_sp_2021 <- prepara_base("sao_paulo", ano = 2021)

ajustar_sp_2021 <- PDLDGLM_clima(
  Y = df_sp_2021$Casos_Resp,
  X = df_sp_2021$pm25,
  data = df_sp_2021$Data,
  lags = 10,
  d = 2,
  fd = 0.97,
  lag_covar = 1,
  covar = df_sp_2021$temp,
  perc = 0.90,
  lado = "acima"
)

ajustar_sp_2021$g1
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/São Paulo", "sp2021_mu.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")
ajustar_sp_2021$g2
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/São Paulo", "sp2021_beta.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")

## SP – 2022 (d = 3, fd = 0.97, lag = 10, covar = Umid < p10, lags_covar = 0)
df_sp_2022 <- prepara_base("sao_paulo", ano = 2022)

ajustar_sp_2022 <- PDLDGLM_clima(
  Y = df_sp_2022$Casos_Resp,
  X = df_sp_2022$pm25,
  data = df_sp_2022$Data,
  lags = 10,
  d = 3,
  fd = 0.97,
  lag_covar = 0,
  covar = df_sp_2022$umid,
  perc = 0.10,
  lado = "abaixo"
)

ajustar_sp_2022$g1
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/São Paulo", "sp2022_mu.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")
ajustar_sp_2022$g2
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/São Paulo", "sp2022_beta.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")
