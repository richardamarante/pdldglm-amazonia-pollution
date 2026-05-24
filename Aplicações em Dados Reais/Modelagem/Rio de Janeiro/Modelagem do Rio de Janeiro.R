# ---
library(rstudioapi)

setwd(dirname(getActiveDocumentContext()$path))
while (!file.exists("PDLDGLM_Geral.R")) setwd(dirname(getwd()))
source("PDLDGLM_Geral.R")
# ---

## RJ – 2018 (d = 3, fd = 0.99, lag = 8, covar = Umid < p10, lags_covar = 3)
df_rj_2018 <- prepara_base("rio_de_janeiro", ano = 2018)

mod_rj_2018 <- PDLDGLM_clima(
  Y = df_rj_2018$Casos_Resp,
  X = df_rj_2018$pm25,
  data = df_rj_2018$Data,
  lags = 8,
  d = 3,
  fd = 0.99,
  lag_covar = 3,
  covar = df_rj_2018$umid,
  perc = 0.10,
  lado = "abaixo"
)

mod_rj_2018$g1
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Rio de Janeiro", "rj2018_mu.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")
mod_rj_2018$g2
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Rio de Janeiro", "rj2018_beta.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")


## RJ – 2022 (d = 2, fd = 0.99, lag = 8, covar = Umid < p10, lags_covar = 1)
df_rj_2022 <- prepara_base("rio_de_janeiro", ano = 2022)

mod_rj_2022 <- PDLDGLM_clima(
  Y = df_rj_2022$Casos_Resp,
  X = df_rj_2022$pm25,
  data = df_rj_2022$Data,
  lags = 8,
  d = 2,
  fd = 0.99,
  lag_covar = 1,
  covar = df_rj_2022$umid,
  perc = 0.10,
  lado = "abaixo"
)

mod_rj_2022$g1
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Rio de Janeiro", "rj2022_mu.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")
mod_rj_2022$g2
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Rio de Janeiro", "rj2022_beta.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")


## RJ – 2024 (d = 2, fd = 0.98, lag = 10, covar = Umid < p10, lags_covar = 0)
df_rj_2024 <- prepara_base("rio_de_janeiro", ano = 2024)

mod_rj_2024 <- PDLDGLM_clima(
  Y = df_rj_2024$Casos_Resp,
  X = df_rj_2024$pm25,
  data = df_rj_2024$Data,
  lags = 10,
  d = 2,
  fd = 0.98,
  lag_covar = 0,
  covar = df_rj_2024$umid,
  perc = 0.10,
  lado = "abaixo"
)

mod_rj_2024$g1
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Rio de Janeiro", "rj2024_mu.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")
mod_rj_2024$g2
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Rio de Janeiro", "rj2024_beta.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")
