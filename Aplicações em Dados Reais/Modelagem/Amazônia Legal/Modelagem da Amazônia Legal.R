# ---
library(rstudioapi)

setwd(dirname(getActiveDocumentContext()$path))
while (!file.exists("PDLDGLM_Geral.R")) setwd(dirname(getwd()))
source("PDLDGLM_Geral.R")
# ---

## Belém – 2015 (d = 2, fd = 0.97, lag = 8, covar = Umid < p10, lags_covar = 3)
df_belem_2015 <- prepara_base("belem", ano = 2015)

ajustar_belem_2015 <- PDLDGLM_clima(
  Y = df_belem_2015$Casos_Resp,
  X = df_belem_2015$pm25,
  data = df_belem_2015$Data,
  lags = 8,
  d = 2,
  fd = 0.97,
  lag_covar = 3,
  covar = df_belem_2015$umid, 
  perc = 0.10,
  lado = "abaixo"
)

ajustar_belem_2015$g1
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Amazônia Legal", "belem2015_mu.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")
ajustar_belem_2015$g2
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Amazônia Legal", "belem2015_beta.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")


## Belém – 2022 (d = 3, fd = 0.98, lag = 10, covar = Temp > p85, lags_covar = 0)
df_belem_2022 <- prepara_base("belem", ano = 2022)

ajustar_belem_2022 <- PDLDGLM_clima(
  Y = df_belem_2022$Casos_Resp,
  X = df_belem_2022$pm25,
  data = df_belem_2022$Data,
  lags = 10,
  d = 3,
  fd = 0.98,
  lag_covar = 0,
  covar = df_belem_2022$temp,
  perc = 0.85,
  lado = "acima"
)

ajustar_belem_2022$g1
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Amazônia Legal", "belem2022_mu.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")
ajustar_belem_2022$g2
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Amazônia Legal", "belem2022_beta.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")


## Boa Vista – 2020 (d = 2, fd = 0.98, lag = 9) 
df_boavista_2020 <- prepara_base("boa_vista", ano = 2020)

ajustar_boavista_2020 <- PDLDGLM(
  Y = df_boavista_2020$Casos_Resp,
  X = df_boavista_2020$pm25,
  data = df_boavista_2020$Data,
  lags = 9,
  d = 2,
  fd = 0.98
)

ajustar_boavista_2020$g1
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Amazônia Legal", "boavista2020_mu.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")
ajustar_boavista_2020$g2
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Amazônia Legal", "boavista2020_beta.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")



## Cuiabá – 2022 (d = 2, fd = 0.98, lag = 9, covar = Temp > p85, lags_covar = 0)
df_cuiaba_2022 <- prepara_base("cuiaba", ano = 2022)

ajustar_cuiaba_2022 <- PDLDGLM_clima(
  Y = df_cuiaba_2022$Casos_Resp,
  X = df_cuiaba_2022$pm25,
  data = df_cuiaba_2022$Data,
  lags = 9,
  d = 2,
  fd = 0.98,
  lag_covar = 0,
  covar = df_cuiaba_2022$temp,
  perc = 0.85,
  lado = "acima"
)

ajustar_cuiaba_2022$g1
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Amazônia Legal", "cuiaba2022_mu.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")
ajustar_cuiaba_2022$g2
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Amazônia Legal", "cuiaba2022_beta.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")


## Macapá – 2022 (d = 3, fd = 0.97, lag = 8, covar = Temp > p90, lags_covar = 0)
df_macapa_2022 <- prepara_base("macapa", ano = 2022)

ajustar_macapa_2022 <- PDLDGLM_clima(
  Y = df_macapa_2022$Casos_Resp,
  X = df_macapa_2022$pm25,
  data = df_macapa_2022$Data,
  lags = 8,
  d = 3,
  fd = 0.97,
  lag_covar = 0,
  covar = df_macapa_2022$temp,
  perc = 0.90,
  lado = "acima"
)

ajustar_macapa_2022$g1
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Amazônia Legal", "macapa2022_mu.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")
ajustar_macapa_2022$g2
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Amazônia Legal", "macapa2022_beta.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")


## Manaus – 2024 (d = 3, fd = 0.98, lag = 12, covar = Temp > p85, lags_covar = 2)
df_manaus_2024 <- prepara_base("manaus", ano = 2024)

ajustar_manaus_2024 <- PDLDGLM_clima(
  Y = df_manaus_2024$Casos_Resp,
  X = df_manaus_2024$pm25,
  data = df_manaus_2024$Data,
  lags = 12,
  d = 3,
  fd = 0.98,
  lag_covar = 2,
  covar = df_manaus_2024$temp,
  perc = 0.85,
  lado = "acima"
)

ajustar_manaus_2024$g1
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Amazônia Legal", "manaus2024_mu.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")
ajustar_manaus_2024$g2
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Amazônia Legal", "manaus2024_beta.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")


## Porto Velho – 2018 (d = 2, fd = 0.97, lag: 10)
df_porto_2018 <- prepara_base("porto_velho", ano = 2018)

ajustar_porto_2018 <- PDLDGLM(
  Y = df_porto_2018$Casos_Resp, X = df_porto_2018$pm25, data = df_porto_2018$Data,
  lags = 10, d = 2, fd = 0.97
)

ajustar_porto_2018$g1
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Amazônia Legal", "porto2018_mu.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")
ajustar_porto_2018$g2
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Amazônia Legal", "porto2018_beta.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")


## Rio Branco – 2019 (d = 3, fd = 0.98, lag = 12, covar = Temp > p85, lags_covar = 7)
df_riobranco_2019 <- prepara_base("rio_branco", ano = 2019)

ajustar_riobranco_2019 <- PDLDGLM_clima(
  Y = df_riobranco_2019$Casos_Resp,
  X = df_riobranco_2019$pm25,
  data = df_riobranco_2019$Data,
  lags = 12,
  d = 3,
  fd = 0.98,
  lag_covar = 7,
  covar = df_riobranco_2019$temp,
  perc = 0.85,
  lado = "acima"
)

ajustar_riobranco_2019$g1
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Amazônia Legal", "riobranco2019_mu.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")
ajustar_riobranco_2019$g2
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Amazônia Legal", "riobranco2019_beta.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")



## São Luís – 2015 (d = 3, fd = 0.99, lag = 10, covar = Umid > p10, lags_covar = 2)
df_saoluis_2015 <- prepara_base("sao_luis", ano = 2015)

ajustar_saoluis_2015 <- PDLDGLM_clima(
  Y = df_saoluis_2015$Casos_Resp,
  X = df_saoluis_2015$pm25,
  data = df_saoluis_2015$Data,
  lags = 10,
  d = 3,
  fd = 0.99,
  lag_covar = 2,
  covar = df_saoluis_2015$umid,
  perc = 0.10,
  lado = "abaixo"
)

ajustar_saoluis_2015$g1
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Amazônia Legal", "saoluis2015_mu.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")
ajustar_saoluis_2015$g2
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Amazônia Legal", "saoluis2015_beta.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")



## Palmas – 2024 (d = 2, fd = 0.99, lag = 8) – sem covar climática (padrão)
df_palmas_2024 <- prepara_base("palmas", ano = 2024)

ajustar_palmas_2024 <- PDLDGLM(
  Y = df_palmas_2024$Casos_Resp,
  X = df_palmas_2024$pm25,
  data = df_palmas_2024$Data,
  lags = 8,
  d = 2,
  fd = 0.99
)

ajustar_palmas_2024$g1
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Amazônia Legal", "palmas_2024_mu.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")
ajustar_palmas_2024$g2
#ggsave(file.path("Aplicações em Dados Reais/Modelagem/Amazônia Legal", "palmas_2024_beta.png"), dpi = 600, width = 12, height = 7, units = "in", bg = "white")
