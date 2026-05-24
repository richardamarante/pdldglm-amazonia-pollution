# ---
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
while (!file.exists("PDLDGLM_Geral.R")) setwd(dirname(getwd()))
source("PDLDGLM_Geral.R")
# ---

destino <- "Aplicações em Dados Reais/Análise Descritiva/Mix (RJ e SP)"
plots_descritiva <- Descritiva(base_rj_sp, padronizacao = "max", facet_type = "wrap", usar_facets = TRUE)

g1_caps_descritiva <- plots_descritiva$resp_pm25
g2_caps_descritiva <- plots_descritiva$clima_pm25
g3_caps_descritiva <- plots_descritiva$tudo
g4_pm_caps_descritiva <- plots_descritiva$serie$pm25
g4_temp_caps_descritiva <- plots_descritiva$serie$temp
g4_umid_caps_descritiva <- plots_descritiva$serie$umid
g4_hosp_caps_descritiva <- plots_descritiva$serie$hosp
g5_caps_descritiva <- plots_descritiva$temp_umid

#ggsave(file.path(destino, "serie_resp_pm25.png"), plot = g1_caps_descritiva, dpi = 600, width = 10, height = 5, units = "in", bg = "white")
#ggsave(file.path(destino, "serie_temp_umid_pm25.png"), plot = g2_caps_descritiva, dpi = 600, width = 10, height = 5, units = "in", bg = "white")
#ggsave(file.path(destino, "Serie_resp_pm25_temp_umid.png"), plot = g3_caps_descritiva, dpi = 600, width = 10, height = 5, units = "in", bg = "white")
#ggsave(file.path(destino, "serie_individual_pm25.png"), plot = g4_pm_caps_descritiva, dpi = 600, width = 10, height = 4.5, units = "in", bg = "white")
#ggsave(file.path(destino, "serie_individual_temp.png"), plot = g4_temp_caps_descritiva, dpi = 600, width = 10, height = 4.5, units = "in", bg = "white")
#ggsave(file.path(destino, "serie_individual_umid.png"), plot = g4_umid_caps_descritiva, dpi = 600, width = 10, height = 4.5, units = "in", bg = "white")
#ggsave(file.path(destino, "serie_individual_hosp.png"), plot = g4_hosp_caps_descritiva, dpi = 600, width = 10, height = 4.5, units = "in", bg = "white")
#ggsave(file.path(destino, "serie_temp_umid.png"), plot = g5_caps_descritiva, dpi = 600, width = 10, height = 5, units = "in", bg = "white")
