# ---
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
while (!file.exists("PDLDGLM_Geral.R")) setwd(dirname(getwd()))
source("PDLDGLM_Geral.R")
# ---

destino <- "Aplicações em Dados Reais/Análise Descritiva/Amazônia Legal"
plots_descritiva <- Descritiva(amz, facet_type = "wrap", padronizacao = "max", usar_facets = TRUE)

g1_max_descritiva <- plots_descritiva$resp_pm25
g2_max_descritiva <- plots_descritiva$clima_pm25
g3_max_descritiva <- plots_descritiva$tudo
g4_pm_descritiva <- plots_descritiva$serie$pm25
g4_temp_descritiva <- plots_descritiva$serie$temp
g4_umid_descritiva <- plots_descritiva$serie$umid
g4_hosp_descritiva <- plots_descritiva$serie$hosp
g5_max_descritiva <- plots_descritiva$temp_umid

#ggsave(file.path(destino, "serie_resp_pm25.png"), plot = g1_max_descritiva, dpi = 600, width = 10, height = 6, units = "in", bg = "white")
#ggsave(file.path(destino, "serie_temp_umid_pm25.png"), plot = g2_max_descritiva, dpi = 600, width = 10, height = 6, units = "in", bg = "white")
#ggsave(file.path(destino, "serie_resp_pm25_temp_umid.png"), plot = g3_max_descritiva, dpi = 600, width = 10, height = 6, units = "in", bg = "white")
#ggsave(file.path(destino, "serie_individual_pm25.png"), plot = g4_pm_descritiva, dpi = 600, width = 10, height = 6, units = "in", bg = "white")
#ggsave(file.path(destino, "serie_individual_temp.png"), plot = g4_temp_descritiva, dpi = 600, width = 10, height = 6, units = "in", bg = "white")
#ggsave(file.path(destino, "serie_individual_umid.png"), plot = g4_umid_descritiva, dpi = 600, width = 10, height = 6, units = "in", bg = "white")
#ggsave(file.path(destino, "serie_individual_hosp.png"), plot = g4_hosp_descritiva, dpi = 600, width = 10, height = 6, units = "in", bg = "white")
#ggsave(file.path(destino, "serie_temp_umid.png"), plot = g5_max_descritiva, dpi = 600, width = 10, height = 6, units = "in", bg = "white")
