# ---
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
while (!file.exists("PDLDGLM_Geral.R")) setwd(dirname(getwd()))
source("PDLDGLM_Geral.R")

# --- Simulação 1 ---

simulacao1 <- SimularPDLDGLM(
  seed = 94,
  x0 = 30, tau2 = 10, lags = 16,
  alpha1 = 1.80, W_alpha = 0.002,
  d = 3, eta = c(5.900, -0.540, 0.028, -0.0013) / 1600,
  d1_level = 0.99, fit_R1_level = 10, fit_R1_reg = 10 # 2 ambos
)

simulacao1$curva$g1_d3
simulacao1$curva$g2_d3

#ggsave(file.path("Estudos de Simulação/3- Modelo Linear Dinâmico Generalizado com Defasagem Polinomial", "pdldglm_curva1_g1.png"), plot = simulacao1$curva$g1_d3,
#       dpi = 600, width = 12, height = 7, units = "in", bg = "white")

#ggsave(file.path("Estudos de Simulação/3- Modelo Linear Dinâmico Generalizado com Defasagem Polinomial", "pdldglm_curva1_g2.png"), plot = simulacao1$curva$g2_d3,
#       dpi = 600, width = 12, height = 6.5, units = "in", bg = "white")

# --- Simulação 2 ---

simulacao2 <- SimularPDLDGLM(
  seed = 82, seed_curva = 82,
  x0 = 30, tau2 = 10, lags = 16,
  alpha1 = 1.80, W_alpha = 0.002,
  d = 2, eta = c(-8.47, 5.17, -0.36) / 1600,
  d1_level = 0.99, fit_R1_level = 10, fit_R1_reg = 10
)

simulacao2$curva$g1_d2
simulacao2$curva$g2_d2
simulacao2$curva$g1_d3
simulacao2$curva$g2_d3

# Estimativas a posteriori dos etas e métricas para comparação
data.frame(
  Modelo = c("Com d = 2", "Com d = 3"),
  LVP    = c(simulacao2$curva$metricas_d2$LVP,  simulacao2$curva$metricas_d3$LVP),
  MASE   = c(simulacao2$curva$metricas_d2$MASE, simulacao2$curva$metricas_d3$MASE),
  IS     = c(simulacao2$curva$metricas_d2$IS,   simulacao2$curva$metricas_d3$IS)
)

simulacao2$curva$ajustes$d2$thetaR_tab
simulacao2$curva$ajustes$d3$thetaR_tab

#ggsave(file.path("Estudos de Simulação/3- Modelo Linear Dinâmico Generalizado com Defasagem Polinomial", "pdldglm_curva2_g1_d2.png"),
#       plot = simulacao2$curva$g1_d2, dpi = 600, width = 12, height = 7, units = "in", bg = "white")

#ggsave(file.path("Estudos de Simulação/3- Modelo Linear Dinâmico Generalizado com Defasagem Polinomial", "pdldglm_curva2_g2_d2.png"),
#       plot = simulacao2$curva$g2_d2, dpi = 600, width = 12, height = 6.5, units = "in", bg = "white")

#ggsave(file.path("Estudos de Simulação/3- Modelo Linear Dinâmico Generalizado com Defasagem Polinomial", "pdldglm_curva2_g1_d3.png"),
#       plot = simulacao2$curva$g1_d3, dpi = 600, width = 12, height = 7, units = "in", bg = "white")

#ggsave(file.path("Estudos de Simulação/3- Modelo Linear Dinâmico Generalizado com Defasagem Polinomial", "pdldglm_curva2_g2_d3.png"),
#       plot = simulacao2$curva$g2_d3, dpi = 600, width = 12, height = 6.5, units = "in", bg = "white")

# --- Simulação 3 ---

simulacao3 <- SimularPDLDGLM(
  seed = 82,
  x0 = 30, tau2 = 10, lags = 16,
  alpha1 = 1.80, W_alpha = 0.002,
  d = 3, eta = c(-1.663, 0.506, 0.143, -0.011) / 1600,
  d1_level = 0.99, fit_R1_level = 10, fit_R1_reg = 10
)

simulacao3$curva$g1_d3
simulacao3$curva$g2_d3

#ggsave(file.path("Estudos de Simulação/3- Modelo Linear Dinâmico Generalizado com Defasagem Polinomial", "pdldglm_curva3_g1.png"), plot = simulacao3$curva$g1_d3,
#       dpi = 600, width = 12, height = 7, units = "in", bg = "white")

#ggsave(file.path("Estudos de Simulação/3- Modelo Linear Dinâmico Generalizado com Defasagem Polinomial", "pdldglm_curva3_g2.png"), plot = simulacao3$curva$g2_d3,
#       dpi = 600, width = 12, height = 6.5, units = "in", bg = "white")
