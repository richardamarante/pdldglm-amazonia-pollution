# ---
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
while (!file.exists("PDLDGLM_Geral.R")) setwd(dirname(getwd()))
source("PDLDGLM_Geral.R")

# --- Geração de Dados e Ajuste ---

set.seed(5)
time1 <- 201

V <- 2; W <- 0.5
m0 <- 20; C0 <- 4

theta <- numeric(time1)
y <- numeric(time1)

theta0 <- rnorm(1, mean = m0, sd = sqrt(C0))

theta[1] <- theta0 + rnorm(1, 0, sqrt(W))
y[1]     <- theta[1] + rnorm(1, 0, sqrt(V))

for (t in 2:time1) {
  theta[t] <- theta[t-1] + rnorm(1, 0, sqrt(W))
  y[t]     <- theta[t] + rnorm(1, 0, sqrt(V))
}
#plot(y, type = "l")

# --- Resultados ---

conhecido <- SimularDLM(
  y = y, 
  theta_true = theta,
  Vt = 2,
  nivel_ic = 0.95,
  deltas = c(0.90, 0.95, 0.99),
  ic_style = "dashed", alpha_ic = 0.45, alpha_lines = 1.00,
  lw_ic = 0.90, lw_main = 1
)
conhecido_grafico <- conhecido$g1_relatorio + coord_cartesian(xlim = c(0,210), ylim = c(0, 28)) # por o diretorio correspondente a essa pasta
#ggsave(file.path("Estudos de Simulação/1- Modelo Linear Dinâmico Normal", "dlm_variancia_conhecida.png"), plot = conhecido_grafico, dpi = 600, width = 12, height = 7, units = "in", bg = "white")

desconhecido <- SimularDLM(
  y = y, 
  theta_true = theta,
  Vt = NULL,
  nivel_ic = 0.95,
  deltas = c(0.90, 0.95, 0.99),
  ic_style = "dashed", alpha_ic = 0.45, alpha_lines = 1.00,
  lw_ic = 0.90, lw_main = 1
)
desconhecido_grafico <- desconhecido$g1_relatorio + coord_cartesian(xlim = c(0,210), ylim = c(0, 28))
#ggsave(file.path("Estudos de Simulação/1- Modelo Linear Dinâmico Normal", "dlm_variancia_desconhecida.png"), plot = desconhecido_grafico, dpi = 600, width = 12, height = 7, units = "in", bg = "white")
