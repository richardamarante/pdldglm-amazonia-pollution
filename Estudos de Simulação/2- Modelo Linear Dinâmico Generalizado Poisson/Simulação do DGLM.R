# --- Trecho de código para carregar automaticamente o PDLDGLM_Geral.R que está na pasta anterior
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
while (!file.exists("PDLDGLM_Geral.R")) setwd(dirname(getwd()))
source("PDLDGLM_Geral.R")

# --- Geração de Dados e Ajuste DGLM Poisson ---
set.seed(5) 
time2 <- 201

V <- NULL
W <- 0.001      # variância de evolução do nível no log; sd ~ 0.141 por passo
m0 <- log(60)   # média inicial de mu perto de 20 contagens
C0 <- 0.2       # variância inicial de theta (log-média);

theta <- numeric(time2)
mu    <- numeric(time2)
y     <- integer(time2)

theta0 <- rnorm(1, mean = m0, sd = sqrt(C0))

theta[1] <- theta0 + rnorm(1, mean = 0, sd = sqrt(W))
mu[1]    <- exp(theta[1])
y[1]     <- rpois(1, lambda = mu[1])

for (t in 2:time2) {
  theta[t] <- theta[t-1] + rnorm(1, 0, sqrt(W))
  
  mu[t]    <- exp(theta[t])
  y[t]     <- rpois(1, lambda = mu[t])
}

Y <- as.numeric(y)  
data <- seq.Date(as.Date("2020-01-01"), by = "day", length.out = length(Y))

ajuste <- SimularDGLM(
  y = y,
  deltas = c(0.90, 0.95, 0.99),
  pred_cred = 0.95,
  m0 = 0,
  C0 = 1000,
  ic_style = "dashed",
  alpha_ic = 0.45,
  alpha_lines = 1.00,
  lw_ic = 0.90,
  lw_main = 1.00
)

g1_relatorio1 <- ajuste$g1_relatorio +
  scale_x_continuous(
    limits = c(0, length(y) - 1),
    breaks = seq(0, length(y) - 1, by = 50),
    expand = ggplot2::expansion(mult = c(0, 0.05))  
  ) +
  coord_cartesian(ylim = c(0, max(y) * 1.10))

# #ggsave(
#   file.path("Estudos de Simulação/2- Modelo Linear Dinâmico Generalizado Poisson", "dglm_poisson.png"),
#   plot = g1_relatorio1,
#   dpi = 600, width = 12, height = 7, units = "in",
#   bg = "white",
#   device = grDevices::png,
#   type = "cairo"
# )
