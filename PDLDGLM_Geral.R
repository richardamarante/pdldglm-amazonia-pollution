# ==============================================================================
# Essa aqui é a minha mini-biblioteca pessoal do projeto. 
# Ela concentra toda a parte de modelagem aplicada, as simulações, a preparação dos dados, alem das funções 
# utilizadas para gerar os gráficos de análise descritiva de forma mais prática.
# 
# A ideia é que seja possível simplesmente dar um source() nesse arquivo nos outros scripts, de modo a evitar 
# repetir código desnecessariamente e garantir que os resultados mantenham-se consistentes e personalizáveis.
# ==============================================================================

library(dplyr)
library(tidyr)
library(kDGLM)
library(ggplot2)
library(MASS)
library(ggthemes)
library(patchwork)

PDLDGLM <- function(Y, X, data, lags, d, fd_nivel = 0.98,
                    padronizar_center = FALSE, padronizar_dp = TRUE,
                    n_amostras = 1000) {
  
  ## --- 1) Construção da matriz de lags da covariável X ---
  n_total <- length(Y) # tamanho total da série Y
  
  q <- lags + 1 # número de colunas da matriz de lags (lag 0 até lags)
  X_mat <- matrix(0, nrow = n_total, ncol = q) # Inicialização da nossa matriz de lags
  X_mat[, 1] <- X # Primeira coluna: lag 0, que é o próprio X
  
  for (j in 2:q) {
    X_mat[, j] <- dplyr::lag(X, j - 1) # Coluna seguintes: X defasado em 1, 2, ..., (q-1) lags
  }
  
  X_mat <- X_mat[q:n_total, ] # removemos as primeiras (q-1) linhas vazias (provenientes do loop anterior)
  
  Y <- Y[q:n_total] # removemos as primeiras (q-1) observações de Y para alinhar com nossa X_mat
  n_efetivo <- length(Y) # tamanho efetivo de Y após o alinhamento
  
  ## --- 2) Calculando as somas de Almon a partir da matriz de lags (com grau d, dado na função) ---
  v <- 1:lags # são os índices 1, 2, ..., lags a ser utilizados naquele somatório das fórmulas de S0, S1, S2, S3
  
  # Sk(t) = somatório de j=1 até lags de [ (j^k) * X_{t-j} ], para k = 0, 1, ..., d
  S0 <- rowSums(X_mat) 
  S1 <- X_mat[, -1] %*%  v # X_mat[, -1] para considerar todas as linhas e colunas, menos a primeira coluna (do lag 0)
  S2 <- X_mat[, -1] %*% (v^2)
  
  if (d == 2) {
    S_bruto <- data.frame(S0, S1, S2)
  } else { # se d == 3, inclui também o S3
    S3 <- X_mat[, -1] %*% (v^3)
    
    S_bruto <- data.frame(S0, S1, S2, S3)
  }
  
  ## --- 3) Padronização das Sk (opcional, depende do que a pessoa marcar nos parâmetros) ---
  
  # (0) Caso ela não padronize nada, vamos seguir com isso aqui. 
  # Os valores abaixo são só pq, mais pra frente, vamos desfazer a padronização das amostras dos etas e, lá, dividimos por S_dp.
  S_dp           <- rep(1, ncol(S_bruto)) # define S_dp = 1 para cada coluna por padrão
  center_vec     <- FALSE # não centraliza por padrão
  S_padronizado  <- S_bruto # mantém igual por padrão
  
  # (1) Se pediu para padronizar pelo desvio-padrão, calcule os dps de cada coluna
  if (padronizar_dp) {
    S_dp <- apply(S_bruto, 2, sd)                           # calculando o desvio padrão de cada coluna do S_bruto
    S_dp[!is.finite(S_dp) | S_dp < 1e-6] <- 1e-6            # Pra evitar dividir por zero/NA/Inf, coloquei um número bem pequeno
  }
  
  # (2) Se pediu para centralizar, calcule as médias
  if (padronizar_center) {
    center_vec <- colMeans(S_bruto)                         # calculando a média de cada coluna do S_bruto
  }
  
  # (3) Se qualquer uma das opções foi ativada, aplica scale() com as escolhas acima
  if (padronizar_center || padronizar_dp) {
    S_padronizado <- as.data.frame(
      scale(S_bruto, center = center_vec, scale = S_dp)     # subtraindo pela média e/ou dividindo pelo desvio padrão
    )
  }
  
  ## --- 4) Modelo kDGLM ---
  # bloco de nível com fator de desconto fd_nivel
  nivel <- polynomial_block(rate = 1, order = 1, D = fd_nivel, name = "Nivel")
  
  # blocos de regressão para S0, S1, ..., Sd (com os coeficientes tratados como constantes no tempo, por isso D = 1)
  b0 <- regression_block(rate = S_padronizado$S0, D = 1, name = "S0")
  b1 <- regression_block(rate = S_padronizado$S1, D = 1, name = "S1")
  b2 <- regression_block(rate = S_padronizado$S2, D = 1, name = "S2")
  
  if (d == 2) {
    bloco <- (nivel + b0 + b1 + b2)
  } else { # se d == 3, inclui também o bloco de regressão para o S3
    b3 <- regression_block(rate = S_padronizado$S3, D = 1, name = "S3")
    bloco <- (nivel + b0 + b1 + b2 + b3)
  }
  
  desfecho <- Poisson(lambda = "rate", data = Y)
  ajuste <- fit_model(
    bloco, 
    y = desfecho)
  
  coefs <- coef(ajuste, lag = -1, eval.pred = TRUE, eval.metric = TRUE, pred.cred  = 0.95)
  
  ## --- 5) Pegando a média estimada a cada tempo ---
  if (!is.null(coefs$ft)) { # como os nomes podem variar dependendo da versão do kDGLM, eu peguei ambos: ft/Qt ou lambda.mean/lambda.cov)
    eta_media_t <- as.numeric(coefs$ft[1, ]) # É o nosso preditor linear η a cada tempo t, ou seja, nosso log(u_t)
    eta_dp_t    <- sqrt(as.numeric(coefs$Qt[1, 1, ])) # desvio padrão pra calcular os intervalos de credibilidade
  } else {
    eta_media_t <- as.numeric(coefs$lambda.mean[1, ]) # É o nosso preditor linear η a cada tempo t, ou seja, nosso log(u_t)
    eta_dp_t    <- sqrt(as.numeric(coefs$lambda.cov[1, 1, ])) # desvio padrão pra calcular os intervalos de credibilidade
  }
  
  mu_estimada   <- exp(eta_media_t) # Exponenciando o log(u_t) pra tirar da escala log
  mu_ic_inf    <- exp(eta_media_t - 1.96 * eta_dp_t)
  mu_ic_sup    <- exp(eta_media_t + 1.96 * eta_dp_t)
  
  ## --- 6) Reconstruindo os betas de cada lag via amostragem bayesiana dos η no tempo final ---
  if (d == 2) {
    indice <- 2:4
  } else {
    indice <- 2:5
  }
  
  estados_media <- coefs$theta.mean # E[θ_t | dados] (Vetor de estados, que tem aquele nível dinâmico + os regressores, no formato das somas Sk)
  estados_covar <- coefs$theta.cov  # Cov[θ_t | dados] (Matriz de covariância desses estados)
  
  etaS_media_T_padronizado <- estados_media[indice, n_efetivo]
  cov_eta_T_padronizado    <- estados_covar[indice, indice, n_efetivo]
  
  # Se n_amostras <= 0, pula a amostragem bayesiana e usa estimativa pontual (com a posteriori da média e covariância)
  if (is.null(n_amostras) || n_amostras <= 0) {
    # Desfazendo a padronização das S_k
    etaS_media_T <- etaS_media_T_padronizado / S_dp[seq_along(indice)]
    
    # Reconstruindo a matriz Cj de Almon
    j <- 0:(q - 1)
    if (d == 2) {
      Cj <- cbind(1, j, j^2)
    } else {
      Cj <- cbind(1, j, j^2, j^3)
    }
    
    beta_pt <- as.numeric(etaS_media_T %*% t(Cj))
    rr_beta_estimado    <- exp(sd(X[q:n_total]) * beta_pt)
    rr_ic_inferior_beta <- rep(NA, q)
    rr_ic_superior_beta <- rep(NA, q)
    
  } else {
    # Amostragem multivariada utilizando a posteriori no tempo final T
    amostras_etaS_padronizado <- MASS::mvrnorm(
      n = n_amostras,
      mu = etaS_media_T_padronizado,
      Sigma = cov_eta_T_padronizado
    )
    
    # Desfazendo a padronização das S_k (se padronizar for false, o S_dp é 1 e nada muda)
    amostras_etaS <- scale(amostras_etaS_padronizado, center = FALSE, scale = S_dp)
    
    # Reconstruindo os Betas para cada lag
    j <- 0:(q - 1)
    
    if (d == 2) {
      Cj <- cbind(1, j, j^2)     # [1, j, j^2]
    } else {
      Cj <- cbind(1, j, j^2, j^3) # [1, j, j^2, j^3]
    }
    
    amostras_beta <- amostras_etaS %*% t(Cj)
    rr_amostras_beta <- exp(sd(X[q:n_total]) * amostras_beta) # transforma cada amostra para a escala de Risco Relativo por +1 DP na regressora
    # Estimativas Pontuais
    rr_beta_estimado    <- colMeans(rr_amostras_beta)
    rr_ic_inferior_beta <- apply(rr_amostras_beta, 2, quantile, probs = 0.025)
    rr_ic_superior_beta <- apply(rr_amostras_beta, 2, quantile, probs = 0.975)
  }
  
  ## --- 7) Gráficos ---
  
  # g1: Y observado vs mu estimado
  t_index <- as.Date(data[q:n_total])
  df_y <- data.frame(
    t_index    = t_index,
    Y          = Y,
    y_estimado = mu_estimada,
    mu_ic_inf  = mu_ic_inf,
    mu_ic_sup  = mu_ic_sup 
  )
  
  # eixos, rótulos e espessura da linha adaptativo ao tamanho da série temporal que colocar
  dt_min <- min(df_y$t_index)
  dt_max <- max(df_y$t_index)
  span_days <- as.integer(dt_max - dt_min)
  span_years <- as.numeric(difftime(dt_max, dt_min, units = "days")) / 365.25
  
  if (span_days <= 366) {
    by <- "1 month";   formato <- "mes"
  } else if (span_days <= 2 * 366) {
    by <- "2 months";  formato <- "mes_ano"
  } else if (span_days <= 4 * 366) {
    by <- "3 months";  formato <- "mes_ano"
  } else if (span_days <= 6 * 366) {
    by <- "6 months";  formato <- "mes_ano"
  } else {
    by <- "1 year";    formato <- "ano"
  }
  
  breaks_seq <- seq(dt_min, dt_max, by = by)
  lw <- ifelse(span_years <= 6, 1.00, 0.90)
  
  mes_pt <- c("01" = "Jan", "02" = "Fev", "03" = "Mar", "04" = "Abr", "05" = "Mai", "06" = "Jun",
              "07" = "Jul", "08" = "Ago", "09" = "Set", "10" = "Out", "11" = "Nov", "12" = "Dez")
  
  lab_fun <- function(d) {
    m <- mes_pt[format(d, "%m")]
    y <- format(d, "%Y")
    
    if (formato == "mes") return(m)
    if (formato == "ano") return(y)
    return(paste0(m, "-", y))
  }
  
  g1 <- ggplot(df_y, aes(x = t_index)) +
    geom_line(aes(y = Y, colour = "Dados"), linewidth = lw) +
    geom_line(aes(y = y_estimado, colour = "Estimativas"), linewidth = lw) +
    geom_ribbon(aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    geom_hline(yintercept = 0) +
    labs(
      x = "Data",
      y = "Internações",
      colour = "",
      fill   = ""
    ) +
    theme_hc() +
    theme(
      axis.title.x   = element_text(size = 35),
      axis.text      = element_text(size = 35),
      axis.text.x    = element_text(size = 35, angle = 90),
      legend.text    = element_text(size = 30),
      legend.title   = element_blank(),
      legend.position = "bottom",
      axis.title.y   = element_text(size = 35)
    ) +
    scale_color_manual(
      values = c("Dados" = "black", "Estimativas" = "red"),
      breaks = c("Estimativas", "Dados"),
      labels = c("Estimativas", "Observações")
    ) +
    scale_fill_manual(values = c("IC 95%" = "blue")) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    ) +
    guides(
      fill   = guide_legend(order = 1),  # IC 95% primeiro
      colour = guide_legend(order = 2)   # depois Estimativas / Observações
    )
  
  # g2: Beta(j)
  df_beta <- data.frame(
    lag = 0:(q - 1),
    rr = as.numeric(rr_beta_estimado),
    rr_lo = as.numeric(rr_ic_inferior_beta),
    rr_hi = as.numeric(rr_ic_superior_beta)
  )
  
  g2 <- ggplot(df_beta, aes(x = lag, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "cadetblue3", color = "cadetblue3",
                alpha = 0.3, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_point(size = 2.5, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Lags", y = "Risco Relativo (RR)") +
    theme_hc(base_size = 1) +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      panel.grid = element_blank(),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = seq(0, q - 1, by = 1))
  
  return(list(
    mu_media    = mu_estimada,
    mu_ic_inf   = mu_ic_inf,
    mu_ic_sup   = mu_ic_sup,
    beta_media  = rr_beta_estimado,
    beta_ic_inf = rr_ic_inferior_beta,
    beta_ic_sup = rr_ic_superior_beta,
    g1 = g1,
    g2 = g2,
    kdglm_coef  = coefs,
    pred_df  = coefs$data,
    ajuste = ajuste
  ))
}





PDLDGLM_clima <- function(
    Y, X, covar, data,
    lags, lag_covar, d,
    perc, lado = c("acima","abaixo"),
    fd_nivel = 0.98,
    padronizar_center = FALSE,
    padronizar_dp     = TRUE,
    n_amostras        = 1000
) {
  lado <- match.arg(lado)
  
  ## --- 1) Construção da matriz de lags da covariável X ---
  n_total <- length(Y) # tamanho total da série Y
  
  q <- lags + 1 # número de colunas da matriz de lags (lag 0 até lags)
  X_mat <- matrix(0, nrow = n_total, ncol = q) # Inicialização da nossa matriz de lags
  X_mat[, 1] <- X # Primeira coluna: lag 0, que é o próprio X
  
  for (j in 2:q) {
    X_mat[, j] <- dplyr::lag(X, j - 1) # Coluna seguintes: X defasado em 1, 2, ..., (q-1) lags
  }
  
  ## --- 1b) Indicadora climática H_t (0/1) e defasagem da covariável ---
  ini <- max(lags + 1, lag_covar + 1) # índice inicial comum (pra remover o "burn-in")
  
  corte <- as.numeric(quantile(covar[ini:n_total], probs = perc, na.rm = TRUE))
  H_raw <- if (lado == "acima") as.numeric(covar > corte) else as.numeric(covar < corte)
  H_def <- dplyr::lag(H_raw, lag_covar) # aplica a defasagem pedida
  
  ## --- 1c) Alinhamento conjunto (poluente com lags e covariável defasada) ---
  X_mat <- X_mat[ini:n_total, ] # removemos as primeiras linhas vazias
  Y     <- Y[ini:n_total]       # removemos as primeiras observações de Y para alinhar
  H_def <- H_def[ini:n_total]   # covariável já alinhada
  n_efetivo <- length(Y)        # tamanho efetivo de Y após o alinhamento
  
  ## --- 2) Calculando as somas de Almon a partir da matriz de lags (com grau d, dado na função) ---
  v <- 1:lags # são os índices 1, 2, ..., lags a ser utilizados naquele somatório das fórmulas de S0, S1, S2, S3
  
  # Sk(t) = somatório de j=1 até lags de [ (j^k) * X_{t-j} ], para k = 0, 1, ..., d
  S0 <- rowSums(X_mat)
  S1 <- X_mat[, -1] %*%  v # X_mat[, -1] para considerar todas as linhas e colunas, menos a primeira coluna (do lag 0)
  S2 <- X_mat[, -1] %*% (v^2)
  
  if (d == 2) {
    S_bruto <- data.frame(S0, S1, S2)
  } else { # se d == 3, inclui também o S3
    S3 <- X_mat[, -1] %*% (v^3)
    
    S_bruto <- data.frame(S0, S1, S2, S3)
  }
  
  ## --- 3) Padronização das Sk (opcional, depende do que a pessoa marcar nos parâmetros) ---
  
  # (0) Caso ela não padronize nada, vamos seguir com isso aqui.
  # Os valores abaixo são só pq, mais pra frente, vamos desfazer a padronização das amostras dos etas e, lá, dividimos por S_dp.
  S_dp           <- rep(1, ncol(S_bruto)) # define S_dp = 1 para cada coluna por padrão
  center_vec     <- FALSE # não centraliza por padrão
  S_padronizado  <- S_bruto # mantém igual por padrão
  
  # (1) Se pediu para padronizar pelo desvio-padrão, calcule os dps de cada coluna
  if (padronizar_dp) {
    S_dp <- apply(S_bruto, 2, sd)                           # calculando o desvio padrão de cada coluna do S_bruto
    S_dp[!is.finite(S_dp) | S_dp < 1e-6] <- 1e-6            # Pra evitar dividir por zero/NA/Inf, coloquei um número bem pequeno
  }
  
  # (2) Se pediu para centralizar, calcule as médias
  if (padronizar_center) {
    center_vec <- colMeans(S_bruto)                         # calculando a média de cada coluna do S_bruto
  }
  
  # (3) Se qualquer uma das opções foi ativada, aplica scale() com as escolhas acima
  if (padronizar_center || padronizar_dp) {
    S_padronizado <- as.data.frame(
      scale(S_bruto, center = center_vec, scale = S_dp)     # subtraindo pela média e/ou dividindo pelo desvio padrão
    )
  }
  
  ## --- 4) Modelo kDGLM ---
  # bloco de nível com fator de desconto fd_nivel
  nivel <- polynomial_block(rate = 1, order = 1, D = fd_nivel, name = "Nivel")
  
  # blocos de regressão para S0, S1, ..., Sd (com os coeficientes tratados como constantes no tempo, por isso D = 1)
  b0 <- regression_block(rate = S_padronizado$S0, D = 1, name = "S0")
  b1 <- regression_block(rate = S_padronizado$S1, D = 1, name = "S1")
  b2 <- regression_block(rate = S_padronizado$S2, D = 1, name = "S2")
  
  if (d == 2) {
    # bloco da covariável climática indicadora (constante no tempo: D = 1)
    h  <- regression_block(rate = H_def, D = 1, name = "H")
    bloco <- (nivel + b0 + b1 + b2 + h)
  } else { # se d == 3, inclui também o bloco de regressão para o S3
    b3 <- regression_block(rate = S_padronizado$S3, D = 1, name = "S3")
    h  <- regression_block(rate = H_def, D = 1, name = "H")
    bloco <- (nivel + b0 + b1 + b2 + b3 + h)
  }
  
  desfecho <- Poisson(lambda = "rate", data = Y)
  ajuste <- fit_model(
    bloco,
    y = desfecho)
  
  coefs <- coef(ajuste, lag = -1, eval.pred = TRUE, eval.metric = TRUE, pred.cred  = 0.95)
  
  ## --- 5) Pegando a média estimada a cada tempo ---
  if (!is.null(coefs$ft)) { # como os nomes podem variar dependendo da versão do kDGLM: ft/Qt ou lambda.mean/lambda.cov)
    eta_media_t <- as.numeric(coefs$ft[1, ]) # É o nosso preditor linear η a cada tempo t, ou seja, nosso log(u_t)
    eta_dp_t    <- sqrt(as.numeric(coefs$Qt[1, 1, ])) # desvio padrão pra calcular os intervalos de credibilidade
  } else {
    eta_media_t <- as.numeric(coefs$lambda.mean[1, ]) # É o nosso preditor linear η a cada tempo t, ou seja, nosso log(u_t)
    eta_dp_t    <- sqrt(as.numeric(coefs$lambda.cov[1, 1, ])) # desvio padrão pra calcular os intervalos de credibilidade
  }
  
  mu_estimada <- exp(eta_media_t) # Exponenciando o log(u_t) pra tirar da escala log
  mu_ic_inf   <- exp(eta_media_t - 1.96 * eta_dp_t)
  mu_ic_sup   <- exp(eta_media_t + 1.96 * eta_dp_t)
  
  ## --- 6.1) Reconstruindo os betas de cada lag via amostragem bayesiana dos η no tempo final ---
  if (d == 2) {
    indice <- 2:4
  } else {
    indice <- 2:5
  }
  
  estados_media <- coefs$theta.mean # E[θ_t | dados] (Vetor de estados, que tem aquele nível dinâmico + os regressores, no formato das somas Sk)
  estados_covar <- coefs$theta.cov  # Cov[θ_t | dados] (Matriz de covariância desses estados)
  
  etaS_media_T_padronizado <- estados_media[indice, n_efetivo]
  cov_eta_T_padronizado    <- estados_covar[indice, indice, n_efetivo]
  
  # Desvio-padrão do X no período efetivo (para a escala de Risco Relativo, como no original)
  sd_X_efetivo <- sd(X[ini:n_total])
  
  # Se n_amostras <= 0, pula a amostragem bayesiana e usa estimativa pontual (com a posteriori da média e covariância)
  if (is.null(n_amostras) || n_amostras <= 0) {
    # Desfazendo a padronização das S_k
    etaS_media_T <- etaS_media_T_padronizado / S_dp[seq_along(indice)]
    
    # Reconstruindo a matriz Cj de Almon
    j <- 0:(q - 1)
    if (d == 2) {
      Cj <- cbind(1, j, j^2)
    } else {
      Cj <- cbind(1, j, j^2, j^3)
    }
    
    beta_pt <- as.numeric(etaS_media_T %*% t(Cj))
    rr_beta_estimado    <- as.numeric(exp(sd_X_efetivo * beta_pt))
    rr_ic_inferior_beta <- rep(NA, q)
    rr_ic_superior_beta <- rep(NA, q)
    
  } else {
    # Amostragem multivariada utilizando a posteriori no tempo final T
    amostras_etaS_padronizado <- MASS::mvrnorm(
      n = n_amostras,
      mu = etaS_media_T_padronizado,
      Sigma = cov_eta_T_padronizado
    )
    
    # Desfazendo a padronização das S_k (se padronizar for false, o S_dp é 1 e nada muda)
    amostras_etaS <- scale(amostras_etaS_padronizado, center = FALSE, scale = S_dp)
    
    # Reconstruindo os Betas para cada lag
    j <- 0:(q - 1)
    
    if (d == 2) {
      Cj <- cbind(1, j, j^2)     # [1, j, j^2]
    } else {
      Cj <- cbind(1, j, j^2, j^3) # [1, j, j^2, j^3]
    }
    
    amostras_beta <- amostras_etaS %*% t(Cj)
    rr_amostras_beta <- exp(sd_X_efetivo * amostras_beta) # transforma cada amostra para a escala de Risco Relativo por +1 DP na regressora
    
    # Estimativas Pontuais
    rr_beta_estimado    <- colMeans(rr_amostras_beta)
    rr_ic_inferior_beta <- apply(rr_amostras_beta, 2, quantile, probs = 0.025)
    rr_ic_superior_beta <- apply(rr_amostras_beta, 2, quantile, probs = 0.975)
  }
  
  ## -- 6.2) Reconstruindo o tau associado ao lag escolhido via amostragem bayesiana dos η no tempo final
  # Lembrando que τ é o coeficiente do bloco H (indicadora climática) no tempo final T.
  
  idx_tau <- if (d == 2) 5 else 6 # No vetor de estados: [Nivel, S0..Sd, H]  => idx_tau = 5 (d=2) ou 6 (d=3)
  
  # Posterior no tempo final T (n_efetivo) para τ (no log)
  tau_media_T <- estados_media[idx_tau, n_efetivo]
  tau_var_T   <- estados_covar[idx_tau, idx_tau, n_efetivo]
  
  if (is.null(n_amostras) || n_amostras <= 0) {
    # Sem amostragem: estimativa pontual + ICs como NA
    rr_tau_estimado    <- exp(as.numeric(tau_media_T))
    rr_ic_inferior_tau <- NA
    rr_ic_superior_tau <- NA
  } else {
    # Amostragem bayesiana univariada de τ ~ Normal(tau_media_T, tau_var_T)
    amostras_tau <- rnorm(n_amostras, mean = as.numeric(tau_media_T), sd = sqrt(as.numeric(tau_var_T)))
    amostras_RR  <- exp(amostras_tau)
    
    # Estimativas pontuais e IC95% no log(τ) e na escala de RR
    rr_tau_estimado <- mean(amostras_RR)
    rr_ic_inferior_tau <- as.numeric(quantile(amostras_RR, probs = 0.025))
    rr_ic_superior_tau <- as.numeric(quantile(amostras_RR, probs = 0.975))
  }
  
  ## --- 7) Gráficos ---
  
  # g1: Y observado vs mu estimado
  t_index <- as.Date(data[ini:n_total])
  df_y <- data.frame(
    t_index    = t_index,
    Y          = Y,
    y_estimado = mu_estimada,
    mu_ic_inf  = mu_ic_inf,
    mu_ic_sup  = mu_ic_sup
  )
  
  # eixos, rótulos e espessura da linha adaptativo ao tamanho da série temporal que colocar
  dt_min <- min(df_y$t_index)
  dt_max <- max(df_y$t_index)
  span_days <- as.integer(dt_max - dt_min)
  span_years <- as.numeric(difftime(dt_max, dt_min, units = "days")) / 365.25
  
  if (span_days <= 366) {
    by <- "1 month";   formato <- "mes"
  } else if (span_days <= 2 * 366) {
    by <- "2 months";  formato <- "mes_ano"
  } else if (span_days <= 4 * 366) {
    by <- "3 months";  formato <- "mes_ano"
  } else if (span_days <= 6 * 366) {
    by <- "6 months";  formato <- "mes_ano"
  } else {
    by <- "1 year";    formato <- "ano"
  }
  
  breaks_seq <- seq(dt_min, dt_max, by = by)
  lw <- ifelse(span_years <= 6, 1.00, 0.90)
  
  mes_pt <- c("01" = "Jan", "02" = "Fev", "03" = "Mar", "04" = "Abr", "05" = "Mai", "06" = "Jun",
              "07" = "Jul", "08" = "Ago", "09" = "Set", "10" = "Out", "11" = "Nov", "12" = "Dez")
  
  lab_fun <- function(d) {
    m <- mes_pt[format(d, "%m")]
    y <- format(d, "%Y")
    
    if (formato == "mes") return(m)
    if (formato == "ano") return(y)
    return(paste0(m, "-", y))
  }
  
  g1 <- ggplot(df_y, aes(x = t_index)) +
    geom_line(aes(y = Y,          colour = "Dados"),       linewidth = lw) +
    geom_line(aes(y = y_estimado, colour = "Estimativas"), linewidth = lw) +
    geom_ribbon(aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    geom_hline(yintercept = 0) +
    labs(
      x = "Data",
      y = "Internações", 
      colour = "",
      fill   = ""
    ) +
    theme_hc() +
    theme(
      axis.title.x    = element_text(size = 35),
      axis.text       = element_text(size = 35),
      axis.text.x     = element_text(size = 35, angle = 90),
      legend.text     = element_text(size = 30),
      legend.title    = element_blank(),
      legend.position = "bottom",
      axis.title.y    = element_text(size = 35)
    ) +
    scale_color_manual(
      values = c("Dados" = "black", "Estimativas" = "red"),
      breaks = c("Estimativas", "Dados"),
      labels = c("Estimativas", "Observações")
    ) +
    scale_fill_manual(values = c("IC 95%" = "blue")) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    ) +
    guides(
      fill   = guide_legend(order = 1),
      colour = guide_legend(order = 2)
    )
  
  # g2: Beta(j)
  df_beta <- data.frame(
    lag = 0:(q - 1),
    rr = as.numeric(rr_beta_estimado),
    rr_lo = as.numeric(rr_ic_inferior_beta),
    rr_hi = as.numeric(rr_ic_superior_beta)
  )
  
  g2 <- ggplot(df_beta, aes(x = lag, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "cadetblue3", color = "cadetblue3",
                alpha = 0.3, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_point(size = 2.5, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Lags", y = "Risco Relativo (RR)") +
    theme_hc(base_size = 1) +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      panel.grid = element_blank(),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = seq(0, q - 1, by = 1))
  
  return(list(
    mu_media    = mu_estimada,
    mu_ic_inf   = mu_ic_inf,
    mu_ic_sup   = mu_ic_sup,
    beta_media  = rr_beta_estimado,
    beta_ic_inf = rr_ic_inferior_beta,
    beta_ic_sup = rr_ic_superior_beta,
    tau_media   = rr_tau_estimado,
    tau_ic_inf  = rr_ic_inferior_tau,
    tau_ic_sup  = rr_ic_superior_tau,
    g1 = g1,
    g2 = g2,
    kdglm_coef  = coefs,
    pred_df     = coefs$data
  ))
}





SimularDLM <- function(
    y,
    deltas      = c(0.90, 0.95, 0.99),
    Vt          = NULL,
    m0          = 0,
    C0          = 1000,
    n0          = 2,
    S0          = 1,
    nivel_ic    = 0.80,
    ic_style    = c("dashed", "ribbon", "none"),
    alpha_ic    = 0.12,
    alpha_lines = 1.00,
    lw_main     = 1.20,
    lw_ic       = 1.00,
    theta_true
) {
  
  ic_style <- match.arg(ic_style)
  
  ## --- 1) Inicialização da série ---
  Y <- as.numeric(y)
  n <- length(Y)
  t_idx <- 0:(n - 1)
  
  ## --- 2) Filtro de Kalman (V Conhecido e V Desconhecido) ---
  filtro_V_conhecido <- function(y, delta, m0, C0, Vt) {
    Tn <- length(y); D <- 1 / delta
    at <- numeric(Tn); Rt <- numeric(Tn); mt <- numeric(Tn)
    Ct <- numeric(Tn); ft <- numeric(Tn); Qt <- numeric(Tn)
    et <- numeric(Tn); At <- numeric(Tn)
    
    at[1] <- m0; Rt[1] <- C0 * D
    ft[1] <- at[1]; Qt[1] <- Rt[1] + Vt
    At[1] <- Rt[1] / Qt[1]
    et[1] <- y[1] - ft[1]
    mt[1] <- at[1] + At[1] * et[1]
    Ct[1] <- Rt[1] - (At[1]^2) * Qt[1]
    
    for (t in 2:Tn) {
      at[t] <- mt[t - 1]
      Rt[t] <- Ct[t - 1] * D
      ft[t] <- at[t]
      Qt[t] <- Rt[t] + Vt
      At[t] <- Rt[t] / Qt[t]
      et[t] <- y[t] - ft[t]
      mt[t] <- at[t] + At[t] * et[t]
      Ct[t] <- Rt[t] - (At[t]^2) * Qt[t]
    }
    list(at=at,Rt=Rt,ft=ft,Qt=Qt,mt=mt,Ct=Ct,et=et,At=At)
  }
  
  filtro_V_desconhecido <- function(y, delta, m0, C0, n0, S0) {
    Tn <- length(y); D <- 1 / delta
    at <- numeric(Tn); Rt <- numeric(Tn); mt <- numeric(Tn)
    Ct <- numeric(Tn); ft <- numeric(Tn); Qt <- numeric(Tn)
    et <- numeric(Tn); At <- numeric(Tn)
    nt <- numeric(Tn); St <- numeric(Tn)
    
    at[1] <- m0; Rt[1] <- C0 * D
    ft[1] <- at[1]; Qt[1] <- Rt[1] + S0
    At[1] <- Rt[1] / Qt[1]
    et[1] <- y[1] - ft[1]
    nt[1] <- n0 + 1
    St[1] <- S0 + (S0 / nt[1]) * (((et[1]^2) / Qt[1]) - 1)
    mt[1] <- at[1] + At[1] * et[1]
    Ct[1] <- (St[1] / S0) * (Rt[1] - (At[1]^2) * Qt[1])
    
    for (t in 2:Tn) {
      at[t] <- mt[t - 1]
      Rt[t] <- Ct[t - 1] * D
      ft[t] <- at[t]
      Qt[t] <- Rt[t] + St[t - 1]
      At[t] <- Rt[t] / Qt[t]
      et[t] <- y[t] - ft[t]
      nt[t] <- nt[t - 1] + 1
      St[t] <- St[t - 1] + (St[t - 1] / nt[t]) * (((et[t]^2) / Qt[t]) - 1)
      mt[t] <- at[t] + At[t] * et[t]
      Ct[t] <- (St[t] / St[t - 1]) * (Rt[t] - (At[t]^2) * Qt[t])
    }
    list(at=at,Rt=Rt,ft=ft,Qt=Qt,mt=mt,Ct=Ct,et=et,At=At,nt=nt,St=St)
  }
  
  ## --- 3) Ajuste dos modelos para cada Fator de Desconto (delta) ---
  ajustes <- list()
  lista_pred <- list()
  
  for (i in seq_along(deltas)) {
    if (!is.null(Vt)) {
      ajustes[[i]] <- filtro_V_conhecido(Y, deltas[i], m0, C0, Vt)
    } else {
      ajustes[[i]] <- filtro_V_desconhecido(Y, deltas[i], m0, C0, n0, S0)
    }
  }
  
  ## --- 4) Extração das estimativas e Intervalos de Credibilidade ---
  alpha <- 1 - nivel_ic
  
  for (i in seq_along(deltas)) {
    ft <- ajustes[[i]]$ft
    Qt <- ajustes[[i]]$Qt
    
    if (!is.null(Vt)) {
      valor_critico <- qnorm(1 - alpha/2)
    } else {
      nt <- ajustes[[i]]$nt
      gl_pred <- numeric(n)
      gl_pred[1] <- n0
      if (n >= 2) gl_pred[2:n] <- nt[1:(n - 1)]
      gl_pred[gl_pred < 1] <- 1
      valor_critico <- qt(1 - alpha/2, df = gl_pred)
    }
    
    lim_inf <- ft - valor_critico * sqrt(Qt)
    lim_sup <- ft + valor_critico * sqrt(Qt)
    
    lista_pred[[i]] <- data.frame(
      t = t_idx, delta = deltas[i], ft = ft, lim_inf = lim_inf, lim_sup = lim_sup
    )
  }
  
  df_predicoes <- do.call(rbind, lista_pred)
  
  ## --- 5) Configuração de cores e legendas ---
  cores_base <- c("#1F78B4", "#984EA3", "#F1B514")
  if (length(deltas) > length(cores_base)) cores_base <- rainbow(length(deltas))
  
  chaves_delta <- paste0("d", formatC(deltas, format="f", digits=2))
  df_predicoes$chave_delta <- factor(paste0("d", formatC(df_predicoes$delta, format="f", digits=2)),
                                     levels = chaves_delta)
  
  mapa_cores <- setNames(cores_base[seq_along(deltas)], chaves_delta)
  
  # Substituição da regex do C++ pela formatação de decimais nativa do R
  texto_labels_delta <- paste0('delta[theta]~"= ', formatC(deltas, format = "f", digits = 2, dec = ","), '"')
  mapa_labels <- setNames(parse(text = texto_labels_delta), chaves_delta)
  
  df_y <- data.frame(t = t_idx, Y = Y)
  
  ## --- 6) Gráficos ---
  p <- ggplot() +
    geom_line(
      data = df_y,
      aes(x = t, y = Y, linetype = "Observações"),
      colour = "black",
      linewidth = lw_main
    ) +
    geom_line(
      data = df_predicoes,
      aes(x = t, y = ft, colour = chave_delta),
      linewidth = lw_main,
      alpha = alpha_lines,
      key_glyph = "point"
    )
  
  if (ic_style == "ribbon") {
    p <- p + geom_ribbon(
      data = df_predicoes,
      aes(x = t, ymin = lim_inf, ymax = lim_sup, fill = chave_delta),
      alpha = alpha_ic,
      show.legend = FALSE
    )
  } else if (ic_style == "dashed") {
    p <- p +
      geom_line(
        data = df_predicoes,
        aes(x = t, y = lim_inf, colour = chave_delta),
        linewidth = lw_ic,
        linetype = "dashed",
        alpha = alpha_ic,
        show.legend = FALSE
      ) +
      geom_line(
        data = df_predicoes,
        aes(x = t, y = lim_sup, colour = chave_delta),
        linewidth = lw_ic,
        linetype = "dashed",
        alpha = alpha_ic,
        show.legend = FALSE
      )
  }
  
  g1_relatorio <- p +
    geom_hline(yintercept = 0) +
    theme_hc() +
    theme(
      axis.title         = element_text(size = 35),
      axis.text          = element_text(size = 30),
      legend.text        = element_text(size = 30),
      legend.title       = element_blank(),
      legend.position    = "bottom",
      legend.box         = "horizontal",
      legend.box.just    = "center",
      legend.box.spacing = unit(0.35, "cm"),
      legend.key.width   = unit(1.0, "cm"),
      legend.key.height  = unit(1.0, "cm")
    ) +
    labs(x = "Tempo", y = "Resposta") +
    scale_linetype_manual(
      values = c("Observações" = "solid")
    ) +
    scale_color_manual(
      values = mapa_cores,
      breaks = chaves_delta,
      labels = unname(mapa_labels)
    ) +
    scale_fill_manual(values = mapa_cores, breaks = chaves_delta, drop = TRUE) +
    scale_x_continuous(
      limits = c(0, n - 1),
      breaks = seq(0, n - 1, by = 50),
      expand = c(0, 0)
    ) +
    guides(
      linetype = guide_legend(
        order = 1,
        nrow  = 1,
        override.aes = list(colour = "black", linewidth = lw_main)
      ),
      colour = guide_legend(
        order = 2,
        nrow  = 1,
        override.aes = list(shape = 15, size = 6, linewidth = 0, alpha = 1)
      )
    )
  
  g1 <- g1_relatorio + theme(legend.position = "none")
  
  return(list(
    Y            = Y,
    pred_df      = df_predicoes,
    fits         = ajustes,
    g1           = g1,
    g1_relatorio = g1_relatorio,
    params       = list(alpha_lines = alpha_lines, lw_main = lw_main, lw_ic = lw_ic)
  ))
}





SimularDGLM <- function(
    y,
    deltas      = c(0.90, 0.95, 0.99),
    pred_cred   = 0.95,
    m0          = 0,
    C0          = 1000,
    ic_style    = c("dashed", "ribbon", "none"),
    alpha_ic    = 0.45,
    alpha_lines = 1.00,
    lw_main     = 1.00,
    lw_ic       = 0.90
) {
  ic_style <- match.arg(ic_style)
  
  Y <- as.numeric(y)
  n <- length(Y) # tamanho total da série Y
  t_idx <- 0:(n - 1)
  
  z <- qnorm(1 - (1 - pred_cred)/2) # valor critico pra calcular os intervalos de credibilidade
  
  ## --- 1) Modelo kDGLM ---
  fits <- list()
  pred_list <- list()
  
  for (i in seq_along(deltas)) {
    D_i <- deltas[i]
    
    # bloco de nível com fator de desconto D_i
    nivel    <- polynomial_block(rate = 1, order = 1, D = D_i, name = "Nivel", a1 = m0, R1 = C0)
    desfecho <- Poisson(lambda = "rate", data = Y)
    ajuste   <- fit_model(nivel, y = desfecho)
    coefs    <- coef(ajuste, lag = 1, eval.pred = TRUE, eval.metric = TRUE, pred.cred = pred_cred)
    
    ## --- 2) Pegando a média estimada a cada tempo ---
    # como os nomes mudam dependendo da versão do kDGLM, eu considerei ambos ft/Qt ou lambda.mean/lambda.cov
    if (!is.null(coefs$ft) && !is.null(coefs$Qt)) {
      eta_hat <- as.numeric(coefs$ft[1, ]) # É o nosso preditor linear eta a cada tempo t, ou seja, nosso log(u_t)
      eta_sd  <- sqrt(as.numeric(coefs$Qt[1, 1, ])) # desvio padrão pra calcular os intervalos de credibilidade
    } else {
      eta_hat <- as.numeric(coefs$lambda.mean[1, ]) # É o nosso preditor linear eta a cada tempo t, ou seja, nosso log(u_t)
      eta_sd  <- sqrt(as.numeric(coefs$lambda.cov[1, 1, ])) # desvio padrão pra calcular os intervalos de credibilidade
    }
    
    mu_hat <- exp(eta_hat) # Exponenciando o log(u_t) pra tirar da escala log
    mu_lo  <- exp(eta_hat - z * eta_sd)
    mu_hi  <- exp(eta_hat + z * eta_sd)
    
    pred_list[[i]] <- data.frame(
      t = t_idx,
      delta = D_i,
      ft = mu_hat,
      lo = mu_lo,
      hi = mu_hi
    )
    
    fits[[i]] <- list(delta = D_i, ajuste = ajuste, coefs = coefs)
  }
  
  pred_df <- do.call(rbind, pred_list)
  
  base_cols <- c("#1F78B4", "#984EA3", "#F1B514")
  if (length(deltas) > length(base_cols)) base_cols <- rainbow(length(deltas))
  
  delta_keys <- paste0("d", formatC(deltas, format="f", digits=2))
  pred_df$delta_key <- factor(paste0("d", formatC(pred_df$delta, format="f", digits=2)),
                              levels = delta_keys)
  
  col_base <- setNames(base_cols[seq_along(deltas)], delta_keys)
  
  delta_lbl_txt <- paste0('delta[theta]~"= ', formatC(deltas, format = "f", digits = 2, dec = ","), '"')
  lab_map <- setNames(parse(text = delta_lbl_txt), delta_keys)
  
  df_y <- data.frame(t = t_idx, Y = Y)
  
  ## --- 3) Gráficos ---
  
  # g1: Y observado vs mu estimado
  p <- ggplot() +
    geom_line(
      data = df_y,
      aes(x = t, y = Y, linetype = "Observações"),
      colour = "black",
      linewidth = lw_main
    ) +
    geom_line(
      data = pred_df,
      aes(x = t, y = ft, colour = delta_key),
      linewidth = lw_main,
      alpha = alpha_lines,
      key_glyph = "point"
    )
  
  if (ic_style == "ribbon") {
    p <- p + geom_ribbon(
      data = pred_df,
      aes(x = t, ymin = lo, ymax = hi, fill = delta_key),
      alpha = alpha_ic,
      show.legend = FALSE
    )
  } else if (ic_style == "dashed") {
    p <- p +
      geom_line(
        data = pred_df,
        aes(x = t, y = lo, colour = delta_key),
        linewidth = lw_ic,
        linetype = "dashed",
        alpha = alpha_ic,
        show.legend = FALSE
      ) +
      geom_line(
        data = pred_df,
        aes(x = t, y = hi, colour = delta_key),
        linewidth = lw_ic,
        linetype = "dashed",
        alpha = alpha_ic,
        show.legend = FALSE
      )
  }
  
  g1_relatorio <- p +
    geom_hline(yintercept = 0) +
    theme_hc() +
    theme(
      axis.title        = element_text(size = 35),
      axis.text         = element_text(size = 30),
      legend.text       = element_text(size = 30),
      legend.title      = element_blank(),
      legend.position   = "bottom",
      
      legend.box         = "horizontal",
      legend.box.just    = "center",
      legend.box.spacing = unit(0.35, "cm"),
      legend.key.width   = unit(1.0, "cm"),
      legend.key.height  = unit(1.0, "cm")
    ) +
    labs(x = "Tempo", y = "Resposta") +
    scale_linetype_manual(
      values = c("Observações" = "solid")
    ) +
    scale_color_manual(
      values = col_base,
      breaks = delta_keys,
      labels = unname(lab_map)
    ) +
    scale_fill_manual(values = col_base, breaks = delta_keys, drop = TRUE) +
    scale_x_continuous(
      limits = c(0, length(Y) - 1),
      breaks = seq(0, length(Y) - 1, by = 50),
      expand = c(0, 0)
    ) +
    guides(
      linetype = guide_legend(
        order = 1,
        nrow = 1,
        override.aes = list(colour = "black", linewidth = lw_main)
      ),
      colour = guide_legend(
        order = 2,
        nrow = 1,
        override.aes = list(shape = 15, size = 6, linewidth = 0, alpha = 1)
      )
    )
  
  list(
    Y = Y,
    pred_df = pred_df,
    fits = fits,
    g1_relatorio = g1_relatorio,
    params = list(alpha_lines = alpha_lines, lw_main = lw_main, lw_ic = lw_ic)
  )
}





SimularPDLDGLM <- function(
  # Parâmetros da defasagem polinomial
  seed = 81, seed_curva = NULL,
  n_total = 365,
  x0 = 25,
  tau2 = 10,
  lags = 16,
  pred_cred = 0.95,
  n_amostras = 4000,
  
  # Parâmetros da geração dos dados do dglm
  alpha1 = 1.80,   # Média do alpha_1 no log
  W_alpha = 0.002, # Variância da evolução do alpha_t no log
  
  # Curva polinomial, grau do polinômio de defasagem e fator de desconto escolhido
  d = 3,
  eta, 
  d1_level = 0.99,
  
  # ----------------------------
  # PRIORES DA GERAÇÃO (para theta_1)
  # theta_1 = (alpha_1, eta_sc0, eta_sc1, ..., eta_scd)'
  # onde eta_sc = eta_utilizado * Ft_dp (mantendo a mesma escala do ajuste)
  # ----------------------------
  sim_m0_alpha = NULL,         
  sim_C0_alpha = 0,            
  
  sim_m0_eta_sc = NULL,        
  sim_C0_eta_sc = 0,           
  sim_cor_eta = NULL,          
  
  # --------------------
  # PRIORES DO AJUSTE 
  # --------------------
  fit_a1_level = NULL,         
  fit_R1_level = NULL,         
  fit_a1_reg = NULL,           
  fit_R1_reg = NULL            
) {
  
  ## --- 1) Geração da Covariável (Passeio Aleatório) ---
  set.seed(seed)
  tau2_raiz <- sqrt(tau2)
  x <- numeric(n_total)
  
  x[1] <- x0 + rnorm(1, 0, tau2_raiz)
  for (t in 2:n_total) {
    x[t] <- x[t-1] + rnorm(1, 0, tau2_raiz)
  }
  
  ## --- 2) Construção da matriz de lags da covariável X ---
  q <- lags + 1 # número de colunas da matriz de lags (lag 0 até lags)
  X_matriz <- matrix(NA, nrow = n_total, ncol = q) # Inicialização da nossa matriz de lags
  X_matriz[, 1] <- x # Primeira coluna: lag 0, que é o próprio X
  
  for (j in 2:q) {
    X_matriz[, j] <- dplyr::lag(x, j - 1) # Coluna seguintes: X defasado em 1, 2, ..., (q-1) lags
  }
  
  X_efetivo <- X_matriz[-c(1:(q - 1)), , drop = FALSE] # removemos as primeiras (q-1) linhas vazias
  T_efetivo <- nrow(X_efetivo)
  v <- 1:(q - 1) # são os índices 1, 2, ..., lags a ser utilizados naquele somatório das fórmulas de S0, S1, S2, S3
  
  ## --- 3) Funções Auxiliares: Somas de Almon e Recuperação dos Betas ---
  
  # Sk(t) = somatório de j=1 até lags de [ (j^k) * X_{t-j} ], para k = 0, 1, ..., d
  construir_S <- function(X_efet, grau) {
    S0 <- rowSums(X_efet)
    X_sem0 <- X_efet[, -1, drop = FALSE] # X_efet[, -1] para considerar todas as linhas e colunas, menos a primeira coluna (do lag 0)
    
    S1 <- as.numeric(X_sem0 %*% (v^1))
    S2 <- as.numeric(X_sem0 %*% (v^2))
    
    if (grau == 2) {
      list(S0 = S0, S1 = S1, S2 = S2)
    } else {
      S3 <- as.numeric(X_sem0 %*% (v^3))
      list(S0 = S0, S1 = S1, S2 = S2, S3 = S3)
    }
  }
  
  beta_por_eta <- function(vetor_eta, grau) {
    j <- 0:lags
    Cj <- sapply(0:grau, function(k) j^k)
    as.numeric(Cj %*% vetor_eta)
  }
  
  criar_bloco_nivel <- function(D) {
    if (is.null(fit_a1_level) && is.null(fit_R1_level)) {
      polynomial_block(rate = 1, order = 1, D = D, name = "Nivel")
    } else if (!is.null(fit_a1_level) && is.null(fit_R1_level)) {
      polynomial_block(rate = 1, order = 1, D = D, name = "Nivel", a1 = fit_a1_level)
    } else if (is.null(fit_a1_level) && !is.null(fit_R1_level)) {
      polynomial_block(rate = 1, order = 1, D = D, name = "Nivel", R1 = fit_R1_level)
    } else {
      polynomial_block(rate = 1, order = 1, D = D, name = "Nivel", a1 = fit_a1_level, R1 = fit_R1_level)
    }
  }
  
  criar_bloco_regressao <- function(rate, D, nome, a1, R1) {
    if (is.null(a1) && is.null(R1)) {
      regression_block(rate = rate, D = D, name = nome)
    } else if (!is.null(a1) && is.null(R1)) {
      regression_block(rate = rate, D = D, name = nome, a1 = a1)
    } else if (is.null(a1) && !is.null(R1)) {
      regression_block(rate = rate, D = D, name = nome, R1 = R1)
    } else {
      regression_block(rate = rate, D = D, name = nome, a1 = a1, R1 = R1)
    }
  }
  
  ## --- 4) Estruturação da Simulação e Curva Principal ---
  rodar_simulacao <- function(nome_curva, grau_geracao, eta_utilizado, d1_nivel, semente = NULL) {
    
    if (!is.null(semente)) set.seed(semente)
    
    # Calculando as somas de Almon a partir da matriz de lags
    S_geracao <- construir_S(X_efetivo, grau_geracao)
    n_estados <- grau_geracao + 2
    
    Ft <- matrix(1, nrow = n_estados, ncol = T_efetivo)
    Ft[2, ] <- S_geracao$S0
    Ft[3, ] <- S_geracao$S1
    Ft[4, ] <- S_geracao$S2
    if (grau_geracao == 3) Ft[5, ] <- S_geracao$S3
    
    # Padronização das Sk
    Ft_dp <- apply(Ft, 1, sd) # calculando o desvio padrão
    Ft_dp[1] <- 1
    Ft_padronizado <- Ft / Ft_dp # dividindo pelo desvio padrão
    
    beta_verdadeiro <- beta_por_eta(eta_utilizado, grau_geracao)
    
    m0_alpha <- if (is.null(sim_m0_alpha)) alpha1 else sim_m0_alpha
    C0_alpha <- as.numeric(sim_C0_alpha)
    
    media_eta_sc_padrao <- eta_utilizado * Ft_dp[2:(grau_geracao + 2)]
    m0_eta_sc <- if (is.null(sim_m0_eta_sc)) media_eta_sc_padrao else sim_m0_eta_sc
    
    if (length(sim_C0_eta_sc) == 1) {
      v_eta <- rep(as.numeric(sim_C0_eta_sc), grau_geracao + 1)
    } else {
      v_eta <- as.numeric(sim_C0_eta_sc)
    }
    
    C0_eta <- if (is.null(sim_cor_eta)) diag(v_eta, grau_geracao + 1) else sim_cor_eta
    
    sim_m0 <- c(m0_alpha, m0_eta_sc)
    sim_C0 <- Matrix::bdiag(matrix(C0_alpha, 1, 1), C0_eta)
    
    if (all(as.matrix(sim_C0) == 0)) {
      theta_1 <- sim_m0
    } else {
      theta_1 <- MASS::mvrnorm(1, mu = sim_m0, Sigma = as.matrix(sim_C0))
    }
    
    G <- diag(1, n_estados)
    W <- diag(c(W_alpha, rep(0, n_estados - 1)))
    
    theta_t <- matrix(0, nrow = n_estados, ncol = T_efetivo)
    theta_t[, 1] <- theta_1
    
    for (t in 2:T_efetivo) {
      theta_t[, t] <- as.numeric(G %*% theta_t[, t-1] + MASS::mvrnorm(1, rep(0, n_estados), W))
    }
    
    eta_linear <- numeric(T_efetivo)
    for (t in 1:T_efetivo) {
      eta_linear[t] <- drop(t(Ft_padronizado[, t]) %*% theta_t[, t])
    }
    
    mu_verdadeiro <- exp(eta_linear) # Exponenciando o log(mu_t) pra tirar da escala log
    Y <- rpois(T_efetivo, lambda = mu_verdadeiro)
    
    ## --- 5) Ajuste do Modelo kDGLM ---
    ajustar_um_grau <- function(grau_ajuste) {
      
      S_ajuste <- construir_S(X_efetivo, grau_ajuste)
      n_estados_ajuste <- grau_ajuste + 2
      
      Ft_ajuste <- matrix(1, nrow = n_estados_ajuste, ncol = T_efetivo)
      Ft_ajuste[2, ] <- S_ajuste$S0
      Ft_ajuste[3, ] <- S_ajuste$S1
      Ft_ajuste[4, ] <- S_ajuste$S2
      if (grau_ajuste == 3) Ft_ajuste[5, ] <- S_ajuste$S3
      
      Ft_dp_ajuste <- apply(Ft_ajuste, 1, sd)
      Ft_dp_ajuste[1] <- 1
      Ft_padronizado_ajuste <- Ft_ajuste / Ft_dp_ajuste
      
      if (is.null(fit_a1_reg)) {
        a1_regs <- vector("list", grau_ajuste + 1)
      } else if (length(fit_a1_reg) == 1) {
        a1_regs <- rep(list(as.numeric(fit_a1_reg)), grau_ajuste + 1)
      } else {
        a1_regs <- as.list(as.numeric(fit_a1_reg))
      }
      
      if (is.null(fit_R1_reg)) {
        R1_regs <- vector("list", grau_ajuste + 1)
      } else if (length(fit_R1_reg) == 1) {
        R1_regs <- rep(list(as.numeric(fit_R1_reg)), grau_ajuste + 1)
      } else {
        R1_regs <- as.list(as.numeric(fit_R1_reg))
      }
      
      # bloco de nível com fator de desconto
      bloco_nivel <- criar_bloco_nivel(D = d1_level)
      
      # blocos de regressão para S0, S1, ..., Sd (com os coeficientes tratados como constantes no tempo, por isso D = 1)
      b0 <- criar_bloco_regressao(rate = Ft_padronizado_ajuste[2, ], D = 1, nome = "S0", a1 = a1_regs[[1]], R1 = R1_regs[[1]])
      b1 <- criar_bloco_regressao(rate = Ft_padronizado_ajuste[3, ], D = 1, nome = "S1", a1 = a1_regs[[2]], R1 = R1_regs[[2]])
      b2 <- criar_bloco_regressao(rate = Ft_padronizado_ajuste[4, ], D = 1, nome = "S2", a1 = a1_regs[[3]], R1 = R1_regs[[3]])
      
      bloco <- bloco_nivel + b0 + b1 + b2
      if (grau_ajuste == 2) {
        blocos_isolados <- list(Nivel = bloco_nivel, S0 = b0, S1 = b1, S2 = b2)
      } else {
        b3 <- criar_bloco_regressao(rate = Ft_padronizado_ajuste[5, ], D = 1, nome = "S3", a1 = a1_regs[[4]], R1 = R1_regs[[4]])
        bloco <- bloco + b3
        blocos_isolados <- list(Nivel = bloco_nivel, S0 = b0, S1 = b1, S2 = b2, S3 = b3)
      }
      
      priori_m0 <- unlist(lapply(blocos_isolados, function(b) as.numeric(b$a1)))
      priori_C0 <- Matrix::bdiag(lapply(blocos_isolados, function(b) as.matrix(b$R1)))
      
      desfecho <- Poisson(lambda = "rate", data = Y)
      ajuste <- fit_model(bloco, y = desfecho)
      coefs  <- coef(ajuste, lag = -1, eval.pred = TRUE, eval.metric = TRUE, pred.cred = pred_cred)
      
      valor_critico <- qnorm(1 - (1 - pred_cred)/2)
      
      ## --- 6) Pegando a média estimada a cada tempo ---
      # como os nomes podem variar dependendo da versão do kDGLM, eu peguei logo ambos ft/Qt ou lambda.mean/lambda.cov
      if (!is.null(coefs$ft)) {
        eta_hat <- as.numeric(coefs$ft[1, ]) # É o nosso preditor linear eta a cada tempo t, ou seja, nosso log(mu_t)
        eta_sd  <- sqrt(as.numeric(coefs$Qt[1, 1, ])) # desvio padrão pra calcular os intervalos de credibilidade
      } else {
        eta_hat <- as.numeric(coefs$lambda.mean[1, ]) # É o nosso preditor linear eta a cada tempo t, ou seja, nosso log(u_t)
        eta_sd  <- sqrt(as.numeric(coefs$lambda.cov[1, 1, ])) # desvio padrão pra calcular os intervalos de credibilidade
      }
      
      mu_hat <- exp(eta_hat) # Exponenciando o log(u_t) pra tirar da escala log
      mu_lim_inf <- exp(eta_hat - valor_critico * eta_sd)
      mu_lim_sup <- exp(eta_hat + valor_critico * eta_sd)
      
      ## --- 7) Gráficos ---
      
      # g1: Y observado vs mu estimado
      df_y <- data.frame(
        t = 0:(length(Y) - 1),
        Y = Y,
        y_estimado = mu_hat,
        mu_lim_inf = mu_lim_inf,
        mu_lim_sup = mu_lim_sup
      )
      
      lw <- 1.00
      
      g1_ajuste <- ggplot(df_y, aes(x = t)) +
        geom_line(aes(y = Y, colour = "Dados"), linewidth = lw) +
        geom_line(aes(y = y_estimado, colour = "Estimativas"), linewidth = lw) +
        geom_ribbon(aes(ymin = mu_lim_inf, ymax = mu_lim_sup, fill = "IC 95%"), alpha = 0.3) +
        geom_hline(yintercept = 0) +
        labs(
          x = "Tempo", 
          y = "Resposta", 
          colour = "", 
          fill = ""
        ) +
        theme_hc() +
        theme(
          axis.title      = element_text(size = 35),
          axis.text       = element_text(size = 30),
          legend.text     = element_text(size = 30),
          legend.title    = element_blank(),
          legend.position = "bottom"
        ) +
        scale_color_manual(
          values = c("Dados" = "black", "Estimativas" = "red"),
          breaks = c("Estimativas", "Dados"),
          labels = c("Estimativas", "Observações")
        ) +
        scale_fill_manual(values = c("IC 95%" = "blue")) +
        scale_x_continuous(breaks = c(0, 100, 200, 300), expand = c(0, 0)) +
        guides(
          fill   = guide_legend(order = 1),
          colour = guide_legend(order = 2)
        )
      
      ## --- 8) Reconstruindo os betas de cada lag via amostragem bayesiana dos eta no tempo final ---
      theta_mean <- coefs$theta.mean # E[theta_t | dados] (Vetor de estados, que tem aquele nível dinâmico + os regressores, no formato das somas Sk)
      theta_cov  <- coefs$theta.cov  # Cov[theta_t | dados] (Matriz de covariância desses estados)
      Tfinal     <- ncol(theta_mean)
      
      idx_eta <- 2:(grau_ajuste + 2)
      eta_mean_sc <- as.numeric(theta_mean[idx_eta, Tfinal])
      eta_cov_sc  <- as.matrix(theta_cov[idx_eta, idx_eta, Tfinal])
      
      if (!corpcor::is.positive.definite(eta_cov_sc)) {
        eta_cov_sc <- corpcor::make.positive.definite(eta_cov_sc)
      }
      
      # Amostragem multivariada utilizando a posteriori no tempo final T
      amostras_eta_sc <- MASS::mvrnorm(n_amostras, mu = eta_mean_sc, Sigma = eta_cov_sc)
      
      # Desfazendo a padronização das S_k
      amostras_eta_bruto <- sweep(amostras_eta_sc, 2, Ft_dp_ajuste[idx_eta], "/")
      
      # Reconstruindo a matriz Cj de Almon
      j <- 0:lags
      Cj <- sapply(0:grau_ajuste, function(k) j^k)
      amostras_beta <- amostras_eta_bruto %*% t(Cj)
      
      # Estimativas Pontuais
      rr_beta_estimado <- colMeans(amostras_beta)
      rr_lim_inf_beta <- apply(amostras_beta, 2, quantile, probs = 0.025)
      rr_lim_sup_beta <- apply(amostras_beta, 2, quantile, probs = 0.975)
      
      df_beta <- data.frame(
        lag = 0:lags,
        rr = rr_beta_estimado,
        rr_lim_inf = rr_lim_inf_beta,
        rr_lim_sup = rr_lim_sup_beta,
        rr_true = beta_verdadeiro
      )
      
      # g2: Beta(j)
      g2_ajuste <- ggplot(df_beta, aes(x = lag, y = rr)) +
        geom_point(size = 6) +
        geom_errorbar(aes(ymin = rr_lim_inf, ymax = rr_lim_sup), width = 0.5, size = 2) +
        geom_point(aes(y = rr_true), size = 6, colour = "indianred1", show.legend = FALSE) +
        geom_hline(yintercept = 0, linewidth = 0.8) +
        labs(x = "Lags", y = bquote(beta)) +
        theme_hc() +
        theme(
          axis.title = element_text(size = 35),
          axis.text  = element_text(size = 30),
          panel.grid = element_blank(),
          legend.position = "none"
        ) +
        scale_x_continuous(breaks = seq(0, lags, by = 1))
      
      ## --- 9) Tabelas com as estimativas a posteriori dos etas e Métricas ---
      
      eta_hat_raw <- eta_mean_sc / Ft_dp_ajuste[idx_eta]
      eta_lo_raw <- apply(amostras_eta_bruto, 2, quantile, probs = 0.025)
      eta_hi_raw <- apply(amostras_eta_bruto, 2, quantile, probs = 0.975)
      
      # Adequando o tamanho do vetor real para a tabela (com zeros se necessário)
      eta_verdadeiro_bruto <- rep(0, grau_ajuste + 1)
      tamanho_utilizado <- min(length(eta_utilizado), grau_ajuste + 1)
      eta_verdadeiro_bruto[1:tamanho_utilizado] <- eta_utilizado[1:tamanho_utilizado]
      
      tabela_eta_log <- data.frame(
        termo = paste0("eta", 0:grau_ajuste),
        valor_verdadeiro = eta_verdadeiro_bruto,
        estimativa = eta_hat_raw,
        limite_inferior = eta_lo_raw,
        limite_superior = eta_hi_raw
      )
      
      base_dados <- coefs$data
      y_obs <- base_dados$Observation
      
      if ("Prediction" %in% names(base_dados)) {
        mu_predito <- base_dados$Prediction
      } else {
        mu_predito <- exp(coefs$lambda.mean[1, ])
      }
      
      ll_vec <- dpois(y_obs, lambda = mu_predito, log = TRUE)
      LVP <- sum(ll_vec, na.rm = TRUE)
      
      MAE <- mean(abs(y_obs - mu_predito), na.rm = TRUE)
      denom <- mean(abs(diff(y_obs)), na.rm = TRUE)
      MASE <- if (is.finite(denom) && denom > 0) MAE / denom else NA
      
      IS <- NA
      if (all(c("C.I.lower", "C.I.upper") %in% names(base_dados))) {
        ci_lo <- base_dados$`C.I.lower`
        ci_hi <- base_dados$`C.I.upper`
        alpha_is <- 0.05
        is_vec <- (ci_hi - ci_lo)
        penal <- ifelse(y_obs < ci_lo, (2 / alpha_is) * (ci_lo - y_obs),
                        ifelse(y_obs > ci_hi, (2 / alpha_is) * (y_obs - ci_hi), 0))
        is_vec <- is_vec + penal
        IS <- sum(is_vec, na.rm = TRUE)
      }
      
      metricas <- list(LVP = LVP, MASE = MASE, IS = IS)
      
      list(
        grau_ajustado = grau_ajuste,
        kdglm_coef = coefs,
        g1 = g1_ajuste,
        g2 = g2_ajuste,
        mu_hat = mu_hat, mu_lim_inf = mu_lim_inf, mu_lim_sup = mu_lim_sup,
        prior_fit = list(m0 = priori_m0, C0 = priori_C0),
        prior_fit_por_bloco = lapply(blocos_isolados, function(b) list(a1 = b$a1, R1 = b$R1)),
        Ft_pad_fit = Ft_dp_ajuste,
        metricas = metricas,
        thetaR_tab = tabela_eta_log
      )
    }
    
    ## --- 10) Execução dos Modelos ---
    ajuste_principal <- ajustar_um_grau(grau_ajuste = grau_geracao)
    
    grau_outro <- if (grau_geracao == 2) 3 else 2
    ajuste_outro <- ajustar_um_grau(grau_ajuste = grau_outro)
    
    if (grau_geracao == 2) {
      ajuste_d2 <- ajuste_principal
      ajuste_d3 <- ajuste_outro
    } else {
      ajuste_d3 <- ajuste_principal
      ajuste_d2 <- ajuste_outro
    }
    
    list(
      nome = nome_curva, d = grau_geracao, lags = lags,
      Y = Y,
      mu_true = mu_verdadeiro,
      beta_true_geracao = beta_verdadeiro,
      kdglm_coef_d2 = ajuste_d2$kdglm_coef,
      kdglm_coef_d3 = ajuste_d3$kdglm_coef,
      g1_d2 = ajuste_d2$g1,
      g2_d2 = ajuste_d2$g2,
      g1_d3 = ajuste_d3$g1,
      g2_d3 = ajuste_d3$g2,
      metricas_d2 = ajuste_d2$metricas,
      metricas_d3 = ajuste_d3$metricas,
      ajustes = list(d2 = ajuste_d2, d3 = ajuste_d3)
    )
  }
  
  resultado_simulacao <- rodar_simulacao(
    nome_curva = "Curva", 
    grau_geracao = d, 
    eta_utilizado = eta, 
    d1_nivel = d1_level, 
    semente = seed_curva
  )
  
  list(
    seed = seed, n_total = n_total, lags = lags, tau2 = tau2,
    x = x,
    curva = resultado_simulacao
  )
}





prepara_base <- function(cidade, ano = NULL, ano1 = NULL, ano2 = NULL) {
  # Código de cada uma das cidades
  cidades <- c(
    rio_de_janeiro = 330455, sao_paulo = 355030, rio_branco = 120040, manaus = 130260,
    porto_velho = 110020, boa_vista = 140010, belem = 150140, macapa = 160030,
    cuiaba = 510340, palmas = 172100, sao_luis = 211130
  )
  
  cidade <- cidades[[cidade]] # Pegar o código da cidade através do nome que a pessoa inseriu no input da função
  
  arquivo <- case_when( # Pegar o nome do arquivo da base de dados dependendo de que cidade a pessoa escolheu
    cidade == cidades[["sao_paulo"]] ~ "Bases de Dados/rds/capital_sp.rds",
    cidade == cidades[["rio_de_janeiro"]] ~ "Bases de Dados/rds/capital_rj.rds",
    TRUE ~ "Bases de Dados/rds/cidades_amazonia_legal.rds"
  )
  
  base <- readRDS(arquivo)
  
  if (!is.null(ano)) {
    data_ini <- as.Date(paste0(ano, "-01-01"))
    data_fim <- as.Date(paste0(ano, "-12-31"))
  } else if (!is.null(ano1) && !is.null(ano2)) {
    data_ini <- as.Date(paste0(ano1, "-01-01"))
    data_fim <- as.Date(paste0(ano2, "-12-31"))
  } else {
    stop("Informe 'ano' ou informe 'ano1' e 'ano2'.")
  }
  
  base %>%
    filter(CodigoMunicipio == cidade, Data >= data_ini, Data <= data_fim) %>%
    transmute(
      CodigoMunicipio,
      Data,
      Casos_Resp = .data[["Casos_Resp"]],
      pm25 = PM2p5,
      temp = Temperatura,
      umid = UmidRel
    ) %>%
    arrange(Data)
}











Descritiva <- function(
    base,
    var_hosp = "Casos_Resp",
    var_pm25 = "pm25",
    var_temp = "temp",
    var_umid = "umid",
    ano_ini = 2015,
    ano_fim = 2024,
    facet_type = c("wrap", "grid"),
    padronizacao = c("max", "minmax"),
    padronizacao_serie = c("nenhuma", "max", "minmax"),
    fixar_zero = FALSE,
    usar_facets = "auto",
    tamanho_fonte = 13.5
) {
  facet_type <- match.arg(facet_type)
  padronizacao <- match.arg(padronizacao)
  padronizacao_serie <- match.arg(padronizacao_serie)
  
  dicionario_cidades <- c(
    "355030" = "São Paulo",
    "330455" = "Rio de Janeiro",
    "150140" = "Belém",
    "140010" = "Boa Vista",
    "510340" = "Cuiabá",
    "160030" = "Macapá",
    "130260" = "Manaus",
    "110020" = "Porto Velho",
    "120040" = "Rio Branco",
    "211130" = "São Luís",
    "172100" = "Palmas"
  )
  
  niveis_ordem <- unname(dicionario_cidades)
  meses <- c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")
  
  tema <- theme_hc(base_size = tamanho_fonte) +
    theme(
      axis.title.x = element_text(size = rel(1.25)),
      axis.title.y = element_text(size = rel(1.25)),
      axis.text = element_text(size = rel(1)),
      legend.text = element_text(size = rel(1)),
      strip.text = element_text(size = rel(1.125)),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "bottom",
      plot.margin = margin(t = 5, r = 5, b = 10, l = 5),
      legend.box.margin = margin(t = 5, r = 5, b = 5, l = 5)
    )
  
  eixo_tempo <- function(dt) {
    dt_min <- min(dt, na.rm = TRUE)
    dt_max <- max(dt, na.rm = TRUE)
    span_days <- as.integer(dt_max - dt_min)
    span_years <- as.numeric(difftime(dt_max, dt_min, units = "days")) / 365.25
    
    if (span_days <= 366) {
      b <- "1 month"; formato <- "mes"
    } else if (span_days <= 2 * 366) {
      b <- "2 months"; formato <- "mes_ano"
    } else if (span_days <= 4 * 366) {
      b <- "3 months"; formato <- "mes_ano"
    } else if (span_days <= 6 * 366) {
      b <- "6 months"; formato <- "mes_ano"
    } else {
      b <- "1 year"; formato <- "ano"
    }
    
    brks <- seq(dt_min, dt_max, by = b)
    
    if (formato == "mes") {
      lbls <- meses[as.integer(format(brks, "%m"))]
    } else if (formato == "mes_ano") {
      lbls <- paste0(meses[as.integer(format(brks, "%m"))], "-", format(brks, "%Y"))
    } else {
      lbls <- format(brks, "%Y")
    }
    
    list(breaks = brks, labels = lbls, lw = ifelse(span_years <= 6, 0.8, 0.6))
  }
  
  inicio <- as.Date(paste0(ano_ini, "-01-01"))
  fim <- as.Date(paste0(ano_fim, "-12-31"))
  
  dados <- base %>%
    filter(Data >= inicio, Data <= fim) %>%
    mutate(
      Cidade = unname(dicionario_cidades[as.character(as.integer(CodigoMunicipio))]),
      Hosp = .data[[var_hosp]],
      PM25 = .data[[var_pm25]],
      Temp = .data[[var_temp]],
      Umid = .data[[var_umid]]
    ) %>%
    filter(!is.na(Cidade), !is.na(Hosp), !is.na(PM25), !is.na(Temp), !is.na(Umid))
  
  dados$Cidade <- factor(dados$Cidade, levels = niveis_ordem)
  
  n_cidades <- length(unique(dados$Cidade))
  aplica_facet <- if (usar_facets == "auto") (n_cidades > 1) else as.logical(usar_facets)
  num_cols <- if (n_cidades <= 2) n_cidades else 3
  
  dados <- dados %>%
    group_by(Cidade) %>%
    mutate(
      min_hosp = min(Hosp, na.rm = TRUE), max_hosp = max(Hosp, na.rm = TRUE),
      min_pm = min(PM25, na.rm = TRUE), max_pm = max(PM25, na.rm = TRUE),
      min_temp = min(Temp, na.rm = TRUE), max_temp = max(Temp, na.rm = TRUE),
      min_umid = min(Umid, na.rm = TRUE), max_umid = max(Umid, na.rm = TRUE)
    ) %>%
    ungroup()
  
  if (padronizacao == "max") {
    dados <- dados %>%
      mutate(
        hosp_norm = ifelse(max_hosp > 0, Hosp / max_hosp, NA),
        pm_norm   = ifelse(max_pm > 0, PM25 / max_pm, NA),
        temp_norm = ifelse(max_temp > 0, Temp / max_temp, NA),
        umid_norm = ifelse(max_umid > 0, Umid / max_umid, NA)
      )
  } else {
    dados <- dados %>%
      mutate(
        hosp_norm = ifelse(max_hosp > min_hosp, (Hosp - min_hosp) / (max_hosp - min_hosp), NA),
        pm_norm   = ifelse(max_pm > min_pm, (PM25 - min_pm) / (max_pm - min_pm), NA),
        temp_norm = ifelse(max_temp > min_temp, (Temp - min_temp) / (max_temp - min_temp), NA),
        umid_norm = ifelse(max_umid > min_umid, (Umid - min_umid) / (max_umid - min_umid), NA)
      )
  }
  
  eixo <- eixo_tempo(dados$Data)
  
  add_facet <- function(g, free_y = FALSE) {
    if (!aplica_facet) return(g)
    scales_opt <- if (free_y) "free_y" else "fixed"
    if (facet_type == "wrap") {
      g + facet_wrap(~Cidade, ncol = num_cols, scales = scales_opt)
    } else {
      g + facet_grid(~Cidade, scales = scales_opt)
    }
  }
  
  # --- 1) Internações + PM2.5 ---
  g1 <- ggplot(dados, aes(x = Data)) +
    geom_line(aes(y = hosp_norm, colour = "hosp"), linewidth = eixo$lw) +
    geom_line(aes(y = pm_norm, colour = "pm"), linewidth = eixo$lw) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, by = 0.25)) +
    scale_color_manual(
      values = c(hosp = "#1F78B4", pm = "#FF7F0E"),
      breaks = c("hosp", "pm"),
      labels = c(hosp = "Internações Hospitalares", pm = "PM2.5")
    ) +
    scale_x_date(breaks = eixo$breaks, labels = eixo$labels, expand = c(0, 0)) +
    labs(x = "Data", y = "Valor padronizado (0–1)", colour = "") +
    tema +
    guides(colour = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(linewidth = 1.5)))
  
  g1 <- add_facet(g1)
  
  # --- 2) PM2.5 + Temperatura + Umidade ---
  g2 <- ggplot(dados, aes(x = Data)) +
    geom_line(aes(y = pm_norm, colour = "pm"), linewidth = eixo$lw) +
    geom_line(aes(y = temp_norm, colour = "temp"), linewidth = eixo$lw) +
    geom_line(aes(y = umid_norm, colour = "umid"), linewidth = eixo$lw) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, by = 0.25)) +
    scale_color_manual(
      values = c(pm = "#FF7F0E", temp = "#F1B514", umid = "#984EA3"),
      breaks = c("pm", "temp", "umid"),
      labels = c(pm = "PM2.5", temp = "Temperatura", umid = "Umidade Relativa")
    ) +
    scale_x_date(breaks = eixo$breaks, labels = eixo$labels, expand = c(0, 0)) +
    labs(x = "Data", y = "Valor padronizado (0–1)", colour = "") +
    tema +
    guides(colour = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(linewidth = 1.5)))
  
  g2 <- add_facet(g2)
  
  # --- 3) Internações + PM2.5 + Temp + Umid ---
  g3 <- ggplot(dados, aes(x = Data)) +
    geom_line(aes(y = hosp_norm, colour = "hosp"), linewidth = eixo$lw) +
    geom_line(aes(y = pm_norm, colour = "pm"), linewidth = eixo$lw) +
    geom_line(aes(y = temp_norm, colour = "temp"), linewidth = eixo$lw) +
    geom_line(aes(y = umid_norm, colour = "umid"), linewidth = eixo$lw) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, by = 0.25)) +
    scale_color_manual(
      values = c(hosp = "#1F78B4", pm = "#FF7F0E", temp = "#F1B514", umid = "#984EA3"),
      breaks = c("hosp", "pm", "temp", "umid"),
      labels = c(hosp = "Internações Hospitalares", pm = "PM2.5", temp = "Temperatura", umid = "Umidade Relativa")
    ) +
    scale_x_date(breaks = eixo$breaks, labels = eixo$labels, expand = c(0, 0)) +
    labs(x = "Data", y = "Valor padronizado (0–1)", colour = "") +
    tema +
    guides(colour = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(linewidth = 1.5)))
  
  g3 <- add_facet(g3)
  
  # --- 4) Séries Individuais ---
  serie <- list()
  for (tipo in c("pm25", "temp", "umid", "hosp")) {
    df4 <- dados
    
    if (tipo == "hosp") {
      df4$Valor <- df4$Hosp; label_y <- "Internações Hospitalares"; cor <- "#1F78B4"
    } else if (tipo == "pm25") {
      df4$Valor <- df4$PM25; label_y <- "PM2.5 (µg/m³)"; cor <- "#FF7F0E"
    } else if (tipo == "temp") {
      df4$Valor <- df4$Temp; label_y <- "Temperatura (°C)"; cor <- "#F1B514"
    } else {
      df4$Valor <- df4$Umid; label_y <- "Umidade Relativa (%)"; cor <- "#984EA3"
    }
    
    if (padronizacao_serie != "nenhuma") {
      df4 <- df4 %>%
        group_by(Cidade) %>%
        mutate(vmin = min(Valor, na.rm = TRUE), vmax = max(Valor, na.rm = TRUE)) %>%
        ungroup()
      
      if (padronizacao_serie == "max") {
        df4 <- df4 %>% mutate(Valor = ifelse(vmax > 0, Valor / vmax, NA))
      } else {
        df4 <- df4 %>% mutate(Valor = ifelse(vmax > vmin, (Valor - vmin) / (vmax - vmin), NA))
      }
      label_y <- paste0(label_y, " (padronizado 0–1)")
    }
    
    eixo <- eixo_tempo(df4$Data)
    
    # Define as quebras e os limites do gráfico
    y_breaks <- pretty(df4$Valor)
    y_min <- min(y_breaks)
    y_max <- max(y_breaks)
    
    # Se for "nenhuma" padronização, a linha base fica no valor minimo do y
    # Se estiver padronizado (0 a 1), a linha fica cravada no 0
    if (padronizacao_serie == "nenhuma") {
      if (fixar_zero) {
        linha_base <- df4 %>% distinct(Cidade) %>% mutate(y_base = 0)
        limite_inferior <- 0
      } else {
        linha_base <- df4 %>% distinct(Cidade) %>% mutate(y_base = y_min)
        limite_inferior <- y_min
      }
    } else {
      linha_base <- df4 %>% distinct(Cidade) %>% mutate(y_base = 0)
      limite_inferior <- 0
    }
    
    g <- ggplot(df4, aes(x = Data, y = Valor)) +
      geom_line(linewidth = eixo$lw, colour = cor) +
      geom_hline(
        data = linha_base,
        aes(yintercept = y_base),
        inherit.aes = FALSE,
        linewidth = 0.6,
        colour = "black"
      ) +
      scale_x_date(breaks = eixo$breaks, labels = eixo$labels, expand = c(0, 0)) +
      labs(x = "Data", y = label_y) +
      tema
    
    if (padronizacao_serie == "nenhuma") {
      g <- g + scale_y_continuous(limits = c(limite_inferior, NA), breaks = y_breaks, expand = c(0, 0))
      g <- add_facet(g, free_y = TRUE)
    } else {
      g <- g + scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25), expand = c(0, 0))
      g <- add_facet(g, free_y = FALSE)
    }
    
    serie[[tipo]] <- g
  }
  
  # --- 5) Temperatura + Umidade ---
  g5 <- ggplot(dados, aes(x = Data)) +
    geom_line(aes(y = temp_norm, colour = "temp"), linewidth = eixo$lw) +
    geom_line(aes(y = umid_norm, colour = "umid"), linewidth = eixo$lw) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, by = 0.25)) +
    scale_color_manual(
      values = c(temp = "#F1B514", umid = "#984EA3"),
      breaks = c("temp", "umid"),
      labels = c(temp = "Temperatura", umid = "Umidade Relativa")
    ) +
    scale_x_date(breaks = eixo$breaks, labels = eixo$labels, expand = c(0, 0)) +
    labs(x = "Data", y = "Valor padronizado (0–1)", colour = "") +
    tema +
    guides(colour = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(linewidth = 1.5)))
  
  g5 <- add_facet(g5)
  
  list(
    resp_pm25 = g1,
    clima_pm25 = g2,
    tudo = g3,
    serie = serie,
    temp_umid = g5
  )
}