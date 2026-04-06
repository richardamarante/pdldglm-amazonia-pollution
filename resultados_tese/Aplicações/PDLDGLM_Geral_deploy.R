PDLDGLM <- function(Y, X, data, lags, d, fd_nivel = 0.99,
                    padronizar_center = FALSE, padronizar_dp = TRUE,
                    n_amostras = 1000) {
  
  if (length(Y) != length(X) || length(Y) != length(data)) {
    stop("Y, X e data devem ter o mesmo comprimento.")
  }
  if (sum(is.na(Y)) > 0 || sum(is.na(X)) > 0) {
    stop("Y e X não podem conter NA.")
  }
  if (min(Y) < 0 || any(Y != round(Y))) {
    stop("Y deve ser uma contagem (inteiro >= 0).")
  }
  if (d != 2 && d != 3) {
    stop("d deve ser 2 ou 3 (grau do polinômio de defasagens).")
  }
  if (lags < 2 || lags != round(lags)) {
    stop("o número de lags deve ser um inteiro >= 2.")
  }
  if ((lags + 1) > length(Y)) {
    stop("Série muito curta para o número de lags informado.")
  }
  
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
    S_bruto <- data.frame(S0 = as.numeric(S0),
                          S1 = as.numeric(S1),
                          S2 = as.numeric(S2))
  } else { # se d == 3, inclui também o S3
    S3 <- X_mat[, -1] %*% (v^3)
    
    S_bruto <- data.frame(S0 = as.numeric(S0),
                          S1 = as.numeric(S1),
                          S2 = as.numeric(S2),
                          S3 = as.numeric(S3))
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
    S_dp[!is.finite(S_dp) | S_dp < 1e-6] <- 1e-6            # Pra evitar dividir por ~zero/NA/Inf, usa um mínimo seguro
  }
  
  # (2) Se pediu para centralizar, calcule as médias
  if (padronizar_center) {
    center_vec <- colMeans(S_bruto)                         # centragem: subtrai a média de cada coluna
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
  if (!is.null(coefs$ft)) { # como os nomes podem variar dependendo da versão do kDGLM: ft/Qt ou lambda.mean/lambda.cov)
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
    etaS_media_T <- as.numeric(etaS_media_T_padronizado / S_dp[seq_along(indice)])
    
    # Reconstruindo a matriz Cj de Almon
    j <- 0:(q - 1)
    if (d == 2) {
      Cj <- cbind(1, j, j^2)
    } else {
      Cj <- cbind(1, j, j^2, j^3)
    }
    
    beta_pt <- as.numeric(etaS_media_T %*% t(Cj))
    rr_beta_estimado    <- as.numeric(exp(sd(X[q:n_total]) * beta_pt)) #### beta_pt
    rr_sd_estimado      <- rep(NA_real_, q)
    rr_ic_inferior_beta <- rep(NA_real_, q)
    rr_ic_superior_beta <- rep(NA_real_, q)
    
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
    ### amostras_beta
    # Estimativas Pontuais
    rr_beta_estimado    <- colMeans(rr_amostras_beta)
    rr_sd_estimado      <- apply(rr_amostras_beta, 2, sd)
    rr_ic_inferior_beta <- apply(rr_amostras_beta, 2, quantile, probs = 0.025)
    rr_ic_superior_beta <- apply(rr_amostras_beta, 2, quantile, probs = 0.975)
  }
  # --------------------------------------------------------------------------------------------
  
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
  
  # Eixo X adaptativo
  dt_min <- min(df_y$t_index)
  dt_max <- max(df_y$t_index)
  span_days <- as.integer(dt_max - dt_min)
  
  # Meses em PT-BR
  mes_pt <- c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")
  
  if (span_days <= 366) {
    by <- "1 month"
    lab_fun <- function(d) mes_pt[as.integer(format(d, "%m"))]
  } else if (span_days <= 2*366) { # entre 1 anos e 2 ano
    by <- "2 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else if (span_days <= 4*366) { # entre 2 anos e 4 ano
    by <- "3 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else if (span_days <= 6*366) {
    by <- "6 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else {
    by <- "1 year"
    lab_fun <- function(d) format(d, "%Y")
  }
  
  breaks_seq <- seq(dt_min, dt_max, by = by) # Começa exatamente no primeiro dia da sua série (mantém o "dia do mês" de início)
  
  # linewidth adaptativo por janela de anos daquelas geom_lines (para poder mudar o período livremente na função)
  span_years <- as.numeric(difftime(dt_max, dt_min, units = "days")) / 365.25 # 365.25 para cobrir anos bissextos
  
  lw <- if (span_years <= 6) {
    1.00
  } else {
    0.90
  }
  
  g1 <- ggplot(df_y, aes(x = t_index)) +
    geom_line(aes(y = Y,          colour = "Dados"),       linewidth = lw) +
    geom_line(aes(y = y_estimado, colour = "Estimativas"), linewidth = lw) +
    geom_ribbon(aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    geom_hline(yintercept = 0) +
    labs(x = "Data", y = "Internações", colour = "", fill = "") +
    theme_hc() +
    theme(
      axis.title.x = element_text(size = 35),
      axis.text    = element_text(size = 35),
      axis.text.x  = element_text(size = 35, angle = 90),
      legend.text  = element_text(size = 30),
      legend.title = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(size = 35)
    ) +
    scale_color_manual(values = c("Dados" = "black", "Estimativas" = "red"),
                       breaks = c("Estimativas", "Dados")) +
    scale_fill_manual(values = c("IC 95%" = "blue")) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  g1_relatorio <- ggplot(df_y, aes(x = t_index)) +
    geom_line(aes(y = Y,          colour = "Dados"),       linewidth = lw) +
    geom_line(aes(y = y_estimado, colour = "Estimativas"), linewidth = lw) +
    geom_ribbon(aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    geom_hline(yintercept = 0) +
    labs(
      x = "Data",
      y = "Internações",   # ou "Internações Hospitalares", se você já trocou
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
      legend.position = "bottom",   # <- AQUI aparece a legenda
      axis.title.y   = element_text(size = 35)
    ) +
    scale_color_manual(
      values = c("Dados" = "black", "Estimativas" = "red"),
      breaks = c("Estimativas", "Dados"),
      labels = c("Estimativas", "Observações")  # <- texto da legenda
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
  
  g1_legado <- ggplot(df_y, aes(x = t_index)) +
    geom_line(aes(y = Y,          colour = "Dados"),       linewidth = 1.6) +
    geom_ribbon(aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    geom_line(aes(y = y_estimado, colour = "Estimativas"), linewidth = 1.6) +
    geom_hline(yintercept = 0) +
    labs(x = "Mês", y = "Internações", colour = "", fill = "") +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 35),
      axis.text    = element_text(size = 35),
      axis.text.x  = element_text(size = 35, angle = 90),
      legend.text  = element_text(size = 30),
      legend.title = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(size = 35)
    ) +
    scale_color_manual(values = c("Dados" = "black", "Estimativas" = "red"),
                       breaks = c("Estimativas", "Dados")) +
    scale_fill_manual(values = c("IC 95%" = "blue")) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
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
                fill = "cadetblue3", color = "cadetblue3", # cadetblue3
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
  
  g2_legado <- ggplot(df_beta, aes(x = lag, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "blue", color = "blue",
                alpha = 0.2, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_point(size = 3, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Lags", y = "Risco Relativo") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = seq(0, q - 1, by = 1))
  
  return(list(
    mu_media    = mu_estimada,
    mu_ic_inf   = mu_ic_inf,
    mu_ic_sup   = mu_ic_sup,
    beta_media  = rr_beta_estimado,
    beta_sd     = rr_sd_estimado,
    beta_ic_inf = rr_ic_inferior_beta,
    beta_ic_sup = rr_ic_superior_beta,
    g1 = g1,
    g1_relatorio = g1_relatorio,
    g1_legado = g1_legado,
    g2 = g2,
    g2_legado = g2_legado,
    kdglm_coef  = coefs,
    pred_df  = coefs$data,
    ajuste1 = ajuste
  ))
}




PDLDGLM_clima <- function(
    Y, X, covar, data,                     # <- adiciona a covariável climática bruta
    lags, lag_covar, d,                    # <- lag da covariável
    perc, lado = c("acima","abaixo"),      # <- limiar por percentil e lado do corte
    perc_sup = NULL,                       # <- opcional: limite superior p/ intervalo interno
    fd_nivel = 0.99,
    padronizar_center = FALSE,
    padronizar_dp     = TRUE,
    n_amostras        = 1000
) {
  lado <- match.arg(lado)
  
  if (length(Y) != length(X) || length(Y) != length(data) || length(Y) != length(covar)) {
    stop("Y, X, covar e data devem ter o mesmo comprimento.")
  }
  if (sum(is.na(Y)) > 0 || sum(is.na(X)) > 0 || sum(is.na(covar)) > 0) {
    stop("Y, X e covar não podem conter NA.")
  }
  if (min(Y) < 0 || any(Y != round(Y))) {
    stop("Y deve ser uma contagem (inteiro >= 0).")
  }
  if (d != 2 && d != 3) {
    stop("d deve ser 2 ou 3 (grau do polinômio de defasagens).")
  }
  if (lags < 2 || lags != round(lags)) {
    stop("o número de lags deve ser um inteiro >= 2.")
  }
  if (lag_covar < 0 || lag_covar != round(lag_covar)) {
    stop("'lag_covar' deve ser um inteiro >= 0.")
  }
  if (perc <= 0 || perc >= 1) {
    stop("'perc' deve estar em (0,1), por exemplo 0.85, 0.95.")
  }
  if (!is.null(perc_sup)) {
    if (!is.numeric(perc_sup) || length(perc_sup) != 1 || !is.finite(perc_sup)) {
      stop("'perc_sup' deve ser escalar numérico em (0,1), quando informado.")
    }
    if (perc_sup <= 0 || perc_sup >= 1) {
      stop("'perc_sup' deve estar em (0,1).")
    }
    if (perc_sup <= perc) {
      stop("'perc_sup' deve ser maior que 'perc' para definir intervalo interno.")
    }
  }
  if ((lags + 1) > length(Y)) {
    stop("Série muito curta para o número de lags informado.")
  }
  
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
  if (ini > n_total) stop("Janela efetiva vazia após alinhamento.")
  
  corte <- as.numeric(stats::quantile(covar[ini:n_total], probs = perc, na.rm = TRUE))
  if (is.null(perc_sup)) {
    H_raw <- if (lado == "acima") as.numeric(covar > corte) else as.numeric(covar < corte)
  } else {
    corte_sup <- as.numeric(stats::quantile(covar[ini:n_total], probs = perc_sup, na.rm = TRUE))
    # intervalo interno: ativa quando covariável cai entre os dois percentis
    H_raw <- as.numeric(covar > corte & covar < corte_sup)
  }
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
    S_bruto <- data.frame(S0 = as.numeric(S0),
                          S1 = as.numeric(S1),
                          S2 = as.numeric(S2))
  } else { # se d == 3, inclui também o S3
    S3 <- X_mat[, -1] %*% (v^3)
    
    S_bruto <- data.frame(S0 = as.numeric(S0),
                          S1 = as.numeric(S1),
                          S2 = as.numeric(S2),
                          S3 = as.numeric(S3))
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
    S_dp[!is.finite(S_dp) | S_dp < 1e-6] <- 1e-6            # Pra evitar dividir por ~zero/NA/Inf, usa um mínimo seguro
  }
  
  # (2) Se pediu para centralizar, calcule as médias
  if (padronizar_center) {
    center_vec <- colMeans(S_bruto)                         # centragem: subtrai a média de cada coluna
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
  sd_X_efetivo <- stats::sd(X[ini:n_total])
  
  # Se n_amostras <= 0, pula a amostragem bayesiana e usa estimativa pontual (com a posteriori da média e covariância)
  if (is.null(n_amostras) || n_amostras <= 0) {
    # Desfazendo a padronização das S_k
    etaS_media_T <- as.numeric(etaS_media_T_padronizado / S_dp[seq_along(indice)])
    
    # Reconstruindo a matriz Cj de Almon
    j <- 0:(q - 1)
    if (d == 2) {
      Cj <- cbind(1, j, j^2)
    } else {
      Cj <- cbind(1, j, j^2, j^3)
    }
    
    beta_pt <- as.numeric(etaS_media_T %*% t(Cj))
    rr_beta_estimado    <- as.numeric(exp(sd_X_efetivo * beta_pt))
    rr_sd_estimado      <- rep(NA_real_, q)
    rr_ic_inferior_beta <- rep(NA_real_, q)
    rr_ic_superior_beta <- rep(NA_real_, q)
    
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
    rr_sd_estimado      <- apply(rr_amostras_beta, 2, sd)
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
    rr_ic_inferior_tau <- NA_real_
    rr_ic_superior_tau <- NA_real_
  } else {
    # Amostragem bayesiana univariada de τ ~ Normal(tau_media_T, tau_var_T)
    amostras_tau <- rnorm(n_amostras, mean = as.numeric(tau_media_T), sd = sqrt(as.numeric(tau_var_T)))
    amostras_RR  <- exp(amostras_tau)
    
    # Estimativas pontuais e IC95% no log(τ) e na escala de RR
    rr_tau_estimado <- mean(amostras_RR)
    rr_ic_inferior_tau <- as.numeric(quantile(amostras_RR, probs = 0.025))
    rr_ic_superior_tau <- as.numeric(quantile(amostras_RR, probs = 0.975))
  }
  
  # --------------------------------------------------------------------------------------------
  
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
  
  # Eixo X adaptativo
  dt_min <- min(df_y$t_index)
  dt_max <- max(df_y$t_index)
  span_days <- as.integer(dt_max - dt_min)
  
  # Meses em PT-BR
  mes_pt <- c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")
  
  if (span_days <= 366) {
    by <- "1 month"
    lab_fun <- function(d) mes_pt[as.integer(format(d, "%m"))]
  } else if (span_days <= 2*366) { # entre 1 anos e 2 ano
    by <- "2 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else if (span_days <= 4*366) { # entre 2 anos e 4 ano
    by <- "3 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else if (span_days <= 6*366) {
    by <- "6 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else {
    by <- "1 year"
    lab_fun <- function(d) format(d, "%Y")
  }
  
  breaks_seq <- seq(dt_min, dt_max, by = by) # Começa exatamente no primeiro dia da sua série (mantém o "dia do mês" de início)
  
  # linewidth adaptativo por janela de anos daquelas geom_lines (para poder mudar o período livremente na função)
  span_years <- as.numeric(difftime(dt_max, dt_min, units = "days")) / 365.25 # 365.25 para cobrir anos bissextos
  
  lw <- if (span_years <= 6) {
    1.00
  } else {
    0.90
  }
  
  g1 <- ggplot2::ggplot(df_y, ggplot2::aes(x = t_index)) +
    ggplot2::geom_line(ggplot2::aes(y = Y,          colour = "Dados"),       linewidth = lw) +
    ggplot2::geom_line(ggplot2::aes(y = y_estimado, colour = "Estimativas"), linewidth = lw) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(x = "Data", y = "Internações", colour = "", fill = "") +
    ggthemes::theme_hc() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 35),
      axis.text    = ggplot2::element_text(size = 35),
      axis.text.x  = ggplot2::element_text(size = 35, angle = 90),
      legend.text  = ggplot2::element_text(size = 30),
      legend.title = ggplot2::element_blank(),
      legend.position = "none",
      axis.title.y = ggplot2::element_text(size = 35)
    ) +
    ggplot2::scale_color_manual(values = c("Dados" = "black", "Estimativas" = "red"),
                                breaks = c("Estimativas", "Dados")) +
    ggplot2::scale_fill_manual(values = c("IC 95%" = "blue")) +
    ggplot2::scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  g1_relatorio <- ggplot2::ggplot(df_y, ggplot2::aes(x = t_index)) +
    ggplot2::geom_line(ggplot2::aes(y = Y,          colour = "Dados"),       linewidth = lw) +
    ggplot2::geom_line(ggplot2::aes(y = y_estimado, colour = "Estimativas"), linewidth = lw) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(
      x = "Data",
      y = "Internações",    # ou "Internações Hospitalares"
      colour = "",
      fill   = ""
    ) +
    ggthemes::theme_hc() +
    ggplot2::theme(
      axis.title.x    = ggplot2::element_text(size = 35),
      axis.text       = ggplot2::element_text(size = 35),
      axis.text.x     = ggplot2::element_text(size = 35, angle = 90),
      legend.text     = ggplot2::element_text(size = 30),
      legend.title    = ggplot2::element_blank(),
      legend.position = "bottom",   # <- legenda aparece aqui
      axis.title.y    = ggplot2::element_text(size = 35)
    ) +
    ggplot2::scale_color_manual(
      values = c("Dados" = "black", "Estimativas" = "red"),
      breaks = c("Estimativas", "Dados"),
      labels = c("Estimativas", "Observações")
    ) +
    ggplot2::scale_fill_manual(values = c("IC 95%" = "blue")) +
    ggplot2::scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    ) +
    ggplot2::guides(
      fill   = ggplot2::guide_legend(order = 1),
      colour = ggplot2::guide_legend(order = 2)
    )
  
  g1_legado <- ggplot(df_y, aes(x = t_index)) +
    geom_ribbon(aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    geom_line(aes(y = Y,          colour = "Dados"),       linewidth = 1.6) +
    geom_line(aes(y = y_estimado, colour = "Estimativas"), linewidth = 1.6) +
    geom_hline(yintercept = 0) +
    labs(x = "Mês", y = "Internações", colour = "", fill = "") +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 35),
      axis.text    = element_text(size = 35),
      axis.text.x  = element_text(size = 35, angle = 90),
      legend.text  = element_text(size = 30),
      legend.title = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(size = 35)
    ) +
    scale_color_manual(values = c("Dados" = "black", "Estimativas" = "red"),
                       breaks = c("Estimativas", "Dados")) +
    scale_fill_manual(values = c("IC 95%" = "blue")) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  # g2: Beta(j)
  df_beta <- data.frame(
    lag = 0:(q - 1),
    rr = as.numeric(rr_beta_estimado),
    rr_lo = as.numeric(rr_ic_inferior_beta),
    rr_hi = as.numeric(rr_ic_superior_beta)
  )
  
  g2 <- ggplot2::ggplot(df_beta, ggplot2::aes(x = lag, y = rr)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = rr_lo, ymax = rr_hi),
                         fill = "cadetblue3", color = "cadetblue3",
                         alpha = 0.3, show.legend = FALSE) +
    ggplot2::geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    ggplot2::geom_point(size = 2.5, colour = "black", show.legend = FALSE) +
    ggplot2::geom_hline(yintercept = 1, linewidth = 0.8) +
    ggplot2::labs(x = "Lags", y = "Risco Relativo (RR)") +
    ggthemes::theme_hc(base_size = 1) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 35),
      axis.text  = ggplot2::element_text(size = 35),
      panel.grid = ggplot2::element_blank(),
      legend.position = "none"
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, q - 1, by = 1))
  
  g2_legado <- ggplot(df_beta, aes(x = lag, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "blue", color = "blue",
                alpha = 0.2, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_point(size = 3, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Lags", y = "Risco Relativo") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = seq(0, q - 1, by = 1))
  
  # juntar g1 (esquerda) e g2 (direita) com patchwork
  g1_g2 <- g1 + g2 + plot_layout(widths = c(1, 1))
  
  return(list(
    mu_media    = mu_estimada,
    mu_ic_inf   = mu_ic_inf,
    mu_ic_sup   = mu_ic_sup,
    beta_media  = rr_beta_estimado,
    beta_sd     = rr_sd_estimado,
    beta_ic_inf = rr_ic_inferior_beta,
    beta_ic_sup = rr_ic_superior_beta,
    tau_media   = rr_tau_estimado,
    tau_ic_inf  = rr_ic_inferior_tau,
    tau_ic_sup  = rr_ic_superior_tau,
    g1 = g1,
    g1_relatorio = g1_relatorio,
    g1_legado = g1_legado,
    g2 = g2,
    g2_legado = g2_legado,
    g1_g2 = g1_g2,
    kdglm_coef  = coefs,
    pred_df     = coefs$data
  ))
}







PDLDGLM_et <- function(Y, X, data, lags, d, fd_nivel = 0.99,
                       padronizar_center = FALSE, padronizar_dp = TRUE,
                       n_amostras = 1000,
                       fd_sazonal = 0.995, periodo_saz = 365,
                       usar_intervencao_covid = FALSE,
                       inicio_covid = as.Date("2020-03-01"),
                       fim_covid = as.Date("2021-12-31")) {
  
  if (length(Y) != length(X) || length(Y) != length(data)) {
    stop("Y, X e data devem ter o mesmo comprimento.")
  }
  if (sum(is.na(Y)) > 0 || sum(is.na(X)) > 0) {
    stop("Y e X não podem conter NA.")
  }
  if (min(Y) < 0 || any(Y != round(Y))) {
    stop("Y deve ser uma contagem (inteiro >= 0).")
  }
  if (d != 2 && d != 3) {
    stop("d deve ser 2 ou 3 (grau do polinômio de defasagens).")
  }
  if (lags < 2 || lags != round(lags)) {
    stop("o número de lags deve ser um inteiro >= 2.")
  }
  if ((lags + 1) > length(Y)) {
    stop("Série muito curta para o número de lags informado.")
  }
  if (periodo_saz < 2 || periodo_saz != round(periodo_saz)) {
    stop("'periodo_saz' deve ser um inteiro >= 2 (ex.: 365 para dados diários).")
  }
  if (!(fd_sazonal > 0 && fd_sazonal <= 1)) {
    stop("'fd_sazonal' deve estar em (0, 1].")
  }
  
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
  data <- data[q:n_total] # alinhar datas também (necessário para a intervenção)
  
  ## --- 2) Calculando as somas de Almon a partir da matriz de lags (com grau d, dado na função) ---
  v <- 1:lags # são os índices 1, 2, ..., lags a ser utilizados naquele somatório das fórmulas de S0, S1, S2, S3
  
  # Sk(t) = somatório de j=1 até lags de [ (j^k) * X_{t-j} ], para k = 0, 1, ..., d
  S0 <- rowSums(X_mat) 
  S1 <- X_mat[, -1] %*%  v # X_mat[, -1] para considerar todas as linhas e colunas, menos a primeira coluna (do lag 0)
  S2 <- X_mat[, -1] %*% (v^2)
  
  if (d == 2) {
    S_bruto <- data.frame(S0 = as.numeric(S0),
                          S1 = as.numeric(S1),
                          S2 = as.numeric(S2))
  } else { # se d == 3, inclui também o S3
    S3 <- X_mat[, -1] %*% (v^3)
    
    S_bruto <- data.frame(S0 = as.numeric(S0),
                          S1 = as.numeric(S1),
                          S2 = as.numeric(S2),
                          S3 = as.numeric(S3))
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
    S_dp[!is.finite(S_dp) | S_dp < 1e-6] <- 1e-6            # Pra evitar dividir por ~zero/NA/Inf, usa um mínimo seguro
  }
  
  # (2) Se pediu para centralizar, calcule as médias
  if (padronizar_center) {
    center_vec <- colMeans(S_bruto)                         # centragem: subtrai a média de cada coluna
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
  
  # bloco de sazonalidade harmônica (senos e cossenos) com fator de desconto fd_sazonal
  sazonal <- harmonic_block(rate = 1, order = 1, period = periodo_saz, D = fd_sazonal, name = "Sazonal")
  
  # blocos de regressão para S0, S1, ..., Sd (com os coeficientes tratados como constantes no tempo, por isso D = 1)
  b0 <- regression_block(rate = S_padronizado$S0, D = 1, name = "S0")
  b1 <- regression_block(rate = S_padronizado$S1, D = 1, name = "S1")
  b2 <- regression_block(rate = S_padronizado$S2, D = 1, name = "S2")
  
  # bloco de intervenção para o período da covid-19
  if (usar_intervencao_covid) {
    tempo_covid <- which(data >= inicio_covid & data <= fim_covid)
    if (length(tempo_covid) > 0) {
      nivel <- intervention(nivel, time = tempo_covid, D = (fd_nivel - 0.005), var.index = 1)
    }
  }
  
  # --- Composição dos blocos ---
  if (d == 2) {
    bloco <- (nivel + b0 + b1 + b2 + sazonal)
  } else { # se d == 3, lembra de colocar também o bloco de regressão para o S3
    b3 <- regression_block(rate = S_padronizado$S3, D = 1, name = "S3")
    bloco <- (nivel + b0 + b1 + b2 + b3 + sazonal)
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
    etaS_media_T <- as.numeric(etaS_media_T_padronizado / S_dp[seq_along(indice)])
    
    # Reconstruindo a matriz Cj de Almon
    j <- 0:(q - 1)
    if (d == 2) {
      Cj <- cbind(1, j, j^2)
    } else {
      Cj <- cbind(1, j, j^2, j^3)
    }
    
    beta_pt <- as.numeric(etaS_media_T %*% t(Cj))
    rr_beta_estimado    <- as.numeric(exp(sd(X[q:n_total]) * beta_pt))
    rr_sd_estimado      <- rep(NA_real_, q)
    rr_ic_inferior_beta <- rep(NA_real_, q)
    rr_ic_superior_beta <- rep(NA_real_, q)
    
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
    rr_sd_estimado      <- apply(rr_amostras_beta, 2, sd)
    rr_ic_inferior_beta <- apply(rr_amostras_beta, 2, quantile, probs = 0.025)
    rr_ic_superior_beta <- apply(rr_amostras_beta, 2, quantile, probs = 0.975)
  }
  # --------------------------------------------------------------------------------------------
  
  ## --- 7) Gráficos ---
  
  # g1: Y observado vs mu estimado
  t_index <- as.Date(data)
  df_y <- data.frame(
    t_index    = t_index,
    Y          = Y,
    y_estimado = mu_estimada,
    mu_ic_inf  = mu_ic_inf,
    mu_ic_sup  = mu_ic_sup 
  )
  
  # Eixo X adaptativo
  dt_min <- min(df_y$t_index)
  dt_max <- max(df_y$t_index)
  span_days <- as.integer(dt_max - dt_min)
  
  # Meses em PT-BR
  mes_pt <- c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")
  
  if (span_days <= 366) {
    by <- "1 month"
    lab_fun <- function(d) mes_pt[as.integer(format(d, "%m"))]
  } else if (span_days <= 2*366) { # entre 1 anos e 2 ano
    by <- "2 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else if (span_days <= 4*366) { # entre 2 anos e 4 ano
    by <- "3 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else if (span_days <= 6*366) {
    by <- "6 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else {
    by <- "1 year"
    lab_fun <- function(d) format(d, "%Y")
  }
  
  breaks_seq <- seq(dt_min, dt_max, by = by) # Começa exatamente no primeiro dia da sua série (mantém o "dia do mês" de início)
  
  # linewidth adaptativo por janela de anos daquelas geom_lines (para poder mudar o período livremente na função)
  span_years <- as.numeric(difftime(dt_max, dt_min, units = "days")) / 365.25 # 365.25 para cobrir anos bissextos
  
  lw <- if (span_years <= 6) {
    1.00
  } else {
    0.90
  }
  
  g1 <- ggplot(df_y, aes(x = t_index)) +
    geom_line(aes(y = Y,          colour = "Dados"),       linewidth = lw) +
    geom_line(aes(y = y_estimado, colour = "Estimativas"), linewidth = lw) +
    geom_ribbon(aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    geom_hline(yintercept = 0) +
    labs(x = "Data", y = "Internações", colour = "", fill = "") +
    theme_hc() +
    theme(
      axis.title.x = element_text(size = 35),
      axis.text    = element_text(size = 35),
      axis.text.x  = element_text(size = 35, angle = 90),
      legend.text  = element_text(size = 30),
      legend.title = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(size = 35)
    ) +
    scale_color_manual(values = c("Dados" = "black", "Estimativas" = "red"),
                       breaks = c("Estimativas", "Dados")) +
    scale_fill_manual(values = c("IC 95%" = "blue")) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  g1_legado <- ggplot(df_y, aes(x = t_index)) +
    geom_line(aes(y = Y,          colour = "Dados"),       linewidth = 1.6) +
    geom_ribbon(aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    geom_line(aes(y = y_estimado, colour = "Estimativas"), linewidth = 1.6) +
    geom_hline(yintercept = 0) +
    labs(x = "Mês", y = "Internações", colour = "", fill = "") +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 35),
      axis.text    = element_text(size = 35),
      axis.text.x  = element_text(size = 35, angle = 90),
      legend.text  = element_text(size = 30),
      legend.title = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(size = 35)
    ) +
    scale_color_manual(values = c("Dados" = "black", "Estimativas" = "red"),
                       breaks = c("Estimativas", "Dados")) +
    scale_fill_manual(values = c("IC 95%" = "blue")) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
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
                fill = "cadetblue3", color = "cadetblue3", # cadetblue3
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
  
  g2_legado <- ggplot(df_beta, aes(x = lag, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "blue", color = "blue",
                alpha = 0.2, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_point(size = 3, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Lags", y = "Risco Relativo") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = seq(0, q - 1, by = 1))
  
  return(list(
    mu_media    = mu_estimada,
    mu_ic_inf   = mu_ic_inf,
    mu_ic_sup   = mu_ic_sup,
    beta_media  = rr_beta_estimado,
    beta_sd     = rr_sd_estimado,
    beta_ic_inf = rr_ic_inferior_beta,
    beta_ic_sup = rr_ic_superior_beta,
    g1 = g1,
    g1_legado = g1_legado,
    g2 = g2,
    g2_legado = g2_legado,
    kdglm_coef  = coefs,
    pred_df  = coefs$data
  ))
}






PDLDGLM_Duo <- function(Y, X, covar, data,
                        lags, lags_covar, d, fd_nivel = 0.99,
                        padronizar_center = FALSE, padronizar_dp = TRUE,
                        n_amostras = 1000) {
  
  if (length(Y) != length(X) || length(Y) != length(covar) || length(Y) != length(data)) {
    stop("Y, X, covar e data devem ter o mesmo comprimento.")
  }
  if (sum(is.na(Y)) > 0 || sum(is.na(X)) > 0 || sum(is.na(covar)) > 0) {
    stop("Y, X e covar não podem conter NA.")
  }
  if (min(Y) < 0 || any(Y != round(Y))) {
    stop("Y deve ser uma contagem (inteiro >= 0).")
  }
  if (d != 2 && d != 3) {
    stop("d deve ser 2 ou 3 (grau do polinômio de defasagens).")
  }
  if (missing(lags) || missing(lags_covar)) {
    stop("Informe 'lags' (poluente) e 'lags_covar' (covariável).")
  }
  if (lags < 2 || lags != round(lags)) {
    stop("'lags' deve ser um inteiro >= 2.")
  }
  if (lags_covar < 2 || lags_covar != round(lags_covar)) {
    stop("'lags_covar' deve ser um inteiro >= 2.")
  }
  n_total <- length(Y) # tamanho total da série Y
  
  qX <- lags       + 1 # número de colunas da matriz de lags de X  (lag 0 até lags)
  qZ <- lags_covar + 1 # número de colunas da matriz de lags de covar (lag 0 até lags_covar)
  
  if ((max(lags, lags_covar) + 1) > n_total) {
    stop("Série muito curta para o número de lags informado (poluente ou covariável).")
  }
  
  ## --- 1) Construção da matriz de lags da covariável X ---
  X_mat <- matrix(0, nrow = n_total, ncol = qX) # Inicialização da nossa matriz de lags para X
  X_mat[, 1] <- X # Primeira coluna: lag 0, que é o próprio X
  
  for (j in 2:qX) {
    X_mat[, j] <- dplyr::lag(X, j - 1) # Colunas seguintes: X defasado em 1, 2, ..., (qX-1) lags
  }
  
  ## --- 1b) Construção da matriz de lags da covariável de confundimento (covar) ---
  covar_mat <- matrix(0, nrow = n_total, ncol = qZ) # Matriz de lags para a covariável
  covar_mat[, 1] <- covar # Primeira coluna: lag 0
  
  for (j in 2:qZ) {
    covar_mat[, j] <- dplyr::lag(covar, j - 1) # Colunas seguintes: covar defasada em 1, 2, ..., (qZ-1) lags
  }
  
  ## --- 1c) Alinhamento conjunto das séries (X com lags e covar com lags) ---
  ini <- max(qX, qZ) # índice inicial comum (remove as linhas com NAs de qualquer uma das matrizes)
  
  X_mat   <- X_mat[ini:n_total, ]
  covar_mat <- covar_mat[ini:n_total, ]
  
  Y     <- Y[ini:n_total]
  X     <- X[ini:n_total]
  covar <- covar[ini:n_total]
  data  <- data[ini:n_total]
  n_efetivo <- length(Y) # tamanho efetivo de Y após o alinhamento
  
  ## --- 2) Calculando as somas de Almon para X e para covar ---
  ## Para o poluente X
  vX <- 1:lags # índices 1, 2, ..., lags a ser utilizados nas somas de Almon de X
  
  # Sk_X(t) = somatório de j=1 até lags de [ (j^k) * X_{t-j} ], para k = 0, 1, ..., d
  S0_X <- rowSums(X_mat)
  S1_X <- X_mat[, -1, drop = FALSE] %*%  vX
  S2_X <- X_mat[, -1, drop = FALSE] %*% (vX^2)
  
  if (d == 2) {
    S_bruto_X <- data.frame(
      S0_X = as.numeric(S0_X),
      S1_X = as.numeric(S1_X),
      S2_X = as.numeric(S2_X)
    )
  } else { # se d == 3, inclui também o S3_X
    S3_X <- X_mat[, -1, drop = FALSE] %*% (vX^3)
    
    S_bruto_X <- data.frame(
      S0_X = as.numeric(S0_X),
      S1_X = as.numeric(S1_X),
      S2_X = as.numeric(S2_X),
      S3_X = as.numeric(S3_X)
    )
  }
  
  ## Para a covariável de confundimento (covar)
  vZ <- 1:lags_covar # índices 1, 2, ..., lags_covar
  
  T0_Z <- rowSums(covar_mat)
  T1_Z <- covar_mat[, -1, drop = FALSE] %*%  vZ
  T2_Z <- covar_mat[, -1, drop = FALSE] %*% (vZ^2)
  
  if (d == 2) {
    S_bruto_Z <- data.frame(
      T0_Z = as.numeric(T0_Z),
      T1_Z = as.numeric(T1_Z),
      T2_Z = as.numeric(T2_Z)
    )
  } else { # se d == 3, inclui também o T3_Z
    T3_Z <- covar_mat[, -1, drop = FALSE] %*% (vZ^3)
    
    S_bruto_Z <- data.frame(
      T0_Z = as.numeric(T0_Z),
      T1_Z = as.numeric(T1_Z),
      T2_Z = as.numeric(T2_Z),
      T3_Z = as.numeric(T3_Z)
    )
  }
  
  ## --- 3) Padronização das Sk de X e das Tk de covar (opcional) ---
  
  ## Para X
  S_dp_X          <- rep(1, ncol(S_bruto_X)) # define S_dp = 1 para cada coluna por padrão
  center_vec_X    <- FALSE                   # não centraliza por padrão
  S_padronizado_X <- S_bruto_X               # mantém igual por padrão
  
  if (padronizar_dp) {
    S_dp_X <- apply(S_bruto_X, 2, sd)
    S_dp_X[!is.finite(S_dp_X) | S_dp_X < 1e-6] <- 1e-6
  }
  if (padronizar_center) {
    center_vec_X <- colMeans(S_bruto_X)
  }
  if (padronizar_center || padronizar_dp) {
    S_padronizado_X <- as.data.frame(
      scale(S_bruto_X, center = center_vec_X, scale = S_dp_X)
    )
  }
  
  ## Para covar
  S_dp_Z          <- rep(1, ncol(S_bruto_Z))
  center_vec_Z    <- FALSE
  S_padronizado_Z <- S_bruto_Z
  
  if (padronizar_dp) {
    S_dp_Z <- apply(S_bruto_Z, 2, sd)
    S_dp_Z[!is.finite(S_dp_Z) | S_dp_Z < 1e-6] <- 1e-6
  }
  if (padronizar_center) {
    center_vec_Z <- colMeans(S_bruto_Z)
  }
  if (padronizar_center || padronizar_dp) {
    S_padronizado_Z <- as.data.frame(
      scale(S_bruto_Z, center = center_vec_Z, scale = S_dp_Z)
    )
  }
  
  ## --- 4) Modelo kDGLM ---
  # bloco de nível com fator de desconto fd_nivel
  nivel <- polynomial_block(rate = 1, order = 1, D = fd_nivel, name = "Nivel")
  
  # blocos de regressão para S0_X, S1_X, ..., Sd_X (poluente)
  b0_X <- regression_block(rate = S_padronizado_X[, 1], D = 1, name = "S0_X")
  b1_X <- regression_block(rate = S_padronizado_X[, 2], D = 1, name = "S1_X")
  b2_X <- regression_block(rate = S_padronizado_X[, 3], D = 1, name = "S2_X")
  
  # blocos de regressão para T0_Z, T1_Z, ..., Td_Z (covariável)
  c0_Z <- regression_block(rate = S_padronizado_Z[, 1], D = 1, name = "T0_Z")
  c1_Z <- regression_block(rate = S_padronizado_Z[, 2], D = 1, name = "T1_Z")
  c2_Z <- regression_block(rate = S_padronizado_Z[, 3], D = 1, name = "T2_Z")
  
  if (d == 2) {
    bloco <- (nivel + b0_X + b1_X + b2_X +
                c0_Z + c1_Z + c2_Z)
  } else { # se d == 3, inclui também o bloco de regressão para S3_X e T3_Z
    b3_X <- regression_block(rate = S_padronizado_X[, 4], D = 1, name = "S3_X")
    c3_Z <- regression_block(rate = S_padronizado_Z[, 4], D = 1, name = "T3_Z")
    bloco <- (nivel + b0_X + b1_X + b2_X + b3_X +
                c0_Z + c1_Z + c2_Z + c3_Z)
  }
  
  desfecho <- Poisson(lambda = "rate", data = Y)
  ajuste <- fit_model(
    bloco,
    y = desfecho)
  
  coefs <- coef(ajuste, lag = -1, eval.pred = TRUE, eval.metric = TRUE, pred.cred = 0.95)
  
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
  
  ## --- 6) Reconstruindo os betas (X) e os taus (covar) por Almon via amostragem bayesiana no tempo final ---
  if (d == 2) {
    indices_beta <- 2:4   # [S0_X, S1_X, S2_X]
    indices_tau  <- 5:7   # [T0_Z, T1_Z, T2_Z]
  } else {
    indices_beta <- 2:5   # [S0_X, S1_X, S2_X, S3_X]
    indices_tau  <- 6:9   # [T0_Z, T1_Z, T2_Z, T3_Z]
  }
  
  estados_media <- coefs$theta.mean # E[θ_t | dados]
  estados_covar <- coefs$theta.cov  # Cov[θ_t | dados]
  
  ## --- 6.1) Betas do poluente X ---
  etaS_beta_media_T_padronizado <- estados_media[indices_beta, n_efetivo]
  cov_eta_beta_T_padronizado    <- estados_covar[indices_beta, indices_beta, n_efetivo]
  
  sd_X_efetivo <- stats::sd(X)
  
  if (is.null(n_amostras) || n_amostras <= 0) {
    etaS_beta_media_T <- as.numeric(etaS_beta_media_T_padronizado / S_dp_X[seq_along(indices_beta)])
    
    j_beta <- 0:(qX - 1)
    if (d == 2) {
      Cj_beta <- cbind(1, j_beta, j_beta^2)
    } else {
      Cj_beta <- cbind(1, j_beta, j_beta^2, j_beta^3)
    }
    
    beta_pt <- as.numeric(etaS_beta_media_T %*% t(Cj_beta))
    rr_beta_estimado    <- as.numeric(exp(sd_X_efetivo * beta_pt))
    rr_sd_estimado      <- rep(NA_real_, qX)
    rr_ic_inferior_beta <- rep(NA_real_, qX)
    rr_ic_superior_beta <- rep(NA_real_, qX)
    
  } else {
    amostras_etaS_beta_padronizado <- MASS::mvrnorm(
      n = n_amostras,
      mu = etaS_beta_media_T_padronizado,
      Sigma = cov_eta_beta_T_padronizado
    )
    
    amostras_etaS_beta <- scale(amostras_etaS_beta_padronizado,
                                center = FALSE, scale = S_dp_X[seq_along(indices_beta)])
    
    j_beta <- 0:(qX - 1)
    if (d == 2) {
      Cj_beta <- cbind(1, j_beta, j_beta^2)
    } else {
      Cj_beta <- cbind(1, j_beta, j_beta^2, j_beta^3)
    }
    
    amostras_beta <- amostras_etaS_beta %*% t(Cj_beta)
    rr_amostras_beta <- exp(sd_X_efetivo * amostras_beta)
    
    rr_beta_estimado    <- colMeans(rr_amostras_beta)
    rr_sd_estimado      <- apply(rr_amostras_beta, 2, sd)
    rr_ic_inferior_beta <- apply(rr_amostras_beta, 2, quantile, probs = 0.025)
    rr_ic_superior_beta <- apply(rr_amostras_beta, 2, quantile, probs = 0.975)
  }
  
  ## --- 6.2) Taus da covariável (covar) ---
  etaS_tau_media_T_padronizado <- estados_media[indices_tau, n_efetivo]
  cov_eta_tau_T_padronizado    <- estados_covar[indices_tau, indices_tau, n_efetivo]
  
  sd_covar_efetivo <- stats::sd(covar)
  
  if (is.null(n_amostras) || n_amostras <= 0) {
    etaS_tau_media_T <- as.numeric(etaS_tau_media_T_padronizado / S_dp_Z[seq_along(indices_tau)])
    
    k_tau <- 0:(qZ - 1)
    if (d == 2) {
      Ck_tau <- cbind(1, k_tau, k_tau^2)
    } else {
      Ck_tau <- cbind(1, k_tau, k_tau^2, k_tau^3)
    }
    
    tau_pt <- as.numeric(etaS_tau_media_T %*% t(Ck_tau))
    rr_tau_estimado    <- as.numeric(exp(sd_covar_efetivo * tau_pt))
    rr_ic_inferior_tau <- rep(NA_real_, qZ)
    rr_ic_superior_tau <- rep(NA_real_, qZ)
    
  } else {
    amostras_etaS_tau_padronizado <- MASS::mvrnorm(
      n = n_amostras,
      mu = etaS_tau_media_T_padronizado,
      Sigma = cov_eta_tau_T_padronizado
    )
    
    amostras_etaS_tau <- scale(amostras_etaS_tau_padronizado,
                               center = FALSE, scale = S_dp_Z[seq_along(indices_tau)])
    
    k_tau <- 0:(qZ - 1)
    if (d == 2) {
      Ck_tau <- cbind(1, k_tau, k_tau^2)
    } else {
      Ck_tau <- cbind(1, k_tau, k_tau^2, k_tau^3)
    }
    
    amostras_tau <- amostras_etaS_tau %*% t(Ck_tau)
    rr_amostras_tau <- exp(sd_covar_efetivo * amostras_tau)
    
    rr_tau_estimado    <- colMeans(rr_amostras_tau)
    rr_ic_inferior_tau <- apply(rr_amostras_tau, 2, quantile, probs = 0.025)
    rr_ic_superior_tau <- apply(rr_amostras_tau, 2, quantile, probs = 0.975)
  }
  
  # --------------------------------------------------------------------------------------------
  
  ## --- 7) Gráficos ---
  
  # g1: Y observado vs mu estimado
  t_index <- as.Date(data)
  df_y <- data.frame(
    t_index    = t_index,
    Y          = Y,
    y_estimado = mu_estimada,
    mu_ic_inf  = mu_ic_inf,
    mu_ic_sup  = mu_ic_sup 
  )
  
  # Eixo X adaptativo
  dt_min <- min(df_y$t_index)
  dt_max <- max(df_y$t_index)
  span_days <- as.integer(dt_max - dt_min)
  
  # Meses em PT-BR
  mes_pt <- c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")
  
  if (span_days <= 366) {
    by <- "1 month"
    lab_fun <- function(d) mes_pt[as.integer(format(d, "%m"))]
  } else if (span_days <= 2*366) { # entre 1 anos e 2 ano
    by <- "2 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else if (span_days <= 4*366) { # entre 2 anos e 4 ano
    by <- "3 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else if (span_days <= 6*366) {
    by <- "6 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else {
    by <- "1 year"
    lab_fun <- function(d) format(d, "%Y")
  }
  
  breaks_seq <- seq(dt_min, dt_max, by = by)
  
  span_years <- as.numeric(difftime(dt_max, dt_min, units = "days")) / 365.25
  
  lw <- if (span_years <= 6) {
    1.00
  } else {
    0.90
  }
  
  g1 <- ggplot(df_y, aes(x = t_index)) +
    geom_line(aes(y = Y,          colour = "Dados"),       linewidth = lw) +
    geom_line(aes(y = y_estimado, colour = "Estimativas"), linewidth = lw) +
    geom_ribbon(aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    geom_hline(yintercept = 0) +
    labs(x = "Data", y = "Internações", colour = "", fill = "") +
    theme_hc() +
    theme(
      axis.title.x = element_text(size = 35),
      axis.text    = element_text(size = 35),
      axis.text.x  = element_text(size = 35, angle = 90),
      legend.text  = element_text(size = 30),
      legend.title = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(size = 35)
    ) +
    scale_color_manual(values = c("Dados" = "black", "Estimativas" = "red"),
                       breaks = c("Estimativas", "Dados")) +
    scale_fill_manual(values = c("IC 95%" = "blue")) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  g1_legado <- ggplot(df_y, aes(x = t_index)) +
    geom_line(aes(y = Y,          colour = "Dados"),       linewidth = 1.6) +
    geom_ribbon(aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    geom_line(aes(y = y_estimado, colour = "Estimativas"), linewidth = 1.6) +
    geom_hline(yintercept = 0) +
    labs(x = "Mês", y = "Internações", colour = "", fill = "") +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 35),
      axis.text    = element_text(size = 35),
      axis.text.x  = element_text(size = 35, angle = 90),
      legend.text  = element_text(size = 30),
      legend.title = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(size = 35)
    ) +
    scale_color_manual(values = c("Dados" = "black", "Estimativas" = "red"),
                       breaks = c("Estimativas", "Dados")) +
    scale_fill_manual(values = c("IC 95%" = "blue")) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  ## g2: Beta(j) do poluente X
  df_beta <- data.frame(
    lag = 0:(qX - 1),
    rr    = as.numeric(rr_beta_estimado),
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
    labs(x = "Lags (X)", y = "Risco Relativo (RR) - poluente") +
    theme_hc(base_size = 1) +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      panel.grid = element_blank(),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = seq(0, qX - 1, by = 1))
  
  g2_legado <- ggplot(df_beta, aes(x = lag, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "blue", color = "blue",
                alpha = 0.2, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_point(size = 3, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Lags (X)", y = "Risco Relativo") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = seq(0, qX - 1, by = 1))
  
  ## g3: Tau(j) da covariável (covar)
  df_tau <- data.frame(
    lag = 0:(qZ - 1),
    rr    = as.numeric(rr_tau_estimado),
    rr_lo = as.numeric(rr_ic_inferior_tau),
    rr_hi = as.numeric(rr_ic_superior_tau)
  )
  
  g3 <- ggplot(df_tau, aes(x = lag, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "cadetblue3", color = "cadetblue3",
                alpha = 0.3, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_point(size = 2.5, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Lags (covariável)", y = "Risco Relativo (RR) - covariável") +
    theme_hc(base_size = 1) +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      panel.grid = element_blank(),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = seq(0, qZ - 1, by = 1))
  
  g3_legado <- ggplot(df_tau, aes(x = lag, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "blue", color = "blue",
                alpha = 0.2, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_point(size = 3, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Lags (covariável)", y = "Risco Relativo") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = seq(0, qZ - 1, by = 1))
  
  return(list(
    mu_media    = mu_estimada,
    mu_ic_inf   = mu_ic_inf,
    mu_ic_sup   = mu_ic_sup,
    beta_media  = rr_beta_estimado,
    beta_sd     = rr_sd_estimado,
    beta_ic_inf = rr_ic_inferior_beta,
    beta_ic_sup = rr_ic_superior_beta,
    tau_media   = rr_tau_estimado,
    tau_ic_inf  = rr_ic_inferior_tau,
    tau_ic_sup  = rr_ic_superior_tau,
    g1          = g1,
    g1_legado   = g1_legado,
    g2          = g2,
    g2_legado   = g2_legado,
    g3          = g3,
    g3_legado   = g3_legado,
    kdglm_coef  = coefs,
    pred_df     = coefs$data,
    ajuste1     = ajuste
  ))
}








avaliar_metricas <- function(
    Y, X, data,
    lags,
    ds  = c(2, 3),
    fds = c(0.95, 0.96, 0.97, 0.98, 0.99, 0.995, 1.00),
    padronizar_center = FALSE,
    padronizar_dp     = TRUE,
    n_amostras        = 0,
    metodo            = "sum"  #modo de agregação de IS e LVP: "sum" ou "mean"
) {
  if (length(Y) != length(X) || length(Y) != length(data)) {
    stop("Y, X e data do mesmo tamanho.")
  }
  if (any(is.na(Y)) || any(is.na(X))) {
    stop("Y e X não podem conter NA.")
  }
  if (min(Y) < 0 || any(Y != round(Y))) {
    stop("Y deve ser contagem (inteiro >= 0).")
  }
  if (missing(lags)) {
    stop("Informe 'lags'.")
  }
  if (length(lags) != 1 || lags < 2 || lags != round(lags)) {
    stop("'lags' deve ser um único inteiro >= 2.")
  }
  Lg <- as.integer(lags)
  
  # Função auxiliar para modificar a forma como calculamos o IS e LVP, dependendo do parâmetro "método"
  if (metodo == "sum") {
    agg_fun <- function(x) { sum(x, na.rm = TRUE) }
  } else {
    agg_fun <- function(x) { mean(x, na.rm = TRUE) }
  }
  
  res <- list()
  for (d in ds) {
    for (fd in fds) {
      fit <- PDLDGLM(
        Y = Y, X = X, data = data,
        lags = Lg, d = d, fd_nivel = fd,
        padronizar_center = padronizar_center,
        padronizar_dp     = padronizar_dp,
        n_amostras        = n_amostras
      )
      
      C <- fit$kdglm_coef
      D <- C$data
      
      y  <- as.numeric(D$Observation)
      
      if ("Prediction" %in% names(D)) {
        mu <- as.numeric(D$Prediction)
      } else {
        mu <- as.numeric(exp(C$lambda.mean[1, ]))
      }
      
      # guarda contra explosões (mu não finito, negativo ou gigantesco)
      LIM_MU <- 1e6
      explodiu <- any(!is.finite(mu)) || any(mu < 0) || any(mu > LIM_MU) || any(!is.finite(y))
      
      if (explodiu) {
        RMSE <- NA_real_
        MAE  <- NA_real_
        MASE <- NA_real_
        IS   <- NA_real_
        LVP  <- NA_real_
        MLPD <- NA_real_
      } else {
        RMSE <- sqrt(mean((y - mu)^2, na.rm = TRUE))
        MAE  <- mean(abs(y - mu),     na.rm = TRUE)
        
        # MASE: MAE / MAE do naive (diferença 1-step)
        denom <- mean(abs(diff(y)), na.rm = TRUE)
        if (is.finite(denom) && denom > 0) {
          MASE <- MAE / denom
        } else {
          MASE <- NA_real_
        }
        
        # IS com if/else explícito
        if (all(c("C.I.lower", "C.I.upper") %in% names(D))) {
          lo <- as.numeric(D$`C.I.lower`)
          hi <- as.numeric(D$`C.I.upper`)
          alpha  <- 0.05
          is_vec <- (hi - lo)
          penal  <- ifelse(y < lo, (2/alpha)*(lo - y),
                           ifelse(y > hi, (2/alpha)*(y - hi), 0))
          is_vec <- is_vec + penal
          IS <- agg_fun(is_vec)
        } else {
          IS <- NA_real_
        }
        
        # LVP Poisson(μ) com agregação sum/mean
        ll_vec <- dpois(y, lambda = mu, log = TRUE)
        LVP    <- agg_fun(ll_vec)
        MLPD   <- mean(ll_vec, na.rm = TRUE)
      }
      
      res[[length(res) + 1]] <- data.frame(
        lags = Lg, d = d, fd = fd,
        MASE = MASE, MAE = MAE, RMSE = RMSE,
        LVP  = LVP,  IS  = IS,
        MLPD = MLPD,
        n_valid = length(y)
      )
    }
  }
  
  tab <- dplyr::bind_rows(res)
  
  # ranking (maximiza LVP; as outras minimizam)
  tab$best_MASE <- tab$MASE == min(tab$MASE, na.rm = TRUE)
  tab$best_MAE  <- tab$MAE  == min(tab$MAE,  na.rm = TRUE)
  tab$best_RMSE <- tab$RMSE == min(tab$RMSE, na.rm = TRUE)
  tab$best_LVP  <- tab$LVP  == max(tab$LVP,  na.rm = TRUE)
  tab$best_IS   <- tab$IS   == min(tab$IS,   na.rm = TRUE)
  
  rsum <- rank(tab$MASE, ties.method = "min") +
    rank(tab$MAE,  ties.method = "min") +
    rank(tab$RMSE, ties.method = "min") +
    rank(tab$IS,   ties.method = "min") +
    rank(-tab$LVP, ties.method = "min")
  tab$rank_sum     <- rsum
  tab$best_overall <- rsum == min(rsum, na.rm = TRUE)
  
  tab[order(tab$rank_sum, tab$d, tab$fd), , drop = FALSE]
}




prepara_base <- function(cidade, ano = NULL, ano1 = NULL, ano2 = NULL) {
  # definir cidades
  cidades <- c(
    rio_de_janeiro = 330455, sao_paulo = 355030, rio_branco = 120040, manaus = 130260,
    porto_velho = 110020, boa_vista = 140010, belem = 150140, macapa = 160030,
    cuiaba = 510340, palmas = 172100, sao_luis = 211130
  )
  
  options(scipen = 999)
  
  # arquivo
  if (is.character(cidade)) cidade <- cidades[[cidade]]
  
  arquivo <- dplyr::case_when(
    cidade == cidades[["sao_paulo"]]      ~ file.path("resultados_tese", "Aplicações", "São Paulo", "base_cidades_sp.rds"),
    cidade == cidades[["rio_de_janeiro"]] ~ file.path("resultados_tese", "Aplicações", "Rio de Janeiro", "base_cidades_rj.rds"),
    TRUE ~ file.path("resultados_tese", "Aplicações", "Amazônia Legal", "base_cidades_amazonia_legal.rds")
  )
  
  base <- readRDS(arquivo)
  nome_casos <- ifelse(cidade %in% cidades[c("sao_paulo", "rio_de_janeiro")], "CasosCID10J", "Casos_Resp")
  
  # ano único = sobrescreve ano1 e ano2
  if (!is.null(ano)) { ano1 <- ano; ano2 <- ano }
  
  data_ini <- as.Date(paste0(ano1, "-01-01"))
  data_fim <- as.Date(paste0(ano2, "-12-31"))
  
  base %>%
    dplyr::filter(CodigoMunicipio == cidade, Data >= data_ini, Data <= data_fim) %>%
    dplyr::transmute(
      Data,
      Casos_Resp = .data[[nome_casos]],
      pm25 = PM2p5,
      co   = CO,
      temp = Temperatura,
      umid = UmidRel
    ) %>%
    dplyr::arrange(Data)
}

























TFDGLM <- function(
    Y,
    X_tf,
    data,
    Z = NULL,
    L = 12,
    family = "Poisson",
    fd_nivel = 0.99,
    fd_tf_state = 0.98,
    fd_tf_beta = 1.00,
    fd_tf_rho = 1.00,
    noise_tf = 0,
    padronizar_tf = TRUE
){
  library(kDGLM)
  library(ggplot2)
  library(MASS)
  
  ## ----------------------------
  ## 1) Checagens básicas
  ## ----------------------------
  if(length(Y) != length(X_tf) || length(Y) != length(data))
    stop("Y, X_tf e data devem ter mesmo comprimento.")
  
  if(any(is.na(Y)) || any(is.na(X_tf)))
    stop("Y e X_tf não podem conter NA.")
  
  if(family != "Poisson")
    stop("Apenas Poisson implementado.")
  
  if(any(Y < 0 | Y != round(Y)))
    stop("Y deve ser contagem.")
  
  n <- length(Y)
  
  ## ----------------------------
  ## 2) Padronizar X_tf
  ## ----------------------------
  x_mean <- mean(X_tf)
  x_sd   <- sd(X_tf)
  
  if(padronizar_tf){
    if(!is.finite(x_sd) || x_sd < 1e-6) x_sd <- 1
    X_tf_sc <- (X_tf - x_mean)/x_sd
  } else {
    X_tf_sc <- X_tf
    x_sd <- 1
  }
  
  ## ----------------------------
  ## 3) Nível dinâmico
  ## ----------------------------
  nivel <- polynomial_block(
    rate  = rep(1, n),
    order = 1,
    D     = fd_nivel,
    name  = "Nivel"
  )
  
  ## ----------------------------
  ## 4) Bloco TF Koyck
  ## ----------------------------
  tf_block <- TF_block(
    rate       = rep(1, n),
    pulse      = X_tf_sc,
    order      = 1,
    noise.var  = noise_tf,
    noise.disc = fd_tf_state,
    D.coef     = fd_tf_rho,
    D.pulse    = fd_tf_beta,
    AR.support = "constrained",
    name       = "TF"
  )
  
  estrutura <- nivel + tf_block
  
  ## ----------------------------
  ## 5) Covariáveis Z
  ## ----------------------------
  if(!is.null(Z)){
    Z <- as.data.frame(Z)
    if(nrow(Z) != n) stop("Dimensão de Z incompatível.")
    
    for(j in seq_len(ncol(Z))){
      estrutura <- estrutura +
        regression_block(
          rate = Z[[j]],
          D = 1,
          name = colnames(Z)[j]
        )
    }
  }
  
  ## ----------------------------
  ## 6) Ajustar modelo
  ## ----------------------------
  desfecho <- Poisson(lambda = "rate", data = Y)
  
  ajuste <- fit_model(estrutura, desfecho)
  
  coefs <- coef(
    ajuste,
    lag = -1,
    eval.pred = TRUE,
    eval.metric = TRUE,
    pred.cred = 0.95
  )
  
  ## ----------------------------
  ## 7) Extrair μ_t
  ## ----------------------------
  if(!is.null(coefs$ft)){
    eta    <- as.numeric(coefs$ft[1, ])
    eta_sd <- sqrt(as.numeric(coefs$Qt[1,1,]))
  } else {
    eta    <- as.numeric(coefs$lambda.mean[1, ])
    eta_sd <- sqrt(as.numeric(coefs$lambda.cov[1,1,]))
  }
  
  mu    <- exp(eta)
  mu_lo <- exp(eta - 1.96*eta_sd)
  mu_hi <- exp(eta + 1.96*eta_sd)
  
  df1 <- data.frame(
    t  = as.Date(data),
    Y  = Y,
    mu = mu,
    lo = mu_lo,
    hi = mu_hi
  )
  
  g1 <- ggplot(df1, aes(t)) +
    geom_line(aes(y = Y), color="black") +
    geom_line(aes(y = mu), color="red") +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill="blue", alpha=.2) +
    labs(x="Data", y="Casos_Resp") +
    theme_minimal(base_size = 16)
  
  ## ----------------------------
  ## 8) Extrair β e ρ
  ## ----------------------------
  theta <- coefs$theta.mean
  Sigma3 <- coefs$theta.cov
  nomes <- rownames(theta)
  Tfinal <- ncol(theta)
  
  idx_beta <- grep("Pulse|effect|beta", nomes, ignore.case = TRUE)[1]
  idx_rho  <- grep("Coef|rho|AR", nomes, ignore.case = TRUE)[1]
  
  if(is.na(idx_beta)) stop("Não achei estado β na TF.")
  if(is.na(idx_rho))  stop("Não achei estado ρ na TF.")
  
  beta <- theta[idx_beta, Tfinal]
  rho  <- theta[idx_rho,  Tfinal]
  
  ## variâncias individuais
  var_rho  <- Sigma3[idx_rho,  idx_rho,  Tfinal]
  var_beta <- Sigma3[idx_beta, idx_beta, Tfinal]
  
  ## ----------------------------
  ## 9) Amostragem posterior (VERSÃO 2 — robusta)
  ## ----------------------------
  rho_s  <- rnorm(5000, mean = rho,  sd = sqrt(var_rho))
  beta_s <- rnorm(5000, mean = beta, sd = sqrt(var_beta))
  
  ## limitar rho_s para estabilidade
  rho_s <- pmin(pmax(rho_s, -0.999), 0.999)
  
  j <- 0:L
  
  RR_matrix <- sapply(j, function(l){
    delta_s <- beta_s * (rho_s^l)
    exp(delta_s)
  })
  
  df2 <- data.frame(
    lag = j,
    RR  = apply(RR_matrix, 2, mean),
    lo  = apply(RR_matrix, 2, quantile, 0.025),
    hi  = apply(RR_matrix, 2, quantile, 0.975)
  )
  
  g2 <- ggplot(df2, aes(lag, RR)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill="lightblue", alpha=.4) +
    geom_line(size=1.2) +
    geom_point(size=2) +
    geom_hline(yintercept = 1, linetype=2) +
    labs(x="Lags (j)", y="RR para +1 DP em X") +
    theme_minimal(base_size=16)
  
  ## ----------------------------
  ## 10) Retorno completo
  ## ----------------------------
  list(
    g1 = g1,
    g2 = g2,
    mu = mu,
    RR = df2,
    ajuste = ajuste,
    beta = beta,
    rho = rho,
    x_sd = x_sd,
    kdglm_coef = coefs
  )
}






# ------------------------------------------------------------------------------
# LM_nivel_sim()
# - Simula um DLM Normal (nível local) e ajusta com filtro Bayesiano
#   usando fator de desconto δθ = 0.90, 0.95, 0.99 (ou qualquer vetor em deltas).
# - Se Vt for informado -> caso com variância observacional conhecida.
# - Se Vt = NULL (default) -> caso com variância observacional desconhecida.
# - Retorna gráficos no estilo das suas funções (theme_hc + fontes grandes).
# ------------------------------------------------------------------------------

LM_nivel_sim <- function(
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
    alpha_lines = 1.00,     # <- só nas linhas ft (geom_line)
    lw_main     = 1.20,     # <- Observações + Estimativas
    lw_ic       = 1.00,     # <- IC tracejado
    theta_true  = NULL
) {
  
  ic_style <- match.arg(ic_style)
  
  ## --- Checagens ---
  if (missing(y)) stop("Informe o vetor 'y'.")
  if (!is.numeric(y) || any(!is.finite(y))) stop("'y' deve ser numérico e finito.")
  if (length(y) < 2) stop("'y' deve ter comprimento >= 2.")
  
  if (!is.numeric(deltas) || any(!is.finite(deltas)) || any(deltas <= 0) || any(deltas > 1))
    stop("'deltas' deve estar em (0,1].")
  deltas <- as.numeric(deltas)
  
  if (!is.null(Vt)) {
    if (!is.numeric(Vt) || length(Vt) != 1 || !is.finite(Vt) || Vt <= 0)
      stop("'Vt' deve ser > 0 quando informado.")
  }
  
  if (!(is.numeric(nivel_ic) && nivel_ic > 0 && nivel_ic < 1))
    stop("'nivel_ic' deve estar em (0,1).")
  
  if (!(is.numeric(alpha_ic) && alpha_ic > 0 && alpha_ic < 1))
    stop("'alpha_ic' deve estar em (0,1).")

  
  if (!(is.numeric(lw_main) && lw_main > 0)) stop("'lw_main' deve ser > 0.")
  if (!(is.numeric(lw_ic)   && lw_ic   > 0)) stop("'lw_ic' deve ser > 0.")
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Instale 'ggplot2'.")
  if (!requireNamespace("ggthemes", quietly = TRUE)) stop("Instale 'ggthemes'.")
  if (!requireNamespace("grid", quietly = TRUE)) stop("Instale 'grid'.")
  
  if (!is.null(theta_true)) {
    if (!is.numeric(theta_true) || length(theta_true) != length(y) || any(!is.finite(theta_true)))
      stop("'theta_true' deve ter mesmo comprimento de 'y'.")
  }
  
  ## --- Série ---
  Y <- as.numeric(y)
  n <- length(Y)
  t_idx <- 0:(n - 1)
  
  ## --- Filtros ---
  filtro_knownV <- function(y, delta, m0, C0, Vt) {
    Tn <- length(y); D <- 1 / delta
    at <- Rt <- mt <- Ct <- ft <- Qt <- et <- At <- numeric(Tn)
    
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
  
  filtro_unknownV <- function(y, delta, m0, C0, n0, S0) {
    Tn <- length(y); D <- 1 / delta
    at <- Rt <- mt <- Ct <- ft <- Qt <- et <- At <- nt <- St <- numeric(Tn)
    
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
  
  fits <- vector("list", length(deltas))
  for (i in seq_along(deltas)) {
    if (!is.null(Vt)) fits[[i]] <- filtro_knownV(Y, deltas[i], m0, C0, Vt)
    else              fits[[i]] <- filtro_unknownV(Y, deltas[i], m0, C0, n0, S0)
  }
  
  ## --- pred_df ---
  alpha <- 1 - nivel_ic
  pred_list <- vector("list", length(deltas))
  
  for (i in seq_along(deltas)) {
    ft <- fits[[i]]$ft
    Qt <- fits[[i]]$Qt
    
    if (!is.null(Vt)) {
      crit <- stats::qnorm(1 - alpha/2)
    } else {
      nt <- fits[[i]]$nt
      df_pred <- numeric(n)
      df_pred[1] <- n0
      if (n >= 2) df_pred[2:n] <- nt[1:(n - 1)]
      df_pred[df_pred < 1] <- 1
      crit <- stats::qt(1 - alpha/2, df = df_pred)
    }
    
    lo <- ft - crit * sqrt(Qt)
    hi <- ft + crit * sqrt(Qt)
    
    pred_list[[i]] <- data.frame(
      t = t_idx, delta = deltas[i], ft = ft, lo = lo, hi = hi
    )
  }

  pred_df <- do.call(rbind, pred_list)
  ## --- Estilo ---
  base_cols <- c("#1F78B4", "#984EA3", "#F1B514")
  if (length(deltas) > length(base_cols)) base_cols <- grDevices::rainbow(length(deltas))
  
  delta_keys <- paste0("d", formatC(deltas, format="f", digits=2))
  pred_df$delta_key <- factor(paste0("d", formatC(pred_df$delta, format="f", digits=2)),
                              levels = delta_keys)
  
  col_base <- stats::setNames(base_cols[seq_along(deltas)], delta_keys)
  
  delta_lbl_txt <- gsub("\\.", ",", sprintf('delta[theta]~"= %0.2f"', deltas))
  lab_map <- stats::setNames(parse(text = delta_lbl_txt), delta_keys)
  
  df_y <- data.frame(t = t_idx, Y = Y)
  
  ## --- Plot ---
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = df_y,
      ggplot2::aes(x = t, y = Y, colour = "Dados"),
      linewidth = lw_main
    ) +
    ggplot2::geom_line(
      data = pred_df,
      ggplot2::aes(x = t, y = ft, colour = delta_key),
      linewidth = lw_main,
      alpha = alpha_lines      # <- AQUI: alpha só das estimativas
    )
  
  if (ic_style == "ribbon") {
    p <- p + ggplot2::geom_ribbon(
      data = pred_df,
      ggplot2::aes(x = t, ymin = lo, ymax = hi, fill = delta_key),
      alpha = alpha_ic,
      show.legend = FALSE
    )
  } else if (ic_style == "dashed") {
    for (key in delta_keys) {
      dfk <- pred_df[pred_df$delta_key == key, , drop = FALSE]
      p <- p +
        ggplot2::geom_line(
          data = dfk,
          ggplot2::aes(x = t, y = lo),
          colour = col_base[[key]],
          linewidth = lw_ic,
          linetype = "dashed",
          alpha = alpha_ic,          # <- AQUI
          show.legend = FALSE
        ) +
        ggplot2::geom_line(
          data = dfk,
          ggplot2::aes(x = t, y = hi),
          colour = col_base[[key]],
          linewidth = lw_ic,
          linetype = "dashed",
          alpha = alpha_ic,          # <- AQUI
          show.legend = FALSE
        )
    }
  }
  
  g1_relatorio <- p +
    ggplot2::geom_hline(yintercept = 0) +
    ggthemes::theme_hc() +
    ggplot2::theme(
      axis.title     = ggplot2::element_text(size = 35),
      axis.text      = ggplot2::element_text(size = 30),
      legend.text    = ggplot2::element_text(size = 30),
      legend.title   = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggplot2::labs(x = "Tempo", y = "Resposta", colour = "") +
    ggplot2::scale_color_manual(
      values = c("Dados" = "black", col_base),
      breaks = c("Dados", delta_keys),
      labels = c("Observações", unname(lab_map))
    ) +
    ggplot2::scale_fill_manual(values = col_base, breaks = delta_keys, drop = TRUE) +
    ggplot2::scale_x_continuous(
      limits = c(0, n - 1),
      breaks = seq(0, n - 1, by = 25),
      expand = c(0, 0)
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(override.aes = list(alpha = 1))
    )
  
  g1 <- g1_relatorio + ggplot2::theme(legend.position = "none")
  
  list(
    Y = Y,
    pred_df = pred_df,
    fits = fits,
    g1 = g1,
    g1_relatorio = g1_relatorio,
    params = list(alpha_lines = alpha_lines, lw_main = lw_main, lw_ic = lw_ic)
  )
}







DGLM_nivel_poisson <- function(
    Y,                          # contagens
    deltas      = c(0.90, 0.95, 0.99),
    nivel_ic    = 0.95,
    ic_style    = c("dashed", "ribbon", "none"),
    alpha_ic    = 0.12,
    alpha_lines = 1.00,         # só nas linhas centrais (mu)
    lw_main     = 1.20,         # observações + estimativas
    lw_ic       = 1.00,         # somente IC (tracejado)
    usar_ci_kdglm = FALSE,      # se TRUE e existir C.I.lower/upper em coefs$data, usa aquilo (escala de Y)
    # ---- tamanhos (pra igualar com LM_nivel_sim ou ajustar)
    size_axis_title  = 35,
    size_axis_text   = 35,
    size_legend_text = 30,
    x_breaks_by      = 25
) {
  
  ic_style <- match.arg(ic_style)
  
  ## --- 0) Checagens ---
  if (missing(Y)) stop("Informe 'Y'.")
  if (!is.numeric(Y) || any(is.na(Y))) stop("'Y' deve ser numérico sem NA.")
  if (any(Y < 0) || any(Y != round(Y))) stop("'Y' deve ser contagem (inteiro >= 0).")
  
  if (!is.numeric(deltas) || any(!is.finite(deltas)) || any(deltas <= 0) || any(deltas > 1))
    stop("'deltas' deve estar em (0,1]. Ex.: c(0.90, 0.95, 0.99).")
  
  # mantém a ORDEM que você passou e remove duplicatas
  deltas <- as.numeric(deltas)
  deltas <- deltas[!duplicated(deltas)]
  if (length(deltas) < 1) stop("'deltas' deve ter pelo menos 1 valor.")
  
  if (!(is.numeric(nivel_ic) && nivel_ic > 0 && nivel_ic < 1))
    stop("'nivel_ic' deve estar em (0,1). Ex.: 0.95")
  
  if (!(is.numeric(alpha_ic) && alpha_ic > 0 && alpha_ic < 1))
    stop("'alpha_ic' deve estar em (0,1).")
  
  if (!(is.numeric(alpha_lines) && alpha_lines > 0 && alpha_lines <= 1))
    stop("'alpha_lines' deve estar em (0,1].")
  
  if (!(is.numeric(lw_main) && lw_main > 0)) stop("'lw_main' deve ser > 0.")
  if (!(is.numeric(lw_ic)   && lw_ic   > 0)) stop("'lw_ic' deve ser > 0.")
  
  if (!requireNamespace("kDGLM", quietly = TRUE))    stop("Instale o pacote 'kDGLM'.")
  if (!requireNamespace("ggplot2", quietly = TRUE))  stop("Instale o pacote 'ggplot2'.")
  if (!requireNamespace("ggthemes", quietly = TRUE)) stop("Instale o pacote 'ggthemes'.")
  if (!requireNamespace("grid", quietly = TRUE))     stop("Instale o pacote 'grid'.")
  
  if (!is.numeric(size_axis_title) || size_axis_title <= 0) stop("'size_axis_title' inválido.")
  if (!is.numeric(size_axis_text)  || size_axis_text  <= 0) stop("'size_axis_text' inválido.")
  if (!is.numeric(size_legend_text)|| size_legend_text<= 0) stop("'size_legend_text' inválido.")
  if (!is.numeric(x_breaks_by) || x_breaks_by <= 0) stop("'x_breaks_by' deve ser > 0.")
  
  ## --- 1) Índice temporal e constantes ---
  Y <- as.numeric(Y)
  n <- length(Y)
  t_idx <- 0:(n - 1)
  
  alpha <- 1 - nivel_ic
  zcrit <- stats::qnorm(1 - alpha/2)
  
  fits <- vector("list", length(deltas))
  names(fits) <- paste0("delta_", formatC(deltas, format = "f", digits = 2))
  
  pred_list <- vector("list", length(deltas))
  
  ## --- 2) Ajuste kDGLM para cada delta ---
  for (i in seq_along(deltas)) {
    
    dlt <- deltas[i]
    
    nivel <- kDGLM::polynomial_block(
      rate  = 1,
      order = 1,
      D     = dlt,
      name  = "Nivel"
    )
    
    desfecho <- kDGLM::Poisson(lambda = "rate", data = Y)
    
    ajuste <- kDGLM::fit_model(nivel, y = desfecho)
    
    # usar o genérico do stats (método do kDGLM faz o dispatch)
    coefs <- stats::coef(
      ajuste,
      lag = -1,
      eval.pred = TRUE,
      eval.metric = TRUE,
      pred.cred = nivel_ic
    )
    
    # --- extrair eta e sd(eta) ---
    if (!is.null(coefs$ft)) {
      eta    <- as.numeric(coefs$ft[1, ])
      eta_sd <- sqrt(as.numeric(coefs$Qt[1, 1, ]))
    } else {
      eta    <- as.numeric(coefs$lambda.mean[1, ])
      eta_sd <- sqrt(as.numeric(coefs$lambda.cov[1, 1, ]))
    }
    
    mu <- exp(eta)
    
    # IC default: aproximando Normal em eta e transformando
    lo_mu <- exp(eta - zcrit * eta_sd)
    hi_mu <- exp(eta + zcrit * eta_sd)
    
    # opcional: usar IC do coefs$data (quando existir) – normalmente no espaço de Y
    if (isTRUE(usar_ci_kdglm) && !is.null(coefs$data)) {
      Dtab <- coefs$data
      if (all(c("C.I.lower", "C.I.upper") %in% names(Dtab))) {
        lo_mu <- as.numeric(Dtab$`C.I.lower`)
        hi_mu <- as.numeric(Dtab$`C.I.upper`)
      }
      if ("Prediction" %in% names(Dtab)) {
        mu <- as.numeric(Dtab$Prediction)
      }
    }
    
    pred_list[[i]] <- data.frame(
      t = t_idx,
      delta = dlt,
      mu = mu,
      lo = lo_mu,
      hi = hi_mu,
      eta = eta,
      eta_sd = eta_sd
    )
    
    fits[[i]] <- list(ajuste = ajuste, coefs = coefs)
  }
  
  pred_df <- do.call(rbind, pred_list)
  
  ## --- 3) Estilo (cores/labels) dependentes de deltas ---
  base_cols <- c("red", "blue", "green", "purple", "orange", "brown")
  if (length(deltas) > length(base_cols)) base_cols <- grDevices::rainbow(length(deltas))
  base_cols <- base_cols[seq_along(deltas)]
  
  delta_keys <- paste0("d", formatC(deltas, format="f", digits=2))
  
  pred_df$delta_key <- factor(
    paste0("d", formatC(pred_df$delta, format="f", digits=2)),
    levels = delta_keys
  )
  
  col_base <- stats::setNames(base_cols, delta_keys)
  
  # legenda no estilo delta_theta com vírgula
  delta_lbl_txt <- gsub("\\.", ",", sprintf('delta[theta]~"= %0.2f"', deltas))
  lab_map <- stats::setNames(parse(text = delta_lbl_txt), delta_keys)
  
  df_y <- data.frame(t = t_idx, Y = Y)
  
  ## --- 4) Plot ---
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = df_y,
      ggplot2::aes(x = t, y = Y, colour = "Dados"),
      linewidth = lw_main
    ) +
    ggplot2::geom_line(
      data = pred_df,
      ggplot2::aes(x = t, y = mu, colour = delta_key),
      linewidth = lw_main,
      alpha = alpha_lines
    )
  
  if (ic_style == "ribbon") {
    
    p <- p + ggplot2::geom_ribbon(
      data = pred_df,
      ggplot2::aes(x = t, ymin = lo, ymax = hi, fill = delta_key),
      alpha = alpha_ic,
      show.legend = FALSE
    )
    
  } else if (ic_style == "dashed") {
    
    # tracejados opacos e sem alpha_lines
    for (key in delta_keys) {
      dfk <- pred_df[pred_df$delta_key == key, , drop = FALSE]
      
      p <- p +
        ggplot2::geom_line(
          data = dfk,
          ggplot2::aes(x = t, y = lo),
          colour = col_base[[key]],
          linewidth = lw_ic,
          linetype = "dashed",
          show.legend = FALSE
        ) +
        ggplot2::geom_line(
          data = dfk,
          ggplot2::aes(x = t, y = hi),
          colour = col_base[[key]],
          linewidth = lw_ic,
          linetype = "dashed",
          show.legend = FALSE
        )
    }
  }
  
  g1_relatorio <- p +
    ggthemes::theme_hc() +
    ggplot2::theme(
      axis.title.x    = ggplot2::element_text(size = size_axis_title),
      axis.title.y    = ggplot2::element_text(size = size_axis_title),
      axis.text       = ggplot2::element_text(size = size_axis_text),
      legend.text     = ggplot2::element_text(size = size_legend_text),
      legend.title    = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.key.width = grid::unit(1.6, "cm"),
      legend.spacing.x = grid::unit(0.35, "cm")
    ) +
    ggplot2::labs(x = "", y = "Dados", colour = "") +
    ggplot2::scale_color_manual(
      values = c("Dados" = "black", col_base),
      breaks = c("Dados", delta_keys),
      labels = c("Observações", unname(lab_map))
    ) +
    ggplot2::scale_fill_manual(values = col_base, breaks = delta_keys, drop = TRUE) +
    ggplot2::scale_x_continuous(
      limits = c(0, n - 1),
      breaks = seq(0, n - 1, by = x_breaks_by),
      expand = c(0, 0)
    )
  
  g1 <- g1_relatorio + ggplot2::theme(legend.position = "none")
  
  list(
    pred_df = pred_df,   # t, delta, mu, lo, hi, eta, eta_sd
    fits = fits,         # ajuste + coefs por delta
    g1 = g1,
    g1_relatorio = g1_relatorio
  )
}







# ------------------------------------------------------------------------------
# sim_PDLDGLM_poisson()
# Simula dados de um DGLM Poisson com defasagem distribuída polinomial (Almon),
# coerente com:
#   y_t | mu_t ~ Poisson(mu_t)
#   log(mu_t) = F_t' theta_t
#   theta_t = G theta_{t-1} + w_t, w_t ~ N(0, W_t)
# onde:
#   theta_t = (alpha_t, eta_0,...,eta_d)'
#   F_t'    = (1, S_{t,0},...,S_{t,d})
#   G       = I
#   W_t     = diag(W_alpha, 0,...,0)  (apenas alpha_t evolui)
#
# OBS: retorna y_full e x_full com comprimento n_total (as primeiras lags
# serão descartadas no ajuste, como no seu PDLDGLM()).
# ------------------------------------------------------------------------------
sim_PDLDGLM_poisson <- function(
    n_total = 365,
    lags    = 16,
    d       = 2,
    seed    = 5,
    
    # --- Covariável X_t (random walk)
    x0      = 2,
    Wx      = 0.001,
    x_drift = 0,
    
    # --- Nível (log-baseline) alpha_t (random walk)
    alpha1  = log(30),
    W_alpha = 0.001,
    
    # --- Coeficientes polinomiais verdadeiros (na escala BRUTA dos S_k)
    # d=2 -> c(eta0, eta1, eta2)
    # d=3 -> c(eta0, eta1, eta2, eta3)
    eta_raw = NULL,
    
    # --- Padronização das S_k (mesmo padrão do seu PDLDGLM)
    padronizar_center = FALSE,
    padronizar_dp     = TRUE,
    
    # --- Datas (opcional)
    data = NULL,
    data_start = as.Date("2020-01-01"),
    
    # --- Guard-rail numérico (evita exp() explodir)
    mu_max = 1e6
) {
  ## Checagens
  if (!is.numeric(n_total) || length(n_total) != 1 || n_total < 5 || n_total != round(n_total))
    stop("'n_total' deve ser inteiro >= 5.")
  if (!is.numeric(lags) || length(lags) != 1 || lags < 2 || lags != round(lags))
    stop("'lags' deve ser inteiro >= 2.")
  if (lags + 1 >= n_total)
    stop("Série muito curta: precisa n_total > lags + 1.")
  if (!(d %in% c(2,3)))
    stop("'d' deve ser 2 ou 3.")
  if (!is.numeric(Wx) || Wx <= 0) stop("'Wx' deve ser > 0.")
  if (!is.numeric(W_alpha) || W_alpha <= 0) stop("'W_alpha' deve ser > 0.")
  if (!is.numeric(alpha1) || length(alpha1) != 1) stop("'alpha1' deve ser escalar numérico.")
  if (!is.numeric(mu_max) || mu_max <= 0) stop("'mu_max' deve ser > 0.")
  
  set.seed(seed)
  
  ## Datas
  if (is.null(data)) {
    data <- seq.Date(data_start, by = "day", length.out = n_total)
  } else {
    if (length(data) != n_total) stop("'data' deve ter comprimento n_total.")
    data <- as.Date(data)
  }
  
  ## 1) Gera X_t (RW suave)
  x <- numeric(n_total)
  x[1] <- x0
  for (t in 2:n_total) {
    x[t] <- x[t-1] + x_drift + rnorm(1, 0, sqrt(Wx))
  }
  
  ## 2) Matriz de lags (lag 0..lags)
  q <- lags + 1
  X_mat <- matrix(NA_real_, nrow = n_total, ncol = q)
  X_mat[,1] <- x
  for (j in 1:lags) {
    X_mat[(j+1):n_total, j+1] <- x[1:(n_total - j)]
  }
  
  ## 3) Recorte efetivo (como no seu PDLDGLM): remove primeiras 'lags'
  idx_eff <- (lags + 1):n_total
  T_eff   <- length(idx_eff)
  
  X_eff <- X_mat[idx_eff, , drop = FALSE]  # sem NA
  
  ## 4) Somas S0..Sd (Almon)
  v <- 1:lags
  S_list <- vector("list", d + 1)
  
  S_list[[1]] <- rowSums(X_eff)  # S0 inclui lag0 (e lags)
  
  X_no0 <- X_eff[, -1, drop = FALSE]  # lags 1..lags
  for (k in 1:d) {
    S_list[[k+1]] <- as.numeric(X_no0 %*% (v^k))
  }
  
  S_raw <- as.data.frame(S_list)
  colnames(S_raw) <- paste0("S", 0:d)
  
  ## 5) Padronização (mesma lógica do PDLDGLM)
  center_vec <- if (padronizar_center) colMeans(S_raw) else rep(0, ncol(S_raw))
  
  scale_vec <- if (padronizar_dp) apply(S_raw, 2, sd) else rep(1, ncol(S_raw))
  scale_vec[!is.finite(scale_vec) | scale_vec < 1e-6] <- 1e-6
  
  S_pad <- sweep(sweep(as.matrix(S_raw), 2, center_vec, "-"), 2, scale_vec, "/")
  S_pad <- as.data.frame(S_pad)
  colnames(S_pad) <- colnames(S_raw)
  
  ## 6) eta_raw default (na escala BRUTA)
  if (is.null(eta_raw)) {
    if (d == 2) eta_raw <- c(0.2, -0.03, 0.001)
    if (d == 3) eta_raw <- c(0.2, -0.03, 0.001, -0.00002)
  }
  eta_raw <- as.numeric(eta_raw)
  if (length(eta_raw) != (d + 1))
    stop(sprintf("'eta_raw' deve ter comprimento %d (d+1).", d+1))
  
  ## 7) Converter para escala padronizada (para o link usar S_pad)
  # log(mu) = alpha + eta_raw' S_raw
  # com S_raw = center + scale * S_pad  =>  eta_pad = eta_raw * scale
  # e intercepto shift: alpha_pad = alpha + eta_raw' center
  eta_pad   <- eta_raw * scale_vec
  alpha1_pad <- alpha1 + sum(eta_raw * center_vec)
  
  ## 8) Simula alpha_t no eixo ORIGINAL (n_total) e aplica no trecho efetivo
  alpha_full <- numeric(n_total)
  alpha_full[1] <- alpha1_pad
  for (t in 2:n_total) {
    alpha_full[t] <- alpha_full[t-1] + rnorm(1, 0, sqrt(W_alpha))
  }
  alpha_eff <- alpha_full[idx_eff]
  
  ## 9) Linear predictor no trecho efetivo
  eta_eff <- as.numeric(alpha_eff + as.matrix(S_pad) %*% eta_pad)
  mu_eff  <- pmin(exp(eta_eff), mu_max)
  
  ## 10) Construir mu_full (baseline fora do trecho efetivo) e gerar y_full
  mu_full <- pmin(exp(alpha_full), mu_max)
  mu_full[idx_eff] <- mu_eff
  
  y_full <- rpois(n_total, lambda = mu_full)
  y_eff  <- y_full[idx_eff]
  
  ## 11) Theta/F no trecho efetivo (pra debug/validação)
  p <- d + 2
  Ft_eff <- rbind(rep(1, T_eff), t(as.matrix(S_pad)))   # p x T_eff
  theta_eff <- matrix(0, nrow = p, ncol = T_eff)
  theta_eff[1, ] <- alpha_eff
  theta_eff[2:p, ] <- matrix(eta_pad, nrow = (p-1), ncol = T_eff)
  
  ## 12) Kernel verdadeiro beta_j (j=0..lags) na escala BRUTA
  j <- 0:lags
  Cj <- sapply(0:d, function(k) j^k)   # (lags+1) x (d+1)
  beta_true <- as.numeric(Cj %*% eta_raw)
  
  list(
    # séries “para usar direto no PDLDGLM” (mesmo comprimento)
    y_full = y_full,
    x_full = x,
    data_full = data,
    
    # trecho efetivo (o que realmente entra no modelo depois do trim)
    idx_eff = idx_eff,
    y_eff = y_eff,
    mu_eff = mu_eff,
    alpha_eff = alpha_eff,
    S_raw = S_raw,
    S_pad = S_pad,
    
    # parâmetros/estruturas úteis
    d = d, lags = lags, n_total = n_total, T_eff = T_eff,
    alpha1 = alpha1, W_alpha = W_alpha,
    x0 = x0, Wx = Wx, x_drift = x_drift,
    eta_raw = eta_raw,
    eta_pad = eta_pad,
    center_vec = center_vec,
    scale_vec = scale_vec,
    beta_true = beta_true,
    
    # debug
    Ft_eff = Ft_eff,
    theta_eff = theta_eff,
    mu_full = mu_full,
    alpha_full = alpha_full
  )
}





# ------------------------------------------------------------------------------
# PDLDGLM_poisson_sim()
# - Simula um DGLM Poisson com Defasagem Polinomial (Almon), conforme:
#   log(mu_t) = alpha_t + sum_{k=0}^d eta_k * S_{t,k}
#   alpha_t = alpha_{t-1} + w_t,  w_t ~ N(0, W_alpha)
#   eta_k = constante (sem evolução)
#
# S_{t,0} = sum_{j=0}^L X_{t-j}
# S_{t,k} = sum_{j=1}^L (j^k) X_{t-j}, k>=1  (j=0 não contribui)
#
# Retorna série completa sem NA (compatível com suas funções, que depois descartam
# as primeiras (lags) observações internamente).
# ------------------------------------------------------------------------------

PDLDGLM_poisson_sim <- function(
    n_total = 365,
    lags    = 16,
    d       = 2,
    eta_true = NULL,            # vetor (eta0..etad), comprimento d+1
    # nível (log) : alpha_1 ~ N(m0, C0), depois RW com W_alpha
    m0      = log(30),
    C0      = 0.02,
    W_alpha = 0.001,
    
    # covariável X (se não passar, gera RW suave)
    X       = NULL,
    x0      = 2,
    W_x     = 0.001,
    
    # datas
    data        = NULL,
    data_inicio = as.Date("2000-01-01"),
    
    # segurança numérica
    cap_mu  = 1e6,
    
    # se TRUE, devolve também matrizes grandes (X_lags, F)
    retornar_matrizes = TRUE
) {
  
  ## -------------------- 0) Checagens --------------------
  if (length(n_total) != 1 || n_total < 5 || n_total != round(n_total))
    stop("'n_total' deve ser inteiro >= 5.")
  
  if (length(lags) != 1 || lags < 2 || lags != round(lags))
    stop("'lags' deve ser inteiro >= 2.")
  
  if (lags + 1 > n_total)
    stop("Série muito curta: precisa n_total >= lags + 1.")
  
  if (length(d) != 1 || d < 0 || d != round(d))
    stop("'d' deve ser inteiro >= 0 (na prática, use 2 ou 3).")
  
  if (is.null(eta_true)) {
    # default conservador (evita explodir mu)
    # (eta0, eta1, eta2)/(10) no espírito do seu exemplo
    if (d == 2) eta_true <- c(2, -0.3, 0.01) / 10
    if (d == 3) eta_true <- c(2, -0.3, 0.01, 0.000) / 10
    if (!(d %in% c(2,3))) eta_true <- rep(0, d+1)
  }
  eta_true <- as.numeric(eta_true)
  if (length(eta_true) != (d + 1))
    stop("Comprimento de 'eta_true' deve ser d+1 (eta0..etad).")
  
  if (!is.numeric(m0) || length(m0) != 1 || !is.finite(m0))
    stop("'m0' inválido.")
  
  if (!is.numeric(C0) || length(C0) != 1 || !is.finite(C0) || C0 < 0)
    stop("'C0' inválido (>=0).")
  
  if (!is.numeric(W_alpha) || length(W_alpha) != 1 || !is.finite(W_alpha) || W_alpha < 0)
    stop("'W_alpha' inválido (>=0).")
  
  if (!is.numeric(cap_mu) || length(cap_mu) != 1 || !is.finite(cap_mu) || cap_mu <= 0)
    stop("'cap_mu' inválido (>0).")
  
  ## -------------------- 1) Datas ------------------------
  if (!is.null(data)) {
    if (length(data) != n_total) stop("'data' deve ter comprimento n_total.")
    data <- as.Date(data)
  } else {
    data <- seq.Date(from = as.Date(data_inicio), by = "day", length.out = n_total)
  }
  
  ## -------------------- 2) Gerar / checar X -------------
  if (is.null(X)) {
    X <- numeric(n_total)
    X[1] <- x0
    if (n_total >= 2) {
      for (t in 2:n_total) X[t] <- X[t-1] + stats::rnorm(1, 0, sqrt(W_x))
    }
  } else {
    X <- as.numeric(X)
    if (length(X) != n_total) stop("'X' deve ter comprimento n_total.")
    if (any(!is.finite(X))) stop("'X' não pode ter NA/Inf.")
  }
  
  ## -------------------- 3) Matriz de lags (sem NA) -------
  # X_lags[t, 1] = X_t, X_lags[t, 2] = X_{t-1}, ..., X_lags[t, lags+1] = X_{t-lags}
  q <- lags + 1
  X_lags <- matrix(0, nrow = n_total, ncol = q)
  
  for (j in 0:lags) {
    idx <- (j + 1):n_total
    X_lags[idx, j + 1] <- X[1:(n_total - j)]
  }
  
  ## -------------------- 4) Somas S0..Sd ------------------
  # S0 = sum_{j=0}^lags X_{t-j}
  # Sk = sum_{j=1}^lags j^k * X_{t-j}, k>=1
  S_mat <- matrix(0, nrow = n_total, ncol = d + 1)
  colnames(S_mat) <- paste0("S", 0:d)
  
  S_mat[, 1] <- rowSums(X_lags)
  
  if (d >= 1) {
    v <- 1:lags
    bloco_lags <- X_lags[, 2:q, drop = FALSE]  # só lags 1..L
    for (k in 1:d) {
      S_mat[, k + 1] <- as.numeric(bloco_lags %*% (v^k))
    }
  }
  
  ## -------------------- 5) Simular alpha_t (nível no log)-
  alpha <- numeric(n_total)
  alpha[1] <- stats::rnorm(1, mean = m0, sd = sqrt(C0))
  
  if (n_total >= 2) {
    for (t in 2:n_total) {
      alpha[t] <- alpha[t-1] + stats::rnorm(1, 0, sqrt(W_alpha))
    }
  }
  
  ## -------------------- 6) Gerar mu_t e y_t --------------
  # log(mu_t) = alpha_t + S_t %*% eta_true
  log_mu <- alpha + as.numeric(S_mat %*% eta_true)
  
  # segurança numérica (evita overflow em exp)
  log_mu_cap <- log(cap_mu)
  log_mu <- pmin(log_mu, log_mu_cap)
  
  mu <- exp(log_mu)
  y  <- stats::rpois(n_total, lambda = mu)
  
  ## -------------------- 7) Betas verdadeiros por lag -----
  # beta_j = eta0 + eta1*j + ... + etad*j^d, j=0..lags
  j_grid <- 0:lags
  Cj <- sapply(0:d, function(k) j_grid^k)  # (lags+1) x (d+1)
  beta_true <- as.numeric(Cj %*% eta_true)
  
  # RR verdadeiro no mesmo espírito do seu g2: RR para +1 DP em X (janela efetiva)
  ini_eff <- lags + 1
  sd_X_eff <- stats::sd(X[ini_eff:n_total])
  if (!is.finite(sd_X_eff) || sd_X_eff < 1e-12) sd_X_eff <- 1
  RR_true <- exp(sd_X_eff * beta_true)
  
  ## -------------------- 8) Empacotar retornos ------------
  df <- data.frame(
    Data   = data,
    Y      = y,
    X      = X,
    alpha  = alpha,
    log_mu = log_mu,
    mu     = mu
  )
  df <- cbind(df, as.data.frame(S_mat))
  
  out <- list(
    df = df,
    Y  = y,
    X  = X,
    data = data,
    alpha = alpha,
    log_mu = log_mu,
    mu = mu,
    
    S = as.data.frame(S_mat),
    
    # verdade (Almon)
    eta_true  = eta_true,
    beta_true = beta_true,
    RR_true   = RR_true,
    lags_grid = j_grid,
    
    # info útil p/ encaixar nas suas rotinas
    lags = lags,
    d    = d,
    ini_eff = ini_eff,
    sd_X_eff = sd_X_eff,
    
    params = list(
      n_total = n_total,
      lags = lags,
      d = d,
      m0 = m0, C0 = C0, W_alpha = W_alpha,
      cap_mu = cap_mu
    )
  )
  
  if (isTRUE(retornar_matrizes)) {
    # F_t = (1, S0, S1, ..., Sd)
    F <- cbind(1, S_mat)
    colnames(F) <- c("1", paste0("S", 0:d))
    out$X_lags <- X_lags
    out$F      <- F
  }
  
  return(out)
}










































# ======================================================================================
# Funções espelhadas do PDLDGLM com sazonalidade harmônica para uso com kDGLM
# Pacotes esperados no ambiente:
# kDGLM, dplyr, MASS, ggplot2, ggthemes e patchwork (opcional, só para gráficos combinados)
# ======================================================================================

PDLDGLM_sazonal <- function(
    Y, X, data, lags, d,
    periodo_sazonal,
    ordem_sazonal = 1,
    fd_nivel = 0.99,
    fd_sazonal = 0.98,
    padronizar_center = FALSE,
    padronizar_dp = TRUE,
    n_amostras = 1000
) {
  
  
  if (length(Y) != length(X) || length(Y) != length(data)) {
    stop("Y, X e data devem ter o mesmo comprimento.")
  }
  if (sum(is.na(Y)) > 0 || sum(is.na(X)) > 0) {
    stop("Y e X não podem conter NA.")
  }
  if (min(Y) < 0 || any(Y != round(Y))) {
    stop("Y deve ser uma contagem (inteiro >= 0).")
  }
  if (d != 2 && d != 3) {
    stop("d deve ser 2 ou 3 (grau do polinômio de defasagens).")
  }
  if (lags < 2 || lags != round(lags)) {
    stop("o número de lags deve ser um inteiro >= 2.")
  }
  if ((lags + 1) > length(Y)) {
    stop("Série muito curta para o número de lags informado.")
  }
  if (!is.numeric(periodo_sazonal) || length(periodo_sazonal) != 1 || !is.finite(periodo_sazonal) ||
      periodo_sazonal <= 1 || periodo_sazonal != round(periodo_sazonal)) {
    stop("'periodo_sazonal' deve ser um inteiro > 1.")
  }
  if (!is.numeric(ordem_sazonal) || length(ordem_sazonal) != 1 || !is.finite(ordem_sazonal) ||
      ordem_sazonal < 1 || ordem_sazonal != round(ordem_sazonal)) {
    stop("'ordem_sazonal' deve ser um inteiro >= 1.")
  }
  if (ordem_sazonal > floor(periodo_sazonal / 2)) {
    stop("'ordem_sazonal' não pode exceder floor(periodo_sazonal/2).")
  }
  
  ## --- 1) Construção da matriz de lags da covariável X ---
  n_total <- length(Y) # tamanho total da série Y
  
  q <- lags + 1 # número de colunas da matriz de lags (lag 0 até lags)
  X_mat <- matrix(0, nrow = n_total, ncol = q) # Inicialização da nossa matriz de lags
  X_mat[, 1] <- X # Primeira coluna: lag 0, que é o próprio X
  
  for (j in 2:q) {
    X_mat[, j] <- dplyr::lag(X, j - 1) # Colunas seguintes: X defasado em 1, 2, ..., (q-1) lags
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
    S_bruto <- data.frame(S0 = as.numeric(S0),
                          S1 = as.numeric(S1),
                          S2 = as.numeric(S2))
  } else { # se d == 3, inclui também o S3
    S3 <- X_mat[, -1] %*% (v^3)
    
    S_bruto <- data.frame(S0 = as.numeric(S0),
                          S1 = as.numeric(S1),
                          S2 = as.numeric(S2),
                          S3 = as.numeric(S3))
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
    S_dp[!is.finite(S_dp) | S_dp < 1e-6] <- 1e-6            # Pra evitar dividir por ~zero/NA/Inf, usa um mínimo seguro
  }
  
  # (2) Se pediu para centralizar, calcule as médias
  if (padronizar_center) {
    center_vec <- colMeans(S_bruto)                         # centragem: subtrai a média de cada coluna
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
  
  # bloco sazonal harmônico com período escolhido pela pessoa
  sazonal <- harmonic_block(rate = 1, period = periodo_sazonal, order = ordem_sazonal,
                            D = fd_sazonal, name = "Sazonal")
  n_sazonal_estados <- 2 * ordem_sazonal
  
  if (d == 2) {
    bloco <- (nivel + b0 + b1 + b2 + sazonal)
  } else { # se d == 3, inclui também o bloco de regressão para o S3
    b3 <- regression_block(rate = S_padronizado$S3, D = 1, name = "S3")
    bloco <- (nivel + b0 + b1 + b2 + b3 + sazonal)
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
  
  mu_estimada   <- exp(eta_media_t) # Exponenciando o log(u_t) pra tirar da escala log
  mu_ic_inf    <- exp(eta_media_t - 1.96 * eta_dp_t)
  mu_ic_sup    <- exp(eta_media_t + 1.96 * eta_dp_t)
  
  ## --- 6.1) Reconstruindo os betas de cada lag via amostragem bayesiana dos η no tempo final ---
  if (d == 2) {
    indice <- 2:4
  } else {
    indice <- 2:5
  }
  
  if (!is.null(coefs$theta.mean)) {
    estados_media <- coefs$theta.mean # E[θ_t | dados]
    estados_covar <- coefs$theta.cov  # Cov[θ_t | dados]
  } else {
    estados_media <- coefs$mt         # E[θ_t | dados] em versões antigas
    estados_covar <- coefs$Ct         # Cov[θ_t | dados] em versões antigas
  }
  
  etaS_media_T_padronizado <- estados_media[indice, n_efetivo]
  cov_eta_T_padronizado    <- estados_covar[indice, indice, n_efetivo]
  cov_eta_T_padronizado    <- matrix(cov_eta_T_padronizado, nrow = length(indice), ncol = length(indice))
  
  # Se n_amostras <= 0, pula a amostragem bayesiana e usa estimativa pontual (com a posteriori da média e covariância)
  if (is.null(n_amostras) || n_amostras <= 0) {
    # Desfazendo a padronização das S_k
    etaS_media_T <- as.numeric(etaS_media_T_padronizado / S_dp[seq_along(indice)])
    
    # Reconstruindo a matriz Cj de Almon
    j <- 0:(q - 1)
    if (d == 2) {
      Cj <- cbind(1, j, j^2)
    } else {
      Cj <- cbind(1, j, j^2, j^3)
    }
    
    beta_pt <- as.numeric(etaS_media_T %*% t(Cj))
    rr_beta_estimado    <- as.numeric(exp(sd(X[q:n_total]) * beta_pt))
    rr_sd_estimado      <- rep(NA_real_, q)
    rr_ic_inferior_beta <- rep(NA_real_, q)
    rr_ic_superior_beta <- rep(NA_real_, q)
    
  } else {
    # Amostragem multivariada utilizando a posteriori no tempo final T
    amostras_etaS_padronizado <- MASS::mvrnorm(
      n = n_amostras,
      mu = etaS_media_T_padronizado,
      Sigma = cov_eta_T_padronizado
    )
    
    if (n_amostras == 1) {
      amostras_etaS_padronizado <- matrix(amostras_etaS_padronizado, nrow = 1)
    }
    
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
    rr_amostras_beta <- exp(sd(X[q:n_total]) * amostras_beta) # transforma cada amostra para a escala de Risco Relativo
    rr_beta_estimado    <- colMeans(rr_amostras_beta)
    rr_sd_estimado      <- apply(rr_amostras_beta, 2, sd)
    rr_ic_inferior_beta <- apply(rr_amostras_beta, 2, quantile, probs = 0.025)
    rr_ic_superior_beta <- apply(rr_amostras_beta, 2, quantile, probs = 0.975)
  }
  
  ## --- 6.2) Recuperando o componente sazonal ao longo do tempo ---
  indice_sazonal_ini <- max(indice) + 1
  indice_sazonal_fim <- indice_sazonal_ini + n_sazonal_estados - 1
  indice_sazonal <- indice_sazonal_ini:indice_sazonal_fim
  
  sazonal_media_t <- rep(NA_real_, n_efetivo)
  sazonal_dp_t    <- rep(NA_real_, n_efetivo)
  
  FF_sazonal <- sazonal$FF
  if (length(dim(FF_sazonal)) == 2) {
    FF_sazonal <- array(FF_sazonal, dim = c(nrow(FF_sazonal), ncol(FF_sazonal), 1))
  }
  
  for (tt in 1:n_efetivo) {
    idx_ff <- min(tt, dim(FF_sazonal)[3])
    f_sazonal_t <- as.numeric(FF_sazonal[, 1, idx_ff])
    
    m_sazonal_t <- estados_media[indice_sazonal, tt]
    C_sazonal_t <- estados_covar[indice_sazonal, indice_sazonal, tt]
    C_sazonal_t <- matrix(C_sazonal_t, nrow = length(indice_sazonal), ncol = length(indice_sazonal))
    
    sazonal_media_t[tt] <- as.numeric(t(f_sazonal_t) %*% m_sazonal_t)
    sazonal_var_t       <- as.numeric(t(f_sazonal_t) %*% C_sazonal_t %*% f_sazonal_t)
    sazonal_var_t       <- max(sazonal_var_t, 0)
    sazonal_dp_t[tt]    <- sqrt(sazonal_var_t)
  }
  
  sazonal_ic_inf   <- sazonal_media_t - 1.96 * sazonal_dp_t
  sazonal_ic_sup   <- sazonal_media_t + 1.96 * sazonal_dp_t
  sazonal_rr_media <- exp(sazonal_media_t)
  sazonal_rr_ic_inf <- exp(sazonal_ic_inf)
  sazonal_rr_ic_sup <- exp(sazonal_ic_sup)
  # --------------------------------------------------------------------------------------------
  
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
  
  # Eixo X adaptativo
  dt_min <- min(df_y$t_index)
  dt_max <- max(df_y$t_index)
  span_days <- as.integer(dt_max - dt_min)
  
  # Meses em PT-BR
  mes_pt <- c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")
  
  if (span_days <= 366) {
    by <- "1 month"
    lab_fun <- function(d) mes_pt[as.integer(format(d, "%m"))]
  } else if (span_days <= 2*366) { # entre 1 anos e 2 ano
    by <- "2 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else if (span_days <= 4*366) { # entre 2 anos e 4 ano
    by <- "3 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else if (span_days <= 6*366) {
    by <- "6 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else {
    by <- "1 year"
    lab_fun <- function(d) format(d, "%Y")
  }
  
  breaks_seq <- seq(dt_min, dt_max, by = by) # Começa exatamente no primeiro dia da sua série (mantém o "dia do mês" de início)
  
  # linewidth adaptativo por janela de anos daquelas geom_lines (para poder mudar o período livremente na função)
  span_years <- as.numeric(difftime(dt_max, dt_min, units = "days")) / 365.25 # 365.25 para cobrir anos bissextos
  
  lw <- if (span_years <= 6) {
    1.00
  } else {
    0.90
  }
  
  g1 <- ggplot(df_y, aes(x = t_index)) +
    geom_line(aes(y = Y,          colour = "Dados"),       linewidth = lw) +
    geom_line(aes(y = y_estimado, colour = "Estimativas"), linewidth = lw) +
    geom_ribbon(aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    geom_hline(yintercept = 0) +
    labs(x = "Data", y = "Internações", colour = "", fill = "") +
    theme_hc() +
    theme(
      axis.title.x = element_text(size = 35),
      axis.text    = element_text(size = 35),
      axis.text.x  = element_text(size = 35, angle = 90),
      legend.text  = element_text(size = 30),
      legend.title = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(size = 35)
    ) +
    scale_color_manual(values = c("Dados" = "black", "Estimativas" = "red"),
                       breaks = c("Estimativas", "Dados")) +
    scale_fill_manual(values = c("IC 95%" = "blue")) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  g1_relatorio <- ggplot(df_y, aes(x = t_index)) +
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
      fill   = guide_legend(order = 1),
      colour = guide_legend(order = 2)
    )
  
  g1_legado <- ggplot(df_y, aes(x = t_index)) +
    geom_line(aes(y = Y,          colour = "Dados"),       linewidth = 1.6) +
    geom_ribbon(aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    geom_line(aes(y = y_estimado, colour = "Estimativas"), linewidth = 1.6) +
    geom_hline(yintercept = 0) +
    labs(x = "Mês", y = "Internações", colour = "", fill = "") +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 35),
      axis.text    = element_text(size = 35),
      axis.text.x  = element_text(size = 35, angle = 90),
      legend.text  = element_text(size = 30),
      legend.title = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(size = 35)
    ) +
    scale_color_manual(values = c("Dados" = "black", "Estimativas" = "red"),
                       breaks = c("Estimativas", "Dados")) +
    scale_fill_manual(values = c("IC 95%" = "blue")) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
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
  
  g2_legado <- ggplot(df_beta, aes(x = lag, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "blue", color = "blue",
                alpha = 0.2, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_point(size = 3, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Lags", y = "Risco Relativo") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = seq(0, q - 1, by = 1))
  
  # g3: componente sazonal ao longo do tempo, em escala de RR
  df_sazonal <- data.frame(
    t_index = t_index,
    rr = as.numeric(sazonal_rr_media),
    rr_lo = as.numeric(sazonal_rr_ic_inf),
    rr_hi = as.numeric(sazonal_rr_ic_sup)
  )
  
  g3 <- ggplot(df_sazonal, aes(x = t_index, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "darkseagreen3", color = "darkseagreen3",
                alpha = 0.3, show.legend = FALSE) +
    geom_line(linewidth = lw, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Data", y = "Efeito Sazonal (RR)") +
    theme_hc(base_size = 1) +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      axis.text.x = element_text(size = 35, angle = 90),
      panel.grid = element_blank(),
      legend.position = "none"
    ) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  g3_legado <- ggplot(df_sazonal, aes(x = t_index, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "darkseagreen3", color = "darkseagreen3",
                alpha = 0.2, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Mês", y = "Efeito Sazonal (RR)") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      axis.text.x = element_text(size = 35, angle = 90),
      legend.position = "none"
    ) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  g1_g2 <- NULL
  g1_g2_g3 <- NULL
  if (requireNamespace("patchwork", quietly = TRUE)) {
    g1_g2 <- patchwork::wrap_plots(g1, g2, ncol = 2)
    g1_g2_g3 <- patchwork::wrap_plots(g1, g2, g3, ncol = 3)
  }
  
  return(list(
    mu_media    = mu_estimada,
    mu_ic_inf   = mu_ic_inf,
    mu_ic_sup   = mu_ic_sup,
    beta_media  = rr_beta_estimado,
    beta_sd     = rr_sd_estimado,
    beta_ic_inf = rr_ic_inferior_beta,
    beta_ic_sup = rr_ic_superior_beta,
    sazonal_media = sazonal_media_t,
    sazonal_dp = sazonal_dp_t,
    sazonal_ic_inf = sazonal_ic_inf,
    sazonal_ic_sup = sazonal_ic_sup,
    sazonal_rr_media = sazonal_rr_media,
    sazonal_rr_ic_inf = sazonal_rr_ic_inf,
    sazonal_rr_ic_sup = sazonal_rr_ic_sup,
    periodo_sazonal = periodo_sazonal,
    ordem_sazonal = ordem_sazonal,
    indice_sazonal = indice_sazonal,
    g1 = g1,
    g1_relatorio = g1_relatorio,
    g1_legado = g1_legado,
    g2 = g2,
    g2_legado = g2_legado,
    g3 = g3,
    g3_legado = g3_legado,
    g1_g2 = g1_g2,
    g1_g2_g3 = g1_g2_g3,
    kdglm_coef  = coefs,
    pred_df  = coefs$data,
    ajuste1 = ajuste
  ))
}


PDLDGLM_clima_sazonal <- function(
    Y, X, covar, data,
    lags, lag_covar, d,
    perc, lado = c("acima","abaixo"),
    perc_sup = NULL,
    periodo_sazonal,
    ordem_sazonal = 1,
    fd_nivel = 0.99,
    fd_sazonal = 0.98,
    padronizar_center = FALSE,
    padronizar_dp     = TRUE,
    n_amostras        = 1000
) {
  lado <- match.arg(lado)
  
  if (length(Y) != length(X) || length(Y) != length(data) || length(Y) != length(covar)) {
    stop("Y, X, covar e data devem ter o mesmo comprimento.")
  }
  if (sum(is.na(Y)) > 0 || sum(is.na(X)) > 0 || sum(is.na(covar)) > 0) {
    stop("Y, X e covar não podem conter NA.")
  }
  if (min(Y) < 0 || any(Y != round(Y))) {
    stop("Y deve ser uma contagem (inteiro >= 0).")
  }
  if (d != 2 && d != 3) {
    stop("d deve ser 2 ou 3 (grau do polinômio de defasagens).")
  }
  if (lags < 2 || lags != round(lags)) {
    stop("o número de lags deve ser um inteiro >= 2.")
  }
  if (lag_covar < 0 || lag_covar != round(lag_covar)) {
    stop("'lag_covar' deve ser um inteiro >= 0.")
  }
  if (perc <= 0 || perc >= 1) {
    stop("'perc' deve estar em (0,1), por exemplo 0.85, 0.95.")
  }
  if (!is.null(perc_sup)) {
    if (!is.numeric(perc_sup) || length(perc_sup) != 1 || !is.finite(perc_sup)) {
      stop("'perc_sup' deve ser escalar numérico em (0,1), quando informado.")
    }
    if (perc_sup <= 0 || perc_sup >= 1) {
      stop("'perc_sup' deve estar em (0,1).")
    }
    if (perc_sup <= perc) {
      stop("'perc_sup' deve ser maior que 'perc' para definir intervalo interno.")
    }
  }
  if ((lags + 1) > length(Y)) {
    stop("Série muito curta para o número de lags informado.")
  }
  if (!is.numeric(periodo_sazonal) || length(periodo_sazonal) != 1 || !is.finite(periodo_sazonal) ||
      periodo_sazonal <= 1 || periodo_sazonal != round(periodo_sazonal)) {
    stop("'periodo_sazonal' deve ser um inteiro > 1.")
  }
  if (!is.numeric(ordem_sazonal) || length(ordem_sazonal) != 1 || !is.finite(ordem_sazonal) ||
      ordem_sazonal < 1 || ordem_sazonal != round(ordem_sazonal)) {
    stop("'ordem_sazonal' deve ser um inteiro >= 1.")
  }
  if (ordem_sazonal > floor(periodo_sazonal / 2)) {
    stop("'ordem_sazonal' não pode exceder floor(periodo_sazonal/2).")
  }
  
  ## --- 1) Construção da matriz de lags da covariável X ---
  n_total <- length(Y) # tamanho total da série Y
  
  q <- lags + 1 # número de colunas da matriz de lags (lag 0 até lags)
  X_mat <- matrix(0, nrow = n_total, ncol = q) # Inicialização da nossa matriz de lags
  X_mat[, 1] <- X # Primeira coluna: lag 0, que é o próprio X
  
  for (j in 2:q) {
    X_mat[, j] <- dplyr::lag(X, j - 1) # Colunas seguintes: X defasado em 1, 2, ..., (q-1) lags
  }
  
  ## --- 1b) Indicadora climática H_t (0/1) e defasagem da covariável ---
  ini <- max(lags + 1, lag_covar + 1) # índice inicial comum (pra remover o "burn-in")
  if (ini > n_total) stop("Janela efetiva vazia após alinhamento.")
  
  corte <- as.numeric(stats::quantile(covar[ini:n_total], probs = perc, na.rm = TRUE))
  corte_sup <- NA_real_
  if (is.null(perc_sup)) {
    H_raw <- if (lado == "acima") as.numeric(covar > corte) else as.numeric(covar < corte)
  } else {
    corte_sup <- as.numeric(stats::quantile(covar[ini:n_total], probs = perc_sup, na.rm = TRUE))
    H_raw <- as.numeric(covar > corte & covar < corte_sup)
  }
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
  S1 <- X_mat[, -1] %*%  v
  S2 <- X_mat[, -1] %*% (v^2)
  
  if (d == 2) {
    S_bruto <- data.frame(S0 = as.numeric(S0),
                          S1 = as.numeric(S1),
                          S2 = as.numeric(S2))
  } else { # se d == 3, inclui também o S3
    S3 <- X_mat[, -1] %*% (v^3)
    
    S_bruto <- data.frame(S0 = as.numeric(S0),
                          S1 = as.numeric(S1),
                          S2 = as.numeric(S2),
                          S3 = as.numeric(S3))
  }
  
  ## --- 3) Padronização das Sk (opcional, depende do que a pessoa marcar nos parâmetros) ---
  
  # (0) Caso ela não padronize nada, vamos seguir com isso aqui.
  # Os valores abaixo são só pq, mais pra frente, vamos desfazer a padronização das amostras dos etas e, lá, dividimos por S_dp.
  S_dp           <- rep(1, ncol(S_bruto))
  center_vec     <- FALSE
  S_padronizado  <- S_bruto
  
  # (1) Se pediu para padronizar pelo desvio-padrão, calcule os dps de cada coluna
  if (padronizar_dp) {
    S_dp <- apply(S_bruto, 2, sd)
    S_dp[!is.finite(S_dp) | S_dp < 1e-6] <- 1e-6
  }
  
  # (2) Se pediu para centralizar, calcule as médias
  if (padronizar_center) {
    center_vec <- colMeans(S_bruto)
  }
  
  # (3) Se qualquer uma das opções foi ativada, aplica scale() com as escolhas acima
  if (padronizar_center || padronizar_dp) {
    S_padronizado <- as.data.frame(
      scale(S_bruto, center = center_vec, scale = S_dp)
    )
  }
  
  ## --- 4) Modelo kDGLM ---
  # bloco de nível com fator de desconto fd_nivel
  nivel <- polynomial_block(rate = 1, order = 1, D = fd_nivel, name = "Nivel")
  
  # blocos de regressão para S0, S1, ..., Sd (com os coeficientes tratados como constantes no tempo, por isso D = 1)
  b0 <- regression_block(rate = S_padronizado$S0, D = 1, name = "S0")
  b1 <- regression_block(rate = S_padronizado$S1, D = 1, name = "S1")
  b2 <- regression_block(rate = S_padronizado$S2, D = 1, name = "S2")
  
  # bloco sazonal harmônico com período escolhido pela pessoa
  sazonal <- harmonic_block(rate = 1, period = periodo_sazonal, order = ordem_sazonal,
                            D = fd_sazonal, name = "Sazonal")
  n_sazonal_estados <- 2 * ordem_sazonal
  
  if (d == 2) {
    # bloco da covariável climática indicadora (constante no tempo: D = 1)
    h  <- regression_block(rate = H_def, D = 1, name = "H")
    bloco <- (nivel + b0 + b1 + b2 + h + sazonal)
  } else {
    b3 <- regression_block(rate = S_padronizado$S3, D = 1, name = "S3")
    h  <- regression_block(rate = H_def, D = 1, name = "H")
    bloco <- (nivel + b0 + b1 + b2 + b3 + h + sazonal)
  }
  
  desfecho <- Poisson(lambda = "rate", data = Y)
  ajuste <- fit_model(
    bloco,
    y = desfecho)
  
  coefs <- coef(ajuste, lag = -1, eval.pred = TRUE, eval.metric = TRUE, pred.cred  = 0.95)
  
  ## --- 5) Pegando a média estimada a cada tempo ---
  if (!is.null(coefs$ft)) {
    eta_media_t <- as.numeric(coefs$ft[1, ])
    eta_dp_t    <- sqrt(as.numeric(coefs$Qt[1, 1, ]))
  } else {
    eta_media_t <- as.numeric(coefs$lambda.mean[1, ])
    eta_dp_t    <- sqrt(as.numeric(coefs$lambda.cov[1, 1, ]))
  }
  
  mu_estimada <- exp(eta_media_t)
  mu_ic_inf   <- exp(eta_media_t - 1.96 * eta_dp_t)
  mu_ic_sup   <- exp(eta_media_t + 1.96 * eta_dp_t)
  
  ## --- 6.1) Reconstruindo os betas de cada lag via amostragem bayesiana dos η no tempo final ---
  if (d == 2) {
    indice <- 2:4
  } else {
    indice <- 2:5
  }
  
  if (!is.null(coefs$theta.mean)) {
    estados_media <- coefs$theta.mean
    estados_covar <- coefs$theta.cov
  } else {
    estados_media <- coefs$mt
    estados_covar <- coefs$Ct
  }
  
  etaS_media_T_padronizado <- estados_media[indice, n_efetivo]
  cov_eta_T_padronizado    <- estados_covar[indice, indice, n_efetivo]
  cov_eta_T_padronizado    <- matrix(cov_eta_T_padronizado, nrow = length(indice), ncol = length(indice))
  
  # Desvio-padrão do X no período efetivo (para a escala de Risco Relativo, como no original)
  sd_X_efetivo <- stats::sd(X[ini:n_total])
  
  # Se n_amostras <= 0, pula a amostragem bayesiana e usa estimativa pontual (com a posteriori da média e covariância)
  if (is.null(n_amostras) || n_amostras <= 0) {
    etaS_media_T <- as.numeric(etaS_media_T_padronizado / S_dp[seq_along(indice)])
    
    j <- 0:(q - 1)
    if (d == 2) {
      Cj <- cbind(1, j, j^2)
    } else {
      Cj <- cbind(1, j, j^2, j^3)
    }
    
    beta_pt <- as.numeric(etaS_media_T %*% t(Cj))
    rr_beta_estimado    <- as.numeric(exp(sd_X_efetivo * beta_pt))
    rr_sd_estimado      <- rep(NA_real_, q)
    rr_ic_inferior_beta <- rep(NA_real_, q)
    rr_ic_superior_beta <- rep(NA_real_, q)
    
  } else {
    amostras_etaS_padronizado <- MASS::mvrnorm(
      n = n_amostras,
      mu = etaS_media_T_padronizado,
      Sigma = cov_eta_T_padronizado
    )
    
    if (n_amostras == 1) {
      amostras_etaS_padronizado <- matrix(amostras_etaS_padronizado, nrow = 1)
    }
    
    amostras_etaS <- scale(amostras_etaS_padronizado, center = FALSE, scale = S_dp)
    
    j <- 0:(q - 1)
    
    if (d == 2) {
      Cj <- cbind(1, j, j^2)
    } else {
      Cj <- cbind(1, j, j^2, j^3)
    }
    
    amostras_beta <- amostras_etaS %*% t(Cj)
    rr_amostras_beta <- exp(sd_X_efetivo * amostras_beta)
    
    rr_beta_estimado    <- colMeans(rr_amostras_beta)
    rr_sd_estimado      <- apply(rr_amostras_beta, 2, sd)
    rr_ic_inferior_beta <- apply(rr_amostras_beta, 2, quantile, probs = 0.025)
    rr_ic_superior_beta <- apply(rr_amostras_beta, 2, quantile, probs = 0.975)
  }
  
  ## --- 6.2) Reconstruindo o tau associado ao bloco H via amostragem bayesiana no tempo final ---
  idx_tau <- if (d == 2) 5 else 6 # No vetor de estados: [Nivel, S0..Sd, H] => tau vem antes da sazonalidade
  
  tau_media_T <- estados_media[idx_tau, n_efetivo]
  tau_var_T   <- estados_covar[idx_tau, idx_tau, n_efetivo]
  
  if (is.null(n_amostras) || n_amostras <= 0) {
    rr_tau_estimado    <- exp(as.numeric(tau_media_T))
    rr_ic_inferior_tau <- NA_real_
    rr_ic_superior_tau <- NA_real_
  } else {
    amostras_tau <- rnorm(n_amostras, mean = as.numeric(tau_media_T), sd = sqrt(as.numeric(tau_var_T)))
    amostras_RR  <- exp(amostras_tau)
    
    rr_tau_estimado <- mean(amostras_RR)
    rr_ic_inferior_tau <- as.numeric(quantile(amostras_RR, probs = 0.025))
    rr_ic_superior_tau <- as.numeric(quantile(amostras_RR, probs = 0.975))
  }
  
  ## --- 6.3) Recuperando o componente sazonal ao longo do tempo ---
  indice_sazonal_ini <- idx_tau + 1
  indice_sazonal_fim <- indice_sazonal_ini + n_sazonal_estados - 1
  indice_sazonal <- indice_sazonal_ini:indice_sazonal_fim
  
  sazonal_media_t <- rep(NA_real_, n_efetivo)
  sazonal_dp_t    <- rep(NA_real_, n_efetivo)
  
  FF_sazonal <- sazonal$FF
  if (length(dim(FF_sazonal)) == 2) {
    FF_sazonal <- array(FF_sazonal, dim = c(nrow(FF_sazonal), ncol(FF_sazonal), 1))
  }
  
  for (tt in 1:n_efetivo) {
    idx_ff <- min(tt, dim(FF_sazonal)[3])
    f_sazonal_t <- as.numeric(FF_sazonal[, 1, idx_ff])
    
    m_sazonal_t <- estados_media[indice_sazonal, tt]
    C_sazonal_t <- estados_covar[indice_sazonal, indice_sazonal, tt]
    C_sazonal_t <- matrix(C_sazonal_t, nrow = length(indice_sazonal), ncol = length(indice_sazonal))
    
    sazonal_media_t[tt] <- as.numeric(t(f_sazonal_t) %*% m_sazonal_t)
    sazonal_var_t       <- as.numeric(t(f_sazonal_t) %*% C_sazonal_t %*% f_sazonal_t)
    sazonal_var_t       <- max(sazonal_var_t, 0)
    sazonal_dp_t[tt]    <- sqrt(sazonal_var_t)
  }
  
  sazonal_ic_inf   <- sazonal_media_t - 1.96 * sazonal_dp_t
  sazonal_ic_sup   <- sazonal_media_t + 1.96 * sazonal_dp_t
  sazonal_rr_media <- exp(sazonal_media_t)
  sazonal_rr_ic_inf <- exp(sazonal_ic_inf)
  sazonal_rr_ic_sup <- exp(sazonal_ic_sup)
  # --------------------------------------------------------------------------------------------
  
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
  
  # Eixo X adaptativo
  dt_min <- min(df_y$t_index)
  dt_max <- max(df_y$t_index)
  span_days <- as.integer(dt_max - dt_min)
  
  # Meses em PT-BR
  mes_pt <- c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")
  
  if (span_days <= 366) {
    by <- "1 month"
    lab_fun <- function(d) mes_pt[as.integer(format(d, "%m"))]
  } else if (span_days <= 2*366) {
    by <- "2 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else if (span_days <= 4*366) {
    by <- "3 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else if (span_days <= 6*366) {
    by <- "6 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else {
    by <- "1 year"
    lab_fun <- function(d) format(d, "%Y")
  }
  
  breaks_seq <- seq(dt_min, dt_max, by = by)
  span_years <- as.numeric(difftime(dt_max, dt_min, units = "days")) / 365.25
  
  lw <- if (span_years <= 6) {
    1.00
  } else {
    0.90
  }
  
  g1 <- ggplot2::ggplot(df_y, ggplot2::aes(x = t_index)) +
    ggplot2::geom_line(ggplot2::aes(y = Y,          colour = "Dados"),       linewidth = lw) +
    ggplot2::geom_line(ggplot2::aes(y = y_estimado, colour = "Estimativas"), linewidth = lw) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(x = "Data", y = "Internações", colour = "", fill = "") +
    ggthemes::theme_hc() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 35),
      axis.text    = ggplot2::element_text(size = 35),
      axis.text.x  = ggplot2::element_text(size = 35, angle = 90),
      legend.text  = ggplot2::element_text(size = 30),
      legend.title = ggplot2::element_blank(),
      legend.position = "none",
      axis.title.y = ggplot2::element_text(size = 35)
    ) +
    ggplot2::scale_color_manual(values = c("Dados" = "black", "Estimativas" = "red"),
                                breaks = c("Estimativas", "Dados")) +
    ggplot2::scale_fill_manual(values = c("IC 95%" = "blue")) +
    ggplot2::scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  g1_relatorio <- ggplot2::ggplot(df_y, ggplot2::aes(x = t_index)) +
    ggplot2::geom_line(ggplot2::aes(y = Y,          colour = "Dados"),       linewidth = lw) +
    ggplot2::geom_line(ggplot2::aes(y = y_estimado, colour = "Estimativas"), linewidth = lw) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(
      x = "Data",
      y = "Internações",
      colour = "",
      fill   = ""
    ) +
    ggthemes::theme_hc() +
    ggplot2::theme(
      axis.title.x    = ggplot2::element_text(size = 35),
      axis.text       = ggplot2::element_text(size = 35),
      axis.text.x     = ggplot2::element_text(size = 35, angle = 90),
      legend.text     = ggplot2::element_text(size = 30),
      legend.title    = ggplot2::element_blank(),
      legend.position = "bottom",
      axis.title.y    = ggplot2::element_text(size = 35)
    ) +
    ggplot2::scale_color_manual(
      values = c("Dados" = "black", "Estimativas" = "red"),
      breaks = c("Estimativas", "Dados"),
      labels = c("Estimativas", "Observações")
    ) +
    ggplot2::scale_fill_manual(values = c("IC 95%" = "blue")) +
    ggplot2::scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    ) +
    ggplot2::guides(
      fill   = ggplot2::guide_legend(order = 1),
      colour = ggplot2::guide_legend(order = 2)
    )
  
  g1_legado <- ggplot(df_y, aes(x = t_index)) +
    geom_ribbon(aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    geom_line(aes(y = Y,          colour = "Dados"),       linewidth = 1.6) +
    geom_line(aes(y = y_estimado, colour = "Estimativas"), linewidth = 1.6) +
    geom_hline(yintercept = 0) +
    labs(x = "Mês", y = "Internações", colour = "", fill = "") +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 35),
      axis.text    = element_text(size = 35),
      axis.text.x  = element_text(size = 35, angle = 90),
      legend.text  = element_text(size = 30),
      legend.title = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(size = 35)
    ) +
    scale_color_manual(values = c("Dados" = "black", "Estimativas" = "red"),
                       breaks = c("Estimativas", "Dados")) +
    scale_fill_manual(values = c("IC 95%" = "blue")) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  # g2: Beta(j)
  df_beta <- data.frame(
    lag = 0:(q - 1),
    rr = as.numeric(rr_beta_estimado),
    rr_lo = as.numeric(rr_ic_inferior_beta),
    rr_hi = as.numeric(rr_ic_superior_beta)
  )
  
  g2 <- ggplot2::ggplot(df_beta, ggplot2::aes(x = lag, y = rr)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = rr_lo, ymax = rr_hi),
                         fill = "cadetblue3", color = "cadetblue3",
                         alpha = 0.3, show.legend = FALSE) +
    ggplot2::geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    ggplot2::geom_point(size = 2.5, colour = "black", show.legend = FALSE) +
    ggplot2::geom_hline(yintercept = 1, linewidth = 0.8) +
    ggplot2::labs(x = "Lags", y = "Risco Relativo (RR)") +
    ggthemes::theme_hc(base_size = 1) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 35),
      axis.text  = ggplot2::element_text(size = 35),
      panel.grid = ggplot2::element_blank(),
      legend.position = "none"
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, q - 1, by = 1))
  
  g2_legado <- ggplot(df_beta, aes(x = lag, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "blue", color = "blue",
                alpha = 0.2, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_point(size = 3, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Lags", y = "Risco Relativo") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = seq(0, q - 1, by = 1))
  
  # g3: componente sazonal ao longo do tempo, em escala de RR
  df_sazonal <- data.frame(
    t_index = t_index,
    rr = as.numeric(sazonal_rr_media),
    rr_lo = as.numeric(sazonal_rr_ic_inf),
    rr_hi = as.numeric(sazonal_rr_ic_sup)
  )
  
  g3 <- ggplot2::ggplot(df_sazonal, ggplot2::aes(x = t_index, y = rr)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = rr_lo, ymax = rr_hi),
                         fill = "darkseagreen3", color = "darkseagreen3",
                         alpha = 0.3, show.legend = FALSE) +
    ggplot2::geom_line(linewidth = lw, colour = "black", show.legend = FALSE) +
    ggplot2::geom_hline(yintercept = 1, linewidth = 0.8) +
    ggplot2::labs(x = "Data", y = "Efeito Sazonal (RR)") +
    ggthemes::theme_hc(base_size = 1) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 35),
      axis.text  = ggplot2::element_text(size = 35),
      axis.text.x = ggplot2::element_text(size = 35, angle = 90),
      panel.grid = ggplot2::element_blank(),
      legend.position = "none"
    ) +
    ggplot2::scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  g3_legado <- ggplot(df_sazonal, aes(x = t_index, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "darkseagreen3", color = "darkseagreen3",
                alpha = 0.2, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Mês", y = "Efeito Sazonal (RR)") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      axis.text.x = element_text(size = 35, angle = 90),
      legend.position = "none"
    ) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  g1_g2 <- NULL
  g1_g2_g3 <- NULL
  if (requireNamespace("patchwork", quietly = TRUE)) {
    g1_g2 <- patchwork::wrap_plots(g1, g2, ncol = 2)
    g1_g2_g3 <- patchwork::wrap_plots(g1, g2, g3, ncol = 3)
  }
  
  return(list(
    mu_media    = mu_estimada,
    mu_ic_inf   = mu_ic_inf,
    mu_ic_sup   = mu_ic_sup,
    beta_media  = rr_beta_estimado,
    beta_sd     = rr_sd_estimado,
    beta_ic_inf = rr_ic_inferior_beta,
    beta_ic_sup = rr_ic_superior_beta,
    tau_media   = rr_tau_estimado,
    tau_ic_inf  = rr_ic_inferior_tau,
    tau_ic_sup  = rr_ic_superior_tau,
    indicadora_climatica = H_def,
    corte = corte,
    corte_sup = corte_sup,
    sazonal_media = sazonal_media_t,
    sazonal_dp = sazonal_dp_t,
    sazonal_ic_inf = sazonal_ic_inf,
    sazonal_ic_sup = sazonal_ic_sup,
    sazonal_rr_media = sazonal_rr_media,
    sazonal_rr_ic_inf = sazonal_rr_ic_inf,
    sazonal_rr_ic_sup = sazonal_rr_ic_sup,
    periodo_sazonal = periodo_sazonal,
    ordem_sazonal = ordem_sazonal,
    indice_sazonal = indice_sazonal,
    g1 = g1,
    g1_relatorio = g1_relatorio,
    g1_legado = g1_legado,
    g2 = g2,
    g2_legado = g2_legado,
    g3 = g3,
    g3_legado = g3_legado,
    g1_g2 = g1_g2,
    g1_g2_g3 = g1_g2_g3,
    kdglm_coef  = coefs,
    pred_df     = coefs$data,
    ajuste1 = ajuste
  ))
}



PDLDGLM_2X_sazonal <- function(
    Y, X1, X2, data,
    lags1, lags2, d1, d2,
    periodo_sazonal,
    ordem_sazonal = 1,
    fd_nivel = 0.99,
    fd_sazonal = 0.98,
    padronizar_center = FALSE,
    padronizar_dp = TRUE,
    n_amostras = 1000
) {
  
  
  if (length(Y) != length(X1) || length(Y) != length(X2) || length(Y) != length(data)) {
    stop("Y, X1, X2 e data devem ter o mesmo comprimento.")
  }
  if (sum(is.na(Y)) > 0 || sum(is.na(X1)) > 0 || sum(is.na(X2)) > 0) {
    stop("Y, X1 e X2 não podem conter NA.")
  }
  if (min(Y) < 0 || any(Y != round(Y))) {
    stop("Y deve ser uma contagem (inteiro >= 0).")
  }
  if (d1 != 2 && d1 != 3) {
    stop("d1 deve ser 2 ou 3 (grau do polinômio de defasagens para X1).")
  }
  if (d2 != 2 && d2 != 3) {
    stop("d2 deve ser 2 ou 3 (grau do polinômio de defasagens para X2).")
  }
  if (lags1 < 2 || lags1 != round(lags1)) {
    stop("'lags1' deve ser um inteiro >= 2.")
  }
  if (lags2 < 2 || lags2 != round(lags2)) {
    stop("'lags2' deve ser um inteiro >= 2.")
  }
  if (max(lags1 + 1, lags2 + 1) > length(Y)) {
    stop("Série muito curta para o número de lags informado.")
  }
  if (!is.numeric(periodo_sazonal) || length(periodo_sazonal) != 1 || !is.finite(periodo_sazonal) ||
      periodo_sazonal <= 1 || periodo_sazonal != round(periodo_sazonal)) {
    stop("'periodo_sazonal' deve ser um inteiro > 1.")
  }
  if (!is.numeric(ordem_sazonal) || length(ordem_sazonal) != 1 || !is.finite(ordem_sazonal) ||
      ordem_sazonal < 1 || ordem_sazonal != round(ordem_sazonal)) {
    stop("'ordem_sazonal' deve ser um inteiro >= 1.")
  }
  if (ordem_sazonal > floor(periodo_sazonal / 2)) {
    stop("'ordem_sazonal' não pode exceder floor(periodo_sazonal/2).")
  }
  
  ## --- 1) Construção das matrizes de lags das covariáveis X1 e X2 ---
  n_total <- length(Y) # tamanho total da série Y
  
  q1 <- lags1 + 1 # número de colunas da matriz de lags de X1 (lag 0 até lags1)
  X1_mat <- matrix(0, nrow = n_total, ncol = q1) # Inicialização da matriz de lags de X1
  X1_mat[, 1] <- X1 # Primeira coluna: lag 0, que é o próprio X1
  
  for (j in 2:q1) {
    X1_mat[, j] <- dplyr::lag(X1, j - 1) # Colunas seguintes: X1 defasado em 1, 2, ..., (q1-1) lags
  }
  
  q2 <- lags2 + 1 # número de colunas da matriz de lags de X2 (lag 0 até lags2)
  X2_mat <- matrix(0, nrow = n_total, ncol = q2) # Inicialização da matriz de lags de X2
  X2_mat[, 1] <- X2 # Primeira coluna: lag 0, que é o próprio X2
  
  for (j in 2:q2) {
    X2_mat[, j] <- dplyr::lag(X2, j - 1) # Colunas seguintes: X2 defasado em 1, 2, ..., (q2-1) lags
  }
  
  ini <- max(q1, q2) # índice inicial comum (pra remover o burn-in conjunto)
  
  X1_mat <- X1_mat[ini:n_total, ] # removemos as primeiras linhas vazias de X1
  X2_mat <- X2_mat[ini:n_total, ] # removemos as primeiras linhas vazias de X2
  Y      <- Y[ini:n_total]        # removemos as primeiras observações de Y para alinhar
  n_efetivo <- length(Y)          # tamanho efetivo de Y após o alinhamento
  
  ## --- 2) Calculando as somas de Almon para X1 e X2 ---
  v1 <- 1:lags1 # índices 1, 2, ..., lags1 usados nas fórmulas de X1_S0, X1_S1, X1_S2, X1_S3
  v2 <- 1:lags2 # índices 1, 2, ..., lags2 usados nas fórmulas de X2_S0, X2_S1, X2_S2, X2_S3
  
  # Somas de Almon para X1
  X1_S0 <- rowSums(X1_mat)
  X1_S1 <- X1_mat[, -1] %*% v1
  X1_S2 <- X1_mat[, -1] %*% (v1^2)
  
  if (d1 == 3) {
    X1_S3 <- X1_mat[, -1] %*% (v1^3)
  }
  
  # Somas de Almon para X2
  X2_S0 <- rowSums(X2_mat)
  X2_S1 <- X2_mat[, -1] %*% v2
  X2_S2 <- X2_mat[, -1] %*% (v2^2)
  
  if (d2 == 3) {
    X2_S3 <- X2_mat[, -1] %*% (v2^3)
  }
  
  if (d1 == 2 && d2 == 2) {
    S_bruto <- data.frame(
      X1_S0 = as.numeric(X1_S0),
      X1_S1 = as.numeric(X1_S1),
      X1_S2 = as.numeric(X1_S2),
      X2_S0 = as.numeric(X2_S0),
      X2_S1 = as.numeric(X2_S1),
      X2_S2 = as.numeric(X2_S2)
    )
  } else if (d1 == 2 && d2 == 3) {
    S_bruto <- data.frame(
      X1_S0 = as.numeric(X1_S0),
      X1_S1 = as.numeric(X1_S1),
      X1_S2 = as.numeric(X1_S2),
      X2_S0 = as.numeric(X2_S0),
      X2_S1 = as.numeric(X2_S1),
      X2_S2 = as.numeric(X2_S2),
      X2_S3 = as.numeric(X2_S3)
    )
  } else if (d1 == 3 && d2 == 2) {
    S_bruto <- data.frame(
      X1_S0 = as.numeric(X1_S0),
      X1_S1 = as.numeric(X1_S1),
      X1_S2 = as.numeric(X1_S2),
      X1_S3 = as.numeric(X1_S3),
      X2_S0 = as.numeric(X2_S0),
      X2_S1 = as.numeric(X2_S1),
      X2_S2 = as.numeric(X2_S2)
    )
  } else {
    S_bruto <- data.frame(
      X1_S0 = as.numeric(X1_S0),
      X1_S1 = as.numeric(X1_S1),
      X1_S2 = as.numeric(X1_S2),
      X1_S3 = as.numeric(X1_S3),
      X2_S0 = as.numeric(X2_S0),
      X2_S1 = as.numeric(X2_S1),
      X2_S2 = as.numeric(X2_S2),
      X2_S3 = as.numeric(X2_S3)
    )
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
    S_dp[!is.finite(S_dp) | S_dp < 1e-6] <- 1e-6            # Pra evitar dividir por ~zero/NA/Inf, usa um mínimo seguro
  }
  
  # (2) Se pediu para centralizar, calcule as médias
  if (padronizar_center) {
    center_vec <- colMeans(S_bruto)                         # centragem: subtrai a média de cada coluna
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
  
  # blocos de regressão para X1_S0, X1_S1, ..., X1_Sd1
  x1_b0 <- regression_block(rate = S_padronizado$X1_S0, D = 1, name = "X1_S0")
  x1_b1 <- regression_block(rate = S_padronizado$X1_S1, D = 1, name = "X1_S1")
  x1_b2 <- regression_block(rate = S_padronizado$X1_S2, D = 1, name = "X1_S2")
  if (d1 == 3) {
    x1_b3 <- regression_block(rate = S_padronizado$X1_S3, D = 1, name = "X1_S3")
  }
  
  # blocos de regressão para X2_S0, X2_S1, ..., X2_Sd2
  x2_b0 <- regression_block(rate = S_padronizado$X2_S0, D = 1, name = "X2_S0")
  x2_b1 <- regression_block(rate = S_padronizado$X2_S1, D = 1, name = "X2_S1")
  x2_b2 <- regression_block(rate = S_padronizado$X2_S2, D = 1, name = "X2_S2")
  if (d2 == 3) {
    x2_b3 <- regression_block(rate = S_padronizado$X2_S3, D = 1, name = "X2_S3")
  }
  
  # bloco sazonal harmônico com período escolhido pela pessoa
  sazonal <- harmonic_block(rate = 1, period = periodo_sazonal, order = ordem_sazonal,
                            D = fd_sazonal, name = "Sazonal")
  n_sazonal_estados <- 2 * ordem_sazonal
  
  if (d1 == 2 && d2 == 2) {
    bloco <- (nivel + x1_b0 + x1_b1 + x1_b2 + x2_b0 + x2_b1 + x2_b2 + sazonal)
  } else if (d1 == 2 && d2 == 3) {
    bloco <- (nivel + x1_b0 + x1_b1 + x1_b2 + x2_b0 + x2_b1 + x2_b2 + x2_b3 + sazonal)
  } else if (d1 == 3 && d2 == 2) {
    bloco <- (nivel + x1_b0 + x1_b1 + x1_b2 + x1_b3 + x2_b0 + x2_b1 + x2_b2 + sazonal)
  } else {
    bloco <- (nivel + x1_b0 + x1_b1 + x1_b2 + x1_b3 + x2_b0 + x2_b1 + x2_b2 + x2_b3 + sazonal)
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
  
  mu_estimada   <- exp(eta_media_t) # Exponenciando o log(u_t) pra tirar da escala log
  mu_ic_inf    <- exp(eta_media_t - 1.96 * eta_dp_t)
  mu_ic_sup    <- exp(eta_media_t + 1.96 * eta_dp_t)
  
  ## --- 6.1) Reconstruindo os betas de cada lag via amostragem bayesiana dos η no tempo final ---
  if (d1 == 2) {
    indice_x1 <- 2:4
  } else {
    indice_x1 <- 2:5
  }
  
  indice_x2_ini <- max(indice_x1) + 1
  if (d2 == 2) {
    indice_x2 <- indice_x2_ini:(indice_x2_ini + 2)
  } else {
    indice_x2 <- indice_x2_ini:(indice_x2_ini + 3)
  }
  
  indice_total <- c(indice_x1, indice_x2)
  
  if (!is.null(coefs$theta.mean)) {
    estados_media <- coefs$theta.mean # E[θ_t | dados] (Vetor de estados)
    estados_covar <- coefs$theta.cov  # Cov[θ_t | dados] (Matriz de covariância dos estados)
  } else {
    estados_media <- coefs$mt         # E[θ_t | dados] em versões antigas
    estados_covar <- coefs$Ct         # Cov[θ_t | dados] em versões antigas
  }
  
  eta12_media_T_padronizado <- estados_media[indice_total, n_efetivo]
  cov_eta12_T_padronizado   <- estados_covar[indice_total, indice_total, n_efetivo]
  cov_eta12_T_padronizado   <- matrix(cov_eta12_T_padronizado, nrow = length(indice_total), ncol = length(indice_total))
  
  # Se n_amostras <= 0, pula a amostragem bayesiana e usa estimativa pontual (com a posteriori da média e covariância)
  if (is.null(n_amostras) || n_amostras <= 0) {
    # Desfazendo a padronização das S_k das duas variáveis
    eta12_media_T <- as.numeric(eta12_media_T_padronizado / S_dp[seq_along(indice_total)])
    
    eta1_media_T <- eta12_media_T[seq_along(indice_x1)]
    eta2_media_T <- eta12_media_T[length(indice_x1) + seq_along(indice_x2)]
    
    # Reconstruindo as matrizes Cj de Almon
    j1 <- 0:(q1 - 1)
    if (d1 == 2) {
      Cj1 <- cbind(1, j1, j1^2)
    } else {
      Cj1 <- cbind(1, j1, j1^2, j1^3)
    }
    
    j2 <- 0:(q2 - 1)
    if (d2 == 2) {
      Cj2 <- cbind(1, j2, j2^2)
    } else {
      Cj2 <- cbind(1, j2, j2^2, j2^3)
    }
    
    beta1_pt <- as.numeric(eta1_media_T %*% t(Cj1))
    beta2_pt <- as.numeric(eta2_media_T %*% t(Cj2))
    
    rr_beta1_estimado    <- as.numeric(exp(beta1_pt))
    rr_sd1_estimado      <- rep(NA_real_, q1)
    rr_ic_inferior1_beta <- rep(NA_real_, q1)
    rr_ic_superior1_beta <- rep(NA_real_, q1)
    
    rr_beta2_estimado    <- as.numeric(exp(beta2_pt))
    rr_sd2_estimado      <- rep(NA_real_, q2)
    rr_ic_inferior2_beta <- rep(NA_real_, q2)
    rr_ic_superior2_beta <- rep(NA_real_, q2)
    
  } else {
    # Amostragem multivariada utilizando a posteriori conjunta no tempo final T
    amostras_eta12_padronizado <- MASS::mvrnorm(
      n = n_amostras,
      mu = eta12_media_T_padronizado,
      Sigma = cov_eta12_T_padronizado
    )
    
    if (n_amostras == 1) {
      amostras_eta12_padronizado <- matrix(amostras_eta12_padronizado, nrow = 1)
    }
    
    # Desfazendo a padronização das S_k (se padronizar for false, o S_dp é 1 e nada muda)
    amostras_eta12 <- scale(amostras_eta12_padronizado, center = FALSE, scale = S_dp[seq_along(indice_total)])
    
    amostras_eta1 <- amostras_eta12[, seq_along(indice_x1), drop = FALSE]
    amostras_eta2 <- amostras_eta12[, length(indice_x1) + seq_along(indice_x2), drop = FALSE]
    
    # Reconstruindo os Betas para cada lag de X1
    j1 <- 0:(q1 - 1)
    if (d1 == 2) {
      Cj1 <- cbind(1, j1, j1^2)
    } else {
      Cj1 <- cbind(1, j1, j1^2, j1^3)
    }
    
    amostras_beta1 <- amostras_eta1 %*% t(Cj1)
    rr_amostras_beta1 <- exp(amostras_beta1)
    
    rr_beta1_estimado    <- colMeans(rr_amostras_beta1)
    rr_sd1_estimado      <- apply(rr_amostras_beta1, 2, sd)
    rr_ic_inferior1_beta <- apply(rr_amostras_beta1, 2, quantile, probs = 0.025)
    rr_ic_superior1_beta <- apply(rr_amostras_beta1, 2, quantile, probs = 0.975)
    
    # Reconstruindo os Betas para cada lag de X2
    j2 <- 0:(q2 - 1)
    if (d2 == 2) {
      Cj2 <- cbind(1, j2, j2^2)
    } else {
      Cj2 <- cbind(1, j2, j2^2, j2^3)
    }
    
    amostras_beta2 <- amostras_eta2 %*% t(Cj2)
    rr_amostras_beta2 <- exp(amostras_beta2)
    
    rr_beta2_estimado    <- colMeans(rr_amostras_beta2)
    rr_sd2_estimado      <- apply(rr_amostras_beta2, 2, sd)
    rr_ic_inferior2_beta <- apply(rr_amostras_beta2, 2, quantile, probs = 0.025)
    rr_ic_superior2_beta <- apply(rr_amostras_beta2, 2, quantile, probs = 0.975)
  }
  
  ## --- 6.2) Recuperando o componente sazonal ao longo do tempo ---
  indice_sazonal_ini <- max(indice_x2) + 1
  indice_sazonal_fim <- indice_sazonal_ini + n_sazonal_estados - 1
  indice_sazonal <- indice_sazonal_ini:indice_sazonal_fim
  
  sazonal_media_t <- rep(NA_real_, n_efetivo)
  sazonal_dp_t    <- rep(NA_real_, n_efetivo)
  
  FF_sazonal <- sazonal$FF
  if (length(dim(FF_sazonal)) == 2) {
    FF_sazonal <- array(FF_sazonal, dim = c(nrow(FF_sazonal), ncol(FF_sazonal), 1))
  }
  
  for (tt in 1:n_efetivo) {
    idx_ff <- min(tt, dim(FF_sazonal)[3])
    f_sazonal_t <- as.numeric(FF_sazonal[, 1, idx_ff])
    
    m_sazonal_t <- estados_media[indice_sazonal, tt]
    C_sazonal_t <- estados_covar[indice_sazonal, indice_sazonal, tt]
    C_sazonal_t <- matrix(C_sazonal_t, nrow = length(indice_sazonal), ncol = length(indice_sazonal))
    
    sazonal_media_t[tt] <- as.numeric(t(f_sazonal_t) %*% m_sazonal_t)
    sazonal_var_t       <- as.numeric(t(f_sazonal_t) %*% C_sazonal_t %*% f_sazonal_t)
    sazonal_var_t       <- max(sazonal_var_t, 0)
    sazonal_dp_t[tt]    <- sqrt(sazonal_var_t)
  }
  
  sazonal_ic_inf   <- sazonal_media_t - 1.96 * sazonal_dp_t
  sazonal_ic_sup   <- sazonal_media_t + 1.96 * sazonal_dp_t
  sazonal_rr_media <- exp(sazonal_media_t)
  sazonal_rr_ic_inf <- exp(sazonal_ic_inf)
  sazonal_rr_ic_sup <- exp(sazonal_ic_sup)
  # --------------------------------------------------------------------------------------------
  
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
  
  # Eixo X adaptativo
  dt_min <- min(df_y$t_index)
  dt_max <- max(df_y$t_index)
  span_days <- as.integer(dt_max - dt_min)
  
  # Meses em PT-BR
  mes_pt <- c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")
  
  if (span_days <= 366) {
    by <- "1 month"
    lab_fun <- function(d) mes_pt[as.integer(format(d, "%m"))]
  } else if (span_days <= 2*366) {
    by <- "2 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else if (span_days <= 4*366) {
    by <- "3 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else if (span_days <= 6*366) {
    by <- "6 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else {
    by <- "1 year"
    lab_fun <- function(d) format(d, "%Y")
  }
  
  breaks_seq <- seq(dt_min, dt_max, by = by)
  span_years <- as.numeric(difftime(dt_max, dt_min, units = "days")) / 365.25
  
  lw <- if (span_years <= 6) {
    1.00
  } else {
    0.90
  }
  
  g1 <- ggplot(df_y, aes(x = t_index)) +
    geom_line(aes(y = Y,          colour = "Dados"),       linewidth = lw) +
    geom_line(aes(y = y_estimado, colour = "Estimativas"), linewidth = lw) +
    geom_ribbon(aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    geom_hline(yintercept = 0) +
    labs(x = "Data", y = "Internações", colour = "", fill = "") +
    theme_hc() +
    theme(
      axis.title.x = element_text(size = 35),
      axis.text    = element_text(size = 35),
      axis.text.x  = element_text(size = 35, angle = 90),
      legend.text  = element_text(size = 30),
      legend.title = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(size = 35)
    ) +
    scale_color_manual(values = c("Dados" = "black", "Estimativas" = "red"),
                       breaks = c("Estimativas", "Dados")) +
    scale_fill_manual(values = c("IC 95%" = "blue")) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  g1_relatorio <- ggplot(df_y, aes(x = t_index)) +
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
      fill   = guide_legend(order = 1),
      colour = guide_legend(order = 2)
    )
  
  g1_legado <- ggplot(df_y, aes(x = t_index)) +
    geom_line(aes(y = Y,          colour = "Dados"),       linewidth = 1.6) +
    geom_ribbon(aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    geom_line(aes(y = y_estimado, colour = "Estimativas"), linewidth = 1.6) +
    geom_hline(yintercept = 0) +
    labs(x = "Mês", y = "Internações", colour = "", fill = "") +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 35),
      axis.text    = element_text(size = 35),
      axis.text.x  = element_text(size = 35, angle = 90),
      legend.text  = element_text(size = 30),
      legend.title = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(size = 35)
    ) +
    scale_color_manual(values = c("Dados" = "black", "Estimativas" = "red"),
                       breaks = c("Estimativas", "Dados")) +
    scale_fill_manual(values = c("IC 95%" = "blue")) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  # g2_x1: Beta(j) da primeira variável
  df_beta1 <- data.frame(
    lag = 0:(q1 - 1),
    rr = as.numeric(rr_beta1_estimado),
    rr_lo = as.numeric(rr_ic_inferior1_beta),
    rr_hi = as.numeric(rr_ic_superior1_beta)
  )
  
  g2_x1 <- ggplot(df_beta1, aes(x = lag, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "cadetblue3", color = "cadetblue3",
                alpha = 0.3, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_point(size = 2.5, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Lags X1", y = "Risco Relativo (RR)") +
    theme_hc(base_size = 1) +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      panel.grid = element_blank(),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = seq(0, q1 - 1, by = 1))
  
  g2_legado_x1 <- ggplot(df_beta1, aes(x = lag, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "blue", color = "blue",
                alpha = 0.2, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_point(size = 3, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Lags X1", y = "Risco Relativo") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = seq(0, q1 - 1, by = 1))
  
  # g2_x2: Beta(j) da segunda variável
  df_beta2 <- data.frame(
    lag = 0:(q2 - 1),
    rr = as.numeric(rr_beta2_estimado),
    rr_lo = as.numeric(rr_ic_inferior2_beta),
    rr_hi = as.numeric(rr_ic_superior2_beta)
  )
  
  g2_x2 <- ggplot(df_beta2, aes(x = lag, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "cadetblue3", color = "cadetblue3",
                alpha = 0.3, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_point(size = 2.5, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Lags X2", y = "Risco Relativo (RR)") +
    theme_hc(base_size = 1) +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      panel.grid = element_blank(),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = seq(0, q2 - 1, by = 1))
  
  g2_legado_x2 <- ggplot(df_beta2, aes(x = lag, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "blue", color = "blue",
                alpha = 0.2, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_point(size = 3, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Lags X2", y = "Risco Relativo") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = seq(0, q2 - 1, by = 1))
  
  # g3: componente sazonal ao longo do tempo, em escala de RR
  df_sazonal <- data.frame(
    t_index = t_index,
    rr = as.numeric(sazonal_rr_media),
    rr_lo = as.numeric(sazonal_rr_ic_inf),
    rr_hi = as.numeric(sazonal_rr_ic_sup)
  )
  
  g3 <- ggplot(df_sazonal, aes(x = t_index, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "darkseagreen3", color = "darkseagreen3",
                alpha = 0.3, show.legend = FALSE) +
    geom_line(linewidth = lw, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Data", y = "Efeito Sazonal (RR)") +
    theme_hc(base_size = 1) +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      axis.text.x = element_text(size = 35, angle = 90),
      panel.grid = element_blank(),
      legend.position = "none"
    ) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  g3_legado <- ggplot(df_sazonal, aes(x = t_index, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "darkseagreen3", color = "darkseagreen3",
                alpha = 0.2, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Mês", y = "Efeito Sazonal (RR)") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      axis.text.x = element_text(size = 35, angle = 90),
      legend.position = "none"
    ) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  g2_x1_x2 <- NULL
  g1_g2_x1_x2 <- NULL
  g1_g2_x1_x2_g3 <- NULL
  if (requireNamespace("patchwork", quietly = TRUE)) {
    g2_x1_x2 <- patchwork::wrap_plots(g2_x1, g2_x2, ncol = 2)
    g1_g2_x1_x2 <- patchwork::wrap_plots(g1, g2_x1, g2_x2, ncol = 3)
    g1_g2_x1_x2_g3 <- patchwork::wrap_plots(g1, g2_x1, g2_x2, g3, ncol = 2)
  }
  
  return(list(
    mu_media    = mu_estimada,
    mu_ic_inf   = mu_ic_inf,
    mu_ic_sup   = mu_ic_sup,
    beta1_media  = rr_beta1_estimado,
    beta1_sd     = rr_sd1_estimado,
    beta1_ic_inf = rr_ic_inferior1_beta,
    beta1_ic_sup = rr_ic_superior1_beta,
    beta2_media  = rr_beta2_estimado,
    beta2_sd     = rr_sd2_estimado,
    beta2_ic_inf = rr_ic_inferior2_beta,
    beta2_ic_sup = rr_ic_superior2_beta,
    indice_beta1 = indice_x1,
    indice_beta2 = indice_x2,
    sazonal_media = sazonal_media_t,
    sazonal_dp = sazonal_dp_t,
    sazonal_ic_inf = sazonal_ic_inf,
    sazonal_ic_sup = sazonal_ic_sup,
    sazonal_rr_media = sazonal_rr_media,
    sazonal_rr_ic_inf = sazonal_rr_ic_inf,
    sazonal_rr_ic_sup = sazonal_rr_ic_sup,
    periodo_sazonal = periodo_sazonal,
    ordem_sazonal = ordem_sazonal,
    indice_sazonal = indice_sazonal,
    g1 = g1,
    g1_relatorio = g1_relatorio,
    g1_legado = g1_legado,
    g2_x1 = g2_x1,
    g2_legado_x1 = g2_legado_x1,
    g2_x2 = g2_x2,
    g2_legado_x2 = g2_legado_x2,
    g2_x1_x2 = g2_x1_x2,
    g3 = g3,
    g3_legado = g3_legado,
    g1_g2_x1_x2 = g1_g2_x1_x2,
    g1_g2_x1_x2_g3 = g1_g2_x1_x2_g3,
    kdglm_coef  = coefs,
    pred_df  = coefs$data,
    ajuste1 = ajuste
  ))
}


PDLDGLM_2X_clima_sazonal <- function(
    Y, X1, X2, covar, data,
    lags1, lags2, lag_covar, d1, d2,
    perc, lado = c("acima","abaixo"),
    perc_sup = NULL,
    periodo_sazonal,
    ordem_sazonal = 1,
    fd_nivel = 0.99,
    fd_sazonal = 0.98,
    padronizar_center = FALSE,
    padronizar_dp     = TRUE,
    n_amostras        = 1000
) {
  lado <- match.arg(lado)
  
  if (length(Y) != length(X1) || length(Y) != length(X2) || length(Y) != length(data) || length(Y) != length(covar)) {
    stop("Y, X1, X2, covar e data devem ter o mesmo comprimento.")
  }
  if (sum(is.na(Y)) > 0 || sum(is.na(X1)) > 0 || sum(is.na(X2)) > 0 || sum(is.na(covar)) > 0) {
    stop("Y, X1, X2 e covar não podem conter NA.")
  }
  if (min(Y) < 0 || any(Y != round(Y))) {
    stop("Y deve ser uma contagem (inteiro >= 0).")
  }
  if (d1 != 2 && d1 != 3) {
    stop("d1 deve ser 2 ou 3 (grau do polinômio de defasagens para X1).")
  }
  if (d2 != 2 && d2 != 3) {
    stop("d2 deve ser 2 ou 3 (grau do polinômio de defasagens para X2).")
  }
  if (lags1 < 2 || lags1 != round(lags1)) {
    stop("'lags1' deve ser um inteiro >= 2.")
  }
  if (lags2 < 2 || lags2 != round(lags2)) {
    stop("'lags2' deve ser um inteiro >= 2.")
  }
  if (lag_covar < 0 || lag_covar != round(lag_covar)) {
    stop("'lag_covar' deve ser um inteiro >= 0.")
  }
  if (perc <= 0 || perc >= 1) {
    stop("'perc' deve estar em (0,1), por exemplo 0.85, 0.95.")
  }
  if (!is.null(perc_sup)) {
    if (!is.numeric(perc_sup) || length(perc_sup) != 1 || !is.finite(perc_sup)) {
      stop("'perc_sup' deve ser escalar numérico em (0,1), quando informado.")
    }
    if (perc_sup <= 0 || perc_sup >= 1) {
      stop("'perc_sup' deve estar em (0,1).")
    }
    if (perc_sup <= perc) {
      stop("'perc_sup' deve ser maior que 'perc' para definir intervalo interno.")
    }
  }
  if (max(lags1 + 1, lags2 + 1) > length(Y)) {
    stop("Série muito curta para o número de lags informado.")
  }
  if (!is.numeric(periodo_sazonal) || length(periodo_sazonal) != 1 || !is.finite(periodo_sazonal) ||
      periodo_sazonal <= 1 || periodo_sazonal != round(periodo_sazonal)) {
    stop("'periodo_sazonal' deve ser um inteiro > 1.")
  }
  if (!is.numeric(ordem_sazonal) || length(ordem_sazonal) != 1 || !is.finite(ordem_sazonal) ||
      ordem_sazonal < 1 || ordem_sazonal != round(ordem_sazonal)) {
    stop("'ordem_sazonal' deve ser um inteiro >= 1.")
  }
  if (ordem_sazonal > floor(periodo_sazonal / 2)) {
    stop("'ordem_sazonal' não pode exceder floor(periodo_sazonal/2).")
  }
  
  ## --- 1) Construção das matrizes de lags das covariáveis X1 e X2 ---
  n_total <- length(Y) # tamanho total da série Y
  
  q1 <- lags1 + 1 # número de colunas da matriz de lags de X1 (lag 0 até lags1)
  X1_mat <- matrix(0, nrow = n_total, ncol = q1) # Inicialização da matriz de lags de X1
  X1_mat[, 1] <- X1 # Primeira coluna: lag 0, que é o próprio X1
  
  for (j in 2:q1) {
    X1_mat[, j] <- dplyr::lag(X1, j - 1) # Colunas seguintes: X1 defasado em 1, 2, ..., (q1-1) lags
  }
  
  q2 <- lags2 + 1 # número de colunas da matriz de lags de X2 (lag 0 até lags2)
  X2_mat <- matrix(0, nrow = n_total, ncol = q2) # Inicialização da matriz de lags de X2
  X2_mat[, 1] <- X2 # Primeira coluna: lag 0, que é o próprio X2
  
  for (j in 2:q2) {
    X2_mat[, j] <- dplyr::lag(X2, j - 1) # Colunas seguintes: X2 defasado em 1, 2, ..., (q2-1) lags
  }
  
  ## --- 1b) Indicadora climática H_t (0/1) e defasagem da covariável ---
  ini <- max(q1, q2, lag_covar + 1) # índice inicial comum (pra remover o burn-in conjunto)
  if (ini > n_total) stop("Janela efetiva vazia após alinhamento.")
  
  corte <- as.numeric(stats::quantile(covar[ini:n_total], probs = perc, na.rm = TRUE))
  corte_sup <- NA_real_
  if (is.null(perc_sup)) {
    H_raw <- if (lado == "acima") as.numeric(covar > corte) else as.numeric(covar < corte)
  } else {
    corte_sup <- as.numeric(stats::quantile(covar[ini:n_total], probs = perc_sup, na.rm = TRUE))
    H_raw <- as.numeric(covar > corte & covar < corte_sup)
  }
  H_def <- dplyr::lag(H_raw, lag_covar) # aplica a defasagem pedida
  
  ## --- 1c) Alinhamento conjunto (X1, X2 e covariável defasada) ---
  X1_mat <- X1_mat[ini:n_total, ] # removemos as primeiras linhas vazias de X1
  X2_mat <- X2_mat[ini:n_total, ] # removemos as primeiras linhas vazias de X2
  Y      <- Y[ini:n_total]        # removemos as primeiras observações de Y para alinhar
  H_def  <- H_def[ini:n_total]    # covariável já alinhada
  n_efetivo <- length(Y)          # tamanho efetivo de Y após o alinhamento
  
  ## --- 2) Calculando as somas de Almon para X1 e X2 ---
  v1 <- 1:lags1 # índices 1, 2, ..., lags1 usados nas fórmulas de X1_S0, X1_S1, X1_S2, X1_S3
  v2 <- 1:lags2 # índices 1, 2, ..., lags2 usados nas fórmulas de X2_S0, X2_S1, X2_S2, X2_S3
  
  # Somas de Almon para X1
  X1_S0 <- rowSums(X1_mat)
  X1_S1 <- X1_mat[, -1] %*% v1
  X1_S2 <- X1_mat[, -1] %*% (v1^2)
  
  if (d1 == 3) {
    X1_S3 <- X1_mat[, -1] %*% (v1^3)
  }
  
  # Somas de Almon para X2
  X2_S0 <- rowSums(X2_mat)
  X2_S1 <- X2_mat[, -1] %*% v2
  X2_S2 <- X2_mat[, -1] %*% (v2^2)
  
  if (d2 == 3) {
    X2_S3 <- X2_mat[, -1] %*% (v2^3)
  }
  
  if (d1 == 2 && d2 == 2) {
    S_bruto <- data.frame(
      X1_S0 = as.numeric(X1_S0),
      X1_S1 = as.numeric(X1_S1),
      X1_S2 = as.numeric(X1_S2),
      X2_S0 = as.numeric(X2_S0),
      X2_S1 = as.numeric(X2_S1),
      X2_S2 = as.numeric(X2_S2)
    )
  } else if (d1 == 2 && d2 == 3) {
    S_bruto <- data.frame(
      X1_S0 = as.numeric(X1_S0),
      X1_S1 = as.numeric(X1_S1),
      X1_S2 = as.numeric(X1_S2),
      X2_S0 = as.numeric(X2_S0),
      X2_S1 = as.numeric(X2_S1),
      X2_S2 = as.numeric(X2_S2),
      X2_S3 = as.numeric(X2_S3)
    )
  } else if (d1 == 3 && d2 == 2) {
    S_bruto <- data.frame(
      X1_S0 = as.numeric(X1_S0),
      X1_S1 = as.numeric(X1_S1),
      X1_S2 = as.numeric(X1_S2),
      X1_S3 = as.numeric(X1_S3),
      X2_S0 = as.numeric(X2_S0),
      X2_S1 = as.numeric(X2_S1),
      X2_S2 = as.numeric(X2_S2)
    )
  } else {
    S_bruto <- data.frame(
      X1_S0 = as.numeric(X1_S0),
      X1_S1 = as.numeric(X1_S1),
      X1_S2 = as.numeric(X1_S2),
      X1_S3 = as.numeric(X1_S3),
      X2_S0 = as.numeric(X2_S0),
      X2_S1 = as.numeric(X2_S1),
      X2_S2 = as.numeric(X2_S2),
      X2_S3 = as.numeric(X2_S3)
    )
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
    S_dp[!is.finite(S_dp) | S_dp < 1e-6] <- 1e-6            # Pra evitar dividir por ~zero/NA/Inf, usa um mínimo seguro
  }
  
  # (2) Se pediu para centralizar, calcule as médias
  if (padronizar_center) {
    center_vec <- colMeans(S_bruto)                         # centragem: subtrai a média de cada coluna
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
  
  # blocos de regressão para X1_S0, X1_S1, ..., X1_Sd1
  x1_b0 <- regression_block(rate = S_padronizado$X1_S0, D = 1, name = "X1_S0")
  x1_b1 <- regression_block(rate = S_padronizado$X1_S1, D = 1, name = "X1_S1")
  x1_b2 <- regression_block(rate = S_padronizado$X1_S2, D = 1, name = "X1_S2")
  if (d1 == 3) {
    x1_b3 <- regression_block(rate = S_padronizado$X1_S3, D = 1, name = "X1_S3")
  }
  
  # blocos de regressão para X2_S0, X2_S1, ..., X2_Sd2
  x2_b0 <- regression_block(rate = S_padronizado$X2_S0, D = 1, name = "X2_S0")
  x2_b1 <- regression_block(rate = S_padronizado$X2_S1, D = 1, name = "X2_S1")
  x2_b2 <- regression_block(rate = S_padronizado$X2_S2, D = 1, name = "X2_S2")
  if (d2 == 3) {
    x2_b3 <- regression_block(rate = S_padronizado$X2_S3, D = 1, name = "X2_S3")
  }
  
  # bloco da covariável climática indicadora (constante no tempo: D = 1)
  h  <- regression_block(rate = H_def, D = 1, name = "H")
  
  # bloco sazonal harmônico com período escolhido pela pessoa
  sazonal <- harmonic_block(rate = 1, period = periodo_sazonal, order = ordem_sazonal,
                            D = fd_sazonal, name = "Sazonal")
  n_sazonal_estados <- 2 * ordem_sazonal
  
  if (d1 == 2 && d2 == 2) {
    bloco <- (nivel + x1_b0 + x1_b1 + x1_b2 + x2_b0 + x2_b1 + x2_b2 + h + sazonal)
  } else if (d1 == 2 && d2 == 3) {
    bloco <- (nivel + x1_b0 + x1_b1 + x1_b2 + x2_b0 + x2_b1 + x2_b2 + x2_b3 + h + sazonal)
  } else if (d1 == 3 && d2 == 2) {
    bloco <- (nivel + x1_b0 + x1_b1 + x1_b2 + x1_b3 + x2_b0 + x2_b1 + x2_b2 + h + sazonal)
  } else {
    bloco <- (nivel + x1_b0 + x1_b1 + x1_b2 + x1_b3 + x2_b0 + x2_b1 + x2_b2 + x2_b3 + h + sazonal)
  }
  
  desfecho <- Poisson(lambda = "rate", data = Y)
  ajuste <- fit_model(
    bloco,
    y = desfecho)
  
  coefs <- coef(ajuste, lag = -1, eval.pred = TRUE, eval.metric = TRUE, pred.cred  = 0.95)
  
  ## --- 5) Pegando a média estimada a cada tempo ---
  if (!is.null(coefs$ft)) {
    eta_media_t <- as.numeric(coefs$ft[1, ])
    eta_dp_t    <- sqrt(as.numeric(coefs$Qt[1, 1, ]))
  } else {
    eta_media_t <- as.numeric(coefs$lambda.mean[1, ])
    eta_dp_t    <- sqrt(as.numeric(coefs$lambda.cov[1, 1, ]))
  }
  
  mu_estimada <- exp(eta_media_t)
  mu_ic_inf   <- exp(eta_media_t - 1.96 * eta_dp_t)
  mu_ic_sup   <- exp(eta_media_t + 1.96 * eta_dp_t)
  
  ## --- 6.1) Reconstruindo os betas de cada lag via amostragem bayesiana dos η no tempo final ---
  if (d1 == 2) {
    indice_x1 <- 2:4
  } else {
    indice_x1 <- 2:5
  }
  
  indice_x2_ini <- max(indice_x1) + 1
  if (d2 == 2) {
    indice_x2 <- indice_x2_ini:(indice_x2_ini + 2)
  } else {
    indice_x2 <- indice_x2_ini:(indice_x2_ini + 3)
  }
  
  idx_tau <- max(indice_x2) + 1 # No vetor de estados: [Nivel, X1_S0.., X2_S0.., H] => tau vem antes da sazonalidade
  indice_total <- c(indice_x1, indice_x2)
  
  if (!is.null(coefs$theta.mean)) {
    estados_media <- coefs$theta.mean # E[θ_t | dados] (Vetor de estados)
    estados_covar <- coefs$theta.cov  # Cov[θ_t | dados] (Matriz de covariância dos estados)
  } else {
    estados_media <- coefs$mt         # E[θ_t | dados] em versões antigas
    estados_covar <- coefs$Ct         # Cov[θ_t | dados] em versões antigas
  }
  
  eta12_media_T_padronizado <- estados_media[indice_total, n_efetivo]
  cov_eta12_T_padronizado   <- estados_covar[indice_total, indice_total, n_efetivo]
  cov_eta12_T_padronizado   <- matrix(cov_eta12_T_padronizado, nrow = length(indice_total), ncol = length(indice_total))
  
  # Desvio-padrão de X1 e X2 no período efetivo (para a escala de Risco Relativo, como no análogo com clima)
  sd_X1_efetivo <- stats::sd(X1[ini:n_total])
  sd_X2_efetivo <- stats::sd(X2[ini:n_total])
  
  # Se n_amostras <= 0, pula a amostragem bayesiana e usa estimativa pontual (com a posteriori da média e covariância)
  if (is.null(n_amostras) || n_amostras <= 0) {
    # Desfazendo a padronização das S_k das duas variáveis
    eta12_media_T <- as.numeric(eta12_media_T_padronizado / S_dp[seq_along(indice_total)])
    
    eta1_media_T <- eta12_media_T[seq_along(indice_x1)]
    eta2_media_T <- eta12_media_T[length(indice_x1) + seq_along(indice_x2)]
    
    # Reconstruindo as matrizes Cj de Almon
    j1 <- 0:(q1 - 1)
    if (d1 == 2) {
      Cj1 <- cbind(1, j1, j1^2)
    } else {
      Cj1 <- cbind(1, j1, j1^2, j1^3)
    }
    
    j2 <- 0:(q2 - 1)
    if (d2 == 2) {
      Cj2 <- cbind(1, j2, j2^2)
    } else {
      Cj2 <- cbind(1, j2, j2^2, j2^3)
    }
    
    beta1_pt <- as.numeric(eta1_media_T %*% t(Cj1))
    beta2_pt <- as.numeric(eta2_media_T %*% t(Cj2))
    
    rr_beta1_estimado    <- as.numeric(exp(sd_X1_efetivo * beta1_pt))
    rr_sd1_estimado      <- rep(NA_real_, q1)
    rr_ic_inferior1_beta <- rep(NA_real_, q1)
    rr_ic_superior1_beta <- rep(NA_real_, q1)
    
    rr_beta2_estimado    <- as.numeric(exp(sd_X2_efetivo * beta2_pt))
    rr_sd2_estimado      <- rep(NA_real_, q2)
    rr_ic_inferior2_beta <- rep(NA_real_, q2)
    rr_ic_superior2_beta <- rep(NA_real_, q2)
    
  } else {
    # Amostragem multivariada utilizando a posteriori conjunta no tempo final T
    amostras_eta12_padronizado <- MASS::mvrnorm(
      n = n_amostras,
      mu = eta12_media_T_padronizado,
      Sigma = cov_eta12_T_padronizado
    )
    
    if (n_amostras == 1) {
      amostras_eta12_padronizado <- matrix(amostras_eta12_padronizado, nrow = 1)
    }
    
    # Desfazendo a padronização das S_k (se padronizar for false, o S_dp é 1 e nada muda)
    amostras_eta12 <- scale(amostras_eta12_padronizado, center = FALSE, scale = S_dp[seq_along(indice_total)])
    
    amostras_eta1 <- amostras_eta12[, seq_along(indice_x1), drop = FALSE]
    amostras_eta2 <- amostras_eta12[, length(indice_x1) + seq_along(indice_x2), drop = FALSE]
    
    # Reconstruindo os Betas para cada lag de X1
    j1 <- 0:(q1 - 1)
    if (d1 == 2) {
      Cj1 <- cbind(1, j1, j1^2)
    } else {
      Cj1 <- cbind(1, j1, j1^2, j1^3)
    }
    
    amostras_beta1 <- amostras_eta1 %*% t(Cj1)
    rr_amostras_beta1 <- exp(sd_X1_efetivo * amostras_beta1)
    
    rr_beta1_estimado    <- colMeans(rr_amostras_beta1)
    rr_sd1_estimado      <- apply(rr_amostras_beta1, 2, sd)
    rr_ic_inferior1_beta <- apply(rr_amostras_beta1, 2, quantile, probs = 0.025)
    rr_ic_superior1_beta <- apply(rr_amostras_beta1, 2, quantile, probs = 0.975)
    
    # Reconstruindo os Betas para cada lag de X2
    j2 <- 0:(q2 - 1)
    if (d2 == 2) {
      Cj2 <- cbind(1, j2, j2^2)
    } else {
      Cj2 <- cbind(1, j2, j2^2, j2^3)
    }
    
    amostras_beta2 <- amostras_eta2 %*% t(Cj2)
    rr_amostras_beta2 <- exp(sd_X2_efetivo * amostras_beta2)
    
    rr_beta2_estimado    <- colMeans(rr_amostras_beta2)
    rr_sd2_estimado      <- apply(rr_amostras_beta2, 2, sd)
    rr_ic_inferior2_beta <- apply(rr_amostras_beta2, 2, quantile, probs = 0.025)
    rr_ic_superior2_beta <- apply(rr_amostras_beta2, 2, quantile, probs = 0.975)
  }
  
  ## --- 6.2) Reconstruindo o tau associado ao bloco H via amostragem bayesiana no tempo final ---
  tau_media_T <- estados_media[idx_tau, n_efetivo]
  tau_var_T   <- estados_covar[idx_tau, idx_tau, n_efetivo]
  
  if (is.null(n_amostras) || n_amostras <= 0) {
    rr_tau_estimado    <- exp(as.numeric(tau_media_T))
    rr_ic_inferior_tau <- NA_real_
    rr_ic_superior_tau <- NA_real_
  } else {
    amostras_tau <- rnorm(n_amostras, mean = as.numeric(tau_media_T), sd = sqrt(as.numeric(tau_var_T)))
    amostras_RR  <- exp(amostras_tau)
    
    rr_tau_estimado <- mean(amostras_RR)
    rr_ic_inferior_tau <- as.numeric(quantile(amostras_RR, probs = 0.025))
    rr_ic_superior_tau <- as.numeric(quantile(amostras_RR, probs = 0.975))
  }
  
  ## --- 6.3) Recuperando o componente sazonal ao longo do tempo ---
  indice_sazonal_ini <- idx_tau + 1
  indice_sazonal_fim <- indice_sazonal_ini + n_sazonal_estados - 1
  indice_sazonal <- indice_sazonal_ini:indice_sazonal_fim
  
  sazonal_media_t <- rep(NA_real_, n_efetivo)
  sazonal_dp_t    <- rep(NA_real_, n_efetivo)
  
  FF_sazonal <- sazonal$FF
  if (length(dim(FF_sazonal)) == 2) {
    FF_sazonal <- array(FF_sazonal, dim = c(nrow(FF_sazonal), ncol(FF_sazonal), 1))
  }
  
  for (tt in 1:n_efetivo) {
    idx_ff <- min(tt, dim(FF_sazonal)[3])
    f_sazonal_t <- as.numeric(FF_sazonal[, 1, idx_ff])
    
    m_sazonal_t <- estados_media[indice_sazonal, tt]
    C_sazonal_t <- estados_covar[indice_sazonal, indice_sazonal, tt]
    C_sazonal_t <- matrix(C_sazonal_t, nrow = length(indice_sazonal), ncol = length(indice_sazonal))
    
    sazonal_media_t[tt] <- as.numeric(t(f_sazonal_t) %*% m_sazonal_t)
    sazonal_var_t       <- as.numeric(t(f_sazonal_t) %*% C_sazonal_t %*% f_sazonal_t)
    sazonal_var_t       <- max(sazonal_var_t, 0)
    sazonal_dp_t[tt]    <- sqrt(sazonal_var_t)
  }
  
  sazonal_ic_inf   <- sazonal_media_t - 1.96 * sazonal_dp_t
  sazonal_ic_sup   <- sazonal_media_t + 1.96 * sazonal_dp_t
  sazonal_rr_media <- exp(sazonal_media_t)
  sazonal_rr_ic_inf <- exp(sazonal_ic_inf)
  sazonal_rr_ic_sup <- exp(sazonal_ic_sup)
  # --------------------------------------------------------------------------------------------
  
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
  
  # Eixo X adaptativo
  dt_min <- min(df_y$t_index)
  dt_max <- max(df_y$t_index)
  span_days <- as.integer(dt_max - dt_min)
  
  # Meses em PT-BR
  mes_pt <- c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")
  
  if (span_days <= 366) {
    by <- "1 month"
    lab_fun <- function(d) mes_pt[as.integer(format(d, "%m"))]
  } else if (span_days <= 2*366) {
    by <- "2 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else if (span_days <= 4*366) {
    by <- "3 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else if (span_days <= 6*366) {
    by <- "6 months"
    lab_fun <- function(d) paste0(mes_pt[as.integer(format(d, "%m"))], "-", format(d, "%Y"))
  } else {
    by <- "1 year"
    lab_fun <- function(d) format(d, "%Y")
  }
  
  breaks_seq <- seq(dt_min, dt_max, by = by)
  span_years <- as.numeric(difftime(dt_max, dt_min, units = "days")) / 365.25
  
  lw <- if (span_years <= 6) {
    1.00
  } else {
    0.90
  }
  
  g1 <- ggplot(df_y, aes(x = t_index)) +
    geom_line(aes(y = Y,          colour = "Dados"),       linewidth = lw) +
    geom_line(aes(y = y_estimado, colour = "Estimativas"), linewidth = lw) +
    geom_ribbon(aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    geom_hline(yintercept = 0) +
    labs(x = "Data", y = "Internações", colour = "", fill = "") +
    theme_hc() +
    theme(
      axis.title.x = element_text(size = 35),
      axis.text    = element_text(size = 35),
      axis.text.x  = element_text(size = 35, angle = 90),
      legend.text  = element_text(size = 30),
      legend.title = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(size = 35)
    ) +
    scale_color_manual(values = c("Dados" = "black", "Estimativas" = "red"),
                       breaks = c("Estimativas", "Dados")) +
    scale_fill_manual(values = c("IC 95%" = "blue")) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  g1_relatorio <- ggplot(df_y, aes(x = t_index)) +
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
      fill   = guide_legend(order = 1),
      colour = guide_legend(order = 2)
    )
  
  g1_legado <- ggplot(df_y, aes(x = t_index)) +
    geom_line(aes(y = Y,          colour = "Dados"),       linewidth = 1.6) +
    geom_ribbon(aes(ymin = mu_ic_inf, ymax = mu_ic_sup, fill = "IC 95%"), alpha = 0.3) +
    geom_line(aes(y = y_estimado, colour = "Estimativas"), linewidth = 1.6) +
    geom_hline(yintercept = 0) +
    labs(x = "Mês", y = "Internações", colour = "", fill = "") +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 35),
      axis.text    = element_text(size = 35),
      axis.text.x  = element_text(size = 35, angle = 90),
      legend.text  = element_text(size = 30),
      legend.title = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(size = 35)
    ) +
    scale_color_manual(values = c("Dados" = "black", "Estimativas" = "red"),
                       breaks = c("Estimativas", "Dados")) +
    scale_fill_manual(values = c("IC 95%" = "blue")) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  # g2_x1: Beta(j) da primeira variável
  df_beta1 <- data.frame(
    lag = 0:(q1 - 1),
    rr = as.numeric(rr_beta1_estimado),
    rr_lo = as.numeric(rr_ic_inferior1_beta),
    rr_hi = as.numeric(rr_ic_superior1_beta)
  )
  
  g2_x1 <- ggplot(df_beta1, aes(x = lag, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "cadetblue3", color = "cadetblue3",
                alpha = 0.3, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_point(size = 2.5, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Lags X1", y = "Risco Relativo (RR)") +
    theme_hc(base_size = 1) +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      panel.grid = element_blank(),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = seq(0, q1 - 1, by = 1))
  
  g2_legado_x1 <- ggplot(df_beta1, aes(x = lag, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "blue", color = "blue",
                alpha = 0.2, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_point(size = 3, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Lags X1", y = "Risco Relativo") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = seq(0, q1 - 1, by = 1))
  
  # g2_x2: Beta(j) da segunda variável
  df_beta2 <- data.frame(
    lag = 0:(q2 - 1),
    rr = as.numeric(rr_beta2_estimado),
    rr_lo = as.numeric(rr_ic_inferior2_beta),
    rr_hi = as.numeric(rr_ic_superior2_beta)
  )
  
  g2_x2 <- ggplot(df_beta2, aes(x = lag, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "cadetblue3", color = "cadetblue3",
                alpha = 0.3, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_point(size = 2.5, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Lags X2", y = "Risco Relativo (RR)") +
    theme_hc(base_size = 1) +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      panel.grid = element_blank(),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = seq(0, q2 - 1, by = 1))
  
  g2_legado_x2 <- ggplot(df_beta2, aes(x = lag, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "blue", color = "blue",
                alpha = 0.2, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_point(size = 3, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Lags X2", y = "Risco Relativo") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = seq(0, q2 - 1, by = 1))
  
  # g3: componente sazonal ao longo do tempo, em escala de RR
  df_sazonal <- data.frame(
    t_index = t_index,
    rr = as.numeric(sazonal_rr_media),
    rr_lo = as.numeric(sazonal_rr_ic_inf),
    rr_hi = as.numeric(sazonal_rr_ic_sup)
  )
  
  g3 <- ggplot(df_sazonal, aes(x = t_index, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "darkseagreen3", color = "darkseagreen3",
                alpha = 0.3, show.legend = FALSE) +
    geom_line(linewidth = lw, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Data", y = "Efeito Sazonal (RR)") +
    theme_hc(base_size = 1) +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      axis.text.x = element_text(size = 35, angle = 90),
      panel.grid = element_blank(),
      legend.position = "none"
    ) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  g3_legado <- ggplot(df_sazonal, aes(x = t_index, y = rr)) +
    geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi),
                fill = "darkseagreen3", color = "darkseagreen3",
                alpha = 0.2, show.legend = FALSE) +
    geom_line(linewidth = 1.2, colour = "black", show.legend = FALSE) +
    geom_hline(yintercept = 1, linewidth = 0.8) +
    labs(x = "Mês", y = "Efeito Sazonal (RR)") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 35),
      axis.text  = element_text(size = 35),
      axis.text.x = element_text(size = 35, angle = 90),
      legend.position = "none"
    ) +
    scale_x_date(
      breaks = breaks_seq,
      labels = lab_fun,
      expand = c(0, 0)
    )
  
  g2_x1_x2 <- NULL
  g1_g2_x1_x2 <- NULL
  g1_g2_x1_x2_g3 <- NULL
  if (requireNamespace("patchwork", quietly = TRUE)) {
    g2_x1_x2 <- patchwork::wrap_plots(g2_x1, g2_x2, ncol = 2)
    g1_g2_x1_x2 <- patchwork::wrap_plots(g1, g2_x1, g2_x2, ncol = 3)
    g1_g2_x1_x2_g3 <- patchwork::wrap_plots(g1, g2_x1, g2_x2, g3, ncol = 2)
  }
  
  return(list(
    mu_media    = mu_estimada,
    mu_ic_inf   = mu_ic_inf,
    mu_ic_sup   = mu_ic_sup,
    beta1_media  = rr_beta1_estimado,
    beta1_sd     = rr_sd1_estimado,
    beta1_ic_inf = rr_ic_inferior1_beta,
    beta1_ic_sup = rr_ic_superior1_beta,
    beta2_media  = rr_beta2_estimado,
    beta2_sd     = rr_sd2_estimado,
    beta2_ic_inf = rr_ic_inferior2_beta,
    beta2_ic_sup = rr_ic_superior2_beta,
    tau_media   = rr_tau_estimado,
    tau_ic_inf  = rr_ic_inferior_tau,
    tau_ic_sup  = rr_ic_superior_tau,
    indicadora_climatica = H_def,
    corte = corte,
    corte_sup = corte_sup,
    indice_beta1 = indice_x1,
    indice_beta2 = indice_x2,
    indice_tau = idx_tau,
    sazonal_media = sazonal_media_t,
    sazonal_dp = sazonal_dp_t,
    sazonal_ic_inf = sazonal_ic_inf,
    sazonal_ic_sup = sazonal_ic_sup,
    sazonal_rr_media = sazonal_rr_media,
    sazonal_rr_ic_inf = sazonal_rr_ic_inf,
    sazonal_rr_ic_sup = sazonal_rr_ic_sup,
    periodo_sazonal = periodo_sazonal,
    ordem_sazonal = ordem_sazonal,
    indice_sazonal = indice_sazonal,
    g1 = g1,
    g1_relatorio = g1_relatorio,
    g1_legado = g1_legado,
    g2_x1 = g2_x1,
    g2_legado_x1 = g2_legado_x1,
    g2_x2 = g2_x2,
    g2_legado_x2 = g2_legado_x2,
    g2_x1_x2 = g2_x1_x2,
    g3 = g3,
    g3_legado = g3_legado,
    g1_g2_x1_x2 = g1_g2_x1_x2,
    g1_g2_x1_x2_g3 = g1_g2_x1_x2_g3,
    kdglm_coef  = coefs,
    pred_df     = coefs$data,
    ajuste1 = ajuste
  ))
}
