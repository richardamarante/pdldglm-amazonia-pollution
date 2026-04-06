suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
  library(plotly)
  library(fresh)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

if (!exists("ic2025_theme_value", mode = "function")) {
  ic2025_theme_value <- function(key, default = getOption("ic2025.theme.colors", default = list())[[as.character(key)]]) {
    vals <- getOption("ic2025.theme.colors", default = list())
    value <- vals[[as.character(key)]]
    if (is.null(value) || !nzchar(value)) default else value
  }
}

.cache_rds <- new.env(parent = emptyenv())
.cache_anos <- new.env(parent = emptyenv())
.cache_modelagem <- new.env(parent = emptyenv())
.cache_duckdb <- new.env(parent = emptyenv())

ic2025_data_mode <- function() {
  modo <- tolower(as.character(getOption("ic2025.data_mode", "legacy")))
  if (!modo %in% c("legacy", "agregada")) modo <- "legacy"
  modo
}


ic2025_base_agregada_path <- function() {
  as.character(getOption("ic2025.agregada_rds", "base_final_era5land_cams_internacoes_obitos_BR_20150101_20251231_FINAL_missing_padronizado.rds"))
}

ic2025_project_root <- function() {
  cands <- unique(c(
    as.character(getOption("ic2025.project_root", "")),
    getwd(),
    ".",
    "..",
    "../.."
  ))
  for (cand in cands) {
    if (!nzchar(cand)) next
    root <- normalizePath(cand, winslash = "/", mustWork = FALSE)
    has_core_files <- file.exists(file.path(root, "app.R")) || file.exists(file.path(root, "funcoes.R"))
    if (has_core_files && dir.exists(file.path(root, "cache_geo"))) {
      return(root)
    }
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

ic2025_is_absolute_path <- function(path) {
  grepl("^(/|~|[A-Za-z]:[/\\\\]|\\\\\\\\)", as.character(path %||% ""))
}

ic2025_resolve_project_path <- function(path, project_root = ic2025_project_root()) {
  path <- as.character(path %||% "")
  if (!nzchar(path)) return(path)
  if (ic2025_is_absolute_path(path)) {
    return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }
  normalizePath(file.path(project_root, path), winslash = "/", mustWork = FALSE)
}

ic2025_is_git_lfs_pointer <- function(path) {
  if (!file.exists(path)) return(FALSE)
  info <- suppressWarnings(file.info(path))
  size <- info$size[[1]]
  if (!is.finite(size) || is.na(size) || size > 4096) return(FALSE)
  lines <- tryCatch(
    readLines(path, n = 3L, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character()
  )
  if (length(lines) < 3L) return(FALSE)
  any(grepl("^version https://git-lfs.github.com/spec/v1$", lines)) &&
    any(grepl("^oid sha256:", lines)) &&
    any(grepl("^size [0-9]+$", lines))
}

ic2025_agregada_duckdb_url <- function() {
  url <- as.character(getOption("ic2025.agregada_duckdb_url", ""))
  if (!nzchar(url)) {
    url <- Sys.getenv("IC2025_AGREGADA_DUCKDB_URL", unset = "")
  }
  if (!nzchar(url)) {
    url <- "https://github.com/richardamarante/dados-painelnacional/releases/latest/download/base_final.duckdb"
  }
  url
}

ic2025_agregada_duckdb_timeout <- function() {
  out <- suppressWarnings(as.integer(getOption("ic2025.agregada_duckdb_timeout", NA_integer_)))
  if (!is.finite(out) || is.na(out)) {
    out <- suppressWarnings(as.integer(Sys.getenv("IC2025_AGREGADA_DUCKDB_TIMEOUT", unset = "3600")))
  }
  if (!is.finite(out) || is.na(out) || out < 60L) out <- 3600L
  out
}

ic2025_agregada_duckdb_cache_dir <- function() {
  dir_out <- as.character(getOption("ic2025.agregada_duckdb_cache_dir", ""))
  if (!nzchar(dir_out)) {
    dir_out <- Sys.getenv("IC2025_AGREGADA_DUCKDB_CACHE_DIR", unset = "")
  }
  if (!nzchar(dir_out)) {
    dir_out <- file.path(tools::R_user_dir("ic2025", which = "cache"), "duckdb")
  }
  dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
  normalizePath(dir_out, winslash = "/", mustWork = FALSE)
}

ic2025_duckdb_file_ready <- function(path) {
  if (!file.exists(path) || ic2025_is_git_lfs_pointer(path)) return(FALSE)
  info <- suppressWarnings(file.info(path))
  size <- info$size[[1]]
  is.finite(size) && !is.na(size) && size > 0
}

ic2025_download_agregada_duckdb <- function(dest_path, url = ic2025_agregada_duckdb_url()) {
  if (!nzchar(url)) {
    stop("URL do DuckDB nao configurada. Defina 'ic2025.agregada_duckdb_url' ou 'IC2025_AGREGADA_DUCKDB_URL'.")
  }

  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
  tmp_path <- paste0(dest_path, ".part")
  if (file.exists(tmp_path)) unlink(tmp_path, force = TRUE)

  old_timeout <- getOption("timeout")
  options(timeout = max(as.integer(old_timeout %||% 60L), ic2025_agregada_duckdb_timeout()))
  on.exit(options(timeout = old_timeout), add = TRUE)
  on.exit(if (file.exists(tmp_path)) unlink(tmp_path, force = TRUE), add = TRUE)

  message("Baixando base DuckDB do GitHub Releases...")
  utils::download.file(
    url = url,
    destfile = tmp_path,
    mode = "wb",
    quiet = FALSE
  )

  if (!ic2025_duckdb_file_ready(tmp_path)) {
    stop("Download do DuckDB falhou ou retornou um arquivo invalido: ", tmp_path)
  }

  if (file.exists(dest_path)) unlink(dest_path, force = TRUE)
  if (!file.rename(tmp_path, dest_path)) {
    stop("Nao foi possivel mover o DuckDB baixado para: ", dest_path)
  }

  normalizePath(dest_path, winslash = "/", mustWork = FALSE)
}

ic2025_storage_backend <- function() {
  backend <- tolower(as.character(getOption("ic2025.storage_backend", "rds")))
  if (!backend %in% c("rds", "duckdb")) backend <- "rds"
  if (identical(backend, "duckdb")) {
    ok_duck <- requireNamespace("duckdb", quietly = TRUE)
    ok_dbi <- requireNamespace("DBI", quietly = TRUE)
    db_path <- tryCatch(
      ic2025_base_agregada_duckdb_path(),
      error = function(e) {
        warning("Backend 'duckdb' solicitado, mas nao foi possivel preparar a base: ", conditionMessage(e))
        ""
      }
    )
    if (!ok_duck || !ok_dbi) {
      warning("Backend 'duckdb' solicitado, mas pacotes 'duckdb'/'DBI' nao estao disponiveis. Voltando para 'rds'.")
      backend <- "rds"
    } else if (!nzchar(db_path) || !file.exists(db_path)) {
      warning("Backend 'duckdb' solicitado, mas arquivo nao encontrado: ", db_path, ". Voltando para 'rds'.")
      backend <- "rds"
    }
  }
  backend
}

ic2025_base_agregada_duckdb_path <- function(download_if_needed = TRUE) {
  configured <- as.character(getOption("ic2025.agregada_duckdb", "base_final.duckdb"))
  local_path <- ic2025_resolve_project_path(configured)

  if (ic2025_duckdb_file_ready(local_path)) {
    return(local_path)
  }
  if (!isTRUE(download_if_needed)) {
    return(local_path)
  }

  cached_path <- file.path(ic2025_agregada_duckdb_cache_dir(), basename(configured))
  if (ic2025_duckdb_file_ready(cached_path)) {
    return(cached_path)
  }

  ic2025_download_agregada_duckdb(cached_path)
}

ic2025_base_agregada_duckdb_table <- function() {
  as.character(getOption("ic2025.agregada_duckdb_table", "base_final_era5land_cams_internacoes_obitos_missing_padronizado"))
}

normalizar_codigo_municipio <- function(x) {
  cod <- suppressWarnings(as.integer(x))
  if (length(cod) == 0) return(integer())
  if (all(is.na(cod))) return(cod)
  # Normaliza por elemento (não pelo max global), pois a base pode misturar
  # códigos com e sem dígito verificador na mesma coluna.
  idx <- !is.na(cod) & cod > 1000000L
  while (any(idx)) {
    cod[idx] <- cod[idx] %/% 10L
    idx <- !is.na(cod) & cod > 1000000L
  }
  cod
}

pick_col <- function(df, opts) {
  hit <- opts[opts %in% names(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

first_non_empty <- function(x, fallback = NA_character_) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0) return(fallback)
  x[[1]]
}

coluna_saude_contagem <- function(nome_coluna) {
  nm <- tolower(as.character(nome_coluna %||% ""))
  if (!nzchar(nm)) return(FALSE)
  grepl("casos|interna|obit|cid10|resp|circ", nm)
}

ler_rds_cache <- function(arq) {
  if (isTRUE(getOption("ic2025.disable_rds_cache", FALSE))) {
    return(readRDS(arq))
  }
  key <- normalizePath(arq, winslash = "/", mustWork = FALSE)
  if (exists(key, envir = .cache_rds, inherits = FALSE)) {
    return(get(key, envir = .cache_rds, inherits = FALSE))
  }
  obj <- readRDS(arq)
  assign(key, obj, envir = .cache_rds)
  obj
}

duckdb_conexao_agregada <- function(use_cache = TRUE) {
  if (!identical(ic2025_storage_backend(), "duckdb")) return(NULL)

  db_path <- ic2025_base_agregada_duckdb_path()
  if (!file.exists(db_path)) {
    stop("Arquivo DuckDB não encontrado: ", db_path)
  }
  db_key <- normalizePath(db_path, winslash = "/", mustWork = FALSE)
  if (isTRUE(use_cache) && exists(db_key, envir = .cache_duckdb, inherits = FALSE)) {
    con <- get(db_key, envir = .cache_duckdb, inherits = FALSE)
    if (!is.null(con) && DBI::dbIsValid(con)) return(con)
  }

  con <- DBI::dbConnect(
    drv = duckdb::duckdb(),
    dbdir = db_path,
    read_only = TRUE
  )
  if (isTRUE(use_cache)) {
    assign(db_key, con, envir = .cache_duckdb)
  }
  con
}

duckdb_info_agregada <- function(use_cache = TRUE) {
  con <- duckdb_conexao_agregada(use_cache = use_cache)
  tbl <- ic2025_base_agregada_duckdb_table()
  campos <- DBI::dbListFields(con, tbl)
  pick <- function(opts) {
    hit <- opts[opts %in% campos]
    if (length(hit) == 0) return(NA_character_)
    hit[[1]]
  }
  list(
    con = con,
    tabela = tbl,
    campos = campos,
    col_cod = pick(c("CodigoMunicipio", "CodigoMunicipio6", "CodMunicipio", "codigo_municipio")),
    col_data = pick(c("Data", "data", "date")),
    col_nome = pick(c("NomeMunicipio", "nome_municipio")),
    col_uf = pick(c("UFMunicipio", "uf", "UF", "abbrev_state")),
    col_resp = pick(c(
      "Casos_Resp", "Internacoes_Resp", "InternacoesResp", "CasosCID10J", "Internacoes", "Internações",
      "CasosCID10I", "Internacoes_Circ", "InternacoesCirc"
    )),
    col_pm25 = pick(c("PM2p5", "pm25", "PM25")),
    col_co = pick(c("CO", "co")),
    col_temp = pick(c("Temperatura", "temp")),
    col_umid = pick(c("UmidRel", "umid", "UmidadeRelativa")),
    col_vento = pick(c("VelocVento", "veloc_vento", "vento")),
    col_sens = pick(c("SensTermica", "sens_termica"))
  )
}

precarregar_base_agregada_duckdb <- function() {
  info <- duckdb_info_agregada()
  q_tbl <- as.character(DBI::dbQuoteIdentifier(info$con, info$tabela))
  DBI::dbGetQuery(info$con, paste0("SELECT 1 AS ok FROM ", q_tbl, " LIMIT 1"))
  invisible(TRUE)
}

carregar_base_agregada_slice <- function(
    codigo_municipio = NULL,
    uf = NULL,
    data_ini = NULL,
    data_fim = NULL,
    colunas = NULL,
    distinct = FALSE,
    duckdb_no_cache = FALSE
) {
  backend <- ic2025_storage_backend()
  if (!backend %in% c("rds", "duckdb")) backend <- "rds"

  if (identical(backend, "rds")) {
    base <- carregar_base_rds(ic2025_base_agregada_path())
    if (is.null(base) || nrow(base) == 0) return(base)
    if (!is.null(codigo_municipio)) {
      cod <- normalizar_codigo_municipio(codigo_municipio)[1]
      if (is.finite(cod)) base <- base[base$CodigoMunicipio == cod, , drop = FALSE]
    }
    if (!is.null(uf)) {
      uf_col <- pick_col(base, c("UFMunicipio", "uf", "UF", "abbrev_state"))
      if (!is.na(uf_col)) {
        base <- base[toupper(trimws(as.character(base[[uf_col]]))) == toupper(trimws(as.character(uf))), , drop = FALSE]
      }
    }
    if (!is.null(data_ini)) base <- base[as.Date(base$Data) >= as.Date(data_ini), , drop = FALSE]
    if (!is.null(data_fim)) base <- base[as.Date(base$Data) <= as.Date(data_fim), , drop = FALSE]
    if (!is.null(colunas)) {
      keep <- unique(c(colunas, "Data", "CodigoMunicipio", "Ano"))
      keep <- keep[keep %in% names(base)]
      base <- base[, keep, drop = FALSE]
    }
    if (isTRUE(distinct)) base <- unique(base)
    return(base)
  }

  info <- duckdb_info_agregada(use_cache = !isTRUE(duckdb_no_cache))
  if (isTRUE(duckdb_no_cache)) {
    on.exit(try(DBI::dbDisconnect(info$con, shutdown = FALSE), silent = TRUE), add = TRUE)
  }
  if (is.na(info$col_cod) || is.na(info$col_data)) {
    stop("Tabela DuckDB sem colunas de município/data reconhecidas.")
  }

  campos <- info$campos
  if (is.null(colunas)) {
    sel_cols <- campos
  } else {
    sel_cols <- unique(c(
      intersect(as.character(colunas), campos),
      info$col_data,
      info$col_cod
    ))
  }
  if (length(sel_cols) == 0) {
    sel_cols <- unique(c(info$col_data, info$col_cod))
  }

  q_tbl <- as.character(DBI::dbQuoteIdentifier(info$con, info$tabela))
  q_sel <- paste(as.character(DBI::dbQuoteIdentifier(info$con, sel_cols)), collapse = ", ")
  if (isTRUE(distinct)) q_sel <- paste0("DISTINCT ", q_sel)

  where <- character()
  params <- list()

  if (!is.null(codigo_municipio)) {
    cod <- normalizar_codigo_municipio(codigo_municipio)[1]
    if (is.finite(cod)) {
      q_cod <- as.character(DBI::dbQuoteIdentifier(info$con, info$col_cod))
      cod_cands <- unique(as.integer(c(cod, cod * 10L, cod * 100L, cod * 1000L)))
      cod_cands <- cod_cands[is.finite(cod_cands)]
      if (length(cod_cands) > 0) {
        placeholders <- paste(rep("?", length(cod_cands)), collapse = ", ")
        where <- c(where, paste0("CAST(", q_cod, " AS BIGINT) IN (", placeholders, ")"))
        params <- c(params, as.list(cod_cands))
      }
    }
  }

  if (!is.null(uf) && !is.na(info$col_uf)) {
    q_uf <- as.character(DBI::dbQuoteIdentifier(info$con, info$col_uf))
    where <- c(where, paste0("UPPER(TRIM(CAST(", q_uf, " AS VARCHAR))) = ?"))
    params <- c(params, list(toupper(trimws(as.character(uf)))))
  }

  if (!is.null(data_ini)) {
    q_data <- as.character(DBI::dbQuoteIdentifier(info$con, info$col_data))
    where <- c(where, paste0("CAST(", q_data, " AS DATE) >= CAST(? AS DATE)"))
    params <- c(params, list(as.character(as.Date(data_ini))))
  }
  if (!is.null(data_fim)) {
    q_data <- as.character(DBI::dbQuoteIdentifier(info$con, info$col_data))
    where <- c(where, paste0("CAST(", q_data, " AS DATE) <= CAST(? AS DATE)"))
    params <- c(params, list(as.character(as.Date(data_fim))))
  }

  sql <- paste0("SELECT ", q_sel, " FROM ", q_tbl)
  if (length(where) > 0) sql <- paste(sql, "WHERE", paste(where, collapse = " AND "))
  df <- if (length(params) > 0) {
    DBI::dbGetQuery(info$con, sql, params = params)
  } else {
    DBI::dbGetQuery(info$con, sql)
  }

  if (!is.data.frame(df) || nrow(df) == 0) {
    if (is.data.frame(df)) {
      df$Data <- as.Date(character())
      df$CodigoMunicipio <- integer()
      df$Ano <- integer()
      return(df)
    }
    return(NULL)
  }

  data_col <- if (info$col_data %in% names(df)) info$col_data else pick_col(df, c("Data", "data", "date"))
  cod_col <- if (info$col_cod %in% names(df)) info$col_cod else pick_col(df, c("CodigoMunicipio", "CodMunicipio", "codigo_municipio"))

  if (!is.na(data_col)) {
    df$Data <- as.Date(df[[data_col]])
  } else if (!("Data" %in% names(df))) {
    df$Data <- as.Date(NA)
  }
  if (!is.na(cod_col)) {
    df$CodigoMunicipio <- normalizar_codigo_municipio(df[[cod_col]])
  } else if (!("CodigoMunicipio" %in% names(df))) {
    df$CodigoMunicipio <- NA_integer_
  }
  df$Ano <- suppressWarnings(as.integer(format(as.Date(df$Data), "%Y")))

  df
}

carregar_base_agregada_media_diaria <- function(
    ufs = NULL,
    data_ini = NULL,
    data_fim = NULL,
    colunas = NULL,
    duckdb_no_cache = FALSE
) {
  backend <- ic2025_storage_backend()
  if (!backend %in% c("rds", "duckdb")) backend <- "rds"
  ufs_sel <- unique(toupper(trimws(as.character(ufs %||% character(0)))))
  ufs_sel <- ufs_sel[nzchar(ufs_sel)]
  filtrar_uf <- length(ufs_sel) > 0

  if (identical(backend, "rds")) {
    base <- carregar_base_rds(ic2025_base_agregada_path())
    if (is.null(base) || nrow(base) == 0) {
      return(tibble::tibble(Data = as.Date(character()), CodigoMunicipio = integer(), Ano = integer()))
    }
    uf_col <- pick_col(base, c("UFMunicipio", "uf", "UF", "abbrev_state"))
    if (is.na(uf_col)) {
      return(tibble::tibble(Data = as.Date(character()), CodigoMunicipio = integer(), Ano = integer()))
    }
    if (isTRUE(filtrar_uf)) {
      base <- base[toupper(trimws(as.character(base[[uf_col]]))) %in% ufs_sel, , drop = FALSE]
    }
    if (!is.null(data_ini)) base <- base[as.Date(base$Data) >= as.Date(data_ini), , drop = FALSE]
    if (!is.null(data_fim)) base <- base[as.Date(base$Data) <= as.Date(data_fim), , drop = FALSE]
    if (nrow(base) == 0) {
      return(tibble::tibble(Data = as.Date(character()), CodigoMunicipio = integer(), Ano = integer()))
    }

    cols_excluir <- unique(c("CodigoMunicipio", "Data", "Ano", "Mes", "Dia", uf_col, "NomeMunicipio", "nome_municipio"))
    cols_num <- names(base)[vapply(base, is.numeric, logical(1))]
    cols_num <- setdiff(cols_num, cols_excluir)
    if (!is.null(colunas)) cols_num <- intersect(cols_num, as.character(colunas))
    if (length(cols_num) == 0) {
      out <- base[, c("Data"), drop = FALSE]
      out <- out[0, , drop = FALSE]
      out$CodigoMunicipio <- NA_integer_
      out$Ano <- integer()
      return(out)
    }

    out <- base %>%
      dplyr::group_by(.data$Data) %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(cols_num),
          ~ {
            z <- suppressWarnings(as.numeric(.x))
            if (all(!is.finite(z))) {
              NA_real_
            } else if (isTRUE(coluna_saude_contagem(dplyr::cur_column()))) {
              sum(z, na.rm = TRUE)
            } else {
              mean(z, na.rm = TRUE)
            }
          }
        ),
        .groups = "drop"
      ) %>%
      dplyr::arrange(.data$Data)
    out$CodigoMunicipio <- NA_integer_
    out$Ano <- suppressWarnings(as.integer(format(as.Date(out$Data), "%Y")))
    return(out)
  }

  info <- duckdb_info_agregada(use_cache = !isTRUE(duckdb_no_cache))
  if (isTRUE(duckdb_no_cache)) {
    on.exit(try(DBI::dbDisconnect(info$con, shutdown = FALSE), silent = TRUE), add = TRUE)
  }
  if (is.na(info$col_data) || is.na(info$col_uf)) {
    stop("Tabela DuckDB sem colunas de data/UF reconhecidas.")
  }

  cols_excluir <- unique(c(
    info$col_cod, info$col_data, info$col_uf, info$col_nome,
    "CodigoMunicipio", "Data", "Ano", "Mes", "Dia",
    "NomeMunicipio", "nome_municipio", "UFMunicipio", "uf", "UF"
  ))
  cols_medias <- setdiff(info$campos, cols_excluir)
  if (!is.null(colunas)) cols_medias <- intersect(cols_medias, as.character(colunas))
  cols_medias <- cols_medias[nzchar(cols_medias)]

  q_tbl <- as.character(DBI::dbQuoteIdentifier(info$con, info$tabela))
  q_data <- as.character(DBI::dbQuoteIdentifier(info$con, info$col_data))
  q_uf <- as.character(DBI::dbQuoteIdentifier(info$con, info$col_uf))

  sel_parts <- c(paste0("CAST(", q_data, " AS DATE) AS ", as.character(DBI::dbQuoteIdentifier(info$con, "Data"))))
  if (length(cols_medias) > 0) {
    expr_medias <- vapply(cols_medias, function(col) {
      q_col <- as.character(DBI::dbQuoteIdentifier(info$con, col))
      q_alias <- as.character(DBI::dbQuoteIdentifier(info$con, col))
      # DuckDB pode armazenar faltantes como NaN; converte NaN para NULL antes da agregação.
      val_expr <- paste0("TRY_CAST(", q_col, " AS DOUBLE)")
      val_clean <- paste0("CASE WHEN isnan(", val_expr, ") THEN NULL ELSE ", val_expr, " END")
      agg_fun <- if (isTRUE(coluna_saude_contagem(col))) "SUM" else "AVG"
      paste0(
        agg_fun, "(", val_clean, ") AS ",
        q_alias
      )
    }, character(1))
    sel_parts <- c(sel_parts, expr_medias)
  }

  where <- character()
  params <- list()

  if (isTRUE(filtrar_uf)) {
    placeholders <- paste(rep("?", length(ufs_sel)), collapse = ", ")
    where <- c(where, paste0("UPPER(TRIM(CAST(", q_uf, " AS VARCHAR))) IN (", placeholders, ")"))
    params <- c(params, as.list(ufs_sel))
  }

  if (!is.null(data_ini)) {
    where <- c(where, paste0("CAST(", q_data, " AS DATE) >= CAST(? AS DATE)"))
    params <- c(params, list(as.character(as.Date(data_ini))))
  }
  if (!is.null(data_fim)) {
    where <- c(where, paste0("CAST(", q_data, " AS DATE) <= CAST(? AS DATE)"))
    params <- c(params, list(as.character(as.Date(data_fim))))
  }

  sql <- paste0(
    "SELECT ", paste(sel_parts, collapse = ", "),
    " FROM ", q_tbl
  )
  if (length(where) > 0) {
    sql <- paste0(sql, " WHERE ", paste(where, collapse = " AND "))
  }
  sql <- paste0(
    sql,
    " GROUP BY CAST(", q_data, " AS DATE)",
    " ORDER BY CAST(", q_data, " AS DATE)"
  )

  out <- DBI::dbGetQuery(info$con, sql, params = params)
  if (!is.data.frame(out) || nrow(out) == 0) {
    return(tibble::tibble(Data = as.Date(character()), CodigoMunicipio = integer(), Ano = integer()))
  }

  out$Data <- as.Date(out$Data)
  out$CodigoMunicipio <- NA_integer_
  out$Ano <- suppressWarnings(as.integer(format(as.Date(out$Data), "%Y")))
  out
}

carregar_base_agregada_estado_media_diaria <- function(
    uf,
    data_ini = NULL,
    data_fim = NULL,
    colunas = NULL,
    duckdb_no_cache = FALSE
) {
  uf_sel <- toupper(trimws(as.character(uf %||% "")))
  if (!nzchar(uf_sel)) {
    return(tibble::tibble(Data = as.Date(character()), CodigoMunicipio = integer(), Ano = integer()))
  }
  carregar_base_agregada_media_diaria(
    ufs = uf_sel,
    data_ini = data_ini,
    data_fim = data_fim,
    colunas = colunas,
    duckdb_no_cache = duckdb_no_cache
  )
}

carregar_base_agregada_media_municipio_periodo <- function(
    var_col,
    data_ini = NULL,
    data_fim = NULL,
    duckdb_no_cache = FALSE
) {
  var_col <- as.character(var_col %||% "")
  if (!nzchar(var_col)) {
    return(tibble::tibble(CodigoMunicipio = integer(), valor = numeric()))
  }

  backend <- ic2025_storage_backend()
  if (!backend %in% c("rds", "duckdb")) backend <- "rds"

  if (identical(backend, "rds")) {
    base <- carregar_base_rds(ic2025_base_agregada_path())
    if (is.null(base) || nrow(base) == 0) {
      return(tibble::tibble(CodigoMunicipio = integer(), valor = numeric()))
    }
    if (!(var_col %in% names(base))) {
      return(tibble::tibble(CodigoMunicipio = integer(), valor = numeric()))
    }
    if (!is.null(data_ini)) base <- base[as.Date(base$Data) >= as.Date(data_ini), , drop = FALSE]
    if (!is.null(data_fim)) base <- base[as.Date(base$Data) <= as.Date(data_fim), , drop = FALSE]
    if (nrow(base) == 0) {
      return(tibble::tibble(CodigoMunicipio = integer(), valor = numeric()))
    }

    z <- suppressWarnings(as.numeric(base[[var_col]]))
    cod <- normalizar_codigo_municipio(base$CodigoMunicipio)
    ok <- is.finite(cod) & is.finite(z)
    if (!any(ok)) {
      return(tibble::tibble(CodigoMunicipio = integer(), valor = numeric()))
    }
    agg_r <- if (isTRUE(coluna_saude_contagem(var_col))) sum else mean
    out <- data.frame(
      CodigoMunicipio = as.integer(cod[ok]),
      valor = as.numeric(z[ok])
    ) %>%
      dplyr::group_by(.data$CodigoMunicipio) %>%
      dplyr::summarise(valor = agg_r(.data$valor, na.rm = TRUE), .groups = "drop")
    return(out)
  }

  info <- duckdb_info_agregada(use_cache = !isTRUE(duckdb_no_cache))
  if (isTRUE(duckdb_no_cache)) {
    on.exit(try(DBI::dbDisconnect(info$con, shutdown = FALSE), silent = TRUE), add = TRUE)
  }
  if (is.na(info$col_cod) || is.na(info$col_data)) {
    stop("Tabela DuckDB sem colunas de município/data reconhecidas.")
  }
  if (!(var_col %in% info$campos)) {
    return(tibble::tibble(CodigoMunicipio = integer(), valor = numeric()))
  }

  q_tbl <- as.character(DBI::dbQuoteIdentifier(info$con, info$tabela))
  q_cod <- as.character(DBI::dbQuoteIdentifier(info$con, info$col_cod))
  q_data <- as.character(DBI::dbQuoteIdentifier(info$con, info$col_data))
  q_var <- as.character(DBI::dbQuoteIdentifier(info$con, var_col))

  val_expr <- paste0("TRY_CAST(", q_var, " AS DOUBLE)")
  val_clean <- paste0("CASE WHEN isnan(", val_expr, ") THEN NULL ELSE ", val_expr, " END")

  where <- character()
  params <- list()
  if (!is.null(data_ini)) {
    where <- c(where, paste0("CAST(", q_data, " AS DATE) >= CAST(? AS DATE)"))
    params <- c(params, list(as.character(as.Date(data_ini))))
  }
  if (!is.null(data_fim)) {
    where <- c(where, paste0("CAST(", q_data, " AS DATE) <= CAST(? AS DATE)"))
    params <- c(params, list(as.character(as.Date(data_fim))))
  }

  agg_fun <- if (isTRUE(coluna_saude_contagem(var_col))) "SUM" else "AVG"
  sql <- paste0(
    "SELECT CAST(", q_cod, " AS BIGINT) AS CodigoMunicipio, ",
    agg_fun, "(", val_clean, ") AS valor ",
    "FROM ", q_tbl
  )
  if (length(where) > 0) {
    sql <- paste0(sql, " WHERE ", paste(where, collapse = " AND "))
  }
  sql <- paste0(sql, " GROUP BY 1")

  out <- if (length(params) > 0) {
    DBI::dbGetQuery(info$con, sql, params = params)
  } else {
    DBI::dbGetQuery(info$con, sql)
  }
  if (!is.data.frame(out) || nrow(out) == 0) {
    return(tibble::tibble(CodigoMunicipio = integer(), valor = numeric()))
  }
  out$CodigoMunicipio <- normalizar_codigo_municipio(out$CodigoMunicipio)
  out$valor <- suppressWarnings(as.numeric(out$valor))
  out <- out[is.finite(out$CodigoMunicipio) & is.finite(out$valor), , drop = FALSE]
  tibble::as_tibble(out)
}

carregar_geometria_mapa_brasil_refinado <- function(
    geo_path = "cache_geo/geobr_municipality_2024.rds",
    plotly_cache_path = "cache_geo/mapa_brasil_plotly_geo_v3.rds",
    processed_cache_path = "cache_geo/mapa_brasil_refinado_processado_v3.rds",
    muni_geojson_path = "cache_geo/mapa_brasil_municipios_v3.geojson",
    state_geojson_path = "cache_geo/mapa_brasil_estados_v3.geojson"
) {
  enriquecer_muni_index <- function(muni_index, muni_sf = NULL) {
    mi <- tibble::as_tibble(muni_index)
    if (!("code_muni" %in% names(mi))) mi$code_muni <- character()
    mi$code_muni <- as.character(normalizar_codigo_municipio(mi$code_muni))
    if (!("abbrev_state" %in% names(mi))) mi$abbrev_state <- NA_character_
    if (!("name_muni" %in% names(mi))) mi$name_muni <- mi$code_muni
    if (!("estado" %in% names(mi))) mi$estado <- NA_character_
    if (!("regiao" %in% names(mi))) mi$regiao <- NA_character_

    if (!is.null(muni_sf) && inherits(muni_sf, "sf")) {
      muni_attr <- sf::st_drop_geometry(muni_sf)
      join_aux <- tibble::tibble(
        code_muni = as.character(normalizar_codigo_municipio(muni_attr$code_muni)),
        abbrev_state_aux = as.character(muni_attr$abbrev_state %||% NA_character_),
        estado_aux = as.character(muni_attr$name_state %||% NA_character_),
        regiao_aux = as.character(muni_attr$name_region %||% NA_character_)
      ) %>%
        dplyr::distinct(.data$code_muni, .keep_all = TRUE)
      mi <- mi %>%
        dplyr::left_join(join_aux, by = "code_muni") %>%
        dplyr::mutate(
          abbrev_state = dplyr::coalesce(.data$abbrev_state, .data$abbrev_state_aux),
          estado = dplyr::coalesce(.data$estado, .data$estado_aux, .data$abbrev_state),
          regiao = dplyr::coalesce(.data$regiao, .data$regiao_aux)
        ) %>%
        dplyr::select(-dplyr::any_of(c("abbrev_state_aux", "estado_aux", "regiao_aux")))
    } else {
      mi <- mi %>%
        dplyr::mutate(
          estado = dplyr::coalesce(.data$estado, .data$abbrev_state)
        )
    }
    mi %>%
      dplyr::distinct(.data$code_muni, .keep_all = TRUE)
  }

  plotly_cache_ok <- function(pg) {
    geojson_exists <- function(x) {
      if (!is.character(x) || length(x) != 1 || !nzchar(x)) return(FALSE)
      nm <- sub("\\?.*$", "", basename(x))
      file.exists(file.path("cache_geo", nm))
    }
    is.list(pg) &&
      all(c("muni_geojson", "state_geojson", "muni_index", "state_codes", "labels") %in% names(pg)) &&
      is.character(pg$muni_geojson) &&
      is.character(pg$state_geojson) &&
      geojson_exists(pg$muni_geojson) &&
      geojson_exists(pg$state_geojson) &&
      is.data.frame(pg$muni_index) &&
      all(c("code_muni", "abbrev_state", "name_muni") %in% names(pg$muni_index))
  }

  if (file.exists(processed_cache_path)) {
    geo_cached <- tryCatch(readRDS(processed_cache_path), error = function(e) NULL)
    ok_cached <- is.list(geo_cached) && all(c("municipios", "estados", "labels") %in% names(geo_cached))
    if (isTRUE(ok_cached)) {
      if (!("plotly_geo" %in% names(geo_cached))) {
        geo_cached$plotly_geo <- NULL
      }
      if (isTRUE(plotly_cache_ok(geo_cached$plotly_geo))) {
        geo_cached$plotly_geo$muni_index <- enriquecer_muni_index(
          geo_cached$plotly_geo$muni_index,
          geo_cached$municipios
        )
      }
      try(saveRDS(geo_cached, processed_cache_path), silent = TRUE)
      return(geo_cached)
    }
  }

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Pacote 'sf' não disponível para o mapa.")
  }
  if (!file.exists(geo_path)) {
    stop("Arquivo de geometria não encontrado: ", geo_path)
  }
  muni <- readRDS(geo_path)
  if (!inherits(muni, "sf")) {
    stop("Geometria inválida em ", geo_path, " (esperado objeto sf).")
  }
  if (!all(c("code_muni", "code_state", "abbrev_state") %in% names(muni))) {
    stop("Geometria sem colunas esperadas (code_muni/code_state/abbrev_state).")
  }

  muni$code_muni <- normalizar_codigo_municipio(muni$code_muni)
  muni <- muni[is.finite(muni$code_muni), , drop = FALSE]

  drop_offshore_parts <- function(sf_obj, codes, x_limit = -33.2) {
    keep <- rep(TRUE, nrow(sf_obj))
    idx <- which(as.character(sf_obj$code_muni) %in% as.character(codes))
    for (i in idx) {
      parts <- sf::st_cast(sf::st_geometry(sf_obj[i, ]), "POLYGON", do_split = TRUE)
      xmid <- vapply(
        seq_along(parts),
        function(j) {
          bb <- sf::st_bbox(parts[j])
          as.numeric((bb["xmin"] + bb["xmax"]) / 2)
        },
        numeric(1)
      )
      parts_keep <- parts[xmid <= x_limit]
      if (length(parts_keep) == 0) {
        keep[i] <- FALSE
        next
      }
      merged <- sf::st_union(parts_keep)
      sf::st_geometry(sf_obj)[i] <- sf::st_cast(merged, "MULTIPOLYGON", warn = FALSE)
    }
    sf_obj[keep, ]
  }

  remove_holes <- function(sf_obj) {
    g <- sf::st_geometry(sf_obj)
    g2 <- sf::st_sfc(
      lapply(g, function(geom) {
        if (inherits(geom, "POLYGON")) {
          sf::st_polygon(list(geom[[1]]))
        } else if (inherits(geom, "MULTIPOLYGON")) {
          sf::st_multipolygon(lapply(geom, function(poly) list(poly[[1]])))
        } else {
          geom
        }
      }),
      crs = sf::st_crs(sf_obj)
    )
    sf::st_geometry(sf_obj) <- g2
    sf_obj
  }
  to_multi_polygons <- function(sf_obj) {
    g_class <- class(sf::st_geometry(sf_obj))[1]
    if (!g_class %in% c("sfc_POLYGON", "sfc_MULTIPOLYGON")) {
      sf_obj <- sf::st_collection_extract(sf_obj, "POLYGON", warn = FALSE)
    }
    sf::st_cast(sf_obj, "MULTIPOLYGON", warn = FALSE)
  }

  offshore_target_codes <- c("2605459", "3205309")
  muni <- drop_offshore_parts(muni, offshore_target_codes, x_limit = -33.2)
  muni <- sf::st_make_valid(muni)
  muni <- remove_holes(muni)
  muni <- to_multi_polygons(muni)

  # Por padrão mantemos geometria completa para fidelidade visual ao mapa
  # original do script refinado. Só simplifica se opção explícita > 0.
  simplify_tol_m <- suppressWarnings(as.numeric(getOption("ic2025.desc_map_simplify_tol_m", 0)))
  if (is.finite(simplify_tol_m) && simplify_tol_m > 0) {
    muni_orig <- muni
    s2_old <- sf::sf_use_s2()
    suppressMessages(sf::sf_use_s2(FALSE))
    ok_simpl <- tryCatch({
      muni_m <- sf::st_transform(muni, 5880)
      muni_m <- sf::st_simplify(muni_m, dTolerance = simplify_tol_m, preserveTopology = TRUE)
      muni_s <- sf::st_transform(muni_m, sf::st_crs(muni_orig))
      muni_s <- sf::st_make_valid(muni_s)
      muni <- to_multi_polygons(muni_s)
      TRUE
    }, error = function(e) {
      warning("Falha ao simplificar geometrias do mapa; mantendo geometria completa. Detalhe: ", conditionMessage(e))
      FALSE
    }, finally = {
      suppressMessages(sf::sf_use_s2(s2_old))
    })
    if (!isTRUE(ok_simpl)) {
      muni <- muni_orig
    }
  }

  state_polys <- muni %>%
    dplyr::group_by(.data$code_state, .data$abbrev_state) %>%
    dplyr::summarise(.groups = "drop")
  state_polys <- sf::st_make_valid(state_polys)
  state_polys <- to_multi_polygons(state_polys)

  state_polys_5880 <- sf::st_make_valid(sf::st_transform(state_polys, 5880))
  state_cent_5880 <- suppressWarnings(sf::st_centroid(state_polys_5880, of_largest_polygon = TRUE))
  is_within_own <- diag(sf::st_within(state_cent_5880, state_polys_5880, sparse = FALSE))

  state_label_geom_5880 <- sf::st_geometry(state_cent_5880)
  if (any(!is_within_own)) {
    state_label_geom_5880[!is_within_own] <-
      sf::st_point_on_surface(sf::st_geometry(state_polys_5880[!is_within_own, ]))
  }
  state_label_geom <- sf::st_transform(state_label_geom_5880, sf::st_crs(state_polys))
  state_label_pts <- sf::st_as_sf(
    data.frame(abbrev_state = state_polys$abbrev_state),
    geometry = state_label_geom,
    crs = sf::st_crs(state_polys)
  )
  state_label_xy <- cbind(
    sf::st_drop_geometry(state_label_pts),
    sf::st_coordinates(state_label_pts)
  )

  plotly_geo <- NULL
  if (file.exists(plotly_cache_path)) {
    plotly_geo <- tryCatch(readRDS(plotly_cache_path), error = function(e) NULL)
    ok_cache <- plotly_cache_ok(plotly_geo)
    if (!isTRUE(ok_cache)) {
      plotly_geo <- NULL
    } else {
      plotly_geo$muni_index <- enriquecer_muni_index(plotly_geo$muni_index, muni)
    }
  }

  if (is.null(plotly_geo)) {
    dir.create(dirname(plotly_cache_path), recursive = TRUE, showWarnings = FALSE)
    dir.create(dirname(muni_geojson_path), recursive = TRUE, showWarnings = FALSE)

    muni_attr <- sf::st_drop_geometry(muni)
    name_col <- if ("name_muni" %in% names(muni_attr)) "name_muni" else NA_character_
    state_col <- if ("name_state" %in% names(muni_attr)) "name_state" else NA_character_
    region_col <- if ("name_region" %in% names(muni_attr)) "name_region" else NA_character_
    muni_index <- tibble::tibble(
      code_muni = as.character(muni_attr$code_muni),
      abbrev_state = as.character(muni_attr$abbrev_state),
      name_muni = if (!is.na(name_col)) as.character(muni_attr[[name_col]]) else as.character(muni_attr$code_muni),
      estado = if (!is.na(state_col)) as.character(muni_attr[[state_col]]) else as.character(muni_attr$abbrev_state),
      regiao = if (!is.na(region_col)) as.character(muni_attr[[region_col]]) else NA_character_
    )
    muni_index <- muni_index %>%
      dplyr::distinct(.data$code_muni, .keep_all = TRUE)

    muni_geo_sf <- muni %>%
      dplyr::mutate(code_muni = as.character(.data$code_muni)) %>%
      dplyr::select(code_muni, abbrev_state)
    state_geo_sf <- state_polys %>%
      dplyr::mutate(abbrev_state = as.character(.data$abbrev_state)) %>%
      dplyr::select(abbrev_state)

    suppressWarnings(unlink(c(muni_geojson_path, state_geojson_path), force = TRUE))
    sf::st_write(muni_geo_sf, muni_geojson_path, driver = "GeoJSON", quiet = TRUE, append = FALSE, delete_dsn = TRUE)
    sf::st_write(state_geo_sf, state_geojson_path, driver = "GeoJSON", quiet = TRUE, append = FALSE, delete_dsn = TRUE)

    muni_ver <- tryCatch(as.integer(file.info(muni_geojson_path)$mtime), error = function(e) as.integer(Sys.time()))
    state_ver <- tryCatch(as.integer(file.info(state_geojson_path)$mtime), error = function(e) as.integer(Sys.time()))
    muni_geojson <- sprintf("ic2025_cache_geo/%s?v=%s", basename(muni_geojson_path), muni_ver)
    state_geojson <- sprintf("ic2025_cache_geo/%s?v=%s", basename(state_geojson_path), state_ver)

    plotly_geo <- list(
      muni_geojson = muni_geojson,
      state_geojson = state_geojson,
      muni_index = enriquecer_muni_index(muni_index, muni),
      state_codes = unique(as.character(sf::st_drop_geometry(state_polys)$abbrev_state)),
      labels = as.data.frame(state_label_xy)
    )
    try(saveRDS(plotly_geo, plotly_cache_path), silent = TRUE)
  }

  out_geo <- list(
    municipios = muni,
    estados = state_polys,
    labels = state_label_xy,
    plotly_geo = plotly_geo
  )
  try(saveRDS(out_geo, processed_cache_path), silent = TRUE)
  out_geo
}

mapa_placeholder_ggplot <- function(
    mensagem = "Sem dados para montar o mapa no período selecionado.",
    periodo_label = NULL
) {
  p_gg <- ggplot2::ggplot() +
    ggplot2::annotate(
      "rect",
      xmin = 0.16,
      xmax = 0.84,
      ymin = 0.40,
      ymax = 0.60,
      fill = ic2025_theme_value("funcoes.box_bg"),
      colour = ic2025_theme_value("funcoes.box_border"),
      linewidth = 0.4
    ) +
    ggplot2::annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = mensagem,
      size = 6.2,
      fontface = "bold",
      colour = ic2025_theme_value("funcoes.text_muted")
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = ic2025_theme_value("funcoes.transparent"), color = NA),
      panel.background = ggplot2::element_rect(fill = ic2025_theme_value("funcoes.transparent"), color = NA),
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )

  if (nzchar(as.character(periodo_label %||% ""))) {
    p_gg <- p_gg +
      ggplot2::annotate(
        "text",
        x = 0.01,
        y = 0.98,
        hjust = 0,
        vjust = 1,
        label = as.character(periodo_label),
        size = 4.2,
        colour = ic2025_theme_value("funcoes.text")
      )
  }

  p_gg
}

extrair_legend_grob_ggplot <- function(p, preferred = c("bottom", "right", "left", "top")) {
  preferred <- match.arg(preferred)
  gt <- ggplot2::ggplotGrob(p)
  lookup <- c(
    bottom = "guide-box-bottom",
    right = "guide-box-right",
    left = "guide-box-left",
    top = "guide-box-top"
  )

  primary_name <- lookup[[preferred]]
  idx <- which(gt$layout$name == primary_name)
  if (length(idx) == 1 && !inherits(gt$grobs[[idx]], "zeroGrob")) {
    return(gt$grobs[[idx]])
  }

  alt_names <- c("guide-box-bottom", "guide-box-right", "guide-box-left", "guide-box-top", "guide-box")
  for (nm in alt_names) {
    idx <- which(gt$layout$name == nm)
    if (length(idx) == 1 && !inherits(gt$grobs[[idx]], "zeroGrob")) {
      return(gt$grobs[[idx]])
    }
  }

  NULL
}

plot_mapa_brasil_refinado_ggplot <- function(
    geo_info,
    medias_df,
    var_col = NULL,
    var_label = "Variável",
    periodo_label = NULL,
    geo_scope = c("brasil", "regiao", "uf"),
    uf_focus = NULL,
    show_legend = TRUE,
    show_state_labels = TRUE,
    panel_title = NULL,
    legend_position = c("right", "bottom")
) {
  geo_scope <- match.arg(geo_scope)
  legend_position <- match.arg(legend_position)
  if (!is.list(geo_info) || !all(c("municipios", "estados", "labels") %in% names(geo_info))) {
    stop("Geometria do mapa indisponível ou inválida.")
  }

  vals <- medias_df %>%
    dplyr::transmute(
      code_muni = as.character(normalizar_codigo_municipio(.data$CodigoMunicipio)),
      valor = suppressWarnings(as.numeric(.data$valor))
    ) %>%
    dplyr::filter(nzchar(.data$code_muni), is.finite(.data$valor)) %>%
    dplyr::distinct(.data$code_muni, .keep_all = TRUE)

  vals_num <- suppressWarnings(as.numeric(vals$valor))
  if (!any(is.finite(vals_num))) {
    return(mapa_placeholder_ggplot(
      "Sem dados para montar o mapa no período selecionado.",
      periodo_label = periodo_label
    ))
  }

  muni_sf <- geo_info$municipios
  state_polys <- geo_info$estados
  state_label_xy <- as.data.frame(geo_info$labels)

  muni_sf$code_muni <- as.character(normalizar_codigo_municipio(muni_sf$code_muni))
  map_df_sf <- muni_sf %>%
    dplyr::left_join(vals, by = "code_muni")

  coord_args <- list(datum = NA, expand = FALSE)
  ajustar_bbox_painel <- function(sf_obj, target_ratio = 1.72, pad_frac = 0.06, min_pad = 0.35) {
    bb <- sf::st_bbox(sf_obj)
    xmin <- as.numeric(bb["xmin"])
    xmax <- as.numeric(bb["xmax"])
    ymin <- as.numeric(bb["ymin"])
    ymax <- as.numeric(bb["ymax"])
    w <- xmax - xmin
    h <- ymax - ymin
    if (!is.finite(w) || !is.finite(h) || w <= 0 || h <= 0) {
      return(NULL)
    }

    cx <- mean(c(xmin, xmax))
    cy <- mean(c(ymin, ymax))
    w2 <- max(w * (1 + 2 * pad_frac), min_pad * 2)
    h2 <- max(h * (1 + 2 * pad_frac), min_pad * 2)
    current_ratio <- w2 / h2

    if (current_ratio < target_ratio) {
      w2 <- h2 * target_ratio
    } else {
      h2 <- w2 / target_ratio
    }

    list(
      xlim = c(cx - w2 / 2, cx + w2 / 2),
      ylim = c(cy - h2 / 2, cy + h2 / 2)
    )
  }

  if (identical(geo_scope, "regiao")) {
    uf_set <- unique(toupper(as.character(uf_focus %||% character(0))))
    uf_set <- uf_set[nzchar(uf_set)]
    map_df_sf <- map_df_sf %>% dplyr::filter(.data$abbrev_state %in% !!uf_set)
    state_polys <- state_polys %>% dplyr::filter(.data$abbrev_state %in% !!uf_set)
    state_label_xy <- state_label_xy %>% dplyr::filter(.data$abbrev_state %in% !!uf_set)
    if (nrow(map_df_sf) == 0 || nrow(state_polys) == 0) {
      return(mapa_placeholder_ggplot("Sem dados para a região selecionada."))
    }
    bbox_adj <- ajustar_bbox_painel(state_polys, target_ratio = 1.72, pad_frac = 0.020, min_pad = 0.22)
    coord_args$xlim <- bbox_adj$xlim
    coord_args$ylim <- bbox_adj$ylim
  } else if (identical(geo_scope, "uf")) {
    uf_sel <- toupper(as.character(uf_focus %||% ""))
    map_df_sf <- map_df_sf %>% dplyr::filter(.data$abbrev_state == !!uf_sel)
    state_polys <- state_polys %>% dplyr::filter(.data$abbrev_state == !!uf_sel)
    state_label_xy <- state_label_xy %>% dplyr::filter(.data$abbrev_state == !!uf_sel)
    if (nrow(map_df_sf) == 0 || nrow(state_polys) == 0) {
      return(mapa_placeholder_ggplot("Sem dados para o estado selecionado."))
    }
    bbox_adj <- ajustar_bbox_painel(state_polys, target_ratio = 1.72, pad_frac = 0.085, min_pad = 0.40)
    coord_args$xlim <- bbox_adj$xlim
    coord_args$ylim <- bbox_adj$ylim
  }

  lims <- as.numeric(stats::quantile(vals_num, probs = c(0.02, 0.98), na.rm = TRUE))
  if (!all(is.finite(lims)) || lims[[1]] >= lims[[2]]) {
    lims <- range(vals_num, finite = TRUE)
  }
  mid <- suppressWarnings(stats::median(vals_num, na.rm = TRUE))
  if (!is.finite(mid)) {
    mid <- mean(lims)
  }
  if (!is.finite(mid) || mid <= lims[[1]] || mid >= lims[[2]]) {
    mid <- mean(lims)
  }
  brks <- pretty(lims, n = 7)
  brks <- brks[is.finite(brks) & brks >= lims[[1]] & brks <= lims[[2]]]
  if (length(brks) == 0) brks <- lims
  max_abs_break <- suppressWarnings(max(abs(brks), na.rm = TRUE))
  is_umidade_relativa <- identical(as.character(var_col %||% ""), "UmidRel")
  scale_low <- if (is_umidade_relativa) {
    ic2025_theme_value("funcoes.map_high")
  } else {
    ic2025_theme_value("funcoes.map_low")
  }
  scale_mid <- ic2025_theme_value("funcoes.map_mid")
  scale_high <- if (is_umidade_relativa) {
    ic2025_theme_value("funcoes.map_low")
  } else {
    ic2025_theme_value("funcoes.map_high")
  }
  bottom_barwidth_mm <- if (!is.finite(max_abs_break)) {
    95
  } else if (max_abs_break >= 10000) {
    135
  } else if (max_abs_break >= 1000) {
    115
  } else {
    95
  }
  muni_border_width <- if (identical(geo_scope, "regiao")) 0.028 else 0.020
  muni_border_alpha <- if (identical(geo_scope, "regiao")) 0.14 else 0.30
  state_border_width <- if (identical(geo_scope, "regiao")) 0.220 else 0.130
  state_border_alpha <- if (identical(geo_scope, "regiao")) 0.84 else 0.75
  state_label_size_main <- 4.66 * 0.80
  state_label_size_df <- 2.60

  p_gg <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = map_df_sf,
      ggplot2::aes(fill = .data$valor, color = .data$valor),
      linewidth = 0.10,
      linejoin = "round",
      lineend = "round"
    ) +
    ggplot2::geom_sf(
      data = map_df_sf,
      fill = NA,
      color = ic2025_theme_value("funcoes.white"),
      linewidth = muni_border_width,
      alpha = muni_border_alpha,
      linejoin = "round",
      lineend = "round"
    ) +
    ggplot2::geom_sf(
      data = state_polys,
      fill = NA,
      color = ic2025_theme_value("funcoes.white"),
      linewidth = state_border_width,
      alpha = state_border_alpha,
      linejoin = "round",
      lineend = "round"
    )

  if (isTRUE(show_state_labels) && is.data.frame(state_label_xy) && all(c("X", "Y", "abbrev_state") %in% names(state_label_xy))) {
    show_df_label <- isTRUE(getOption("ic2025.desc_map_show_df_label", FALSE))
    shadow_ok <- isTRUE(tryCatch(requireNamespace("shadowtext", quietly = TRUE), error = function(e) FALSE))
    lab_main <- dplyr::filter(state_label_xy, .data$abbrev_state != "DF")
    lab_df <- if (isTRUE(show_df_label)) {
      dplyr::filter(state_label_xy, .data$abbrev_state == "DF")
    } else {
      state_label_xy[0, , drop = FALSE]
    }

    if (isTRUE(shadow_ok)) {
      p_gg <- p_gg +
        shadowtext::geom_shadowtext(
          data = lab_main,
          ggplot2::aes(x = .data$X, y = .data$Y, label = .data$abbrev_state),
          inherit.aes = FALSE,
          colour = ic2025_theme_value("funcoes.white"),
          bg.colour = ic2025_theme_value("funcoes.black"),
          bg.r = 0.08,
          size = state_label_size_main,
          fontface = "bold",
          alpha = 1,
          check_overlap = TRUE
        ) +
        shadowtext::geom_shadowtext(
          data = lab_df,
          ggplot2::aes(x = .data$X, y = .data$Y, label = .data$abbrev_state),
          inherit.aes = FALSE,
          colour = ic2025_theme_value("funcoes.white"),
          bg.colour = ic2025_theme_value("funcoes.black"),
          bg.r = 0.08,
          size = state_label_size_df,
          fontface = "bold",
          alpha = 1,
          check_overlap = TRUE
        )
    } else {
      p_gg <- p_gg +
        ggplot2::geom_text(
          data = lab_main,
          ggplot2::aes(x = .data$X, y = .data$Y, label = .data$abbrev_state),
          inherit.aes = FALSE,
          colour = ic2025_theme_value("funcoes.white"),
          size = state_label_size_main,
          fontface = "bold",
          alpha = 1,
          check_overlap = TRUE
        ) +
        ggplot2::geom_text(
          data = lab_df,
          ggplot2::aes(x = .data$X, y = .data$Y, label = .data$abbrev_state),
          inherit.aes = FALSE,
          colour = ic2025_theme_value("funcoes.white"),
          size = state_label_size_df,
          fontface = "bold",
          alpha = 1,
          check_overlap = TRUE
        )
    }
  }

  p_gg <- p_gg +
    ggplot2::scale_fill_gradient2(
      low = scale_low,
      mid = scale_mid,
      high = scale_high,
      midpoint = mid,
      limits = lims,
      oob = scales::squish,
      breaks = brks,
      labels = scales::label_number(accuracy = 1),
      name = NULL
    ) +
    ggplot2::scale_color_gradient2(
      low = scale_low,
      mid = scale_mid,
      high = scale_high,
      midpoint = mid,
      limits = lims,
      oob = scales::squish,
      guide = "none"
    ) +
    do.call(ggplot2::coord_sf, coord_args) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        barheight = grid::unit(90, "mm"),
        barwidth = grid::unit(5, "mm"),
        ticks.colour = ic2025_theme_value("funcoes.text_muted"),
        ticks.linewidth = 0.35,
        frame.colour = ic2025_theme_value("funcoes.text_muted")
      )
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = if (isTRUE(show_legend)) legend_position else "none",
      legend.justification = c(0, 0.5),
      legend.text = ggplot2::element_text(size = 15, color = ic2025_theme_value("funcoes.text_muted")),
      legend.margin = ggplot2::margin(0, 2, 0, 2),
      legend.box.margin = ggplot2::margin(0, 0, 0, 0),
      plot.margin = ggplot2::margin(0, 0, 0, 0),
      legend.background = ggplot2::element_rect(fill = ic2025_theme_value("funcoes.transparent"), color = NA),
      plot.background = ggplot2::element_rect(fill = ic2025_theme_value("funcoes.transparent"), color = NA),
      panel.background = ggplot2::element_rect(fill = ic2025_theme_value("funcoes.transparent"), color = NA),
      plot.title = ggplot2::element_text(size = 11, face = "bold", color = ic2025_theme_value("dashboard.ink_soft"), hjust = 0)
    )

  if (isTRUE(show_legend) && identical(legend_position, "bottom")) {
    p_gg <- p_gg +
      ggplot2::guides(
        fill = ggplot2::guide_colorbar(
          direction = "horizontal",
          barheight = grid::unit(4, "mm"),
          barwidth = grid::unit(bottom_barwidth_mm, "mm"),
          ticks.colour = ic2025_theme_value("funcoes.text_muted"),
          ticks.linewidth = 0.35,
          frame.colour = ic2025_theme_value("funcoes.text_muted")
        )
      ) +
      ggplot2::theme(
        legend.justification = c(0.5, 0),
        legend.box.margin = ggplot2::margin(2, 0, 0, 0)
      )
  }

  if (nzchar(as.character(panel_title %||% ""))) {
    p_gg <- p_gg + ggplot2::labs(title = as.character(panel_title))
  }

  if (nzchar(as.character(periodo_label %||% ""))) {
    p_gg <- p_gg +
      ggplot2::annotate(
        "text",
        x = -Inf,
        y = Inf,
        label = as.character(periodo_label),
        hjust = -0.1,
        vjust = 1.4,
        size = 4.2,
        colour = ic2025_theme_value("funcoes.text")
      )
  }

  p_gg
}

# Compatibilidade temporária com a fase anterior do mapa interativo
mapa_interativo_placeholder <- mapa_placeholder_ggplot
plot_mapa_brasil_refinado_interativo <- plot_mapa_brasil_refinado_ggplot

pal_verde_base <- c(
  ic2025_theme_value("funcoes.sidebar_bg"),
  ic2025_theme_value("funcoes.sidebar_hover"),
  ic2025_theme_value("funcoes.sidebar_accent"),
  ic2025_theme_value("funcoes.sidebar_light"),
  ic2025_theme_value("funcoes.sidebar_soft")
)
pal_verde <- grDevices::colorRampPalette(pal_verde_base)

hex2rgba <- function(hex, alpha = 0.7) {
  if (exists("cor_css", mode = "function")) {
    return(cor_css(hex, opacidade = alpha))
  }
  rgb <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.3f)", rgb[1], rgb[2], rgb[3], alpha)
}

tema_amazonia <- create_theme(
  adminlte_color(
    light_blue = pal_verde_base[3],
    green = pal_verde_base[3],
    aqua = pal_verde_base[4]
  ),
  adminlte_sidebar(
    dark_bg = pal_verde_base[1],
    dark_hover_bg = pal_verde_base[2],
    dark_color = ic2025_theme_value("funcoes.white"),
    dark_submenu_color = ic2025_theme_value("funcoes.white"),
    dark_submenu_hover_color = ic2025_theme_value("funcoes.sidebar_soft")
  ),
  adminlte_global(
    content_bg = ic2025_theme_value("funcoes.content_bg"),
    box_bg = ic2025_theme_value("funcoes.white"),
    info_box_bg = ic2025_theme_value("funcoes.white")
  ),
  adminlte_vars(
    border_radius_base = "8px"
  )
)

formatar_numero <- function(x, d = 2) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.finite(x), formatC(x, format = "f", digits = d, decimal.mark = ","), "NA")
}

carregar_base_rds <- function(caminho) {
  if (!file.exists(caminho)) return(NULL)
  x <- ler_rds_cache(caminho)
  if (!is.data.frame(x)) return(NULL)

  data_col <- pick_col(x, c("Data", "data", "date"))
  cod_col <- pick_col(x, c("CodigoMunicipio", "CodMunicipio", "codigo_municipio"))

  if (is.na(data_col)) return(NULL)

  x$Data <- as.Date(x[[data_col]])
  if (is.na(cod_col)) {
    x$CodigoMunicipio <- NA_integer_
  } else {
    x$CodigoMunicipio <- normalizar_codigo_municipio(x[[cod_col]])
  }
  x$Ano <- suppressWarnings(as.integer(format(x$Data, "%Y")))

  x
}

carregar_nucleo_pdldglm <- function(script = "resultados_tese/Aplicações/PDLDGLM_Geral_deploy.R") {
  env <- new.env(parent = globalenv())
  if (!file.exists(script)) {
    return(list(ok = FALSE, env = env, msg = "Arquivo PDLDGLM_Geral.R nao encontrado."))
  }
  out <- tryCatch({
    sys.source(script, envir = env)
    list(ok = TRUE, env = env, msg = "Nucleo carregado.")
  }, error = function(e) {
    list(ok = FALSE, env = env, msg = paste("Falha ao carregar nucleo:", conditionMessage(e)))
  })
  out
}

status_dependencias_pdldglm <- function() {
  pkgs <- c("kDGLM", "ggplot2", "ggthemes", "patchwork", "MASS", "Matrix", "dplyr")
  tibble::tibble(
    pacote = pkgs,
    instalado = vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
  )
}

inicializar_dependencias_pdldglm <- function() {
  st <- status_dependencias_pdldglm()
  if (!all(st$instalado)) {
    faltando <- st$pacote[!st$instalado]
    stop(paste0("Dependencias ausentes: ", paste(faltando, collapse = ", "), "."))
  }
  suppressPackageStartupMessages({
    library(kDGLM)
    library(ggplot2)
    library(ggthemes)
    library(patchwork)
    library(MASS)
    library(Matrix)
    library(dplyr)
  })
  invisible(TRUE)
}

catalogo_cidades_tese <- function() {
  if (!identical(ic2025_data_mode(), "agregada")) {
    return(tibble::tribble(
      ~chave, ~codigo, ~rotulo, ~base,
      "rio_de_janeiro", 330455L, "Rio de Janeiro", "rj",
      "sao_paulo", 355030L, "Sao Paulo", "sp",
      "rio_branco", 120040L, "Rio Branco", "amz",
      "manaus", 130260L, "Manaus", "amz",
      "porto_velho", 110020L, "Porto Velho", "amz",
      "boa_vista", 140010L, "Boa Vista", "amz",
      "belem", 150140L, "Belem", "amz",
      "macapa", 160030L, "Macapa", "amz",
      "cuiaba", 510340L, "Cuiaba", "amz",
      "palmas", 172100L, "Palmas", "amz",
      "sao_luis", 211130L, "Sao Luis", "amz"
    ))
  }

  backend <- ic2025_storage_backend()
  base_ref <- if (identical(backend, "duckdb")) ic2025_base_agregada_duckdb_path() else ic2025_base_agregada_path()
  if (!file.exists(base_ref)) {
    stop("Base agregada não encontrada: ", base_ref)
  }

  cache_key <- paste0(
    "catalogo_agregado::", backend, "::",
    normalizePath(base_ref, winslash = "/", mustWork = FALSE)
  )
  if (exists(cache_key, envir = .cache_modelagem, inherits = FALSE)) {
    return(get(cache_key, envir = .cache_modelagem, inherits = FALSE))
  }

  if (identical(backend, "duckdb")) {
    info <- duckdb_info_agregada()
    cols <- unique(na.omit(c(info$col_cod, info$col_nome, info$col_uf)))
    if (is.na(info$col_cod) || length(cols) == 0) {
      stop("Tabela DuckDB sem colunas mínimas para catálogo de cidades.")
    }
    # Caminho rápido: evita DISTINCT com Data (que explode cardinalidade).
    base <- tryCatch({
      con <- info$con
      q_tbl <- as.character(DBI::dbQuoteIdentifier(con, info$tabela))
      q_cols <- as.character(DBI::dbQuoteIdentifier(con, cols))
      sql <- paste0(
        "SELECT DISTINCT ",
        paste(q_cols, collapse = ", "),
        " FROM ",
        q_tbl
      )
      out <- DBI::dbGetQuery(con, sql)
      out$CodigoMunicipio <- normalizar_codigo_municipio(out[[info$col_cod]])
      out
    }, error = function(e) {
      warning(
        "[catalogo_cidades_tese] Falha no caminho rápido DuckDB; usando fallback legado. Detalhe: ",
        conditionMessage(e)
      )
      carregar_base_agregada_slice(colunas = cols, distinct = TRUE)
    })
    nome_col <- if (!is.na(info$col_nome) && info$col_nome %in% names(base)) info$col_nome else NA_character_
    uf_col <- if (!is.na(info$col_uf) && info$col_uf %in% names(base)) info$col_uf else NA_character_
  } else {
    base <- carregar_base_rds(base_ref)
    nome_col <- pick_col(base, c("NomeMunicipio", "nome_municipio"))
    uf_col <- pick_col(base, c("UFMunicipio", "uf", "UF", "abbrev_state"))
  }
  if (is.null(base) || nrow(base) == 0) {
    stop("Base agregada inválida ou vazia: ", base_ref)
  }

  nome <- if (!is.na(nome_col)) as.character(base[[nome_col]]) else rep(NA_character_, nrow(base))
  uf <- if (!is.na(uf_col)) as.character(base[[uf_col]]) else rep(NA_character_, nrow(base))
  cod <- normalizar_codigo_municipio(base$CodigoMunicipio)

  ok <- is.finite(cod)
  tab0 <- data.frame(
    codigo = cod[ok],
    nome = nome[ok],
    uf = uf[ok],
    stringsAsFactors = FALSE
  )
  tab0 <- tab0[!duplicated(tab0$codigo), , drop = FALSE]
  tab0$nome <- ifelse(is.na(tab0$nome) | !nzchar(trimws(tab0$nome)), paste0("Município ", tab0$codigo), tab0$nome)
  tab0$uf <- ifelse(is.na(tab0$uf), "", toupper(trimws(tab0$uf)))

  tab <- tibble::tibble(
    chave = as.character(tab0$codigo),
    codigo = as.integer(tab0$codigo),
    rotulo = ifelse(nzchar(tab0$uf), paste0(tab0$nome, " (", tab0$uf, ")"), tab0$nome),
    base = "br"
  ) %>%
    arrange(rotulo)

  assign(cache_key, tab, envir = .cache_modelagem)
  tab
}

carregar_base_modelagem_local <- function(chave, ano1, ano2) {
  if (identical(ic2025_data_mode(), "agregada")) {
    backend <- ic2025_storage_backend()
    base_ref <- if (identical(backend, "duckdb")) ic2025_base_agregada_duckdb_path() else ic2025_base_agregada_path()
    if (!file.exists(base_ref)) stop("Base agregada não encontrada: ", base_ref)

    cat <- catalogo_cidades_tese()
    alvo <- cat %>% filter(chave == !!chave)
    if (nrow(alvo) == 0) stop("Cidade invalida.")

    codigo_alvo <- as.integer(alvo$codigo[[1]])
    data_ini <- as.Date(sprintf("%d-01-01", as.integer(ano1)))
    data_fim <- as.Date(sprintf("%d-12-31", as.integer(ano2)))

    if (identical(backend, "duckdb")) {
      info <- duckdb_info_agregada()
      cols <- unique(na.omit(c(
        info$col_cod, info$col_data, info$col_nome, info$col_resp,
        info$col_pm25, info$col_co, info$col_temp, info$col_umid,
        info$col_vento, info$col_sens
      )))
      raw <- carregar_base_agregada_slice(
        codigo_municipio = codigo_alvo,
        data_ini = data_ini,
        data_fim = data_fim,
        colunas = cols,
        distinct = FALSE
      )
      if (is.null(raw) || nrow(raw) == 0) stop("Sem dados para essa cidade e periodo.")

      vec_num <- function(col_name) {
        if (is.na(col_name) || !nzchar(col_name) || !(col_name %in% names(raw))) return(rep(NA_real_, nrow(raw)))
        suppressWarnings(as.numeric(raw[[col_name]]))
      }
      vec_int <- function(col_name) {
        if (is.na(col_name) || !nzchar(col_name) || !(col_name %in% names(raw))) return(rep(NA_integer_, nrow(raw)))
        suppressWarnings(as.integer(raw[[col_name]]))
      }
      vec_chr <- function(col_name) {
        if (is.na(col_name) || !nzchar(col_name) || !(col_name %in% names(raw))) return(rep(NA_character_, nrow(raw)))
        as.character(raw[[col_name]])
      }

      nome_muni <- vec_chr(info$col_nome)
      fallback_nome <- first_non_empty(nome_muni, fallback = alvo$rotulo[[1]])

      out <- tibble::tibble(
        Data = as.Date(raw$Data),
        Ano = as.integer(format(as.Date(raw$Data), "%Y")),
        NomeMunicipio = ifelse(is.na(nome_muni) | !nzchar(trimws(nome_muni)), fallback_nome, nome_muni),
        Casos_Resp = vec_int(info$col_resp),
        pm25 = vec_num(info$col_pm25),
        co = vec_num(info$col_co),
        temp = vec_num(info$col_temp),
        umid = vec_num(info$col_umid),
        VelocVento = vec_num(info$col_vento),
        SensTermica = vec_num(info$col_sens)
      ) %>%
        arrange(Data)

      if (nrow(out) == 0) stop("Sem dados para essa cidade e periodo.")
      return(out)
    }

    # caminho legado da base agregada em RDS
    arq <- ic2025_base_agregada_path()
    cache_key <- paste0("agregada_info::", normalizePath(arq, winslash = "/", mustWork = FALSE))
    if (exists(cache_key, envir = .cache_modelagem, inherits = FALSE)) {
      info <- get(cache_key, envir = .cache_modelagem, inherits = FALSE)
    } else {
      base <- ler_rds_cache(arq)
      if (!is.data.frame(base)) stop("Arquivo de base agregada inválido.")

      col_cod <- pick_col(base, c("CodigoMunicipio", "CodMunicipio", "codigo_municipio"))
      col_data <- pick_col(base, c("Data", "data", "date"))
      if (is.na(col_cod) || is.na(col_data)) {
        stop("Base agregada sem colunas de município/data.")
      }

      col_nome <- pick_col(base, c("NomeMunicipio", "nome_municipio"))
      col_resp <- pick_col(base, c(
        "Casos_Resp", "Internacoes_Resp", "InternacoesResp", "CasosCID10J", "Internacoes", "Internações",
        "CasosCID10I", "Internacoes_Circ", "InternacoesCirc"
      ))
      col_pm25 <- pick_col(base, c("PM2p5", "pm25", "PM25"))
      col_co <- pick_col(base, c("CO", "co"))
      col_temp <- pick_col(base, c("Temperatura", "temp"))
      col_umid <- pick_col(base, c("UmidRel", "umid", "UmidadeRelativa"))
      col_vento <- pick_col(base, c("VelocVento", "veloc_vento", "vento"))
      col_sens <- pick_col(base, c("SensTermica", "sens_termica"))

      codigo <- normalizar_codigo_municipio(base[[col_cod]])
      data <- as.Date(base[[col_data]])
      ano <- suppressWarnings(as.integer(format(data, "%Y")))

      info <- list(
        base = base,
        codigo = codigo,
        data = data,
        ano = ano,
        col_nome = col_nome,
        col_resp = col_resp,
        col_pm25 = col_pm25,
        col_co = col_co,
        col_temp = col_temp,
        col_umid = col_umid,
        col_vento = col_vento,
        col_sens = col_sens
      )
      assign(cache_key, info, envir = .cache_modelagem)
    }

    idx <- which(is.finite(info$codigo) & info$codigo == codigo_alvo & !is.na(info$data) & info$data >= data_ini & info$data <= data_fim)
    if (length(idx) == 0) stop("Sem dados para essa cidade e periodo.")

    vec_num <- function(col_name) {
      if (is.na(col_name) || !nzchar(col_name)) return(rep(NA_real_, length(idx)))
      suppressWarnings(as.numeric(info$base[[col_name]][idx]))
    }
    vec_int <- function(col_name) {
      if (is.na(col_name) || !nzchar(col_name)) return(rep(NA_integer_, length(idx)))
      suppressWarnings(as.integer(info$base[[col_name]][idx]))
    }
    vec_chr <- function(col_name) {
      if (is.na(col_name) || !nzchar(col_name)) return(rep(NA_character_, length(idx)))
      as.character(info$base[[col_name]][idx])
    }

    nome_muni <- vec_chr(info$col_nome)
    fallback_nome <- first_non_empty(nome_muni, fallback = alvo$rotulo[[1]])

    out <- tibble::tibble(
      Data = info$data[idx],
      Ano = info$ano[idx],
      NomeMunicipio = ifelse(is.na(nome_muni) | !nzchar(trimws(nome_muni)), fallback_nome, nome_muni),
      Casos_Resp = vec_int(info$col_resp),
      pm25 = vec_num(info$col_pm25),
      co = vec_num(info$col_co),
      temp = vec_num(info$col_temp),
      umid = vec_num(info$col_umid),
      VelocVento = vec_num(info$col_vento),
      SensTermica = vec_num(info$col_sens)
    ) %>%
      arrange(Data)

    if (nrow(out) == 0) stop("Sem dados para essa cidade e periodo.")
    return(out)
  }

  cat <- catalogo_cidades_tese()
  alvo <- cat %>% filter(chave == !!chave)
  if (nrow(alvo) == 0) stop("Cidade invalida.")

  arq <- dplyr::case_when(
    alvo$base[[1]] == "sp" ~ "resultados_tese/Aplicações/São Paulo/base_cidades_sp.rds",
    alvo$base[[1]] == "rj" ~ "resultados_tese/Aplicações/Rio de Janeiro/base_cidades_rj.rds",
    TRUE ~ "resultados_tese/Aplicações/Amazônia Legal/base_cidades_amazonia_legal.rds"
  )
  if (!file.exists(arq)) stop("Base local nao encontrada: ", arq)

  key <- normalizePath(arq, winslash = "/", mustWork = FALSE)
  if (exists(key, envir = .cache_modelagem, inherits = FALSE)) {
    base_norm <- get(key, envir = .cache_modelagem, inherits = FALSE)
  } else {
    base <- ler_rds_cache(arq)
    if (!is.data.frame(base)) stop("Arquivo de base invalido.")

    pick_col <- function(opts) {
      hit <- opts[opts %in% names(base)]
      if (length(hit) == 0) return(NA_character_)
      hit[[1]]
    }

    var_resp <- dplyr::case_when(
      "Casos_Resp" %in% names(base) ~ "Casos_Resp",
      "Internacoes_Resp" %in% names(base) ~ "Internacoes_Resp",
      "InternacoesResp" %in% names(base) ~ "InternacoesResp",
      "CasosCID10J" %in% names(base) ~ "CasosCID10J",
      "CasosCID10I" %in% names(base) ~ "CasosCID10I",
      "Internacoes_Circ" %in% names(base) ~ "Internacoes_Circ",
      "InternacoesCirc" %in% names(base) ~ "InternacoesCirc",
      TRUE ~ NA_character_
    )
    if (is.na(var_resp)) stop("Variavel de resposta nao encontrada na base.")

    var_nome <- pick_col(c("NomeMunicipio", "nome_municipio"))
    var_pm25 <- pick_col(c("PM2p5", "pm25", "PM25"))
    var_co <- pick_col(c("CO", "co"))
    var_temp <- pick_col(c("Temperatura", "temp"))
    var_umid <- pick_col(c("UmidRel", "umid"))

    if (is.na(var_pm25) || is.na(var_temp) || is.na(var_umid)) {
      stop("Base sem colunas essenciais (PM2p5/Temperatura/UmidRel).")
    }

    base_norm <- base %>%
      mutate(Data = as.Date(Data)) %>%
      transmute(
        CodigoMunicipio,
        Data,
        NomeMunicipio = if (!is.na(var_nome)) as.character(.data[[var_nome]]) else NA_character_,
        Casos_Resp = suppressWarnings(as.integer(.data[[var_resp]])),
        pm25 = suppressWarnings(as.numeric(.data[[var_pm25]])),
        co = if (!is.na(var_co)) suppressWarnings(as.numeric(.data[[var_co]])) else NA_real_,
        temp = suppressWarnings(as.numeric(.data[[var_temp]])),
        umid = suppressWarnings(as.numeric(.data[[var_umid]]))
      )
    assign(key, base_norm, envir = .cache_modelagem)
  }

  data_ini <- as.Date(sprintf("%d-01-01", as.integer(ano1)))
  data_fim <- as.Date(sprintf("%d-12-31", as.integer(ano2)))

  out <- base_norm %>%
    filter(
      CodigoMunicipio == alvo$codigo[[1]],
      Data >= data_ini,
      Data <= data_fim
    ) %>%
    mutate(
      Ano = as.integer(format(Data, "%Y")),
      NomeMunicipio = dplyr::if_else(
        is.na(NomeMunicipio) | NomeMunicipio == "",
        alvo$rotulo[[1]],
        NomeMunicipio
      )
    ) %>%
    dplyr::select(Data, Ano, NomeMunicipio, Casos_Resp, pm25, co, temp, umid) %>%
    arrange(Data)

  if (nrow(out) == 0) stop("Sem dados para essa cidade e periodo.")
  out
}

anos_disponiveis_cidade <- function(chave) {
  cache_key <- paste(ic2025_data_mode(), chave, sep = "|")
  if (exists(cache_key, envir = .cache_anos, inherits = FALSE)) {
    return(get(cache_key, envir = .cache_anos, inherits = FALSE))
  }

  if (identical(ic2025_data_mode(), "agregada")) {
    backend <- ic2025_storage_backend()
    cat <- catalogo_cidades_tese()
    alvo <- cat %>% filter(chave == !!chave)
    if (nrow(alvo) == 0) return(integer())

    if (identical(backend, "duckdb")) {
      anos <- tryCatch({
        info <- duckdb_info_agregada()
        if (is.na(info$col_cod) || is.na(info$col_data)) {
          return(integer())
        }
        codigo_alvo <- as.integer(alvo$codigo[[1]])
        cod_cands <- unique(as.integer(c(
          codigo_alvo,
          codigo_alvo * 10L,
          codigo_alvo * 100L,
          codigo_alvo * 1000L
        )))
        cod_cands <- cod_cands[is.finite(cod_cands)]
        if (length(cod_cands) == 0) {
          return(integer())
        }

        con <- info$con
        q_tbl <- as.character(DBI::dbQuoteIdentifier(con, info$tabela))
        q_cod <- as.character(DBI::dbQuoteIdentifier(con, info$col_cod))
        q_data <- as.character(DBI::dbQuoteIdentifier(con, info$col_data))
        placeholders <- paste(rep("?", length(cod_cands)), collapse = ", ")
        sql <- paste0(
          "SELECT DISTINCT EXTRACT(YEAR FROM CAST(", q_data, " AS DATE)) AS ano ",
          "FROM ", q_tbl, " ",
          "WHERE CAST(", q_cod, " AS BIGINT) IN (", placeholders, ") ",
          "ORDER BY ano"
        )
        out <- DBI::dbGetQuery(con, sql, params = as.list(cod_cands))
        if (!is.data.frame(out) || nrow(out) == 0 || !("ano" %in% names(out))) {
          return(integer())
        }
        sort(unique(suppressWarnings(as.integer(out$ano))))
      }, error = function(e) integer())
      if (length(anos) == 0) {
        anos <- integer()
      }
    } else {
      arq <- ic2025_base_agregada_path()
      if (!file.exists(arq)) return(integer())

      map_key <- paste0("anos_agregado::", normalizePath(arq, winslash = "/", mustWork = FALSE))
      if (exists(map_key, envir = .cache_anos, inherits = FALSE)) {
        anos_map <- get(map_key, envir = .cache_anos, inherits = FALSE)
      } else {
        base <- carregar_base_rds(arq)
        if (is.null(base) || !all(c("CodigoMunicipio", "Data") %in% names(base))) return(integer())
        tab <- unique(data.frame(
          codigo = suppressWarnings(as.integer(base$CodigoMunicipio)),
          ano = suppressWarnings(as.integer(format(as.Date(base$Data), "%Y"))),
          stringsAsFactors = FALSE
        ))
        tab <- tab[is.finite(tab$codigo) & is.finite(tab$ano), , drop = FALSE]
        anos_map <- split(tab$ano, as.character(tab$codigo))
        anos_map <- lapply(anos_map, function(v) sort(unique(as.integer(v))))
        assign(map_key, anos_map, envir = .cache_anos)
      }
      anos <- anos_map[[as.character(as.integer(alvo$codigo[[1]]))]]
      if (is.null(anos)) anos <- integer()
    }

    assign(cache_key, anos, envir = .cache_anos)
    return(anos)
  }

  cat <- catalogo_cidades_tese()
  alvo <- cat %>% filter(chave == !!chave)
  if (nrow(alvo) == 0) return(integer())

  arq <- dplyr::case_when(
    alvo$base[[1]] == "sp" ~ "resultados_tese/Aplicações/São Paulo/base_cidades_sp.rds",
    alvo$base[[1]] == "rj" ~ "resultados_tese/Aplicações/Rio de Janeiro/base_cidades_rj.rds",
    TRUE ~ "resultados_tese/Aplicações/Amazônia Legal/base_cidades_amazonia_legal.rds"
  )
  if (!file.exists(arq)) return(integer())
  base <- ler_rds_cache(arq)
  if (!is.data.frame(base) || !all(c("CodigoMunicipio", "Data") %in% names(base))) return(integer())

  anos <- sort(unique(as.integer(format(as.Date(base$Data[base$CodigoMunicipio == alvo$codigo[[1]]]), "%Y"))))
  assign(cache_key, anos, envir = .cache_anos)
  anos
}

preset_modelagem_cidade <- function(chave) {
  norm_preset_key <- function(x) {
    x <- as.character(x %||% "")
    x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
    x <- tolower(trimws(x))
    x <- gsub("\\s*\\([a-z]{2}\\)\\s*$", "", x, perl = TRUE)
    x <- gsub("[^a-z0-9]+", "_", x, perl = TRUE)
    x <- gsub("^_+|_+$", "", x, perl = TRUE)
    x
  }

  presets <- list(
    # Fonte definitiva: scripts em
    # "Entrega de Arquivos Finais do Relatório/Aplicações em Dados Reais/Modelagem".
    # Para cidades com mais de um ano, usa o caso mais recente modelado no script.
    rio_de_janeiro = list(ano = 2024L, modelo = "clima", lags = 10L, d = 2L, fd = 0.98, lag_covar = 0L, covar = "umid", perc = 0.10, lado = "abaixo"),
    sao_paulo = list(ano = 2022L, modelo = "clima", lags = 10L, d = 3L, fd = 0.97, lag_covar = 0L, covar = "umid", perc = 0.10, lado = "abaixo"),
    belem = list(ano = 2022L, modelo = "clima", lags = 10L, d = 3L, fd = 0.98, lag_covar = 0L, covar = "temp", perc = 0.85, lado = "acima"),
    boa_vista = list(ano = 2020L, modelo = "pdldglm", lags = 9L, d = 2L, fd = 0.98, lag_covar = 0L, covar = "temp", perc = 0.85, lado = "acima"),
    cuiaba = list(ano = 2022L, modelo = "clima", lags = 9L, d = 2L, fd = 0.98, lag_covar = 0L, covar = "temp", perc = 0.85, lado = "acima"),
    macapa = list(ano = 2022L, modelo = "clima", lags = 8L, d = 3L, fd = 0.97, lag_covar = 0L, covar = "temp", perc = 0.90, lado = "acima"),
    manaus = list(ano = 2024L, modelo = "clima", lags = 12L, d = 3L, fd = 0.98, lag_covar = 2L, covar = "temp", perc = 0.85, lado = "acima"),
    porto_velho = list(ano = 2018L, modelo = "pdldglm", lags = 10L, d = 2L, fd = 0.97, lag_covar = 0L, covar = "temp", perc = 0.85, lado = "acima"),
    rio_branco = list(ano = 2019L, modelo = "clima", lags = 12L, d = 3L, fd = 0.98, lag_covar = 7L, covar = "temp", perc = 0.85, lado = "acima"),
    sao_luis = list(ano = 2015L, modelo = "clima", lags = 10L, d = 3L, fd = 0.99, lag_covar = 2L, covar = "umid", perc = 0.10, lado = "abaixo"),
    palmas = list(ano = 2024L, modelo = "pdldglm", lags = 8L, d = 2L, fd = 0.99, lag_covar = 0L, covar = "temp", perc = 0.85, lado = "acima")
  )

  aliases <- c(
    "330455" = "rio_de_janeiro",
    "355030" = "sao_paulo",
    "120040" = "rio_branco",
    "130260" = "manaus",
    "110020" = "porto_velho",
    "140010" = "boa_vista",
    "150140" = "belem",
    "160030" = "macapa",
    "510340" = "cuiaba",
    "172100" = "palmas",
    "211130" = "sao_luis"
  )

  chave0 <- as.character(chave %||% "")
  chave1 <- if (chave0 %in% names(aliases)) as.character(unname(aliases[chave0])) else NULL
  if (is.null(chave1)) {
    chave_norm0 <- norm_preset_key(chave0)
    chave1 <- if (chave_norm0 %in% names(aliases)) as.character(unname(aliases[chave_norm0])) else NULL
  }
  if (is.null(chave1)) {
    chave_norm <- norm_preset_key(chave0)
    preset_keys <- names(presets)
    preset_keys_norm <- setNames(preset_keys, vapply(preset_keys, norm_preset_key, character(1)))
    chave1 <- unname(preset_keys_norm[chave_norm])
    if (length(chave1) == 0L || !nzchar(chave1[[1]] %||% "")) chave1 <- NULL
  }
  if (is.null(chave1)) chave1 <- chave0

  p <- presets[[chave1]]
  if (is.null(p)) {
    p <- list(ano = NA_integer_, modelo = "clima", lags = 10L, d = 2L, fd = 0.98, lag_covar = 0L, covar = "temp", perc = 0.85, lado = "acima")
  }
  p
}

lag_modo_modelagem <- function(modo = c("suavizado", "filtrado", "one_step")) {
  modo <- match.arg(modo)
  switch(
    modo,
    suavizado = -1L,
    filtrado = 0L,
    one_step = 1L
  )
}

extrair_mu_ic_kdglm <- function(coefs_obj) {
  if (!is.null(coefs_obj$ft) && !is.null(coefs_obj$Qt)) {
    eta <- as.numeric(coefs_obj$ft[1, ])
    eta_sd <- sqrt(as.numeric(coefs_obj$Qt[1, 1, ]))
  } else if (!is.null(coefs_obj$lambda.mean) && !is.null(coefs_obj$lambda.cov)) {
    eta <- as.numeric(coefs_obj$lambda.mean[1, ])
    eta_sd <- sqrt(as.numeric(coefs_obj$lambda.cov[1, 1, ]))
  } else {
    stop("Objeto de coeficientes sem estrutura reconhecida para extracao de mu.")
  }

  tibble::tibble(
    mu = exp(eta),
    lo = exp(eta - 1.96 * eta_sd),
    hi = exp(eta + 1.96 * eta_sd)
  )
}

# Versao equivalente a DGLM_poisson_nivel_sim1 do script de simulacoes,
# retornando as estruturas usadas no dashboard (sem ggplot).
DGLM_poisson_nivel_sim1 <- function(
    y,
    deltas = c(0.90, 0.95, 0.99),
    pred_cred = 0.95,
    m0 = 0,
    C0 = 1000,
    lag_coef = 1L,
    ic_style = c("dashed", "ribbon", "none"),
    alpha_ic = 0.45,
    alpha_lines = 1.00,
    lw_main = 1.00,
    lw_ic = 0.90
) {
  ic_style <- match.arg(ic_style)
  Y <- as.numeric(y)
  if (any(!is.finite(Y)) || length(Y) < 2) stop("'y' invalido para DGLM.")
  if (!is.numeric(deltas) || any(!is.finite(deltas)) || any(deltas <= 0) || any(deltas > 1)) {
    stop("'deltas' deve estar em (0,1].")
  }
  if (!(is.numeric(pred_cred) && pred_cred > 0 && pred_cred < 1)) stop("'pred_cred' invalido.")
  if (!(is.numeric(m0) && length(m0) == 1 && is.finite(m0))) stop("'m0' invalido.")
  if (!(is.numeric(C0) && length(C0) == 1 && is.finite(C0) && C0 > 0)) stop("'C0' invalido.")

  z <- stats::qnorm(1 - (1 - pred_cred) / 2)
  t_idx <- seq_along(Y)
  pred_list <- vector("list", length(deltas))
  fits <- vector("list", length(deltas))

  for (i in seq_along(deltas)) {
    D_i <- as.numeric(deltas[[i]])
    nivel <- kDGLM::polynomial_block(rate = 1, order = 1, D = D_i, name = "Nivel", a1 = m0, R1 = C0)
    desfecho <- kDGLM::Poisson(lambda = "rate", data = Y)
    ajuste <- kDGLM::fit_model(nivel, y = desfecho)
    coefs <- stats::coef(ajuste, lag = as.integer(lag_coef), eval.pred = TRUE, eval.metric = TRUE, pred.cred = pred_cred)

    if (!is.null(coefs$ft) && !is.null(coefs$Qt)) {
      eta_hat <- as.numeric(coefs$ft[1, ])
      eta_sd <- sqrt(as.numeric(coefs$Qt[1, 1, ]))
    } else {
      eta_hat <- as.numeric(coefs$lambda.mean[1, ])
      eta_sd <- sqrt(as.numeric(coefs$lambda.cov[1, 1, ]))
    }

    mu_hat <- exp(eta_hat)
    mu_lo <- exp(eta_hat - z * eta_sd)
    mu_hi <- exp(eta_hat + z * eta_sd)

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
  list(
    Y = Y,
    pred_df = pred_df,
    fits = fits,
    params = list(alpha_lines = alpha_lines, lw_main = lw_main, lw_ic = lw_ic, alpha_ic = alpha_ic, ic_style = ic_style)
  )
}
