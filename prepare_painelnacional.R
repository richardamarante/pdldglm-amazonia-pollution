`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

pn_write_text <- function(path, text) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(text, con = con, useBytes = TRUE)
  invisible(normalizePath(path, winslash = "/", mustWork = FALSE))
}

pn_read_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

pn_detect_script_path <- function() {
  cmd <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", cmd, value = TRUE)
  if (length(hit) > 0) {
    return(normalizePath(sub("^--file=", "", hit[[1]]), winslash = "/", mustWork = FALSE))
  }
  for (frm in rev(sys.frames())) {
    ofile <- frm$ofile %||% ""
    if (nzchar(ofile)) {
      return(normalizePath(ofile, winslash = "/", mustWork = FALSE))
    }
  }
  ""
}

pn_detect_target_root <- function(target_root = NULL) {
  cands <- unique(c(
    as.character(target_root %||% ""),
    dirname(pn_detect_script_path()),
    getwd()
  ))
  for (cand in cands) {
    if (!nzchar(cand)) next
    root <- normalizePath(cand, winslash = "/", mustWork = FALSE)
    if (file.exists(file.path(root, "app.R")) &&
        file.exists(file.path(root, "funcoes.R")) &&
        dir.exists(file.path(root, "spatial_app"))) {
      return(root)
    }
  }
  stop("Nao consegui detectar a raiz do projeto painelnacional.")
}

pn_detect_source_root <- function(source_root = NULL) {
  cands <- unique(c(
    as.character(source_root %||% ""),
    Sys.getenv("IC2025_SOURCE_ROOT", unset = ""),
    "/home/richardmelo/coding/ic2025",
    "//wsl.localhost/Ubuntu-24.04/home/richardmelo/coding/ic2025"
  ))
  for (cand in cands) {
    if (!nzchar(cand)) next
    root <- normalizePath(cand, winslash = "/", mustWork = FALSE)
    if (file.exists(file.path(root, "app.R")) &&
        file.exists(file.path(root, "funcoes.R")) &&
        dir.exists(file.path(root, "cache_geo"))) {
      return(root)
    }
  }
  NULL
}

pn_find_function_bounds <- function(text, fun_name) {
  pat <- paste0(fun_name, "\\s*<-\\s*function\\s*\\(")
  hit <- regexpr(pat, text, perl = TRUE)
  if (hit[[1]] == -1L) {
    stop("Nao encontrei a funcao: ", fun_name)
  }
  start <- hit[[1]]
  tail_text <- substr(text, start, nchar(text))
  brace_rel <- regexpr("\\{", tail_text, perl = TRUE)
  if (brace_rel[[1]] == -1L) {
    stop("Nao encontrei a abertura da funcao: ", fun_name)
  }
  brace_start <- start + brace_rel[[1]] - 1L
  depth <- 0L
  end <- NA_integer_
  i <- brace_start
  n <- nchar(text)
  while (i <= n) {
    ch <- substr(text, i, i)
    if (identical(ch, "{")) {
      depth <- depth + 1L
    } else if (identical(ch, "}")) {
      depth <- depth - 1L
      if (depth == 0L) {
        end <- i
        break
      }
    }
    i <- i + 1L
  }
  if (!is.finite(end)) {
    stop("Nao consegui fechar a funcao: ", fun_name)
  }
  c(start = start, end = end)
}

pn_replace_function <- function(text, fun_name, replacement) {
  bounds <- pn_find_function_bounds(text, fun_name)
  paste0(
    substr(text, 1L, bounds[["start"]] - 1L),
    replacement,
    substr(text, bounds[["end"]] + 1L, nchar(text))
  )
}

pn_replace_between_patterns <- function(text, start_pattern, end_pattern, replacement) {
  start_hit <- regexpr(start_pattern, text, perl = TRUE)
  if (start_hit[[1]] == -1L) {
    stop("Nao encontrei o padrao inicial: ", start_pattern)
  }
  start_idx <- start_hit[[1]]
  tail_text <- substr(text, start_idx, nchar(text))
  end_hit <- regexpr(end_pattern, tail_text, perl = TRUE)
  if (end_hit[[1]] == -1L) {
    stop("Nao encontrei o padrao final: ", end_pattern)
  }
  end_idx <- start_idx + end_hit[[1]] - 2L
  paste0(
    substr(text, 1L, start_idx - 1L),
    replacement,
    substr(text, end_idx + 1L, nchar(text))
  )
}

pn_replace_assignment_block <- function(text, start_pattern, replacement) {
  hit <- regexpr(start_pattern, text, perl = TRUE)
  if (hit[[1]] == -1L) {
    stop("Nao encontrei o bloco: ", start_pattern)
  }
  start <- hit[[1]]
  tail_text <- substr(text, start, nchar(text))
  brace_rel <- regexpr("\\{", tail_text, perl = TRUE)
  if (brace_rel[[1]] == -1L) {
    stop("Nao encontrei a abertura do bloco: ", start_pattern)
  }
  brace_start <- start + brace_rel[[1]] - 1L
  depth <- 0L
  end <- NA_integer_
  i <- brace_start
  n <- nchar(text)
  while (i <= n) {
    ch <- substr(text, i, i)
    if (identical(ch, "{")) {
      depth <- depth + 1L
    } else if (identical(ch, "}")) {
      depth <- depth - 1L
      if (depth == 0L) {
        end <- i
        break
      }
    }
    i <- i + 1L
  }
  if (!is.finite(end)) {
    stop("Nao consegui fechar o bloco: ", start_pattern)
  }
  while (end < nchar(text) && substr(text, end + 1L, end + 1L) %in% c(")", " ", "\t")) {
    end <- end + 1L
  }
  paste0(
    substr(text, 1L, start - 1L),
    replacement,
    substr(text, end + 1L, nchar(text))
  )
}

pn_copy_file <- function(src_root, dst_root, rel_path) {
  src <- file.path(src_root, rel_path)
  dst <- file.path(dst_root, rel_path)
  if (!file.exists(src)) return(FALSE)
  dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(src, dst, overwrite = TRUE, copy.mode = TRUE)
  if (!isTRUE(ok)) {
    stop("Falha ao copiar arquivo: ", rel_path)
  }
  TRUE
}

pn_copy_tree <- function(src_dir, dst_dir) {
  if (!dir.exists(src_dir)) return(FALSE)
  rel_paths <- list.files(
    src_dir,
    recursive = TRUE,
    all.files = TRUE,
    include.dirs = TRUE,
    no.. = TRUE
  )
  rel_paths <- rel_paths[nzchar(rel_paths)]
  if (length(rel_paths) == 0L) {
    dir.create(dst_dir, recursive = TRUE, showWarnings = FALSE)
    return(TRUE)
  }
  keep <- !grepl("(^|/)(\\.git|\\.Rhistory|\\.DS_Store|desktop\\.ini)($|/)", rel_paths, perl = TRUE)
  rel_paths <- rel_paths[keep]
  unlink(dst_dir, recursive = TRUE, force = TRUE)
  dir.create(dst_dir, recursive = TRUE, showWarnings = FALSE)
  for (rel in sort(rel_paths)) {
    src <- file.path(src_dir, rel)
    dst <- file.path(dst_dir, rel)
    if (dir.exists(src)) {
      dir.create(dst, recursive = TRUE, showWarnings = FALSE)
    } else {
      dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
      ok <- file.copy(src, dst, overwrite = TRUE, copy.mode = TRUE)
      if (!isTRUE(ok)) {
        stop("Falha ao copiar item: ", normalizePath(src, winslash = "/", mustWork = FALSE))
      }
    }
  }
  TRUE
}

pn_sync_runtime_assets <- function(source_root, target_root) {
  if (is.null(source_root)) {
    message("Sem raiz de origem detectada; pulando sync de assets/vendors.")
    return(invisible(FALSE))
  }

  files_to_copy <- c(
    "www/custom-flat.css",
    "www/custom.css",
    "www/profile.css",
    "www/perfil_les.png",
    "www/profile.png",
    "www/profile_placeholder.png",
    "www/sidebar-skins.css",
    "www/sidebar-style-picker.css",
    "www/spatial/spatial-shell.css",
    "www/spatial/spatial-shell.js"
  )
  for (rel_path in files_to_copy) {
    pn_copy_file(source_root, target_root, rel_path)
  }

  pn_copy_tree(
    file.path(source_root, "vendor", "curbcut", "curbcut", "inst", "fonts"),
    file.path(target_root, "vendor", "curbcut", "curbcut", "inst", "fonts")
  )
  pn_copy_tree(
    file.path(source_root, "vendor", "curbcut", "cc.landing"),
    file.path(target_root, "vendor", "curbcut", "cc.landing")
  )
  pn_copy_tree(
    file.path(source_root, "vendor", "curbcut", "cc.map"),
    file.path(target_root, "vendor", "curbcut", "cc.map")
  )

  invisible(TRUE)
}

pn_patch_app_r <- function(target_root,
                           duckdb_url = "https://github.com/richardamarante/dados-painelnacional/releases/download/v1.0/base_final.duckdb",
                           duckdb_timeout = 3600L,
                           enable_spatial_compact_desktop = FALSE) {
  path <- file.path(target_root, "app.R")
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")

  drop_block <- function(lines, start_pat, end_pat) {
    start_idx <- grep(start_pat, lines, perl = TRUE)
    if (length(start_idx) == 0L) return(lines)
    start_idx <- start_idx[[1]]
    end_idx <- grep(end_pat, lines[seq.int(start_idx, length(lines))], perl = TRUE)
    if (length(end_idx) == 0L) return(lines)
    end_idx <- start_idx + end_idx[[1]] - 1L
    lines[-seq.int(start_idx, end_idx)]
  }

  insert_after <- function(lines, anchor_pat, new_lines) {
    idx <- grep(anchor_pat, lines, perl = TRUE)
    if (length(idx) == 0L) {
      stop("Nao encontrei ancora em app.R: ", anchor_pat)
    }
    idx <- idx[[1]]
    append(lines, values = new_lines, after = idx)
  }

  lines <- lines[!grepl("^DATA_SOURCE_AGGREGATED_DUCKDB_URL <- ", lines)]
  lines <- lines[!grepl("^DATA_SOURCE_AGGREGATED_DUCKDB_TIMEOUT <- ", lines)]
  lines <- lines[!grepl("^ENABLE_SPATIAL_COMPACT_DESKTOP <- ", lines)]
  lines <- lines[!grepl("^\\s*ic2025\\.agregada_duckdb_url = ", lines)]
  lines <- lines[!grepl("^\\s*ic2025\\.agregada_duckdb_timeout = ", lines)]
  lines <- drop_block(lines, "^\\s*if \\(isTRUE\\(ENABLE_SPATIAL_COMPACT_DESKTOP\\)\\) \\{$", "^\\s*\\},?$")

  lines <- insert_after(
    lines,
    "^DATA_SOURCE_AGGREGATED_DUCKDB <- ",
    c(
      sprintf(
        'DATA_SOURCE_AGGREGATED_DUCKDB_URL <- "%s"',
        gsub('(["\\\\])', '\\\\\\1', duckdb_url, perl = TRUE)
      ),
      paste0("DATA_SOURCE_AGGREGATED_DUCKDB_TIMEOUT <- ", as.integer(duckdb_timeout), "L")
    )
  )
  lines <- insert_after(
    lines,
    "^DATA_STORAGE_BACKEND <- ",
    paste0("ENABLE_SPATIAL_COMPACT_DESKTOP <- ", if (isTRUE(enable_spatial_compact_desktop)) "TRUE" else "FALSE")
  )
  lines <- insert_after(
    lines,
    "^\\s*ic2025\\.agregada_duckdb = DATA_SOURCE_AGGREGATED_DUCKDB,",
    c(
      "  ic2025.agregada_duckdb_url = DATA_SOURCE_AGGREGATED_DUCKDB_URL,",
      "  ic2025.agregada_duckdb_timeout = DATA_SOURCE_AGGREGATED_DUCKDB_TIMEOUT,"
    )
  )
  lines <- insert_after(
    lines,
    'href = asset_href\\("spatial/spatial-shell\\.css"\\)\\),$',
    c(
      "      if (isTRUE(ENABLE_SPATIAL_COMPACT_DESKTOP)) {",
      '        tags$link(rel = "stylesheet", type = "text/css", href = asset_href("spatial/spatial-shell-compact.css"))',
      "      },"
    )
  )

  pn_write_text(path, paste(lines, collapse = "\n"))
  invisible(path)
}

pn_funcoes_block <- function() {
  r"---(
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
)---"
}

pn_patch_funcoes_r <- function(target_root) {
  path <- file.path(target_root, "funcoes.R")
  text <- pn_read_text(path)
  start_pat <- "ic2025_base_agregada_path\\s*<-\\s*function\\s*\\("
  end_pat <- "\nic2025_base_agregada_duckdb_table\\s*<-\\s*function\\s*\\(\\)\\s*\\{"
  text <- pn_replace_between_patterns(text, start_pat, end_pat, pn_funcoes_block())
  pn_write_text(path, text)
  invisible(path)
}

pn_spatial_detect_project_root_fun <- function() {
  r"---(
spatial_detect_project_root <- function() {
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
    if ((file.exists(file.path(root, "app.R")) || file.exists(file.path(root, "funcoes.R"))) &&
        dir.exists(file.path(root, "cache_geo"))) {
      return(root)
    }
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}
)---"
}

pn_spatial_ensure_vendor_package_fun <- function() {
  r"---(
spatial_ensure_vendor_package <- function(package, pkg_dir, project_root = spatial_detect_project_root()) {
  spatial_activate_vendor_library(project_root = project_root)
  if (requireNamespace(package, quietly = TRUE)) {
    return(TRUE)
  }
  if (!dir.exists(pkg_dir)) {
    return(FALSE)
  }

  lib <- spatial_vendor_library_path(project_root = project_root)
  imports <- spatial_package_imports(pkg_dir)
  missing_imports <- imports[!vapply(imports, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing_imports) > 0) {
    try(
      utils::install.packages(
        missing_imports,
        lib = lib,
        repos = spatial_default_repos(),
        quiet = TRUE
      ),
      silent = TRUE
    )
  }

  try(
    utils::install.packages(
      pkg_dir,
      lib = lib,
      repos = NULL,
      type = "source",
      quiet = TRUE
    ),
    silent = TRUE
  )

  requireNamespace(package, quietly = TRUE)
}
)---"
}

pn_spatial_theme_drop_block <- function() {
  r"---(
output$theme_drop_ui <- shiny::renderUI({
      fallback_ui <- shiny::div(
        class = "ic2025-spatial-theme-fallback",
        shiny::selectInput(
          ns("theme_group_fallback"),
          NULL,
          choices = stats::setNames(unique(var_catalog$group), unique(var_catalog$group)),
          selected = current_group(),
          selectize = FALSE
        )
      )
      if (!isTRUE(cc_landing_ready)) {
        return(fallback_ui)
      }
      theme_drop_input <- tryCatch(
        get("theme_drop_input", envir = getNamespace(paste0("cc", ".landing")), inherits = FALSE),
        error = function(e) NULL
      )
      if (is.null(theme_drop_input)) {
        return(fallback_ui)
      }
      tryCatch(
        theme_drop_input(
          inputId = ns("theme_drop"),
          pages = theme_pages,
          width = "250px",
          theme = spatial_theme_internal_label(current_group()),
          home_str = "Voltar",
          translation_df = theme_translation_df,
          lang = "fr"
        ),
        error = function(e) {
          fallback_ui
        }
      )
    })
)---"
}

pn_patch_spatial_module_r <- function(target_root) {
  path <- file.path(target_root, "spatial_app", "spatial_module.R")
  text <- pn_read_text(path)
  text <- pn_replace_function(text, "spatial_detect_project_root", pn_spatial_detect_project_root_fun())
  text <- pn_replace_function(text, "spatial_ensure_vendor_package", pn_spatial_ensure_vendor_package_fun())
  text <- pn_replace_between_patterns(
    text,
    "cc_landing_ready <- isTRUE\\(spatial_ensure_curbcut_packages\\(project_root = project_root\\)\\)\n",
    "spatial_register_geo_resources\\(project_root = project_root\\)",
    r"---(
cc_landing_ready <- isTRUE(spatial_ensure_curbcut_packages(project_root = project_root))
    db_path <- if (exists("ic2025_base_agregada_duckdb_path", mode = "function")) {
      ic2025_base_agregada_duckdb_path()
    } else {
      file.path(project_root, "base_final.duckdb")
    }
    spatial_register_geo_resources(project_root = project_root)
)---"
  )
  text <- pn_replace_assignment_block(
    text,
    "output\\$theme_drop_ui\\s*<-\\s*shiny::renderUI\\s*\\(",
    pn_spatial_theme_drop_block()
  )
  pn_write_text(path, text)
  invisible(path)
}

pn_compact_css <- function() {
  r"---(
@media (min-width: 1081px) and (max-height: 820px), (min-width: 1081px) and (max-width: 1450px) {
  .ic2025-spatial-app {
    --sp-left-w: 282px;
    --sp-right-w: 274px;
  }

  .ic2025-spatial-toolbar {
    top: 6px;
    left: 12px;
    gap: 8px;
  }

  .ic2025-spatial-toolbar-left {
    gap: 6px;
  }

  .ic2025-spatial-theme-drop,
  .ic2025-spatial-theme-fallback {
    min-width: 248px;
  }

  .ic2025-spatial-theme-fallback .form-control {
    height: 40px;
    font-size: 16px;
    border-radius: 14px;
  }

  .ic2025-spatial-icon-btn.btn.btn-default {
    width: 32px;
    height: 32px;
    font-size: 14px;
  }

  .ic2025-spatial-sidebar {
    top: 52px;
    left: 12px;
    max-height: calc(100vh - 64px);
  }

  .ic2025-spatial-rightpanel {
    top: 12px;
    right: 12px;
    max-height: calc(100vh - 24px);
  }

  .ic2025-spatial-sidebar-inner {
    max-height: calc(100vh - 64px);
  }

  .ic2025-spatial-sidebar-scroll,
  .ic2025-spatial-rightpanel-scroll {
    padding: 12px;
  }

  .ic2025-spatial-sidebar-title {
    font-size: 29px;
  }

  .ic2025-spatial-sidebar-title-row {
    margin-bottom: 12px;
  }

  .ic2025-spatial-section {
    padding-top: 10px;
    margin-top: 10px;
  }

  .ic2025-spatial-section-head {
    margin-bottom: 8px;
    font-size: 13px;
  }

  .ic2025-spatial-section label,
  .ic2025-spatial-section .control-label {
    margin-bottom: 5px;
    font-size: 14px;
  }

  .ic2025-spatial-section .form-group {
    margin-bottom: 10px;
  }

  .ic2025-spatial-section input.form-control,
  .ic2025-spatial-section select.form-control,
  .ic2025-spatial-section textarea.form-control,
  .ic2025-spatial-section .selectize-control.single .selectize-input {
    font-size: 13px;
    padding: 5px 10px;
  }

  .ic2025-spatial-section .selectize-control.single .selectize-input {
    min-height: 40px;
  }

  .ic2025-spatial-time-card,
  .ic2025-spatial-scale-card {
    gap: 8px;
  }

  .ic2025-spatial-time-range-status,
  .ic2025-spatial-mini-label {
    font-size: 10px;
  }

  .ic2025-spatial-time-range .irs--shiny {
    height: 46px;
  }

  .ic2025-spatial-time-axis {
    font-size: 10px;
  }

  .ic2025-spatial-note {
    margin-top: 6px;
    padding: 9px 11px;
    font-size: 13px;
    border-radius: 12px;
  }

  .ic2025-spatial-legend-wrap {
    margin-top: 10px;
    padding-top: 10px;
  }

  .ic2025-spatial-timeline-shell {
    top: 12px;
    left: calc(var(--sp-left-w) + 30px);
    right: calc(var(--sp-right-w) + 30px);
  }

  .ic2025-spatial-timeline-card {
    min-width: 390px;
    max-width: min(680px, 100%);
    padding: 7px 12px 6px;
  }

  .ic2025-spatial-timeline-head {
    gap: 10px;
  }

  .ic2025-spatial-timeline-toggle label {
    gap: 6px;
    font-size: 12px;
  }

  .ic2025-spatial-timeline-value {
    font-size: 15px;
  }

  .ic2025-spatial-timeline-card .irs--shiny {
    height: 38px;
  }

  .ic2025-spatial-timeline-axis,
  .ic2025-spatial-timeline-edge-label {
    font-size: 10px;
  }

  .ic2025-spatial-timeline-footer {
    margin-top: 6px;
    padding: 0 6px;
  }

  .ic2025-spatial-player-btn.btn.btn-default {
    width: 28px;
    height: 28px;
    min-width: 28px;
  }

  .ic2025-spatial-player-btn-main.btn.btn-default {
    width: 32px;
    height: 32px;
    min-width: 32px;
  }

  .ic2025-spatial-table-pane,
  .ic2025-spatial-portrait {
    top: 78px;
    left: 320px;
    right: 304px;
    bottom: 76px;
  }

  .ic2025-spatial-table-pane {
    padding: 14px;
    gap: 10px;
  }

  .ic2025-spatial-portrait {
    padding: 14px 16px 10px;
  }

  .ic2025-spatial-rightpanel-title {
    font-size: 25px;
    margin-bottom: 8px;
  }

  .ic2025-spatial-rightpanel-title-small {
    font-size: 20px;
    margin-top: 6px;
  }

  .ic2025-spatial-rightpanel-body {
    font-size: 15px;
    line-height: 1.38;
  }

  .ic2025-spatial-rightpanel-body p {
    margin-bottom: 10px;
  }

  .ic2025-spatial-view-btn.btn.btn-default {
    height: 44px;
    padding: 0 1.6rem;
  }

  .ic2025-spatial-main .mapboxgl-ctrl-bottom-right {
    bottom: 68px;
    right: 12px;
  }
}
)---"
}

pn_write_compact_css <- function(target_root) {
  path <- file.path(target_root, "www", "spatial", "spatial-shell-compact.css")
  pn_write_text(path, pn_compact_css())
  invisible(path)
}

prepare_painelnacional <- function(target_root = NULL,
                                   source_root = NULL,
                                   sync_runtime_assets = TRUE,
                                   duckdb_url = "https://github.com/richardamarante/dados-painelnacional/releases/download/v1.0/base_final.duckdb",
                                   duckdb_timeout = 3600L,
                                   enable_spatial_compact_desktop = FALSE) {
  target_root <- pn_detect_target_root(target_root)
  source_root <- pn_detect_source_root(source_root)

  message("Target root: ", target_root)
  if (!is.null(source_root)) {
    message("Source root: ", source_root)
  }

  if (isTRUE(sync_runtime_assets)) {
    pn_sync_runtime_assets(source_root, target_root)
  }

  pn_patch_app_r(
    target_root = target_root,
    duckdb_url = duckdb_url,
    duckdb_timeout = duckdb_timeout,
    enable_spatial_compact_desktop = enable_spatial_compact_desktop
  )
  pn_patch_funcoes_r(target_root = target_root)
  pn_patch_spatial_module_r(target_root = target_root)
  pn_write_compact_css(target_root = target_root)

  invisible(list(
    target_root = target_root,
    source_root = source_root,
    duckdb_url = duckdb_url,
    duckdb_timeout = as.integer(duckdb_timeout),
    enable_spatial_compact_desktop = isTRUE(enable_spatial_compact_desktop)
  ))
}
