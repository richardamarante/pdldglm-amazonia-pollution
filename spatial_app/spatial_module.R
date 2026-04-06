spatial_coalesce <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

if (!exists("ic2025_theme_value", mode = "function")) {
  ic2025_theme_value <- function(key, default = getOption("ic2025.theme.colors", default = list())[[as.character(key)]]) {
    vals <- getOption("ic2025.theme.colors", default = list())
    value <- vals[[as.character(key)]]
    if (is.null(value) || !nzchar(value)) default else value
  }
}

SPATIAL_ALL_REGION <- "__BRASIL__"
SPATIAL_ALL_STATE <- "__ALL_UF__"
SPATIAL_ALL_CITY <- "__ALL_CITY__"
SPATIAL_ZOOM_THRESHOLD_IN <- 6.9
SPATIAL_ZOOM_THRESHOLD_OUT <- 6.7
SPATIAL_TABLE_PAGE_LENGTH <- 15L
SPATIAL_TABLE_MAX_PAGES <- 15L
SPATIAL_ACTIVE_REFRESH_MS <- 120L
SPATIAL_ACTIVE_SERIES_REFRESH_MS <- 150L
SPATIAL_BACKGROUND_REFRESH_MS <- 900L

.spatial_cache <- local({
  env <- new.env(parent = emptyenv())
  env$geoms <- new.env(parent = emptyenv())
  env$geom_index <- new.env(parent = emptyenv())
  env$meta <- new.env(parent = emptyenv())
  env$geojson <- new.env(parent = emptyenv())
  env$geojson_order <- character(0)
  env
})

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

spatial_default_mapbox_token <- function() {
  tok <- Sys.getenv("IC2025_MAPBOX_TOKEN", unset = "")
  if (!nzchar(tok)) tok <- Sys.getenv("MAPBOX_TOKEN", unset = "")
  if (!nzchar(tok)) {
    tok <- paste0(
      "pk.eyJ1IjoiY3VyYmN1dCIsImEiOiJjbGprYnVwOTQwaDAzM2xwaWdjbTB6bzdlIn0.",
      "Ks1cOI6v2i8jiIjk38s_kg"
    )
  }
  tok
}

spatial_default_mapbox_username <- function() {
  out <- Sys.getenv("IC2025_MAPBOX_USERNAME", unset = "")
  if (!nzchar(out)) out <- "curbcut"
  out
}

spatial_default_mapbox_style_id <- function() {
  out <- Sys.getenv("IC2025_MAPBOX_STYLE_ID", unset = "")
  if (!nzchar(out)) out <- "cljkciic3002h01qveq5z1wrp"
  out
}

spatial_var_catalog <- function() {
  tibble::tribble(
    ~group, ~code, ~label, ~agg, ~digits, ~suffix, ~available_end, ~summary_noun,
    "Clima", "Temperatura", "Temperatura Média", "avg", 1L, " °C", as.Date("2025-08-31"), "média",
    "Clima", "TemperaturaMin", "Temperatura Mínima", "avg", 1L, " °C", as.Date("2025-12-31"), "média",
    "Clima", "TemperaturaMax", "Temperatura Máxima", "avg", 1L, " °C", as.Date("2025-12-31"), "média",
    "Clima", "UmidRel", "Umidade Relativa", "avg", 1L, " %", as.Date("2025-08-31"), "média",
    "Clima", "VelocVento", "Velocidade do Vento", "avg", 1L, " km/h", as.Date("2025-08-31"), "média",
    "Clima", "SensTermica", "Sensação Térmica", "avg", 1L, " °C", as.Date("2025-08-31"), "média",
    "Qualidade do Ar", "PM2p5", "PM2.5", "avg", 1L, " µg/m³", as.Date("2025-08-31"), "média",
    "Qualidade do Ar", "PM10", "PM10", "avg", 1L, " µg/m³", as.Date("2025-08-31"), "média",
    "Qualidade do Ar", "CO", "CO", "avg", 2L, " ppm", as.Date("2025-08-31"), "média",
    "Saúde", "Internacoes_Resp", "Internações Respiratórias", "sum", 0L, "", as.Date("2025-12-31"), "total",
    "Saúde", "Internacoes_Circ", "Internações Circulatórias", "sum", 0L, "", as.Date("2025-12-31"), "total",
    "Saúde", "Obitos_Resp", "Óbitos Respiratórios", "sum", 0L, "", as.Date("2025-12-31"), "total",
    "Saúde", "Obitos_Circ", "Óbitos Circulatórios", "sum", 0L, "", as.Date("2025-12-31"), "total"
  )
}

spatial_var_info <- function(code) {
  tab <- spatial_var_catalog()
  hit <- tab[tab$code %in% as.character(code %||% ""), , drop = FALSE]
  if (nrow(hit) == 0) {
    stop("Variável espacial não reconhecida: ", as.character(code))
  }
  hit[1, , drop = FALSE]
}

spatial_scalar_string <- function(x, fallback = "") {
  val <- as.character(x %||% fallback)
  val <- val[!is.na(val)]
  if (length(val) == 0) return(as.character(fallback))
  val[[1]]
}

spatial_vars_for_group <- function(group, catalog = spatial_var_catalog()) {
  grp <- spatial_scalar_string(group, unique(catalog$group)[[1]])
  vars <- catalog[catalog$group %in% grp, , drop = FALSE]
  if (nrow(vars) == 0) {
    vars <- catalog[1, , drop = FALSE]
  }
  vars
}

spatial_resolve_group <- function(group, catalog = spatial_var_catalog()) {
  groups <- unique(catalog$group)
  grp <- spatial_scalar_string(group, groups[[1]])
  if (!grp %in% groups) groups[[1]] else grp
}

spatial_resolve_variable <- function(code, group, catalog = spatial_var_catalog()) {
  vars <- spatial_vars_for_group(group, catalog = catalog)
  selected <- spatial_scalar_string(code, vars$code[[1]])
  if (!selected %in% vars$code) vars$code[[1]] else selected
}

spatial_theme_pages <- function(catalog = spatial_var_catalog()) {
  theme_lookup <- c(
    "Clima" = "Climate",
    "Qualidade do Ar" = "Ecology",
    "Saúde" = "Health"
  )
  theme_vals <- unname(theme_lookup[as.character(catalog$group)])
  theme_vals[is.na(theme_vals) | !nzchar(theme_vals)] <- as.character(catalog$group)[is.na(theme_vals) | !nzchar(theme_vals)]
  data.frame(
    id = as.character(catalog$code),
    theme = theme_vals,
    nav_title = as.character(catalog$label),
    stringsAsFactors = FALSE
  )
}

spatial_theme_translation_df <- function(catalog = spatial_var_catalog(), home_str = "Voltar") {
  rows <- data.frame(
    en = c("Climate", "Ecology", "Health", as.character(catalog$label), home_str),
    fr = c("Clima", "Qualidade do Ar", "Saúde", as.character(catalog$label), home_str),
    stringsAsFactors = FALSE
  )
  rows[!duplicated(rows$en), , drop = FALSE]
}

spatial_theme_internal_label <- function(group) {
  theme_lookup <- c(
    "Clima" = "Climate",
    "Qualidade do Ar" = "Ecology",
    "Saúde" = "Health"
  )
  out <- unname(theme_lookup[as.character(group)])
  if (is.na(out) || !nzchar(out)) as.character(group) else out
}

spatial_section_label <- function(icon_name, text) {
  htmltools::tags$div(
    class = "ic2025-spatial-section-head",
    shiny::icon(icon_name, class = "ic2025-spatial-section-icon"),
    htmltools::tags$span(text)
  )
}

spatial_resolve_compare_var <- function(code, primary_code = NULL, catalog = spatial_var_catalog()) {
  valid <- c("__NONE__", as.character(catalog$code))
  selected <- spatial_scalar_string(code, "__NONE__")
  if (!selected %in% valid || identical(selected, primary_code)) {
    return("__NONE__")
  }
  selected
}

spatial_resolve_period <- function(period, default = c(as.Date("2024-01-01"), as.Date("2024-12-31"))) {
  vals <- as.Date(period)
  if (length(vals) < 2 || any(is.na(vals[1:2]))) {
    return(as.Date(default))
  }
  as.Date(vals[1:2])
}

spatial_format_value <- function(code, x) {
  info <- spatial_var_info(code)
  val <- suppressWarnings(as.numeric(x))
  out <- rep("Sem dado", length(val))
  ok <- is.finite(val)
  if (any(ok)) {
    base <- format(
      round(val[ok], digits = info$digits[[1]]),
      nsmall = info$digits[[1]],
      decimal.mark = ",",
      big.mark = "."
    )
    out[ok] <- paste0(base, info$suffix[[1]])
  }
  out
}

spatial_format_break_label <- function(code, x, include_suffix = FALSE, digits = NULL) {
  info <- spatial_var_info(code)
  val <- suppressWarnings(as.numeric(x))
  out <- rep("", length(val))
  ok <- is.finite(val)
  digits_use <- suppressWarnings(as.integer(digits %||% info$digits[[1]]))
  if (length(digits_use) == 0L) {
    digits_use <- rep(as.integer(info$digits[[1]]), length(val))
  } else {
    digits_use <- rep_len(digits_use, length(val))
  }
  digits_use[!is.finite(digits_use)] <- info$digits[[1]]
  digits_use <- pmax(0L, digits_use)
  if (any(ok)) {
    base <- vapply(which(ok), function(i) {
      format(
        round(val[[i]], digits = digits_use[[i]]),
        nsmall = digits_use[[i]],
        decimal.mark = ",",
        big.mark = "."
      )
    }, character(1))
    out[ok] <- if (isTRUE(include_suffix)) paste0(base, info$suffix[[1]]) else base
  }
  out
}

spatial_legend_label_digits <- function(x, fallback = 1L, max_digits = 3L) {
  vals <- suppressWarnings(as.numeric(x))
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) {
    return(max(0L, as.integer(fallback %||% 0L)))
  }
  vals_unique <- sort(unique(vals))
  if (length(vals_unique) <= 1L || all(abs(vals_unique - round(vals_unique)) < 1e-9)) {
    return(0L)
  }

  max_abs <- max(abs(vals_unique), na.rm = TRUE)
  min_unique_digits <- max_digits
  for (digits in 0:max_digits) {
    rounded <- round(vals_unique, digits = digits)
    if (length(unique(rounded)) == length(vals_unique)) {
      min_unique_digits <- digits
      break
    }
  }

  if (max_abs >= 1000 || max_abs >= 100) {
    return(0L)
  }
  if (max_abs >= 10) {
    return(if (min_unique_digits == 0L) 0L else 1L)
  }
  if (max_abs >= 1) {
    return(min(min_unique_digits, 2L))
  }
  min(min_unique_digits, max_digits)
}

spatial_format_bin_label <- function(code, left, right, digits = NULL) {
  base_digits <- max(0L, as.integer(digits %||% 0L))
  attempts <- unique(c(
    if (max(abs(c(left, right)), na.rm = TRUE) >= 1) 0L else integer(0),
    base_digits,
    seq.int(from = max(0L, base_digits), to = 3L)
  ))

  for (digits_try in attempts) {
    left_txt <- spatial_format_break_label(code, left, include_suffix = FALSE, digits = digits_try)[[1]]
    right_txt <- spatial_format_break_label(code, right, include_suffix = FALSE, digits = digits_try)[[1]]
    if (!nzchar(left_txt) && !nzchar(right_txt)) {
      return("")
    }
    if (!identical(left_txt, right_txt)) {
      return(paste0(left_txt, "–", right_txt))
    }
  }

  left_txt <- spatial_format_break_label(code, left, include_suffix = FALSE, digits = base_digits)[[1]]
  right_txt <- spatial_format_break_label(code, right, include_suffix = FALSE, digits = base_digits)[[1]]
  if (!nzchar(left_txt) && !nzchar(right_txt)) {
    return("")
  }
  if (identical(left_txt, right_txt)) {
    return(right_txt)
  }
  paste0(left_txt, "–", right_txt)
}

spatial_metric_label <- function(code) {
  info <- spatial_var_info(code)
  paste0(info$summary_noun[[1]], " de ", tolower(info$label[[1]]))
}

spatial_metric_phrase <- function(code, snapshot = FALSE) {
  info <- spatial_var_info(code)
  if (isTRUE(snapshot)) {
    tolower(info$label[[1]])
  } else {
    paste0(info$summary_noun[[1]], " de ", tolower(info$label[[1]]))
  }
}

spatial_format_date_pt <- function(x) {
  out <- tryCatch(as.Date(x), error = function(e) as.Date(NA))
  if (length(out) == 0 || is.na(out[[1]])) return("")
  format(out[[1]], "%d/%m/%Y")
}

spatial_format_period_label <- function(x, frequency = c("D", "W", "M")) {
  frequency <- match.arg(frequency)
  out <- tryCatch(as.Date(x), error = function(e) as.Date(NA))
  if (length(out) == 0) return(character(0))
  months_pt <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
  labels <- rep("", length(out))
  ok <- !is.na(out)
  if (!any(ok)) {
    return(labels)
  }
  labels[ok] <- switch(
    frequency,
    D = vapply(out[ok], spatial_format_date_pt, character(1)),
    W = paste0("Semana de ", vapply(out[ok], spatial_format_date_pt, character(1))),
    M = paste0(
      months_pt[as.integer(format(out[ok], "%m"))],
      "/",
      format(out[ok], "%Y")
    )
  )
  labels
}

spatial_level_label <- function(level) {
  switch(
    as.character(level),
    state = "UF",
    municipio = "município",
    "localidade"
  )
}

spatial_remove_holes <- function(sf_obj) {
  geoms <- sf::st_geometry(sf_obj)
  cleaned <- sf::st_sfc(
    lapply(geoms, function(geom) {
      if (inherits(geom, "POLYGON")) {
        return(sf::st_polygon(list(geom[[1]])))
      }
      if (inherits(geom, "MULTIPOLYGON")) {
        return(sf::st_multipolygon(lapply(geom, function(poly) list(poly[[1]]))))
      }
      geom
    }),
    crs = sf::st_crs(sf_obj)
  )
  sf::st_geometry(sf_obj) <- cleaned
  sf_obj
}

spatial_get_geometry_layer <- function(level = c("state", "municipio"), project_root = spatial_detect_project_root()) {
  level <- match.arg(level)
  key <- paste(normalizePath(project_root, winslash = "/", mustWork = FALSE), level, sep = "|")
  if (exists(key, envir = .spatial_cache$geoms, inherits = FALSE)) {
    return(get(key, envir = .spatial_cache$geoms, inherits = FALSE))
  }

  layer <- if (identical(level, "state")) {
    sf::st_read(file.path(project_root, "cache_geo", "mapa_brasil_estados_v3.geojson"), quiet = TRUE)
  } else {
    sf::st_read(file.path(project_root, "cache_geo", "mapa_brasil_municipios_v3.geojson"), quiet = TRUE)
  }
  layer <- sf::st_transform(layer, 4326)
  layer <- sf::st_make_valid(layer)
  if (identical(level, "state")) {
    layer <- spatial_remove_holes(layer)
  }
  if ("abbrev_state" %in% names(layer)) {
    layer$abbrev_state <- trimws(as.character(layer$abbrev_state))
  }
  if ("code_muni" %in% names(layer)) {
    layer$code_muni <- trimws(as.character(layer$code_muni))
  }

  assign(key, layer, envir = .spatial_cache$geoms)
  layer
}

spatial_geometry_index <- function(level = c("state", "municipio"), project_root = spatial_detect_project_root()) {
  level <- match.arg(level)
  key <- paste(normalizePath(project_root, winslash = "/", mustWork = FALSE), level, sep = "|")
  if (exists(key, envir = .spatial_cache$geom_index, inherits = FALSE)) {
    return(get(key, envir = .spatial_cache$geom_index, inherits = FALSE))
  }

  layer <- spatial_get_geometry_layer(level, project_root = project_root)
  key_col <- if (identical(level, "state")) "abbrev_state" else "code_muni"
  aux_col <- if (identical(level, "state")) NULL else "abbrev_state"
  geoms <- sf::st_geometry(layer)
  bbox_mat <- t(vapply(seq_along(geoms), function(i) {
    bbox <- sf::st_bbox(geoms[[i]])
    c(
      xmin = unname(as.numeric(bbox[["xmin"]])),
      ymin = unname(as.numeric(bbox[["ymin"]])),
      xmax = unname(as.numeric(bbox[["xmax"]])),
      ymax = unname(as.numeric(bbox[["ymax"]]))
    )
  }, numeric(4)))
  index <- data.frame(
    key = trimws(as.character(layer[[key_col]])),
    xmin = bbox_mat[, "xmin"],
    ymin = bbox_mat[, "ymin"],
    xmax = bbox_mat[, "xmax"],
    ymax = bbox_mat[, "ymax"],
    stringsAsFactors = FALSE
  )
  if (!is.null(aux_col) && aux_col %in% names(layer)) {
    index$aux <- trimws(as.character(layer[[aux_col]]))
  }
  assign(key, index, envir = .spatial_cache$geom_index)
  index
}

spatial_normalize_keys <- function(keys) {
  keys <- trimws(as.character(keys %||% character(0)))
  unique(keys[!is.na(keys) & nzchar(keys)])
}

spatial_filter_map_data_keys <- function(dat, keys) {
  if (!is.data.frame(dat)) {
    return(tibble::tibble())
  }
  keys <- spatial_normalize_keys(keys)
  if (length(keys) == 0) {
    return(dat[0, , drop = FALSE])
  }
  if (!"key" %in% names(dat)) {
    return(dat[0, , drop = FALSE])
  }
  dat[trimws(as.character(dat$key)) %in% keys, , drop = FALSE]
}

spatial_keys_in_bounds <- function(level = c("state", "municipio"), bounds, project_root = spatial_detect_project_root(), keys = NULL) {
  level <- match.arg(level)
  allowed_keys <- spatial_normalize_keys(keys)
  if (is.null(bounds)) {
    return(allowed_keys)
  }
  index <- spatial_geometry_index(level, project_root = project_root)
  if (nrow(index) == 0) {
    return(character(0))
  }
  keep <- index$xmax >= as.numeric(bounds$west) &
    index$xmin <= as.numeric(bounds$east) &
    index$ymax >= as.numeric(bounds$south) &
    index$ymin <= as.numeric(bounds$north)
  if (length(allowed_keys) > 0) {
    keep <- keep & index$key %in% allowed_keys
  }
  unique(index$key[keep])
}

spatial_get_geometries <- function(project_root = spatial_detect_project_root()) {
  list(
    estados = spatial_get_geometry_layer("state", project_root = project_root),
    municipios = spatial_get_geometry_layer("municipio", project_root = project_root)
  )
}

spatial_query_metadata <- function(con, table_name, project_root = spatial_detect_project_root()) {
  key <- normalizePath(project_root, winslash = "/", mustWork = FALSE)
  if (exists(key, envir = .spatial_cache$meta, inherits = FALSE)) {
    return(get(key, envir = .spatial_cache$meta, inherits = FALSE))
  }

  q_tbl <- as.character(DBI::dbQuoteIdentifier(con, table_name))
  estados <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT DISTINCT TRIM(CAST(RegiaoMunicipio AS VARCHAR)) AS regiao, ",
      "TRIM(CAST(UFMunicipio AS VARCHAR)) AS uf, ",
      "TRIM(CAST(EstadoMunicipio AS VARCHAR)) AS estado ",
      "FROM ", q_tbl, " ",
      "WHERE UFMunicipio IS NOT NULL AND EstadoMunicipio IS NOT NULL ",
      "ORDER BY regiao, estado"
    )
  )
  cidades <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT DISTINCT TRIM(CAST(CodigoMunicipio6 AS VARCHAR)) AS code_muni, ",
      "TRIM(CAST(NomeMunicipio AS VARCHAR)) AS nome, ",
      "TRIM(CAST(UFMunicipio AS VARCHAR)) AS uf, ",
      "TRIM(CAST(EstadoMunicipio AS VARCHAR)) AS estado, ",
      "TRIM(CAST(RegiaoMunicipio AS VARCHAR)) AS regiao ",
      "FROM ", q_tbl, " ",
      "WHERE CodigoMunicipio6 IS NOT NULL AND NomeMunicipio IS NOT NULL ",
      "ORDER BY uf, nome, code_muni"
    )
  )

  estados$rotulo <- sprintf("%s (%s)", estados$estado, estados$uf)
  cidades$rotulo <- sprintf("%s/%s", cidades$nome, cidades$uf)

  out <- list(estados = estados, cidades = cidades)
  assign(key, out, envir = .spatial_cache$meta)
  out
}

spatial_state_choices <- function(meta, region_value) {
  estados <- meta$estados
  if (!identical(region_value, SPATIAL_ALL_REGION)) {
    estados <- estados[estados$regiao %in% region_value, , drop = FALSE]
  }
  c(
    "Todas as UFs" = SPATIAL_ALL_STATE,
    stats::setNames(as.character(estados$uf), estados$rotulo)
  )
}

spatial_city_choices <- function(meta, uf_value) {
  cidades <- meta$cidades
  if (!identical(uf_value, SPATIAL_ALL_STATE)) {
    cidades <- cidades[cidades$uf %in% uf_value, , drop = FALSE]
  } else {
    cidades <- cidades[0, , drop = FALSE]
  }
  c(
    "Todos os municípios" = SPATIAL_ALL_CITY,
    stats::setNames(as.character(cidades$code_muni), cidades$rotulo)
  )
}

spatial_effective_end <- function(code, end_date) {
  info <- spatial_var_info(code)
  min(as.Date(end_date), as.Date(info$available_end[[1]]))
}

spatial_agg_expr <- function(code, q_col) {
  info <- spatial_var_info(code)
  if (identical(info$agg[[1]], "sum")) {
    sprintf("SUM(CAST(%s AS DOUBLE))", q_col)
  } else {
    sprintf("AVG(CAST(%s AS DOUBLE))", q_col)
  }
}

spatial_prepare_query_view <- function(con, table_name, view_name = "ic2025_spatial_query_base") {
  source_fields <- DBI::dbListFields(con, table_name)
  passthrough_fields <- setdiff(
    source_fields,
    c("Data", "RegiaoMunicipio", "UFMunicipio", "CodigoMunicipio6", "EstadoMunicipio", "NomeMunicipio")
  )

  q_tbl <- as.character(DBI::dbQuoteIdentifier(con, table_name))
  q_view <- as.character(DBI::dbQuoteIdentifier(con, view_name))
  select_sql <- c(
    sprintf('CAST(%s AS DATE) AS %s',
      DBI::dbQuoteIdentifier(con, "Data"),
      DBI::dbQuoteIdentifier(con, "data_ref")
    ),
    sprintf('TRIM(CAST(%s AS VARCHAR)) AS %s',
      DBI::dbQuoteIdentifier(con, "RegiaoMunicipio"),
      DBI::dbQuoteIdentifier(con, "regiao_key")
    ),
    sprintf('TRIM(CAST(%s AS VARCHAR)) AS %s',
      DBI::dbQuoteIdentifier(con, "UFMunicipio"),
      DBI::dbQuoteIdentifier(con, "uf_key")
    ),
    sprintf('TRIM(CAST(%s AS VARCHAR)) AS %s',
      DBI::dbQuoteIdentifier(con, "CodigoMunicipio6"),
      DBI::dbQuoteIdentifier(con, "muni_key")
    ),
    sprintf('TRIM(CAST(%s AS VARCHAR)) AS %s',
      DBI::dbQuoteIdentifier(con, "EstadoMunicipio"),
      DBI::dbQuoteIdentifier(con, "estado_label")
    ),
    sprintf('TRIM(CAST(%s AS VARCHAR)) AS %s',
      DBI::dbQuoteIdentifier(con, "NomeMunicipio"),
      DBI::dbQuoteIdentifier(con, "muni_label")
    )
  )

  if (length(passthrough_fields) > 0) {
    select_sql <- c(
      select_sql,
      vapply(
        passthrough_fields,
        function(field) as.character(DBI::dbQuoteIdentifier(con, field)),
        character(1)
      )
    )
  }

  sql <- paste0(
    "CREATE OR REPLACE TEMP VIEW ", q_view,
    " AS SELECT ", paste(select_sql, collapse = ", "),
    " FROM ", q_tbl
  )
  DBI::dbExecute(con, sql)
  view_name
}

spatial_query_map_data <- function(
    con,
    table_name,
    code,
    start_date,
    end_date,
    level = c("state", "municipio"),
    region = SPATIAL_ALL_REGION,
    uf = SPATIAL_ALL_STATE,
    keys = NULL
) {
  level <- match.arg(level)
  start_date <- as.Date(start_date)
  end_date <- spatial_effective_end(code, end_date)
  if (!is.finite(start_date) || !is.finite(end_date) || start_date > end_date) {
    return(tibble::tibble())
  }

  q_tbl <- as.character(DBI::dbQuoteIdentifier(con, table_name))
  q_date <- as.character(DBI::dbQuoteIdentifier(con, "data_ref"))
  q_col <- as.character(DBI::dbQuoteIdentifier(con, code))

  where <- c(
    sprintf("%s >= CAST(? AS DATE)", q_date),
    sprintf("%s <= CAST(? AS DATE)", q_date),
    sprintf("%s IS NOT NULL", q_col)
  )
  params <- list(as.character(start_date), as.character(end_date))

  if (!identical(region, SPATIAL_ALL_REGION)) {
    q_reg <- as.character(DBI::dbQuoteIdentifier(con, "regiao_key"))
    where <- c(where, sprintf("%s = ?", q_reg))
    params <- c(params, list(as.character(region)))
  }
  if (identical(level, "municipio") && !identical(uf, SPATIAL_ALL_STATE)) {
    q_uf <- as.character(DBI::dbQuoteIdentifier(con, "uf_key"))
    where <- c(where, sprintf("%s = ?", q_uf))
    params <- c(params, list(as.character(uf)))
  }
  if (!is.null(keys)) {
    keys <- trimws(as.character(keys))
    keys <- unique(keys[!is.na(keys) & nzchar(keys)])
    if (length(keys) == 0) {
      return(tibble::tibble())
    }
    key_col <- if (identical(level, "state")) "uf_key" else "muni_key"
    q_key <- as.character(DBI::dbQuoteIdentifier(con, key_col))
    where <- c(
      where,
      sprintf(
        "%s IN (%s)",
        q_key,
        paste(rep("?", length(keys)), collapse = ", ")
      )
    )
    params <- c(params, as.list(keys))
  }

  select_sql <- if (identical(level, "state")) {
    paste0(
      DBI::dbQuoteIdentifier(con, "uf_key"), " AS key, ",
      "MIN(", DBI::dbQuoteIdentifier(con, "estado_label"), ") AS label, ",
      spatial_agg_expr(code, q_col), " AS value"
    )
  } else {
    paste0(
      DBI::dbQuoteIdentifier(con, "muni_key"), " AS key, ",
      "MIN(", DBI::dbQuoteIdentifier(con, "muni_label"), ") AS label, ",
      "MIN(", DBI::dbQuoteIdentifier(con, "uf_key"), ") AS uf, ",
      spatial_agg_expr(code, q_col), " AS value"
    )
  }

  group_sql <- if (identical(level, "state")) {
    paste0(" GROUP BY ", DBI::dbQuoteIdentifier(con, "uf_key"))
  } else {
    paste0(" GROUP BY ", DBI::dbQuoteIdentifier(con, "muni_key"))
  }

  sql <- paste0(
    "SELECT ", select_sql,
    " FROM ", q_tbl,
    " WHERE ", paste(where, collapse = " AND "),
    group_sql
  )

  out <- DBI::dbGetQuery(con, sql, params = params)
  if (!is.data.frame(out) || nrow(out) == 0) return(tibble::tibble())
  out$key <- trimws(as.character(out$key))
  out$value <- suppressWarnings(as.numeric(out$value))
  out <- out[is.finite(out$value), , drop = FALSE]
  out
}

spatial_query_available_dates <- function(
    con,
    table_name,
    code,
    start_date,
    end_date,
    region = SPATIAL_ALL_REGION,
    uf = SPATIAL_ALL_STATE
) {
  start_date <- as.Date(start_date)
  end_date <- spatial_effective_end(code, end_date)
  if (!is.finite(start_date) || !is.finite(end_date) || start_date > end_date) {
    return(as.Date(character(0)))
  }

  q_tbl <- as.character(DBI::dbQuoteIdentifier(con, table_name))
  q_date <- as.character(DBI::dbQuoteIdentifier(con, "data_ref"))
  q_col <- as.character(DBI::dbQuoteIdentifier(con, code))

  where <- c(
    sprintf("%s >= CAST(? AS DATE)", q_date),
    sprintf("%s <= CAST(? AS DATE)", q_date),
    sprintf("%s IS NOT NULL", q_col)
  )
  params <- list(as.character(start_date), as.character(end_date))

  if (!identical(region, SPATIAL_ALL_REGION)) {
    q_reg <- as.character(DBI::dbQuoteIdentifier(con, "regiao_key"))
    where <- c(where, sprintf("%s = ?", q_reg))
    params <- c(params, list(as.character(region)))
  }
  if (!identical(uf, SPATIAL_ALL_STATE)) {
    q_uf <- as.character(DBI::dbQuoteIdentifier(con, "uf_key"))
    where <- c(where, sprintf("%s = ?", q_uf))
    params <- c(params, list(as.character(uf)))
  }

  sql <- paste0(
    "SELECT DISTINCT ", q_date, " AS data_ref ",
    "FROM ", q_tbl, " ",
    "WHERE ", paste(where, collapse = " AND "),
    " ORDER BY 1"
  )

  out <- DBI::dbGetQuery(con, sql, params = params)
  if (!is.data.frame(out) || nrow(out) == 0) return(as.Date(character(0)))
  as.Date(out$data_ref)
}

spatial_query_snapshot_data <- function(
    con,
    table_name,
    code,
    start_date,
    end_date,
    level = c("state", "municipio"),
    region = SPATIAL_ALL_REGION,
    uf = SPATIAL_ALL_STATE,
    keys = NULL
) {
  level <- match.arg(level)
  start_date <- as.Date(start_date)
  end_date <- spatial_effective_end(code, end_date)
  if (!is.finite(start_date) || !is.finite(end_date) || start_date > end_date) {
    return(tibble::tibble())
  }

  q_tbl <- as.character(DBI::dbQuoteIdentifier(con, table_name))
  q_date <- as.character(DBI::dbQuoteIdentifier(con, "data_ref"))
  q_col <- as.character(DBI::dbQuoteIdentifier(con, code))

  where <- c(
    sprintf("%s >= CAST(? AS DATE)", q_date),
    sprintf("%s <= CAST(? AS DATE)", q_date),
    sprintf("%s IS NOT NULL", q_col)
  )
  params <- list(as.character(start_date), as.character(end_date))

  if (!identical(region, SPATIAL_ALL_REGION)) {
    q_reg <- as.character(DBI::dbQuoteIdentifier(con, "regiao_key"))
    where <- c(where, sprintf("%s = ?", q_reg))
    params <- c(params, list(as.character(region)))
  }
  if (identical(level, "municipio") && !identical(uf, SPATIAL_ALL_STATE)) {
    q_uf <- as.character(DBI::dbQuoteIdentifier(con, "uf_key"))
    where <- c(where, sprintf("%s = ?", q_uf))
    params <- c(params, list(as.character(uf)))
  }
  if (!is.null(keys)) {
    keys <- trimws(as.character(keys))
    keys <- unique(keys[!is.na(keys) & nzchar(keys)])
    if (length(keys) == 0) {
      return(tibble::tibble())
    }
    key_col <- if (identical(level, "state")) "uf_key" else "muni_key"
    q_key <- as.character(DBI::dbQuoteIdentifier(con, key_col))
    where <- c(
      where,
      sprintf(
        "%s IN (%s)",
        q_key,
        paste(rep("?", length(keys)), collapse = ", ")
      )
    )
    params <- c(params, as.list(keys))
  }

  select_sql <- if (identical(level, "state")) {
    paste0(
      q_date, " AS data_ref, ",
      DBI::dbQuoteIdentifier(con, "uf_key"), " AS key, ",
      "MIN(", DBI::dbQuoteIdentifier(con, "estado_label"), ") AS label, ",
      spatial_agg_expr(code, q_col), " AS value"
    )
  } else {
    paste0(
      q_date, " AS data_ref, ",
      DBI::dbQuoteIdentifier(con, "muni_key"), " AS key, ",
      "MIN(", DBI::dbQuoteIdentifier(con, "muni_label"), ") AS label, ",
      "MIN(", DBI::dbQuoteIdentifier(con, "uf_key"), ") AS uf, ",
      spatial_agg_expr(code, q_col), " AS value"
    )
  }

  group_sql <- if (identical(level, "state")) {
    paste0(" GROUP BY 1, ", DBI::dbQuoteIdentifier(con, "uf_key"))
  } else {
    paste0(" GROUP BY 1, ", DBI::dbQuoteIdentifier(con, "muni_key"))
  }

  sql <- paste0(
    "SELECT ", select_sql,
    " FROM ", q_tbl,
    " WHERE ", paste(where, collapse = " AND "),
    group_sql,
    " ORDER BY 1, 2"
  )

  out <- DBI::dbGetQuery(con, sql, params = params)
  if (!is.data.frame(out) || nrow(out) == 0) return(tibble::tibble())
  out$data_ref <- as.Date(out$data_ref)
  out$key <- trimws(as.character(out$key))
  out$value <- suppressWarnings(as.numeric(out$value))
  out <- out[is.finite(out$value), , drop = FALSE]
  out
}

spatial_scope_from_inputs <- function(scale, region, uf, city) {
  if (identical(scale, "municipio") && !identical(city, SPATIAL_ALL_CITY)) {
    return(list(type = "municipio", value = as.character(city)))
  }
  if (!identical(uf, SPATIAL_ALL_STATE)) {
    return(list(type = "uf", value = as.character(uf)))
  }
  if (!identical(region, SPATIAL_ALL_REGION)) {
    return(list(type = "regiao", value = as.character(region)))
  }
  list(type = "brasil", value = "Brasil")
}

spatial_query_series_data <- function(
    con,
    table_name,
    primary_code,
    secondary_code = NULL,
    start_date,
    end_date,
    frequency = c("D", "W", "M"),
    scope = list(type = "brasil", value = "Brasil")
) {
  frequency <- match.arg(frequency)
  codes <- c(primary_code, if (!is.null(secondary_code) && !identical(secondary_code, "__NONE__")) secondary_code)
  max_end <- min(vapply(codes, spatial_effective_end, as.Date(end_date), FUN.VALUE = as.Date("2000-01-01")))
  start_date <- as.Date(start_date)
  end_date <- as.Date(max_end)
  if (!is.finite(start_date) || !is.finite(end_date) || start_date > end_date) {
    return(tibble::tibble())
  }

  q_tbl <- as.character(DBI::dbQuoteIdentifier(con, table_name))
  q_date <- as.character(DBI::dbQuoteIdentifier(con, "data_ref"))
  bucket_expr <- switch(
    frequency,
    D = q_date,
    W = sprintf("DATE_TRUNC('week', %s)", q_date),
    M = sprintf("DATE_TRUNC('month', %s)", q_date)
  )

  where <- c(
    sprintf("%s >= CAST(? AS DATE)", q_date),
    sprintf("%s <= CAST(? AS DATE)", q_date)
  )
  params <- list(as.character(start_date), as.character(end_date))

  if (identical(scope$type, "regiao")) {
    q_reg <- as.character(DBI::dbQuoteIdentifier(con, "regiao_key"))
    where <- c(where, sprintf("%s = ?", q_reg))
    params <- c(params, list(as.character(scope$value)))
  } else if (identical(scope$type, "uf")) {
    q_uf <- as.character(DBI::dbQuoteIdentifier(con, "uf_key"))
    where <- c(where, sprintf("%s = ?", q_uf))
    params <- c(params, list(as.character(scope$value)))
  } else if (identical(scope$type, "municipio")) {
    q_cod <- as.character(DBI::dbQuoteIdentifier(con, "muni_key"))
    where <- c(where, sprintf("%s = ?", q_cod))
    params <- c(params, list(as.character(scope$value)))
  }

  primary_col <- as.character(DBI::dbQuoteIdentifier(con, primary_code))
  select_sql <- paste0(
    bucket_expr, " AS periodo, ",
    spatial_agg_expr(primary_code, primary_col), " AS valor"
  )

  if (!is.null(secondary_code) && !identical(secondary_code, "__NONE__")) {
    secondary_col <- as.character(DBI::dbQuoteIdentifier(con, secondary_code))
    select_sql <- paste0(
      select_sql, ", ",
      spatial_agg_expr(secondary_code, secondary_col), " AS valor_secundario"
    )
  }

  sql <- paste0(
    "SELECT ", select_sql,
    " FROM ", q_tbl,
    " WHERE ", paste(where, collapse = " AND "),
    " GROUP BY 1 ORDER BY 1"
  )

  out <- DBI::dbGetQuery(con, sql, params = params)
  if (!is.data.frame(out) || nrow(out) == 0) return(tibble::tibble())
  out$periodo <- as.Date(out$periodo)
  out$valor <- suppressWarnings(as.numeric(out$valor))
  if ("valor_secundario" %in% names(out)) {
    out$valor_secundario <- suppressWarnings(as.numeric(out$valor_secundario))
  }
  out
}

spatial_distribution_summary <- function(df, key) {
  if (!is.data.frame(df) || nrow(df) == 0 || !("value" %in% names(df))) return(NULL)
  vals <- df$value[is.finite(df$value)]
  if (length(vals) == 0) return(NULL)
  row <- df[df$key %in% as.character(key), , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  value <- suppressWarnings(as.numeric(row$value[[1]]))
  if (!is.finite(value)) return(NULL)
  pct <- mean(vals <= value)
  rank_desc <- rank(-vals, ties.method = "min")[match(value, vals)]
  list(
    value = value,
    percentile = pct,
    rank_desc = rank_desc,
    n = length(vals)
  )
}

spatial_percentile_text <- function(p) {
  if (!is.finite(p)) return("sem referência comparável")
  if (p >= 0.90) return("entre os maiores valores do recorte atual")
  if (p >= 0.75) return("acima da maior parte do recorte atual")
  if (p >= 0.55) return("ligeiramente acima da faixa central do recorte atual")
  if (p >= 0.45) return("muito próximo da faixa central do recorte atual")
  if (p >= 0.25) return("ligeiramente abaixo da faixa central do recorte atual")
  if (p >= 0.10) return("entre os menores valores do recorte atual")
  "na ponta inferior da distribuição atual"
}

spatial_make_legend <- function(code, values, palette_colors = c(
  ic2025_theme_value("spatial.blue_0"),
  ic2025_theme_value("spatial.blue_1"),
  ic2025_theme_value("spatial.blue_2"),
  ic2025_theme_value("spatial.blue_3"),
  ic2025_theme_value("spatial.blue_4"),
  ic2025_theme_value("spatial.blue_5")
), na_color = ic2025_theme_value("spatial.legend_na")) {
  vals <- suppressWarnings(as.numeric(values))
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return(NULL)
  probs <- seq(0, 1, length.out = length(palette_colors) + 1)
  bins <- unique(as.numeric(stats::quantile(vals, probs = probs, na.rm = TRUE, names = FALSE, type = 7)))
  if (length(bins) < 2) {
    val_ref <- vals[[1]]
    delta <- max(abs(val_ref) * 0.01, 0.5)
    bins <- c(val_ref - delta, val_ref + delta)
  }
  if (length(bins) < 4L && length(unique(vals)) > 1L) {
    pretty_bins <- pretty(range(vals, finite = TRUE), n = length(palette_colors))
    pretty_bins <- unique(c(min(vals, na.rm = TRUE), pretty_bins, max(vals, na.rm = TRUE)))
    pretty_bins <- pretty_bins[is.finite(pretty_bins)]
    pretty_bins <- pretty_bins[
      pretty_bins >= (min(vals, na.rm = TRUE) - 1e-9) &
      pretty_bins <= (max(vals, na.rm = TRUE) + 1e-9)
    ]
    if (length(pretty_bins) >= 2L) {
      bins <- pretty_bins
    }
  }
  bins <- sort(unique(as.numeric(bins)))
  colors <- palette_colors[seq_len(max(1L, min(length(palette_colors), length(bins) - 1L)))]
  label_digits <- spatial_legend_label_digits(bins, fallback = spatial_var_info(code)$digits[[1]])
  pal <- leaflet::colorBin(
    palette = colors,
    domain = vals,
    bins = bins,
    pretty = FALSE,
    na.color = na_color
  )
  bin_labels <- vapply(
    seq_len(length(colors)),
    function(i) spatial_format_bin_label(code, bins[[i]], bins[[i + 1L]], digits = label_digits),
    character(1)
  )
  list(
    pal = pal,
    bins = bins,
    colors = colors,
    na_color = na_color,
    label_digits = label_digits,
    bin_labels = bin_labels
  )
}

spatial_mapbox_tiles <- function() {
  token <- spatial_default_mapbox_token()
  username <- spatial_default_mapbox_username()
  style_id <- spatial_default_mapbox_style_id()
  list(
    template = paste0(
      "https://api.mapbox.com/styles/v1/",
      username, "/", style_id, "/tiles/256/{z}/{x}/{y}@2x?access_token=", token
    ),
    attribution = "Mapbox"
  )
}

spatial_mapbox_style_url <- function() {
  paste0(
    "mapbox://styles/",
    spatial_default_mapbox_username(),
    "/",
    spatial_default_mapbox_style_id()
  )
}

spatial_bbox_payload <- function(sf_obj) {
  bbox <- sf::st_bbox(sf_obj)
  list(
    xmin = unname(as.numeric(bbox[["xmin"]])),
    ymin = unname(as.numeric(bbox[["ymin"]])),
    xmax = unname(as.numeric(bbox[["xmax"]])),
    ymax = unname(as.numeric(bbox[["ymax"]]))
  )
}

spatial_parse_view_bounds <- function(payload) {
  bounds <- payload$bounds %||% NULL
  if (is.null(bounds) || !is.list(bounds)) return(NULL)
  vals <- c(
    west = suppressWarnings(as.numeric(bounds$west %||% NA_real_)),
    south = suppressWarnings(as.numeric(bounds$south %||% NA_real_)),
    east = suppressWarnings(as.numeric(bounds$east %||% NA_real_)),
    north = suppressWarnings(as.numeric(bounds$north %||% NA_real_))
  )
  if (any(!is.finite(vals))) return(NULL)
  as.list(vals)
}

spatial_expand_view_bounds <- function(bounds, ratio = 0.2) {
  if (is.null(bounds)) return(NULL)
  width <- max(0.1, as.numeric(bounds$east) - as.numeric(bounds$west))
  height <- max(0.1, as.numeric(bounds$north) - as.numeric(bounds$south))
  list(
    west = as.numeric(bounds$west) - (width * ratio),
    south = as.numeric(bounds$south) - (height * ratio),
    east = as.numeric(bounds$east) + (width * ratio),
    north = as.numeric(bounds$north) + (height * ratio)
  )
}

spatial_bounds_to_sfc <- function(bounds) {
  if (is.null(bounds)) return(NULL)
  sf::st_as_sfc(
    sf::st_bbox(
      c(
        xmin = as.numeric(bounds$west),
        ymin = as.numeric(bounds$south),
        xmax = as.numeric(bounds$east),
        ymax = as.numeric(bounds$north)
      ),
      crs = sf::st_crs(4326)
    )
  )
}

spatial_bounds_signature <- function(bounds, digits = 2L) {
  if (is.null(bounds)) return("all")
  vals <- c(
    west = suppressWarnings(as.numeric(bounds$west %||% NA_real_)),
    south = suppressWarnings(as.numeric(bounds$south %||% NA_real_)),
    east = suppressWarnings(as.numeric(bounds$east %||% NA_real_)),
    north = suppressWarnings(as.numeric(bounds$north %||% NA_real_))
  )
  if (any(!is.finite(vals))) return("all")
  paste(formatC(vals, format = "f", digits = digits), collapse = "|")
}

spatial_keys_signature <- function(keys) {
  keys <- sort(unique(trimws(as.character(keys %||% character(0)))))
  keys <- keys[!is.na(keys) & nzchar(keys)]
  if (length(keys) == 0) return("empty")

  chars <- utf8ToInt(paste(keys, collapse = "|"))
  mod <- 2147483629
  hash <- 0L
  if (length(chars) > 0) {
    for (idx in seq_along(chars)) {
      hash <- (hash * 131L + as.integer(chars[[idx]])) %% mod
    }
  }

  paste0(length(keys), "-", sprintf("%08x", as.integer(hash)))
}

spatial_geojson_matrix_coords <- function(mat) {
  if (is.null(mat) || !is.matrix(mat) || nrow(mat) == 0) return(list())
  lapply(seq_len(nrow(mat)), function(i) unname(as.numeric(mat[i, ])))
}

spatial_geojson_geometry <- function(geom) {
  geom <- suppressWarnings(sf::st_zm(geom, drop = TRUE, what = "ZM"))
  geom_class <- class(geom)

  if ("POINT" %in% geom_class) {
    return(list(type = "Point", coordinates = unname(as.numeric(geom))))
  }
  if ("MULTIPOINT" %in% geom_class) {
    return(list(type = "MultiPoint", coordinates = spatial_geojson_matrix_coords(unclass(geom))))
  }
  if ("LINESTRING" %in% geom_class) {
    return(list(type = "LineString", coordinates = spatial_geojson_matrix_coords(unclass(geom))))
  }
  if ("MULTILINESTRING" %in% geom_class) {
    return(list(
      type = "MultiLineString",
      coordinates = lapply(geom, spatial_geojson_matrix_coords)
    ))
  }
  if ("POLYGON" %in% geom_class) {
    return(list(
      type = "Polygon",
      coordinates = lapply(geom, spatial_geojson_matrix_coords)
    ))
  }
  if ("MULTIPOLYGON" %in% geom_class) {
    return(list(
      type = "MultiPolygon",
      coordinates = lapply(
        geom,
        function(poly) lapply(poly, spatial_geojson_matrix_coords)
      )
    ))
  }

  stop("Geometria não suportada para GeoJSON: ", paste(geom_class, collapse = "/"))
}

spatial_sf_to_geojson_fallback <- function(sf_obj) {
  sf_out <- suppressWarnings(sf::st_zm(sf_obj, drop = TRUE, what = "ZM"))
  props <- sf::st_drop_geometry(sf_out)
  geoms <- sf::st_geometry(sf_out)
  features <- lapply(seq_len(nrow(sf_out)), function(i) {
    list(
      type = "Feature",
      properties = as.list(props[i, , drop = FALSE]),
      geometry = spatial_geojson_geometry(geoms[[i]])
    )
  })
  jsonlite::toJSON(
    list(type = "FeatureCollection", features = features),
    auto_unbox = TRUE,
    digits = 7,
    null = "null",
    na = "null"
  )
}

spatial_sf_to_geojson <- function(sf_obj) {
  sf_out <- suppressWarnings(sf::st_zm(sf_obj, drop = TRUE, what = "ZM"))
  if (requireNamespace("geojsonsf", quietly = TRUE)) {
    return(geojsonsf::sf_geojson(sf_out))
  }
  spatial_sf_to_geojson_fallback(sf_out)
}

spatial_geojson_cache_get <- function(key) {
  if (!exists(key, envir = .spatial_cache$geojson, inherits = FALSE)) return(NULL)
  get(key, envir = .spatial_cache$geojson, inherits = FALSE)
}

spatial_geojson_cache_set <- function(key, value, max_items = 12L) {
  assign(key, value, envir = .spatial_cache$geojson)
  .spatial_cache$geojson_order <- c(setdiff(.spatial_cache$geojson_order, key), key)
  if (length(.spatial_cache$geojson_order) > max_items) {
    drop_keys <- utils::head(.spatial_cache$geojson_order, -max_items)
    for (drop_key in drop_keys) {
      if (exists(drop_key, envir = .spatial_cache$geojson, inherits = FALSE)) {
        rm(list = drop_key, envir = .spatial_cache$geojson)
      }
    }
    .spatial_cache$geojson_order <- utils::tail(.spatial_cache$geojson_order, max_items)
  }
  value
}

spatial_geo_resource_prefix <- function() {
  "ic2025spatialgeo"
}

spatial_vendor_library_path <- function(project_root = spatial_detect_project_root()) {
  path <- file.path(project_root, ".ic2025_r_libs")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

spatial_activate_vendor_library <- function(project_root = spatial_detect_project_root()) {
  lib <- spatial_vendor_library_path(project_root = project_root)
  libs <- normalizePath(.libPaths(), winslash = "/", mustWork = FALSE)
  if (!lib %in% libs) {
    .libPaths(c(lib, .libPaths()))
  }
  lib
}

spatial_default_repos <- function() {
  repos <- getOption("repos")
  if (is.null(repos) || length(repos) == 0) {
    repos <- c(CRAN = "https://cloud.r-project.org")
  }
  repos <- repos[nzchar(repos) & repos != "@CRAN@"]
  if (length(repos) == 0) {
    repos <- c(CRAN = "https://cloud.r-project.org")
  }
  repos
}

spatial_package_imports <- function(pkg_dir) {
  desc_path <- file.path(pkg_dir, "DESCRIPTION")
  if (!file.exists(desc_path)) return(character(0))
  desc <- tryCatch(read.dcf(desc_path), error = function(e) NULL)
  if (is.null(desc) || !("Imports" %in% colnames(desc))) return(character(0))
  imports <- unlist(strsplit(as.character(desc[1, "Imports"]), ",", fixed = TRUE))
  imports <- trimws(gsub("\\s*\\(.*\\)$", "", imports))
  imports[nzchar(imports) & !imports %in% c("base", "utils", "stats", "graphics", "methods")]
}

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

spatial_ensure_runtime_package <- function(package, project_root = spatial_detect_project_root()) {
  spatial_activate_vendor_library(project_root = project_root)
  if (requireNamespace(package, quietly = TRUE)) {
    return(TRUE)
  }

  lib <- spatial_vendor_library_path(project_root = project_root)
  try(
    utils::install.packages(
      package,
      lib = lib,
      repos = spatial_default_repos(),
      quiet = TRUE
    ),
    silent = TRUE
  )

  requireNamespace(package, quietly = TRUE)
}

spatial_ensure_curbcut_packages <- function(project_root = spatial_detect_project_root()) {
  landing_dir <- file.path(project_root, "vendor", "curbcut", "cc.landing")
  map_dir <- file.path(project_root, "vendor", "curbcut", "cc.map")
  landing_ok <- spatial_ensure_vendor_package("cc.landing", landing_dir, project_root = project_root)
  suppressWarnings(
    try(spatial_ensure_vendor_package("cc.map", map_dir, project_root = project_root), silent = TRUE)
  )
  suppressWarnings(
    try(spatial_ensure_runtime_package("rintrojs", project_root = project_root), silent = TRUE)
  )
  landing_ok
}

spatial_register_geo_resources <- function(project_root = spatial_detect_project_root()) {
  prefix <- spatial_geo_resource_prefix()
  cache_dir <- file.path(project_root, "cache_geo")
  if (!prefix %in% names(shiny::resourcePaths())) {
    shiny::addResourcePath(prefix, cache_dir)
  }
  prefix
}

spatial_geo_source_url <- function(level, project_root = spatial_detect_project_root()) {
  prefix <- spatial_register_geo_resources(project_root = project_root)
  file_name <- if (identical(as.character(level), "state")) {
    "mapa_brasil_estados_v3.geojson"
  } else {
    "mapa_brasil_municipios_v3.geojson"
  }
  version <- as.integer(file.info(file.path(project_root, "cache_geo", file_name))$mtime)
  sprintf("/%s/%s?v=%s", prefix, file_name, version)
}

spatial_vendor_fonts_prefix <- function() {
  "ic2025spatialfonts"
}

spatial_register_vendor_font_resources <- function(project_root = spatial_detect_project_root()) {
  prefix <- spatial_vendor_fonts_prefix()
  font_dir <- file.path(project_root, "vendor", "curbcut", "curbcut", "inst", "fonts")
  if (!dir.exists(font_dir)) {
    font_dir <- file.path(project_root, "vendor", "curbcut", "curbcut-montreal", "www", "fonts")
  }
  if (dir.exists(font_dir) && !prefix %in% names(shiny::resourcePaths())) {
    shiny::addResourcePath(prefix, font_dir)
  }
  prefix
}

spatial_region_choices <- function(meta) {
  c(
    "Brasil" = SPATIAL_ALL_REGION,
    stats::setNames(unique(meta$estados$regiao), unique(meta$estados$regiao))
  )
}

spatial_default_preferences <- function() {
  list(
    region_default = SPATIAL_ALL_REGION,
    location_default = NULL
  )
}

spatial_normalize_saved_location <- function(location_default, meta) {
  if (is.null(location_default) || !is.list(location_default)) {
    return(NULL)
  }

  region_value <- spatial_scalar_string(location_default$region, SPATIAL_ALL_REGION)
  region_values <- unname(spatial_region_choices(meta))
  if (!region_value %in% region_values) {
    region_value <- SPATIAL_ALL_REGION
  }

  state_values <- unname(spatial_state_choices(meta, region_value))
  state_value <- spatial_scalar_string(location_default$state, SPATIAL_ALL_STATE)
  if (!state_value %in% state_values) {
    state_value <- SPATIAL_ALL_STATE
  }

  city_values <- unname(spatial_city_choices(meta, state_value))
  city_value <- spatial_scalar_string(location_default$city, SPATIAL_ALL_CITY)
  if (!city_value %in% city_values) {
    city_value <- SPATIAL_ALL_CITY
  }

  if (identical(region_value, SPATIAL_ALL_REGION) &&
      identical(state_value, SPATIAL_ALL_STATE) &&
      identical(city_value, SPATIAL_ALL_CITY)) {
    return(NULL)
  }

  list(
    region = region_value,
    state = state_value,
    city = city_value
  )
}

spatial_normalize_preferences <- function(prefs, meta) {
  defaults <- spatial_default_preferences()
  if (is.null(prefs) || !is.list(prefs)) {
    return(defaults)
  }

  region_values <- unname(spatial_region_choices(meta))
  region_default <- spatial_scalar_string(prefs$region_default, defaults$region_default)
  if (!region_default %in% region_values) {
    region_default <- defaults$region_default
  }

  list(
    region_default = region_default,
    location_default = spatial_normalize_saved_location(prefs$location_default %||% NULL, meta = meta)
  )
}

spatial_tutorial_steps <- function(ns) {
  build_element <- function(id) sprintf("#%s", ns(id))

  data.frame(
    element = c(
      build_element("map"),
      build_element("title_texts"),
      build_element("left_widgets"),
      build_element("legend_div"),
      build_element("zoom_div"),
      build_element("compare_panel"),
      build_element("timeline_card"),
      build_element("explore_full"),
      build_element("floating_panel_content"),
      build_element("help"),
      build_element("coverage")
    ),
    intro = c(
      "Os mapas são interativos: você pode arrastar, aproximar, afastar e clicar nas áreas para obter mais informações.",
      "Cada página espacial explora um indicador específico. Aqui você vê o título atual e o botão de contexto do indicador.",
      "Escolha indicador, comparação, tempo e geografia. O mapa se atualiza conforme os filtros mudam.",
      "A legenda mostra como os valores foram traduzidos em cores no recorte atual.",
      "Com a escala automática ligada, a visualização alterna entre UF e município conforme o zoom. Desligando essa opção, clicar em uma UF abre os municípios daquele recorte até você afastar o mapa de novo.",
      "Se quiser comparar a variável principal com uma segunda variável, selecione-a aqui e a série temporal será atualizada.",
      "Ative a análise de um dia específico para percorrer as datas disponíveis no período filtrado.",
      "O painel da direita resume o recorte atual ou a localidade selecionada e compara o valor com a distribuição observada.",
      "Use estes botões para alternar entre mapa, tabela e série temporal. As três visões refletem os mesmos filtros.",
      "Você pode rever este tour a qualquer momento clicando novamente neste botão.",
      "A engrenagem abre as opções avançadas, como região padrão e localização padrão da Visualização Espacial."
    ),
    position = c("auto", "right", "right", "auto", "left", "left", "bottom", "left", "top", "left", "left"),
    title = c(
      "Mapa",
      "Tema",
      "Seleção de variáveis",
      "Legenda",
      "Escala",
      "Comparação",
      "Linha do tempo",
      "Explore",
      "Troca de visualização",
      "Tutorial",
      "Opções avançadas"
    ),
    stringsAsFactors = FALSE
  )
}

spatial_settings_panel_ui <- function(ns, meta, prefs, current_region, current_state, current_city) {
  prefs <- spatial_normalize_preferences(prefs, meta = meta)
  location_default <- prefs$location_default %||% list(
    region = current_region,
    state = current_state,
    city = current_city
  )

  lock_region_choices <- spatial_region_choices(meta)
  lock_region <- spatial_scalar_string(location_default$region, current_region)
  if (!lock_region %in% unname(lock_region_choices)) {
    lock_region <- SPATIAL_ALL_REGION
  }

  lock_state_choices <- spatial_state_choices(meta, lock_region)
  lock_state <- spatial_scalar_string(location_default$state, current_state)
  if (!lock_state %in% unname(lock_state_choices)) {
    lock_state <- SPATIAL_ALL_STATE
  }

  lock_city_choices <- spatial_city_choices(meta, lock_state)
  lock_city <- spatial_scalar_string(location_default$city, current_city)
  if (!lock_city %in% unname(lock_city_choices)) {
    lock_city <- SPATIAL_ALL_CITY
  }

  shiny::div(
    class = "ic2025-spatial-settings-modal",
    shiny::div(
      class = "ic2025-spatial-settings-section",
      shiny::strong("Alterar região padrão"),
      shiny::p("Essa preferência é aplicada quando não houver uma localização padrão salva."),
      shiny::radioButtons(
        ns("settings_region_default"),
        label = NULL,
        inline = TRUE,
        selected = prefs$region_default,
        choiceNames = names(lock_region_choices),
        choiceValues = unname(lock_region_choices)
      )
    ),
    shiny::hr(),
    shiny::div(
      class = "ic2025-spatial-settings-section",
      shiny::strong("Salvar localização padrão"),
      shiny::p("A localização salva será reaplicada automaticamente sempre que a Visualização Espacial for aberta."),
      shiny::selectInput(
        ns("settings_lock_region"),
        "Região",
        choices = lock_region_choices,
        selected = lock_region,
        selectize = FALSE
      ),
      shiny::selectInput(
        ns("settings_lock_state"),
        "UF",
        choices = lock_state_choices,
        selected = lock_state,
        selectize = FALSE
      ),
      shiny::selectInput(
        ns("settings_lock_city"),
        "Município",
        choices = lock_city_choices,
        selected = lock_city,
        selectize = FALSE
      ),
      htmltools::tags$div(
        class = "ic2025-spatial-settings-actions",
        shiny::actionButton(ns("settings_save_location"), "Salvar localização padrão", icon = shiny::icon("check")),
        shiny::actionButton(ns("settings_use_current"), "Usar recorte atual", icon = shiny::icon("crosshairs")),
        shiny::actionButton(ns("settings_clear_default_location"), "Limpar localização padrão", icon = shiny::icon("eraser"))
      )
    )
  )
}

spatial_app_ui <- function(id) {
  ns <- shiny::NS(id)
  spatial_register_vendor_font_resources(spatial_detect_project_root())
  var_catalog <- spatial_var_catalog()
  group_choices <- unique(var_catalog$group)
  default_group <- group_choices[[1]]
  group_vars <- var_catalog[var_catalog$group %in% default_group, , drop = FALSE]
  default_variable <- if ("UmidRel" %in% group_vars$code) "UmidRel" else group_vars$code[[1]]

  shiny::tagList(
    if (requireNamespace("rintrojs", quietly = TRUE)) rintrojs::introjsUI(),
    shinyjs::useShinyjs(),
    shiny::div(
      id = ns("root"),
      class = "ic2025-spatial-app is-booting",
      shiny::div(
        class = "ic2025-spatial-group-store",
        shiny::selectInput(
          ns("group"),
          NULL,
          choices = stats::setNames(group_choices, group_choices),
          selected = default_group,
          width = "220px",
          selectize = FALSE
        ),
        shiny::actionButton(ns("back"), label = NULL, class = "ic2025-spatial-hidden-back")
      ),
      shiny::div(
        class = "ic2025-spatial-stage",
        shiny::div(
          id = ns("map_div"),
          class = "ic2025-spatial-main",
          shiny::tabsetPanel(
            id = ns("view_tabs"),
            type = "hidden",
            selected = "map",
            shiny::tabPanel(
              "map",
              shiny::div(
                id = ns("map"),
                class = "ic2025-spatial-mapbox"
              )
            ),
            shiny::tabPanel(
              "table",
              shiny::div(
                class = "ic2025-spatial-table-pane",
                shiny::div(
                  class = "ic2025-spatial-table-shell",
                  DT::DTOutput(ns("table"), height = "100%")
                ),
                shiny::div(
                  class = "ic2025-spatial-table-tools",
                  shiny::div(
                    class = "ic2025-spatial-table-downloads",
                    shiny::downloadButton(ns("download_csv"), "Baixar .csv", class = "ic2025-spatial-download-btn"),
                    shiny::downloadButton(ns("download_rds"), "Baixar .rds", class = "ic2025-spatial-download-btn")
                  ),
                  shiny::uiOutput(ns("table_info"))
                )
              )
            ),
            shiny::tabPanel(
              "portrait",
              shiny::div(
                class = "ic2025-spatial-portrait",
                shiny::div(class = "ic2025-spatial-portrait-title", shiny::uiOutput(ns("portrait_title"))),
                plotly::plotlyOutput(ns("portrait_plot"), height = "68vh")
              )
            )
          ),
          shiny::div(
            id = ns("floating_panel_content"),
            class = "ic2025-spatial-view-toggle",
            shiny::actionButton(ns("view_map"), "Mapa", icon = shiny::icon("map"), class = "ic2025-spatial-view-btn is-active"),
            shiny::actionButton(ns("view_table"), "Tabela", icon = shiny::icon("table"), class = "ic2025-spatial-view-btn"),
            shiny::actionButton(ns("view_portrait"), "Série", icon = shiny::icon("chart-line"), class = "ic2025-spatial-view-btn")
          )
        ),
        shiny::div(
          class = "ic2025-spatial-toolbar",
          shiny::div(
            class = "ic2025-spatial-toolbar-left",
            shiny::div(class = "ic2025-spatial-theme-drop", shiny::uiOutput(ns("theme_drop_ui"))),
            shiny::actionButton(ns("help"), label = NULL, icon = shiny::icon("question"), class = "ic2025-spatial-icon-btn"),
            shiny::actionButton(ns("coverage"), label = NULL, icon = shiny::icon("cog"), class = "ic2025-spatial-icon-btn")
          )
        ),
        shiny::div(
          class = "ic2025-spatial-timeline-shell",
          shiny::div(
            id = ns("timeline_card"),
            class = "ic2025-spatial-timeline-card",
            shiny::div(
              class = "ic2025-spatial-timeline-head",
              shiny::div(
                class = "ic2025-spatial-timeline-toggle",
                shiny::checkboxInput(ns("use_snapshot_date"), "Analisar um dia específico", value = FALSE)
              ),
              shiny::div(
                class = "ic2025-spatial-timeline-status",
                htmltools::tags$span(class = "ic2025-spatial-timeline-eyebrow", shiny::textOutput(ns("timeline_eyebrow"), inline = TRUE)),
                htmltools::tags$strong(class = "ic2025-spatial-timeline-value", shiny::textOutput(ns("timeline_value"), inline = TRUE))
              )
            ),
            shiny::div(
              id = ns("timeline_slider_panel"),
              class = "ic2025-spatial-timeline-slider-wrap",
              shiny::sliderInput(
                ns("snapshot_index"),
                label = NULL,
                min = 1,
                max = 1,
                value = 1,
                step = 1,
                ticks = FALSE,
                width = "100%"
              ),
              shiny::div(
                class = "ic2025-spatial-timeline-footer",
                shiny::div(
                  class = "ic2025-spatial-timeline-footer-side is-start",
                  shiny::actionButton(ns("play_prev"), label = NULL, icon = shiny::icon("backward"), class = "ic2025-spatial-player-btn"),
                  htmltools::tags$span(class = "ic2025-spatial-timeline-edge-label", shiny::textOutput(ns("timeline_start_label"), inline = TRUE))
                ),
                shiny::div(
                  class = "ic2025-spatial-timeline-footer-center",
                  shiny::actionButton(ns("play_toggle"), label = NULL, icon = shiny::icon("play"), class = "ic2025-spatial-player-btn ic2025-spatial-player-btn-main")
                ),
                shiny::div(
                  class = "ic2025-spatial-timeline-footer-side is-end",
                  htmltools::tags$span(class = "ic2025-spatial-timeline-edge-label", shiny::textOutput(ns("timeline_end_label"), inline = TRUE)),
                  shiny::actionButton(ns("play_next"), label = NULL, icon = shiny::icon("forward"), class = "ic2025-spatial-player-btn")
                )
              )
            )
          )
        ),
        shiny::div(
          class = "ic2025-spatial-sidebar",
          shiny::div(class = "ic2025-spatial-sidebar-inner",
            shiny::div(id = ns("left_widgets"), class = "ic2025-spatial-sidebar-scroll",
              shiny::div(
                id = ns("title_texts"),
                class = "ic2025-spatial-sidebar-title-row",
                shiny::div(class = "ic2025-spatial-sidebar-title", shiny::textOutput(ns("variable_title"))),
                shiny::actionLink(
                  ns("panel_info"),
                  label = NULL,
                  icon = shiny::icon("info-circle", class = "ic2025-spatial-title-info-icon"),
                  class = "ic2025-spatial-title-info-link"
                )
              ),
              shiny::div(
                class = "ic2025-spatial-section",
                spatial_section_label("sliders", "Indicador"),
                shiny::selectInput(ns("variable"), NULL, choices = stats::setNames(group_vars$code, group_vars$label), selected = default_variable, selectize = FALSE)
              ),
              shiny::div(
                id = ns("compare_panel"),
                class = "ic2025-spatial-section",
                style = "display:none;",
                spatial_section_label("balance-scale", "Comparação"),
                shiny::selectInput(
                  ns("compare_var"),
                  NULL,
                  choices = c("Nenhuma" = "__NONE__", stats::setNames(var_catalog$code, paste(var_catalog$group, "·", var_catalog$label))),
                  selected = "__NONE__",
                  selectize = FALSE
                )
              ),
              shiny::div(
                class = "ic2025-spatial-section",
                spatial_section_label("calendar", "Tempo"),
                shiny::div(
                  class = "ic2025-spatial-time-card",
                  shiny::div(
                    class = "ic2025-spatial-time-range",
                    htmltools::tags$div(class = "ic2025-spatial-time-range-status", shiny::textOutput(ns("period_badge"))),
                    shiny::sliderInput(
                      ns("period"),
                      NULL,
                      min = as.Date("2015-01-01"),
                      max = as.Date("2025-12-31"),
                      value = c(as.Date("2024-01-01"), as.Date("2024-12-31")),
                      timeFormat = "%d/%m/%Y",
                      width = "100%",
                      ticks = FALSE,
                      step = 1
                    ),
                    htmltools::tags$div(
                      class = "ic2025-spatial-time-axis",
                      htmltools::tags$span("01/01/2015"),
                      htmltools::tags$span("31/12/2025")
                    )
                  ),
                  shiny::div(
                    id = ns("frequency_panel"),
                    class = "ic2025-spatial-time-frequency",
                    style = "display:none;",
                    htmltools::tags$div(class = "ic2025-spatial-mini-label", "Agregação"),
                    shiny::selectInput(
                      ns("frequency"),
                      NULL,
                      choices = c("Diária" = "D", "Semanal" = "W", "Mensal" = "M"),
                      selected = "D",
                      selectize = FALSE
                    )
                  )
                )
              ),
              shiny::div(
                class = "ic2025-spatial-section",
                spatial_section_label("globe", "Geografia"),
                shiny::selectInput(ns("region"), NULL, choices = c("Brasil" = SPATIAL_ALL_REGION), selected = SPATIAL_ALL_REGION, selectize = FALSE),
                shiny::selectInput(ns("state"), NULL, choices = c("Todas as UFs" = SPATIAL_ALL_STATE), selected = SPATIAL_ALL_STATE, selectize = FALSE),
                shiny::selectInput(ns("city"), NULL, choices = c("Todos os municípios" = SPATIAL_ALL_CITY), selected = SPATIAL_ALL_CITY, selectize = FALSE)
              ),
              shiny::div(
                id = ns("zoom_div"),
                class = "ic2025-spatial-section",
                spatial_section_label("filter", "Escala"),
                shiny::div(
                  class = "ic2025-spatial-scale-card",
                  shiny::div(
                    class = "ic2025-spatial-scale-toggle",
                    shiny::checkboxInput(
                      ns("auto_scale"),
                      "Escala automática",
                      value = FALSE
                    )
                  )
                )
              ),
              shiny::uiOutput(ns("coverage_note")),
              shiny::div(id = ns("legend_div"), class = "ic2025-spatial-legend-wrap", shiny::uiOutput(ns("legend_ui")))
            )
          )
        ),
        shiny::div(
          id = ns("explore_full"),
          class = "ic2025-spatial-rightpanel",
          shiny::div(class = "ic2025-spatial-rightpanel-scroll",
            shiny::div(class = "ic2025-spatial-rightpanel-title", shiny::uiOutput(ns("selection_title"))),
            shiny::div(class = "ic2025-spatial-rightpanel-body", shiny::uiOutput(ns("selection_text"))),
            plotly::plotlyOutput(ns("distribution_plot"), height = "180px"),
            plotly::plotlyOutput(ns("quick_portrait_plot"), height = "210px"),
            shiny::actionLink(ns("clear_selection"), "Limpar seleção", class = "ic2025-spatial-clear"),
            shiny::hr(),
            shiny::div(class = "ic2025-spatial-rightpanel-title ic2025-spatial-rightpanel-title-small", "Você sabia?"),
            shiny::div(class = "ic2025-spatial-rightpanel-body", shiny::uiOutput(ns("did_you_know")))
          )
        ),
        shiny::uiOutput(ns("overlay_ui"))
      )
    )
  )
}

spatial_app_server <- function(id, active = shiny::reactive(TRUE), refresh = shiny::reactive(0L)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    project_root <- spatial_detect_project_root()
    spatial_activate_vendor_library(project_root = project_root)
    cc_landing_ready <- isTRUE(spatial_ensure_curbcut_packages(project_root = project_root))
    db_path <- if (exists("ic2025_base_agregada_duckdb_path", mode = "function")) {
      ic2025_base_agregada_duckdb_path()
    } else {
      file.path(project_root, "base_final.duckdb")
    }
    spatial_register_geo_resources(project_root = project_root)
    spatial_register_vendor_font_resources(project_root = project_root)
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
    table_name <- DBI::dbListTables(con)[1]
    session$onSessionEnded(function() {
      try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
    })

    meta <- spatial_query_metadata(con, table_name, project_root = project_root)
    query_table_name <- spatial_prepare_query_view(con, table_name)
    var_catalog <- spatial_var_catalog()
    default_group <- unique(var_catalog$group)[[1]]
    default_period <- c(as.Date("2024-01-01"), as.Date("2024-12-31"))
    theme_pages <- spatial_theme_pages(var_catalog)
    theme_translation_df <- spatial_theme_translation_df(var_catalog, home_str = "Voltar")
    pending_menu_variable <- shiny::reactiveVal(NULL)
    snapshot_store <- new.env(parent = emptyenv())
    snapshot_store_order <- character(0)
    snapshot_store_get <- function(key) {
      if (!exists(key, envir = snapshot_store, inherits = FALSE)) return(NULL)
      get(key, envir = snapshot_store, inherits = FALSE)
    }
    snapshot_store_set <- function(key, value, max_items = 3L) {
      assign(key, value, envir = snapshot_store)
      snapshot_store_order <<- c(setdiff(snapshot_store_order, key), key)
      if (length(snapshot_store_order) > max_items) {
        drop_keys <- utils::head(snapshot_store_order, -max_items)
        for (drop_key in drop_keys) {
          if (exists(drop_key, envir = snapshot_store, inherits = FALSE)) {
            rm(list = drop_key, envir = snapshot_store)
          }
        }
        snapshot_store_order <<- utils::tail(snapshot_store_order, max_items)
      }
      value
    }
    map_store <- new.env(parent = emptyenv())
    map_store_order <- character(0)
    map_store_get <- function(key) {
      if (!exists(key, envir = map_store, inherits = FALSE)) return(NULL)
      get(key, envir = map_store, inherits = FALSE)
    }
    map_store_set <- function(key, value, max_items = 12L) {
      assign(key, value, envir = map_store)
      map_store_order <<- c(setdiff(map_store_order, key), key)
      if (length(map_store_order) > max_items) {
        drop_keys <- utils::head(map_store_order, -max_items)
        for (drop_key in drop_keys) {
          if (exists(drop_key, envir = map_store, inherits = FALSE)) {
            rm(list = drop_key, envir = map_store)
          }
        }
        map_store_order <<- utils::tail(map_store_order, max_items)
      }
      value
    }
    payload_store <- new.env(parent = emptyenv())
    payload_store_order <- character(0)
    payload_store_get <- function(key) {
      if (!exists(key, envir = payload_store, inherits = FALSE)) return(NULL)
      get(key, envir = payload_store, inherits = FALSE)
    }
    payload_store_set <- function(key, value, max_items = 24L) {
      assign(key, value, envir = payload_store)
      payload_store_order <<- c(setdiff(payload_store_order, key), key)
      if (length(payload_store_order) > max_items) {
        drop_keys <- utils::head(payload_store_order, -max_items)
        for (drop_key in drop_keys) {
          if (exists(drop_key, envir = payload_store, inherits = FALSE)) {
            rm(list = drop_key, envir = payload_store)
          }
        }
        payload_store_order <<- utils::tail(payload_store_order, max_items)
      }
      value
    }
    series_store <- new.env(parent = emptyenv())
    series_store_order <- character(0)
    series_store_get <- function(key) {
      if (!exists(key, envir = series_store, inherits = FALSE)) return(NULL)
      get(key, envir = series_store, inherits = FALSE)
    }
    series_store_set <- function(key, value, max_items = 32L) {
      assign(key, value, envir = series_store)
      series_store_order <<- c(setdiff(series_store_order, key), key)
      if (length(series_store_order) > max_items) {
        drop_keys <- utils::head(series_store_order, -max_items)
        for (drop_key in drop_keys) {
          if (exists(drop_key, envir = series_store, inherits = FALSE)) {
            rm(list = drop_key, envir = series_store)
          }
        }
        series_store_order <<- utils::tail(series_store_order, max_items)
      }
      value
    }
    player_direction <- shiny::reactiveVal(0L)
    player_resume_direction <- shiny::reactiveVal(1L)
    applied_frame_key <- shiny::reactiveVal("")
    applied_timeline_value <- shiny::reactiveVal("")
    zoom_detail_mode <- shiny::reactiveVal("state")
    overlay_mode <- shiny::reactiveVal(NULL)
    tutorial_ready <- requireNamespace("rintrojs", quietly = TRUE)
    tutorial_autostart_checked <- shiny::reactiveVal(FALSE)
    saved_preferences <- shiny::reactiveVal(spatial_default_preferences())
    municipio_background_warm_started <- shiny::reactiveVal(FALSE)

    prefs_storage_key <- paste0(ns("preferences"), "::v1")
    tutorial_storage_key <- paste0(ns("tutorial"), "::seen")

    current_group <- shiny::reactive({
      spatial_resolve_group(input$group, catalog = var_catalog)
    })

    current_variable <- shiny::reactive({
      spatial_resolve_variable(input$variable, current_group(), catalog = var_catalog)
    })

    current_compare_var <- shiny::reactive({
      spatial_resolve_compare_var(input$compare_var, primary_code = current_variable(), catalog = var_catalog)
    })

    current_period <- shiny::reactive({
      spatial_resolve_period(input$period, default = default_period)
    })

    current_region <- shiny::reactive({
      spatial_scalar_string(input$region, SPATIAL_ALL_REGION)
    })

    current_state <- shiny::reactive({
      spatial_scalar_string(input$state, SPATIAL_ALL_STATE)
    })

    current_city <- shiny::reactive({
      spatial_scalar_string(input$city, SPATIAL_ALL_CITY)
    })

    current_view_tab <- shiny::reactive({
      as.character(input$view_tabs %||% "map")
    })

    shiny::observe({
      session$sendCustomMessage(
        "ic2025-spatial-view-state",
        list(
          id = ns("root"),
          view = current_view_tab()
        )
      )
    })

    escala_automatica_ativa <- shiny::reactive({
      isTRUE(input$auto_scale)
    })

    persist_spatial_preferences <- function(prefs) {
      prefs <- spatial_normalize_preferences(prefs, meta = meta)
      saved_preferences(prefs)
      session$sendCustomMessage(
        "ic2025-spatial-prefs-save",
        list(
          storageKey = prefs_storage_key,
          preferences = prefs
        )
      )
      invisible(prefs)
    }

    apply_spatial_preferences <- function(prefs) {
      prefs <- spatial_normalize_preferences(prefs, meta = meta)
      if (!is.null(prefs$location_default)) {
        region_value <- prefs$location_default$region
        state_value <- prefs$location_default$state
        city_value <- prefs$location_default$city
      } else {
        region_value <- prefs$region_default
        state_value <- SPATIAL_ALL_STATE
        city_value <- SPATIAL_ALL_CITY
      }

      region_choices <- spatial_region_choices(meta)
      if (!region_value %in% unname(region_choices)) {
        region_value <- SPATIAL_ALL_REGION
      }
      state_choices <- spatial_state_choices(meta, region_value)
      if (!state_value %in% unname(state_choices)) {
        state_value <- SPATIAL_ALL_STATE
      }
      city_choices <- spatial_city_choices(meta, state_value)
      if (!city_value %in% unname(city_choices)) {
        city_value <- SPATIAL_ALL_CITY
      }

      shiny::freezeReactiveValue(input, "region")
      shiny::freezeReactiveValue(input, "state")
      shiny::freezeReactiveValue(input, "city")
      shiny::updateSelectInput(session, "region", choices = region_choices, selected = region_value)
      shiny::updateSelectInput(session, "state", choices = state_choices, selected = state_value)
      shiny::updateSelectInput(session, "city", choices = city_choices, selected = city_value)
    }

    current_map_zoom <- shiny::reactive({
      payload <- input$map_view_state
      suppressWarnings(as.numeric(payload$zoom %||% NA_real_))
    })

    current_map_bounds <- shiny::reactive({
      spatial_parse_view_bounds(input$map_view_state)
    })

    municipio_view_context <- function(for_level = "municipio") {
      cities_meta <- meta$cidades
      if (!identical(current_region(), SPATIAL_ALL_REGION)) {
        cities_meta <- cities_meta[cities_meta$regiao %in% current_region(), , drop = FALSE]
      }
      if (!identical(current_state(), SPATIAL_ALL_STATE)) {
        cities_meta <- cities_meta[cities_meta$uf %in% current_state(), , drop = FALSE]
      }
      scope_keys <- spatial_normalize_keys(cities_meta$code_muni)
      visible_keys <- scope_keys
      subset_parts <- character(0)

      if (!identical(current_state(), SPATIAL_ALL_STATE)) {
        subset_parts <- c(subset_parts, paste("uf", current_state(), sep = "|"))
      }
      if (!identical(current_city(), SPATIAL_ALL_CITY)) {
        visible_keys <- intersect(scope_keys, spatial_normalize_keys(current_city()))
        subset_parts <- c(subset_parts, paste("city", current_city(), sep = "|"))
      } else if (identical(for_level, "municipio") && isTRUE(escala_automatica_ativa())) {
        view_bounds <- spatial_expand_view_bounds(current_map_bounds(), ratio = 0.22)
        if (!is.null(view_bounds)) {
          view_keys <- spatial_keys_in_bounds(
            "municipio",
            bounds = view_bounds,
            project_root = project_root,
            keys = scope_keys
          )
          visible_keys <- intersect(scope_keys, view_keys)
          subset_parts <- c(
            subset_parts,
            paste("view", spatial_bounds_signature(view_bounds, digits = 2L), sep = "|")
          )
        }
      }
      subset_signature <- if (length(subset_parts) > 0) paste(subset_parts, collapse = "||") else "all"
      visible_keys <- spatial_normalize_keys(visible_keys)

      list(
        scope_keys = scope_keys,
        keys = visible_keys,
        subset_signature = subset_signature
      )
    }

    snapshot_cache_key_for <- function(level) {
      period_now <- current_period()
      paste(
        current_variable(),
        level,
        current_region(),
        current_state(),
        format(period_now[[1]], "%Y%m%d"),
        format(period_now[[2]], "%Y%m%d"),
        sep = "|"
      )
    }

    snapshot_preload_allowed_for <- function(level) {
      period_now <- current_period()
      day_span <- as.integer(period_now[[2]] - period_now[[1]]) + 1L
      if (!is.finite(day_span) || day_span < 1L) return(FALSE)

      est_units <- if (identical(level, "state")) {
        length(spatial_state_choices(meta, current_region())) - 1L
      } else if (!identical(current_state(), SPATIAL_ALL_STATE)) {
        sum(meta$cidades$uf %in% current_state())
      } else if (!identical(current_region(), SPATIAL_ALL_REGION)) {
        sum(meta$cidades$regiao %in% current_region())
      } else {
        nrow(meta$cidades)
      }

      (day_span * max(est_units, 1L)) <= 3000000L
    }

    snapshot_preload_allowed <- shiny::reactive({
      snapshot_preload_allowed_for(current_scale())
    })

    shiny::observeEvent(active(), {
      if (!isTRUE(active()) || isTRUE(municipio_background_warm_started())) {
        return(invisible(NULL))
      }
      municipio_background_warm_started(TRUE)
      later::later(function() {
        try(spatial_get_geometry_layer("municipio", project_root = project_root), silent = TRUE)
        try(spatial_geometry_index("municipio", project_root = project_root), silent = TRUE)
      }, delay = 0.35)
    }, ignoreInit = FALSE)

    snapshot_data_for_level <- function(level) {
      if (!isTRUE(input$use_snapshot_date)) {
        return(NULL)
      }
      if (!isTRUE(snapshot_preload_allowed_for(level))) {
        return(NULL)
      }
      key <- snapshot_cache_key_for(level)
      cached <- snapshot_store_get(key)
      if (is.data.frame(cached)) {
        return(cached)
      }
      period_now <- current_period()
      cached <- spatial_query_snapshot_data(
        con = con,
        table_name = query_table_name,
        code = current_variable(),
        start_date = period_now[[1]],
        end_date = period_now[[2]],
        level = level,
        region = current_region(),
        uf = current_state()
      )
      snapshot_store_set(key, cached)
    }

    snapshot_data_cache <- shiny::reactive({
      snapshot_data_for_level(current_scale())
    })

    available_map_dates <- shiny::reactive({
      if (isTRUE(input$use_snapshot_date)) {
        cached <- snapshot_data_cache()
        if (is.data.frame(cached) && nrow(cached) > 0) {
          return(sort(unique(as.Date(cached$data_ref))))
        }
      }
      period_now <- current_period()
      spatial_query_available_dates(
        con = con,
        table_name = query_table_name,
        code = current_variable(),
        start_date = period_now[[1]],
        end_date = period_now[[2]],
        region = current_region(),
        uf = current_state()
      )
    })

    current_snapshot_index <- shiny::reactive({
      dates <- available_map_dates()
      if (length(dates) == 0) return(NA_integer_)
      idx <- suppressWarnings(as.integer(input$snapshot_index %||% length(dates)))
      if (length(idx) == 0 || !is.finite(idx[[1]])) return(length(dates))
      idx <- as.integer(idx[[1]])
      idx <- max(1L, min(idx, length(dates)))
      idx
    })

    current_snapshot_date <- shiny::reactive({
      dates <- available_map_dates()
      idx <- current_snapshot_index()
      if (length(dates) == 0 || !is.finite(idx)) return(as.Date(NA))
      as.Date(dates[[idx]])
    })

    current_frame_key <- shiny::reactive({
      if (isTRUE(use_snapshot_date())) {
        snap_date <- current_snapshot_date()
        if (is.na(snap_date)) return("")
        return(format(as.Date(snap_date), "%Y-%m-%d"))
      }
      period_now <- current_map_period()
      paste(format(period_now[[1]], "%Y-%m-%d"), format(period_now[[2]], "%Y-%m-%d"), sep = "|")
    })

    use_snapshot_date <- shiny::reactive({
      isTRUE(input$use_snapshot_date) &&
        length(available_map_dates()) > 0 &&
        !is.na(current_snapshot_date())
    })

    current_map_period <- shiny::reactive({
      if (isTRUE(use_snapshot_date())) {
        snap_date <- current_snapshot_date()
        return(c(snap_date, snap_date))
      }
      current_period()
    })

    shiny::observeEvent(TRUE, {
      session$sendCustomMessage(
        "ic2025-spatial-prefs-load",
        list(
          inputId = ns("client_prefs"),
          storageKey = prefs_storage_key
        )
      )
    }, once = TRUE, ignoreInit = FALSE)

    shiny::observeEvent(input$client_prefs, {
      payload <- input$client_prefs
      prefs <- payload$prefs %||% payload
      prefs <- spatial_normalize_preferences(prefs, meta = meta)
      saved_preferences(prefs)
      apply_spatial_preferences(prefs)
    }, ignoreInit = TRUE)

    shiny::observeEvent(active(), {
      if (!isTRUE(active()) || isTRUE(tutorial_autostart_checked()) || !isTRUE(tutorial_ready)) {
        return(invisible(NULL))
      }
      tutorial_autostart_checked(TRUE)
      return(invisible(NULL))
    }, ignoreInit = FALSE)

    shiny::observeEvent(TRUE, {
      shiny::updateSelectInput(
        session,
        "region",
        choices = spatial_region_choices(meta),
        selected = current_region()
      )
    }, once = TRUE, ignoreInit = FALSE)

    shiny::observeEvent(current_group(), {
      vars <- spatial_vars_for_group(current_group(), catalog = var_catalog)
      pending_selected <- pending_menu_variable()
      selected <- if (!is.null(pending_selected) && pending_selected %in% vars$code) {
        pending_selected
      } else {
        current_variable()
      }
      shiny::freezeReactiveValue(input, "variable")
      shiny::updateSelectInput(
        session,
        "variable",
        choices = stats::setNames(vars$code, vars$label),
        selected = selected
      )
      if (!is.null(pending_selected) && pending_selected %in% vars$code) {
        pending_menu_variable(NULL)
      }
    }, ignoreInit = FALSE)

    shiny::observeEvent(current_variable(), {
      compare_catalog <- var_catalog[!var_catalog$code %in% current_variable(), , drop = FALSE]
      selected <- current_compare_var()
      shiny::freezeReactiveValue(input, "compare_var")
      shiny::updateSelectInput(
        session,
        "compare_var",
        choices = c("Nenhuma" = "__NONE__", stats::setNames(compare_catalog$code, paste(compare_catalog$group, "·", compare_catalog$label))),
        selected = selected
      )
    }, ignoreInit = FALSE)

    shiny::observeEvent(current_region(), {
      region_now <- current_region()
      state_choices <- spatial_state_choices(meta, region_now)
      state_sel <- current_state()
      if (!state_sel %in% unname(state_choices)) state_sel <- SPATIAL_ALL_STATE
      shiny::freezeReactiveValue(input, "state")
      shiny::updateSelectInput(session, "state", choices = state_choices, selected = state_sel)
    }, ignoreInit = FALSE)

    shiny::observeEvent(current_state(), {
      state_now <- current_state()
      city_choices <- spatial_city_choices(meta, state_now)
      city_sel <- current_city()
      if (!city_sel %in% unname(city_choices)) city_sel <- SPATIAL_ALL_CITY
      shiny::freezeReactiveValue(input, "city")
      shiny::updateSelectInput(session, "city", choices = city_choices, selected = city_sel)
    }, ignoreInit = FALSE)

    shiny::observe({
      if (!isTRUE(escala_automatica_ativa())) {
        if (!identical(zoom_detail_mode(), "state")) zoom_detail_mode("state")
        return(invisible(NULL))
      }
      zoom_now <- current_map_zoom()
      if (!is.finite(zoom_now)) {
        if (!identical(zoom_detail_mode(), "state")) zoom_detail_mode("state")
        return(invisible(NULL))
      }
      if (identical(zoom_detail_mode(), "municipio")) {
        if (zoom_now <= SPATIAL_ZOOM_THRESHOLD_OUT) zoom_detail_mode("state")
      } else {
        if (zoom_now >= SPATIAL_ZOOM_THRESHOLD_IN) zoom_detail_mode("municipio")
      }
    })

    current_scale <- shiny::reactive({
      if (isTRUE(escala_automatica_ativa())) {
        return(zoom_detail_mode())
      }
      if (!identical(current_city(), SPATIAL_ALL_CITY)) {
        return("municipio")
      }
      if (!identical(current_state(), SPATIAL_ALL_STATE)) {
        return("municipio")
      }
      "state"
    })

    shiny::observeEvent(input$map_manual_drill_reset, {
      if (isTRUE(escala_automatica_ativa())) {
        return(invisible(NULL))
      }
      if (!identical(current_city(), SPATIAL_ALL_CITY)) {
        shiny::freezeReactiveValue(input, "city")
        shiny::updateSelectInput(session, "city", selected = SPATIAL_ALL_CITY)
      }
      if (!identical(current_state(), SPATIAL_ALL_STATE)) {
        shiny::freezeReactiveValue(input, "state")
        shiny::updateSelectInput(session, "state", selected = SPATIAL_ALL_STATE)
      }
    }, ignoreInit = TRUE)

    current_scope <- shiny::reactive({
      spatial_scope_from_inputs(
        scale = current_scale(),
        region = current_region(),
        uf = current_state(),
        city = current_city()
      )
    })

    shiny::observeEvent(
      list(input$use_snapshot_date, current_variable(), current_period(), current_scale(), current_region(), current_state()),
      {
        player_direction(0L)
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(input$play_prev, {
      player_direction(-1L)
      player_resume_direction(-1L)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$play_next, {
      player_direction(1L)
      player_resume_direction(1L)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$play_toggle, {
      if (!isTRUE(input$use_snapshot_date)) {
        shiny::updateCheckboxInput(session, "use_snapshot_date", value = TRUE)
      }
      if (identical(player_direction(), 0L)) {
        player_direction(player_resume_direction())
      } else {
        player_direction(0L)
      }
    }, ignoreInit = TRUE)

    shiny::observe({
      if (!isTRUE(active())) {
        if (!identical(player_direction(), 0L)) player_direction(0L)
        return(invisible(NULL))
      }
      if (!isTRUE(use_snapshot_date())) {
        if (!identical(player_direction(), 0L)) player_direction(0L)
        return(invisible(NULL))
      }
      dates <- available_map_dates()
      if (length(dates) <= 1L) {
        if (!identical(player_direction(), 0L)) player_direction(0L)
        return(invisible(NULL))
      }
      if (!identical(applied_frame_key(), current_frame_key())) {
        return(invisible(NULL))
      }
      direction_now <- player_direction()
      if (identical(direction_now, 0L)) {
        return(invisible(NULL))
      }
      next_idx <- current_snapshot_index() + direction_now
      if (next_idx < 1L || next_idx > length(dates)) {
        player_direction(0L)
        return(invisible(NULL))
      }
      shiny::updateSliderInput(session, "snapshot_index", value = next_idx)
    })

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

    theme_drop_click <- shiny::reactive({
      payload <- input[["theme_drop"]]
      if (is.null(payload) || !is.list(payload) || !identical(payload$event %||% "", "page_link")) {
        return(NULL)
      }
      as.character(payload$page %||% "")
    })

    shiny::observeEvent(theme_drop_click(), {
      click_id <- as.character(theme_drop_click() %||% "")
      if (!nzchar(click_id)) return(invisible(NULL))
      if (identical(click_id, "home")) {
        shinyjs::click("back")
        return(invisible(NULL))
      }
      hit <- var_catalog[var_catalog$code %in% click_id, , drop = FALSE]
      if (nrow(hit) == 0) return(invisible(NULL))
      pending_menu_variable(hit$code[[1]])
      shiny::updateSelectInput(session, "group", selected = hit$group[[1]])
      if (identical(hit$group[[1]], current_group())) {
        shiny::updateSelectInput(session, "variable", selected = hit$code[[1]])
        pending_menu_variable(NULL)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$theme_group_fallback, {
      selected_group <- spatial_resolve_group(input$theme_group_fallback, catalog = var_catalog)
      shiny::updateSelectInput(session, "group", selected = selected_group)
    }, ignoreInit = TRUE)

    output$timeline_eyebrow <- shiny::renderText({
      if (isTRUE(use_snapshot_date())) "Dia em análise" else "Modo atual"
    })

    output$timeline_value <- shiny::renderText({
      period_now <- current_period()
      if (isTRUE(use_snapshot_date())) {
        snap_date <- current_snapshot_date()
        if (is.na(snap_date)) return("Sem datas com cobertura")
        synced_value <- applied_timeline_value()
        if (nzchar(synced_value) && identical(applied_frame_key(), current_frame_key())) {
          return(synced_value)
        }
        return(spatial_format_date_pt(snap_date))
      }
      paste0(
        "Média do período ",
        spatial_format_date_pt(period_now[[1]]),
        " a ",
        spatial_format_date_pt(period_now[[2]])
      )
    })

    output$timeline_start_label <- shiny::renderText({
      dates <- available_map_dates()
      if (length(dates) == 0) "" else spatial_format_date_pt(dates[[1]])
    })

    output$timeline_end_label <- shiny::renderText({
      dates <- available_map_dates()
      if (length(dates) == 0) "" else spatial_format_date_pt(dates[[length(dates)]])
    })

    shiny::observeEvent(list(available_map_dates(), input$use_snapshot_date), {
      dates <- available_map_dates()
      idx <- if (length(dates) > 0) current_snapshot_index() else 1L
      shiny::updateSliderInput(
        session,
        "snapshot_index",
        min = 1,
        max = max(length(dates), 1L),
        value = if (length(dates) > 0) idx else 1L
      )
    }, ignoreInit = FALSE)

    shiny::observe({
      show_slider <- isTRUE(input$use_snapshot_date) && length(available_map_dates()) > 0
      shinyjs::toggle(id = "timeline_slider_panel", condition = show_slider)
    })

    shiny::observe({
      player_dir <- player_direction()
      has_controls <- isTRUE(input$use_snapshot_date) && length(available_map_dates()) > 1
      shinyjs::toggle(id = "play_prev", condition = has_controls)
      shinyjs::toggle(id = "play_toggle", condition = has_controls)
      shinyjs::toggle(id = "play_next", condition = has_controls)
      shinyjs::removeClass(selector = paste0("#", ns("play_prev")), class = "is-active")
      shinyjs::removeClass(selector = paste0("#", ns("play_toggle")), class = "is-active")
      shinyjs::removeClass(selector = paste0("#", ns("play_next")), class = "is-active")
      if (identical(player_dir, -1L)) shinyjs::addClass(selector = paste0("#", ns("play_prev")), class = "is-active")
      if (identical(player_dir, 1L)) shinyjs::addClass(selector = paste0("#", ns("play_next")), class = "is-active")
      if (!identical(player_dir, 0L)) shinyjs::addClass(selector = paste0("#", ns("play_toggle")), class = "is-active")
      shiny::updateActionButton(
        session,
        "play_toggle",
        icon = shiny::icon(if (identical(player_dir, 0L)) "play" else "pause")
      )
    })

    shiny::observeEvent(input$map_frame_applied, {
      payload <- input$map_frame_applied
      applied_frame_key(as.character(payload$key %||% ""))
      applied_timeline_value(as.character(payload$displayValue %||% ""))
    }, ignoreInit = TRUE)

    output$variable_title <- shiny::renderText({
      spatial_var_info(current_variable())$label[[1]]
    })

    output$period_badge <- shiny::renderText({
      period_now <- current_period()
      paste0(
        spatial_format_date_pt(period_now[[1]]),
        " até ",
        spatial_format_date_pt(period_now[[2]])
      )
    })

    map_cache_key_for <- function(level, cache_suffix = NULL) {
      if (isTRUE(use_snapshot_date())) {
        snap_date <- current_snapshot_date()
        paste(
          "snapshot",
          current_variable(),
          level,
          current_region(),
          current_state(),
          cache_suffix %||% "all",
          format(as.Date(snap_date), "%Y%m%d"),
          sep = "|"
        )
      } else {
        period_now <- current_map_period()
        paste(
          "period",
          current_variable(),
          level,
          current_region(),
          current_state(),
          cache_suffix %||% "all",
          format(period_now[[1]], "%Y%m%d"),
          format(period_now[[2]], "%Y%m%d"),
          sep = "|"
        )
      }
    }

    map_data_for_level <- function(level, cache_suffix = NULL, keys = NULL) {
      key <- map_cache_key_for(level, cache_suffix = cache_suffix)
      cached <- map_store_get(key)
      if (is.data.frame(cached)) {
        if (is.null(keys)) {
          return(cached)
        }
        return(spatial_filter_map_data_keys(cached, keys))
      }
      out <- if (isTRUE(use_snapshot_date())) {
        snap_date <- current_snapshot_date()
        if (is.na(snap_date)) {
          tibble::tibble()
        } else {
          cached_snap <- snapshot_data_for_level(level)
          if (is.data.frame(cached_snap) && nrow(cached_snap) > 0) {
            out_now <- cached_snap[cached_snap$data_ref %in% as.Date(snap_date), , drop = FALSE]
            out_now$data_ref <- NULL
            out_now
          } else {
            spatial_query_map_data(
              con = con,
              table_name = query_table_name,
              code = current_variable(),
              start_date = snap_date,
              end_date = snap_date,
              level = level,
              region = current_region(),
              uf = current_state()
            )
          }
        }
      } else {
        period_now <- current_map_period()
        spatial_query_map_data(
          con = con,
          table_name = query_table_name,
          code = current_variable(),
          start_date = period_now[[1]],
          end_date = period_now[[2]],
          level = level,
          region = current_region(),
          uf = current_state()
        )
      }
      out <- map_store_set(key, out)
      if (is.null(keys)) {
        out
      } else {
        spatial_filter_map_data_keys(out, keys)
      }
    }

    map_data <- shiny::reactive({
      level <- current_scale()
      if (!identical(level, "municipio")) {
        return(map_data_for_level(level))
      }
      view_ctx <- municipio_view_context("municipio")
      map_data_for_level("municipio", keys = view_ctx$keys)
    })

    legend_reference_data_for_level <- function(level) {
      if (identical(level, "municipio")) {
        return(map_data_for_level("municipio"))
      }
      map_data_for_level(level)
    }

    map_sf <- shiny::reactive({
      dat <- map_data()
      if (identical(current_scale(), "state")) {
        shp <- spatial_get_geometry_layer("state", project_root = project_root)
        out <- dplyr::left_join(shp, dat, by = c("abbrev_state" = "key"))
        out$key <- out$abbrev_state
        out$place_label <- dplyr::if_else(is.na(out$label), out$abbrev_state, out$label)
      } else {
        shp <- spatial_get_geometry_layer("municipio", project_root = project_root)
        if (!identical(current_state(), SPATIAL_ALL_STATE)) {
          shp <- shp[shp$abbrev_state %in% current_state(), , drop = FALSE]
        }
        out <- dplyr::left_join(shp, dat, by = c("code_muni" = "key"))
        out$key <- out$code_muni
        out$place_label <- ifelse(is.na(out$label), paste0(out$code_muni, "/", out$abbrev_state), paste0(out$label, "/", out$abbrev_state))
      }
      out$selected_flag <- FALSE
      if (identical(current_scale(), "municipio") && !identical(current_city(), SPATIAL_ALL_CITY)) {
        out$selected_flag <- out$key %in% as.character(current_city())
      }
      if (identical(current_scale(), "state") && !identical(current_state(), SPATIAL_ALL_STATE)) {
        out$selected_flag <- out$key %in% as.character(current_state())
      }
      out
    })

    current_legend <- shiny::reactive({
      spatial_make_legend(current_variable(), legend_reference_data_for_level(current_scale())$value)
    })

    output$legend_ui <- shiny::renderUI({
      leg <- current_legend()
      if (is.null(leg)) {
        return(htmltools::tags$div(class = "ic2025-spatial-legend-empty", "Sem legenda disponível para o recorte atual."))
      }
      cols <- leg$colors
      bin_labels <- as.character(leg$bin_labels %||% character(0))
      total_cells <- length(cols) + 1L
      htmltools::tags$div(
        class = "ic2025-spatial-legend",
        htmltools::tags$div(class = "ic2025-spatial-legend-head", "Legenda"),
        htmltools::tags$div(
          class = "ic2025-spatial-legend-scale",
          htmltools::tags$div(
            class = "ic2025-spatial-legend-scale-main ic2025-spatial-legend-scale-main--grid",
            style = sprintf("grid-template-columns: repeat(%d, minmax(0, 1fr));", total_cells),
            htmltools::tags$span(
              class = "ic2025-spatial-legend-segment ic2025-spatial-legend-na",
              style = sprintf("background:%s;", as.character(leg$na_color %||% ic2025_theme_value("spatial.legend_na")))
            ),
            lapply(seq_along(cols), function(i) {
              htmltools::tags$span(
                class = "ic2025-spatial-legend-segment",
                style = sprintf("background:%s;", cols[[i]])
              )
            })
          ),
          htmltools::tags$div(
            class = "ic2025-spatial-legend-label-grid",
            style = sprintf("grid-template-columns: repeat(%d, minmax(0, 1fr));", total_cells),
            htmltools::tags$span(
              class = "ic2025-spatial-legend-label-cell",
              htmltools::tags$span(class = "ic2025-spatial-legend-label", "N/A")
            ),
            lapply(seq_along(bin_labels), function(i) {
              htmltools::tags$span(
                class = "ic2025-spatial-legend-label-cell",
                htmltools::tags$span(class = "ic2025-spatial-legend-label", bin_labels[[i]])
              )
            })
          ),
          htmltools::tags$div(
            class = "ic2025-spatial-legend-caption",
            paste0(
              spatial_var_info(current_variable())$label[[1]],
              spatial_var_info(current_variable())$suffix[[1]]
            )
          )
        )
      )
    })

    output$coverage_note <- shiny::renderUI({
      period_now <- current_period()
      end_eff <- spatial_effective_end(current_variable(), period_now[[2]])
      notes <- list()
      if (isTRUE(escala_automatica_ativa()) &&
          identical(current_state(), SPATIAL_ALL_STATE)) {
        notes[[length(notes) + 1L]] <- htmltools::tags$p(
          if (identical(current_scale(), "municipio")) {
            "Os munícipios visíveis na tela estão sendo renderizados."
          } else {
            "Com a escala automática habilitada, basta aproximar o zoom para renderizar os munícipios visíveis na tela."
          }
        )
      } else if (!isTRUE(escala_automatica_ativa())) {
        notes[[length(notes) + 1L]] <- htmltools::tags$p(
          if (identical(current_scale(), "municipio")) {
            "Os municípios do recorte selecionado estão ativos. Para voltar à visão por UFs, afaste o zoom do mapa."
          } else {
            "Com a escala automática desligada, clique em uma UF para abrir os municípios daquele recorte."
          }
        )
      }
      if (as.Date(period_now[[2]]) > end_eff) {
        notes[[length(notes) + 1L]] <- htmltools::tags$p(
          paste0(
            "O indicador selecionado tem cobertura efetiva até ",
            format(end_eff, "%d/%m/%Y"),
            ". O mapa e os resumos foram ajustados automaticamente."
          )
        )
      }
      if (isTRUE(input$use_snapshot_date) && !isTRUE(snapshot_preload_allowed())) {
        notes[[length(notes) + 1L]] <- htmltools::tags$p(
          "A reprodução diária deste recorte está em modo progressivo para evitar excesso de memória em períodos muito longos."
        )
      }
      if (length(notes) == 0) return(NULL)
      htmltools::tags$div(
        class = "ic2025-spatial-note",
        htmltools::tagList(notes)
      )
    })

    payload_cache_key_for <- function(level) {
      frame_key <- current_frame_key()
      if (identical(level, "municipio")) {
        view_ctx <- municipio_view_context(level)
        subset_signature <- view_ctx$subset_signature
      } else {
        subset_signature <- paste(current_region(), current_state(), current_city(), sep = "|")
      }
      paste(
        level,
        current_variable(),
        frame_key,
        current_region(),
        current_state(),
        current_city(),
        subset_signature,
        refresh(),
        sep = "|"
      )
    }

    build_map_scale_payload <- function(level) {
      cache_key <- payload_cache_key_for(level)
      cached <- payload_store_get(cache_key)
      if (is.list(cached) &&
          !is.null(cached$sourceKeyProperty) &&
          !is.null(cached$visibleKeys) &&
          !is.null(cached$featureData)) {
        return(cached)
      }
      has_state_focus <- !identical(current_state(), SPATIAL_ALL_STATE)
      has_city_focus <- !identical(current_city(), SPATIAL_ALL_CITY)
      level_is_municipio <- identical(level, "municipio")
      source_key_property <- if (identical(level, "state")) "abbrev_state" else "code_muni"
      source_url <- spatial_geo_source_url(level, project_root = project_root)
      source_geojson <- NULL
      dat_level <- NULL

      visible_keys_level <- if (identical(level, "state")) {
        states_meta <- meta$estados
        if (!identical(current_region(), SPATIAL_ALL_REGION)) {
          states_meta <- states_meta[states_meta$regiao %in% current_region(), , drop = FALSE]
        }
        spatial_normalize_keys(states_meta$uf)
      } else {
        municipio_view_context(level)$keys
      }

      if (level_is_municipio) {
        dat_level <- tibble::as_tibble(map_data_for_level(level, keys = visible_keys_level))
      } else {
        dat_level <- tibble::as_tibble(map_data_for_level(level))
      }

      legend_reference <- tibble::as_tibble(legend_reference_data_for_level(level))

      if (!"key" %in% names(dat_level)) {
        dat_level$key <- character(0)
      }
      dat_level$key <- trimws(as.character(dat_level$key))
      legend_obj <- spatial_make_legend(current_variable(), legend_reference$value)
      pal <- if (is.null(legend_obj)) {
        function(x) ic2025_theme_value("spatial.legend_fallback")
      } else {
        legend_obj$pal
      }

      if (identical(level, "state")) {
        dat_level$place_label <- ifelse(is.na(dat_level$label), dat_level$key, dat_level$label)
        dat_level$selected_flag <- !identical(current_state(), SPATIAL_ALL_STATE) & dat_level$key %in% as.character(current_state())
      } else {
        dat_level$uf <- trimws(as.character(dat_level$uf %||% ""))
        dat_level$place_label <- ifelse(is.na(dat_level$label), paste0(dat_level$key, "/", dat_level$uf), paste0(dat_level$label, "/", dat_level$uf))
        dat_level$selected_flag <- !identical(current_city(), SPATIAL_ALL_CITY) & dat_level$key %in% as.character(current_city())
      }
      dat_level <- dat_level[dat_level$key %in% visible_keys_level, , drop = FALSE]

      dat_level$fill_color <- ifelse(is.finite(dat_level$value), pal(dat_level$value), ic2025_theme_value("spatial.legend_fallback"))
      dat_level$fill_opacity <- ifelse(
        is.finite(dat_level$value),
        1,
        if (level_is_municipio) 0.16 else 0.45
      )

      feature_data_level <- lapply(seq_len(nrow(dat_level)), function(i) {
        list(
          key = as.character(dat_level$key[[i]]),
          placeLabel = as.character(dat_level$place_label[[i]] %||% dat_level$key[[i]]),
          valueLabel = spatial_format_value(current_variable(), dat_level$value[[i]])[[1]],
          fillColor = as.character(dat_level$fill_color[[i]]),
          fillOpacity = as.numeric(dat_level$fill_opacity[[i]])
        )
      })

      payload_out <- list(
        sourceUrl = source_url,
        geojson = source_geojson,
        sourceKeyProperty = source_key_property,
        visibleKeys = visible_keys_level,
        featureData = feature_data_level
      )
      payload_store_set(cache_key, payload_out)
    }

    build_preload_scale_payload <- function(level) {
      list(
        sourceUrl = spatial_geo_source_url(level, project_root = project_root),
        geojson = NULL,
        sourceKeyProperty = if (identical(level, "municipio")) "code_muni" else "abbrev_state",
        visibleKeys = character(0),
        featureData = list()
      )
    }

    last_map_extent_signature <- shiny::reactiveVal(NULL)
    shiny::observe({
      refresh()
      is_municipio <- identical(current_scale(), "municipio")
      auto_scale <- isTRUE(escala_automatica_ativa())
      has_state_focus <- !identical(current_state(), SPATIAL_ALL_STATE)
      has_city_focus <- !identical(current_city(), SPATIAL_ALL_CITY)
      current_payload <- build_map_scale_payload(current_scale())
      if (is.null(current_payload)) return(invisible(NULL))
      visible_keys <- current_payload$visibleKeys
      alternate_scale <- if (identical(current_scale(), "state")) "municipio" else "state"
      scale_payloads <- stats::setNames(list(current_payload), current_scale())
      should_load_alternate <- FALSE
      if (identical(alternate_scale, "state")) {
        should_load_alternate <- TRUE
      } else if (isTRUE(auto_scale)) {
        zoom_now <- current_map_zoom()
        should_load_alternate <- has_state_focus || has_city_focus ||
          (is.finite(zoom_now) && zoom_now >= (SPATIAL_ZOOM_THRESHOLD_IN - 0.35))
        if (isTRUE(should_load_alternate) &&
            identical(current_state(), SPATIAL_ALL_STATE) &&
            identical(current_city(), SPATIAL_ALL_CITY)) {
          current_map_bounds()
        }
      }
      if (isTRUE(should_load_alternate)) {
        alternate_payload <- build_map_scale_payload(alternate_scale)
        if (!is.null(alternate_payload)) {
          scale_payloads[[alternate_scale]] <- alternate_payload
        }
      } else if (
        !isTRUE(auto_scale) &&
        identical(current_scale(), "state") &&
        identical(current_state(), SPATIAL_ALL_STATE) &&
        identical(current_city(), SPATIAL_ALL_CITY)
      ) {
        scale_payloads[["municipio"]] <- build_preload_scale_payload("municipio")
      }

      extent_signature <- paste(
        current_region(),
        current_state(),
        current_city(),
        sep = "|"
      )
      prior_extent_signature <- last_map_extent_signature()
      default_extent_signature <- paste(SPATIAL_ALL_REGION, SPATIAL_ALL_STATE, SPATIAL_ALL_CITY, sep = "|")
      fit_bounds <- !identical(prior_extent_signature, extent_signature) &&
        !(is.null(prior_extent_signature) && identical(extent_signature, default_extent_signature))
      last_map_extent_signature(extent_signature)

      bbox_payload <- NULL
      if (isTRUE(fit_bounds)) {
        bbox_keys <- if (identical(current_scale(), "municipio") && !identical(current_city(), SPATIAL_ALL_CITY)) {
          as.character(current_city())
        } else if (identical(current_scale(), "municipio")) {
          municipio_view_context("state")$scope_keys
        } else if (identical(current_scale(), "state") && !identical(current_state(), SPATIAL_ALL_STATE)) {
          as.character(current_state())
        } else {
          visible_keys
        }
        bbox_layer <- if (identical(current_scale(), "state")) {
          shp <- spatial_get_geometry_layer("state", project_root = project_root)
          shp[shp$abbrev_state %in% bbox_keys, , drop = FALSE]
        } else {
          shp <- spatial_get_geometry_layer("municipio", project_root = project_root)
          shp[shp$code_muni %in% bbox_keys, , drop = FALSE]
        }
        if (inherits(bbox_layer, "sf") && nrow(bbox_layer) > 0) {
          bbox_payload <- spatial_bbox_payload(bbox_layer)
        }
      }

      metric_label <- spatial_metric_label(current_variable())
      timeline_value_text <- if (isTRUE(use_snapshot_date())) {
        spatial_format_date_pt(current_snapshot_date())
      } else {
        period_now <- current_map_period()
        paste0(
          "Média do período ",
          spatial_format_date_pt(period_now[[1]]),
          " a ",
          spatial_format_date_pt(period_now[[2]])
        )
      }
      fit_padding <- if (is_municipio) {
        list(top = 94, right = 348, bottom = 106, left = 344)
      } else {
        list(top = 84, right = 340, bottom = 98, left = 338)
      }
      fit_max_zoom <- if (is_municipio) {
        if (has_city_focus) 9.8 else if (has_state_focus) 7.4 else 4.9
      } else {
        if (has_state_focus) 5.5 else 4.2
      }

      session$sendCustomMessage(
        "ic2025-spatial-mapbox",
        list(
          id = ns("map"),
          scale = current_scale(),
          activeScale = current_scale(),
          autoScaleEnabled = isTRUE(auto_scale),
          resetScalePayloads = TRUE,
          zoomThresholdIn = SPATIAL_ZOOM_THRESHOLD_IN,
          zoomThresholdOut = SPATIAL_ZOOM_THRESHOLD_OUT,
          clickInputId = ns("map_shape_click"),
          viewStateInputId = ns("map_view_state"),
          drillResetInputId = ns("map_manual_drill_reset"),
          frameAppliedInputId = ns("map_frame_applied"),
          frameKey = current_frame_key(),
          timelineValueText = timeline_value_text,
          timelineValueOutputId = ns("timeline_value"),
          accessToken = spatial_default_mapbox_token(),
          styleUrl = spatial_mapbox_style_url(),
          sourceUrl = current_payload$sourceUrl %||% NULL,
          geojson = current_payload$geojson %||% NULL,
          sourceKeyProperty = current_payload$sourceKeyProperty,
          metricLabel = metric_label,
          visibleKeys = visible_keys,
          featureData = current_payload$featureData,
          scalePayloads = scale_payloads,
          bbox = bbox_payload,
          fitPadding = fit_padding,
          fitMaxZoom = fit_max_zoom,
          fitBounds = fit_bounds
        )
      )
    })

    shiny::observeEvent(input$map_shape_click, {
      click_id <- as.character(input$map_shape_click$id %||% "")
      click_scale <- as.character(input$map_shape_click$scale %||% current_scale())
      if (!nzchar(click_id)) return(invisible(NULL))
      if (identical(click_scale, "state")) {
        shiny::updateSelectInput(session, "state", selected = click_id)
      } else {
        shiny::updateSelectInput(session, "city", selected = click_id)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$clear_selection, {
      if (!identical(current_city(), SPATIAL_ALL_CITY)) {
        shiny::updateSelectInput(session, "city", selected = SPATIAL_ALL_CITY)
      } else if (!identical(current_state(), SPATIAL_ALL_STATE)) {
        shiny::updateSelectInput(session, "state", selected = SPATIAL_ALL_STATE)
      }
    })

    shiny::observeEvent(input$back, {
      session$sendCustomMessage("ic2025-spatial-return", list(target = "desc"))
    }, ignoreInit = TRUE)

    selected_key <- shiny::reactive({
      if (identical(current_scale(), "municipio") && !identical(current_city(), SPATIAL_ALL_CITY)) {
        return(as.character(current_city()))
      }
      if (identical(current_scale(), "state") && !identical(current_state(), SPATIAL_ALL_STATE)) {
        return(as.character(current_state()))
      }
      NULL
    })

    selected_row <- shiny::reactive({
      map_panel_refresh_key()
      key <- isolate(selected_key())
      dat <- isolate(map_data())
      if (is.null(key) || nrow(dat) == 0) return(NULL)
      hit <- dat[dat$key %in% key, , drop = FALSE]
      if (nrow(hit) == 0) return(NULL)
      hit[1, , drop = FALSE]
    })

    single_city_focus <- shiny::reactive({
      identical(current_scope()$type %||% "", "municipio") &&
        !identical(current_city(), SPATIAL_ALL_CITY)
    })

    selected_city_series <- shiny::reactive({
      map_panel_refresh_key()
      if (!isTRUE(single_city_focus())) {
        return(NULL)
      }
      dat <- series_data_cached(isolate(series_refresh_source()))
      if (!is.data.frame(dat) || nrow(dat) == 0) {
        return(NULL)
      }
      dat <- dat[is.finite(dat$valor), , drop = FALSE]
      if (nrow(dat) == 0) {
        return(NULL)
      }
      dat
    })

    output$selection_title <- shiny::renderUI({
      row <- selected_row()
      if (is.null(row)) {
        return(htmltools::tags$span("Panorama Atual"))
      }
      htmltools::tags$span(as.character(row$label[[1]] %||% row$key[[1]]))
    })

    analysis_refresh_source <- shiny::reactive({
      list(
        variable = current_variable(),
        scale = current_scale(),
        region = current_region(),
        state = current_state(),
        city = current_city(),
        frame = current_frame_key()
      )
    })

    analysis_refresh_fast <- shiny::debounce(
      analysis_refresh_source,
      millis = SPATIAL_ACTIVE_REFRESH_MS
    )

    analysis_refresh_slow <- shiny::debounce(
      analysis_refresh_source,
      millis = SPATIAL_BACKGROUND_REFRESH_MS
    )

    map_panel_refresh_key <- shiny::reactive({
      if (identical(current_view_tab(), "map")) {
        analysis_refresh_fast()
      } else {
        analysis_refresh_slow()
      }
    })

    table_panel_refresh_key <- shiny::reactive({
      if (identical(current_view_tab(), "table")) {
        analysis_refresh_fast()
      } else {
        analysis_refresh_slow()
      }
    })

    output$selection_text <- shiny::renderUI({
      map_panel_refresh_key()
      dat <- isolate(map_data())
      if (!is.data.frame(dat) || nrow(dat) == 0) {
        return(htmltools::tags$p("Nenhum dado foi encontrado para os filtros selecionados."))
      }
      row <- isolate(selected_row())
      snapshot_mode <- isTRUE(use_snapshot_date())
      snapshot_date <- current_snapshot_date()
      temporal_prefix <- if (snapshot_mode) {
        paste0("No dia ", spatial_format_date_pt(snapshot_date))
      } else {
        "No período filtrado"
      }
      if (is.null(row)) {
        media <- mean(dat$value, na.rm = TRUE)
        mediana <- stats::median(dat$value, na.rm = TRUE)
        max_row <- dat[which.max(dat$value), , drop = FALSE]
        return(htmltools::tagList(
          htmltools::tags$p(
            paste0(
              "O recorte atual reúne ", nrow(dat), " ",
              if (identical(current_scale(), "state")) "UFs" else "municípios",
              ". ", temporal_prefix, ", a ", spatial_metric_phrase(current_variable(), snapshot = snapshot_mode),
              " ficou em ", spatial_format_value(current_variable(), media),
              ", com mediana de ", spatial_format_value(current_variable(), mediana), "."
            )
          ),
          htmltools::tags$p(
            paste0(
              "O maior valor observado no mapa foi de ",
              spatial_format_value(current_variable(), max_row$value[[1]]),
              " em ", max_row$label[[1]], "."
            )
          )
        ))
      }

      city_series <- isolate(selected_city_series())
      if (isTRUE(single_city_focus()) && is.data.frame(city_series) && nrow(city_series) > 0) {
        min_series_row <- city_series[which.min(city_series$valor), , drop = FALSE]
        max_series_row <- city_series[which.max(city_series$valor), , drop = FALSE]
        return(htmltools::tagList(
          htmltools::tags$p(
            paste0(
              "Em ", row$label[[1]], ", ", if (snapshot_mode) paste0("no dia ", spatial_format_date_pt(snapshot_date), ", a ") else "a ",
              spatial_metric_phrase(current_variable(), snapshot = snapshot_mode),
              " foi de ", spatial_format_value(current_variable(), row$value[[1]]), "."
            )
          ),
          htmltools::tags$p(
            paste0(
              "Considerando apenas a série desta cidade no período filtrado, os registros oscilaram entre ",
              spatial_format_value(current_variable(), min_series_row$valor[[1]]),
              " e ",
              spatial_format_value(current_variable(), max_series_row$valor[[1]]),
              "."
            )
          )
        ))
      }

      stats_now <- spatial_distribution_summary(dat, row$key[[1]])
      htmltools::tagList(
        htmltools::tags$p(
          paste0(
            "Em ", row$label[[1]], ", ", if (snapshot_mode) paste0("no dia ", spatial_format_date_pt(snapshot_date), ", a ") else "a ",
            spatial_metric_phrase(current_variable(), snapshot = snapshot_mode),
            " foi de ", spatial_format_value(current_variable(), row$value[[1]]), "."
          )
        ),
        htmltools::tags$p(
          paste0(
            "Esse resultado está ", spatial_percentile_text(stats_now$percentile),
            ", ocupando a posição ", stats_now$rank_desc,
            " de ", stats_now$n, " no ranking do mapa atual."
          )
        )
      )
    })

    output$did_you_know <- shiny::renderUI({
      map_panel_refresh_key()
      dat <- isolate(map_data())
      if (!is.data.frame(dat) || nrow(dat) == 0) {
        return(htmltools::tags$p("Ajuste os filtros para gerar um destaque automático do recorte."))
      }
      city_series <- isolate(selected_city_series())
      if (isTRUE(single_city_focus()) && is.data.frame(city_series) && nrow(city_series) > 0) {
        frequency_now <- spatial_scalar_string(input$frequency, "D")
        max_row <- city_series[which.max(city_series$valor), , drop = FALSE]
        min_row <- city_series[which.min(city_series$valor), , drop = FALSE]
        max_label <- spatial_format_period_label(max_row$periodo[[1]], frequency = frequency_now)
        min_label <- spatial_format_period_label(min_row$periodo[[1]], frequency = frequency_now)
        selected_now <- isolate(selected_row())
        city_label <- if (is.null(selected_now)) {
          as.character(current_city())
        } else {
          as.character(selected_now$label[[1]] %||% current_city())
        }
        return(htmltools::tagList(
          htmltools::tags$p(
            paste0(
              "No período filtrado, o maior valor de ",
              spatial_var_info(current_variable())$label[[1]],
              " em ",
              city_label,
              " ocorreu em ",
              max_label,
              ": ",
              spatial_format_value(current_variable(), max_row$valor[[1]]),
              "."
            )
          ),
          htmltools::tags$p(
            paste0(
              "Na outra ponta, o menor valor apareceu em ",
              min_label,
              ", com ",
              spatial_format_value(current_variable(), min_row$valor[[1]]),
              "."
            )
          )
        ))
      }
      max_row <- dat[which.max(dat$value), , drop = FALSE]
      min_row <- dat[which.min(dat$value), , drop = FALSE]
      snapshot_mode <- isTRUE(use_snapshot_date())
      snapshot_date <- current_snapshot_date()
      htmltools::tagList(
        htmltools::tags$p(
          paste0(
            if (snapshot_mode) {
              paste0("No dia ", spatial_format_date_pt(snapshot_date), ", ", max_row$label[[1]])
            } else {
              paste0("No período filtrado, ", max_row$label[[1]])
            },
            " registrou o maior valor de ", spatial_var_info(current_variable())$label[[1]],
            ": ", spatial_format_value(current_variable(), max_row$value[[1]]), "."
          )
        ),
        htmltools::tags$p(
          paste0(
            "Na outra ponta, ", min_row$label[[1]],
            " ficou com ", spatial_format_value(current_variable(), min_row$value[[1]]), "."
          )
        )
      )
    })

    build_portrait_plot <- function(dat, compact = FALSE) {
      shiny::validate(shiny::need(nrow(dat) > 0, "Sem série temporal para o recorte atual."))
      var_info <- spatial_var_info(current_variable())
      frequency_now <- spatial_scalar_string(input$frequency, "M")
      series_primary_color <- ic2025_theme_value("spatial.series_primary")
      series_secondary_color <- ic2025_theme_value("spatial.series_secondary")
      paper_color <- ic2025_theme_value("spatial.outline_light")
      transparent_color <- ic2025_theme_value("spatial.transparent")
      card_soft_color <- ic2025_theme_value("spatial.card_soft")
      ink_color <- ic2025_theme_value("spatial.black")
      ink_soft_color <- ic2025_theme_value("spatial.ink_20")
      grid_soft_color <- ic2025_theme_value("spatial.grid_soft")
      grid_color <- ic2025_theme_value("spatial.grid")
      dat$period_label <- spatial_format_period_label(dat$periodo, frequency = frequency_now)
      compare_active <- !identical(current_compare_var(), "__NONE__") && "valor_secundario" %in% names(dat)
      hover_template_primary <- paste0(
        "%{customdata}<br>",
        var_info$label[[1]],
        ": %{text}<extra></extra>"
      )
      p <- plotly::plot_ly(
        data = dat,
        x = ~periodo,
        y = ~valor,
        customdata = ~period_label,
        type = "scatter",
        mode = "lines",
        name = var_info$label[[1]],
        text = ~spatial_format_value(current_variable(), valor),
        hoverinfo = "text",
        hovertemplate = hover_template_primary,
        line = list(color = series_primary_color, width = if (compact) 2.6 else 3.2)
      )
      if (compare_active) {
        sec_info <- spatial_var_info(current_compare_var())
        hover_template_secondary <- paste0(
          "%{customdata}<br>",
          sec_info$label[[1]],
          ": %{text}<extra></extra>"
        )
        p <- p %>% plotly::add_trace(
          y = ~valor_secundario,
          customdata = ~period_label,
          text = ~spatial_format_value(current_compare_var(), valor_secundario),
          name = sec_info$label[[1]],
          yaxis = "y2",
          type = "scatter",
          mode = "lines",
          hoverinfo = "text",
          hovertemplate = hover_template_secondary,
          line = list(color = series_secondary_color, width = if (compact) 2.2 else 2.6, dash = "dot")
        )
      }
      p %>%
        plotly::layout(
          margin = if (compact) list(t = 8, r = if (compare_active) 28 else 10, b = 34, l = 36) else list(t = if (compare_active) 38 else 18, r = 44, b = 52, l = 60),
          paper_bgcolor = transparent_color,
          plot_bgcolor = transparent_color,
          hovermode = "x unified",
          hoverdistance = 22,
          spikedistance = 1000,
          hoverlabel = list(
            bgcolor = card_soft_color,
            bordercolor = ink_color,
            font = list(color = ink_color, size = if (compact) 10 else 11)
          ),
          showlegend = compare_active && !compact,
          legend = list(
            orientation = "h",
            x = 0,
            xanchor = "left",
            y = 1.08,
            yanchor = "bottom",
            font = list(size = if (compact) 10 else 12)
          ),
          xaxis = list(
            title = if (compact) list(text = "Período", font = list(size = 12)) else "",
            tickformat = if (identical(frequency_now, "M")) "%m/%Y" else "%d/%m/%y",
            tickfont = list(size = if (compact) 10 else 12),
            showspikes = !compact,
            spikemode = "across",
            spikecolor = ink_soft_color,
            spikethickness = 1,
            zeroline = FALSE,
            showgrid = TRUE,
            gridcolor = grid_soft_color,
            fixedrange = compact
          ),
          yaxis = list(
            title = if (compact) "" else var_info$label[[1]],
            zeroline = FALSE,
            gridcolor = grid_color,
            showspikes = FALSE,
            tickfont = list(size = if (compact) 10 else 12),
            fixedrange = compact
          ),
          yaxis2 = list(
            title = if (compare_active && !compact) spatial_var_info(current_compare_var())$label[[1]] else NULL,
            overlaying = "y",
            side = "right",
            showgrid = FALSE,
            showspikes = FALSE,
            tickfont = list(size = if (compact) 10 else 12),
            fixedrange = compact
          )
        ) %>%
        plotly::config(
          displayModeBar = FALSE,
          responsive = TRUE,
          staticPlot = FALSE,
          scrollZoom = FALSE,
          doubleClick = "reset"
        )
    }

    output$distribution_plot <- plotly::renderPlotly({
      map_panel_refresh_key()
      dat <- isolate(map_data())
      shiny::validate(shiny::need(nrow(dat) > 0, ""))
      row <- isolate(selected_row())
      city_series <- isolate(selected_city_series())
      use_city_series <- isTRUE(single_city_focus()) && is.data.frame(city_series) && nrow(city_series) > 1
      value_ref <- if (is.null(row)) NA_real_ else suppressWarnings(as.numeric(row$value[[1]]))
      vals <- if (isTRUE(use_city_series)) {
        suppressWarnings(as.numeric(city_series$valor))
      } else {
        suppressWarnings(as.numeric(dat$value))
      }
      vals <- vals[is.finite(vals)]
      shiny::validate(shiny::need(length(vals) > 1, ""))
      legend_obj <- if (isTRUE(use_city_series)) NULL else isolate(current_legend())
      unique_value_count <- length(unique(vals))
      bin_number <- max(5L, min(15L, ceiling(0.8 * unique_value_count)))
      hist_obj <- graphics::hist(
        vals,
        breaks = bin_number,
        plot = FALSE,
        include.lowest = TRUE,
        right = TRUE
      )
      hist_df <- tibble::tibble(
        xmin = head(hist_obj$breaks, -1),
        xmax = tail(hist_obj$breaks, -1),
        x = hist_obj$mids,
        count = hist_obj$counts
      )
      shiny::validate(shiny::need(nrow(hist_df) > 0 && sum(hist_df$count, na.rm = TRUE) > 0, ""))
      hist_df$share <- hist_df$count / sum(hist_df$count)
      hist_base_fill <- ic2025_theme_value("spatial.histogram_fill")
      paper_color <- ic2025_theme_value("spatial.outline_light")
      transparent_color <- ic2025_theme_value("spatial.transparent")
      card_soft_color <- ic2025_theme_value("spatial.card_soft")
      ink_color <- ic2025_theme_value("spatial.black")
      grid_color <- ic2025_theme_value("spatial.grid")
      hist_df$fill <- if (is.null(legend_obj)) {
        rep(hist_base_fill, nrow(hist_df))
      } else {
        bins_ref <- suppressWarnings(as.numeric(legend_obj$bins))
        bins_ref <- bins_ref[is.finite(bins_ref)]
        cols_ref <- legend_obj$colors %||% character(0)
        if (length(bins_ref) >= 2 && length(cols_ref) >= 1) {
          idx <- findInterval(hist_df$x, vec = bins_ref, rightmost.closed = TRUE, all.inside = TRUE)
          idx <- pmax(1L, pmin(idx, length(cols_ref)))
          cols_ref[idx]
        } else {
          rep(hist_base_fill, nrow(hist_df))
        }
      }
      label_digits <- legend_obj$label_digits %||% spatial_legend_label_digits(c(hist_df$xmin, hist_df$xmax))
      hist_df$left_label <- spatial_format_break_label(current_variable(), hist_df$xmin, include_suffix = TRUE, digits = label_digits)
      hist_df$right_label <- spatial_format_break_label(current_variable(), hist_df$xmax, include_suffix = TRUE, digits = label_digits)
      hist_df$count_label <- format(hist_df$count, big.mark = ".", decimal.mark = ",", trim = TRUE)
      hist_df$bin_width <- pmax(hist_df$xmax - hist_df$xmin, 1e-9)
      hist_df$hover_text <- paste0(
        "Faixa: ", hist_df$left_label, " a ", hist_df$right_label,
        "<br>Participação: ", format(round(hist_df$share * 100, 1), nsmall = 1, decimal.mark = ",", trim = TRUE), "%",
        "<br>Localidades: ", hist_df$count_label
      )
      p <- plotly::plot_ly(
        data = hist_df,
        x = ~x,
        y = ~share,
        customdata = ~hover_text,
        hovertemplate = "%{customdata}<extra></extra>"
      ) %>%
        plotly::add_bars(
          width = hist_df$bin_width,
          marker = list(color = hist_df$fill, line = list(color = paper_color, width = 1.1))
        ) %>%
        plotly::layout(
          margin = list(t = 6, r = 8, b = 42, l = 34),
          paper_bgcolor = transparent_color,
          plot_bgcolor = transparent_color,
          bargap = 0,
          hoverlabel = list(
            bgcolor = card_soft_color,
            bordercolor = ink_color,
            font = list(color = ink_color, size = 11)
          ),
          xaxis = list(
            title = spatial_var_info(current_variable())$label[[1]],
            zeroline = FALSE,
            showgrid = FALSE,
            tickfont = list(size = 11),
            fixedrange = TRUE
          ),
          yaxis = list(
            title = "",
            zeroline = FALSE,
            showgrid = TRUE,
            gridcolor = grid_color,
            tickformat = ".0%",
            tickfont = list(size = 11),
            fixedrange = TRUE
          )
        )
      if (is.finite(value_ref)) {
        p <- p %>% plotly::layout(
          shapes = list(
            list(
              type = "line",
              x0 = value_ref, x1 = value_ref,
              y0 = 0, y1 = 1, yref = "paper",
              line = list(color = ink_color, width = 3)
            )
          )
        )
      }
      p %>%
        plotly::config(displayModeBar = FALSE, responsive = TRUE, scrollZoom = FALSE, staticPlot = FALSE)
    })

    output$quick_portrait_plot <- plotly::renderPlotly({
      build_portrait_plot(quick_series_data(), compact = TRUE)
    })

    table_page_size <- SPATIAL_TABLE_PAGE_LENGTH
    table_page_limit <- SPATIAL_TABLE_MAX_PAGES
    table_row_cap <- table_page_size * table_page_limit

    table_uses_series <- shiny::reactive({
      isTRUE(single_city_focus())
    })

    table_full_data <- shiny::reactive({
      table_panel_refresh_key()
      if (isTRUE(table_uses_series())) {
        series_dat <- series_data_cached(isolate(series_refresh_source()))
        shiny::validate(shiny::need(is.data.frame(series_dat) && nrow(series_dat) > 0, "Nenhum dado para exibir em tabela."))
        frequency_now <- spatial_scalar_string(input$frequency, "D")
        value_col <- spatial_var_info(current_variable())$label[[1]]
        tab <- tibble::tibble(
          `Período` = spatial_format_period_label(series_dat$periodo, frequency = frequency_now),
          .periodo_raw = as.Date(series_dat$periodo),
          .key = format(as.Date(series_dat$periodo), "%Y-%m-%d"),
          .raw_value = suppressWarnings(as.numeric(series_dat$valor))
        )
        tab[[value_col]] <- vapply(tab$.raw_value, function(x) spatial_format_value(current_variable(), x), character(1))
        tab <- tab[order(tab$.periodo_raw), c("Período", value_col, ".periodo_raw", ".key", ".raw_value"), drop = FALSE]
        rownames(tab) <- NULL
        return(tab)
      }

      dat <- isolate(map_data())
      shiny::validate(shiny::need(nrow(dat) > 0, "Nenhum dado para exibir em tabela."))

      if (identical(current_scale(), "state")) {
        tab <- dat[, c("label", "key", "value"), drop = FALSE]
        names(tab) <- c("Estado", "UF", spatial_var_info(current_variable())$label[[1]])
      } else {
        keep_cols <- intersect(c("label", "uf", "value", "key"), names(dat))
        tab <- dat[, keep_cols, drop = FALSE]
        if (!"key" %in% names(tab)) tab$key <- dat$key
        names(tab)[seq_len(4L)] <- c("Município", "UF", spatial_var_info(current_variable())$label[[1]], ".key")
      }

      value_col <- spatial_var_info(current_variable())$label[[1]]
      if (!".key" %in% names(tab)) {
        tab$.key <- tab[[2]]
      }
      tab$.raw_value <- suppressWarnings(as.numeric(tab[[value_col]]))
      tab[[value_col]] <- vapply(tab$.raw_value, function(x) spatial_format_value(current_variable(), x), character(1))
      tab <- tab[order(-tab$.raw_value, tab[[1]], na.last = TRUE), , drop = FALSE]
      rownames(tab) <- NULL
      tab
    })

    table_display_data <- shiny::reactive({
      utils::head(table_full_data(), table_row_cap)
    })

    output$table <- DT::renderDT({
      tab <- table_display_data()
      display_cols <- setdiff(names(tab), c(".raw_value", ".key", ".periodo_raw"))
      DT::datatable(
        tab[, display_cols, drop = FALSE],
        rownames = FALSE,
        options = list(
          pageLength = table_page_size,
          autoWidth = TRUE,
          dom = "tip",
          paging = TRUE,
          lengthChange = FALSE,
          ordering = FALSE,
          searching = FALSE,
          pagingType = "full_numbers",
          language = list(
            info = "Mostrando _START_ a _END_ de _TOTAL_ entradas",
            infoEmpty = "Mostrando 0 a 0 de 0 entradas",
            infoFiltered = "",
            paginate = list(first = "Primeira", previous = "Anterior", `next` = "Próxima", last = "Última")
          )
        ),
        selection = if (isTRUE(table_uses_series())) "none" else "single"
      )
    }, server = FALSE)
    shiny::outputOptions(output, "table", suspendWhenHidden = TRUE)

    shiny::observeEvent(input$table_rows_selected, {
      if (isTRUE(table_uses_series())) return(invisible(NULL))
      idx <- suppressWarnings(as.integer(input$table_rows_selected %||% NA_integer_))
      if (!is.finite(idx) || idx < 1L) return(invisible(NULL))
      tab <- table_display_data()
      if (nrow(tab) < idx) return(invisible(NULL))
      selected_key <- as.character(tab$.key[[idx]] %||% "")
      if (!nzchar(selected_key)) return(invisible(NULL))
      if (identical(current_scale(), "state")) {
        shiny::updateSelectInput(session, "state", selected = selected_key)
      } else {
        shiny::updateSelectInput(session, "city", selected = selected_key)
      }
      shiny::updateTabsetPanel(session, "view_tabs", selected = "map")
      shinyjs::removeClass(selector = paste0("#", ns("view_table")), class = "is-active")
      shinyjs::removeClass(selector = paste0("#", ns("view_portrait")), class = "is-active")
      shinyjs::addClass(selector = paste0("#", ns("view_map")), class = "is-active")
      session$sendCustomMessage("ic2025-spatial-resize", list())
    }, ignoreInit = TRUE)

    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        if (isTRUE(table_uses_series())) {
          paste0("visualizacao_espacial_serie_", current_variable(), "_", current_city(), ".csv")
        } else {
          paste0("visualizacao_espacial_", tolower(current_scale()), "_", current_variable(), ".csv")
        }
      },
      content = function(file) {
        export_tab <- table_full_data()
        export_tab <- export_tab[, setdiff(names(export_tab), c(".raw_value", ".key", ".periodo_raw")), drop = FALSE]
        utils::write.csv(export_tab, file, row.names = FALSE, fileEncoding = "UTF-8")
      },
      contentType = "text/csv"
    )

    output$download_rds <- shiny::downloadHandler(
      filename = function() {
        if (isTRUE(table_uses_series())) {
          paste0("visualizacao_espacial_serie_", current_variable(), "_", current_city(), ".rds")
        } else {
          paste0("visualizacao_espacial_", tolower(current_scale()), "_", current_variable(), ".rds")
        }
      },
      content = function(file) {
        export_tab <- table_full_data()
        export_tab <- export_tab[, setdiff(names(export_tab), c(".raw_value", ".key", ".periodo_raw")), drop = FALSE]
        saveRDS(export_tab, file = file)
      },
      contentType = "application/octet-stream"
    )

    output$table_info <- shiny::renderUI({
      tab_full <- table_full_data()
      tab_display <- table_display_data()
      total_rows <- nrow(tab_full)
      shown_rows <- nrow(tab_display)
      if (isTRUE(table_uses_series())) {
        return(htmltools::tags$div(
          class = "ic2025-spatial-table-info",
          htmltools::tags$h4("Visão geral"),
          htmltools::tags$p(
            class = "ic2025-spatial-table-note",
            paste0(
              "A tabela resume a série temporal da cidade selecionada, usando a mesma frequência da série. Mostrando ",
              shown_rows,
              " de ",
              total_rows,
              " linhas."
            )
          ),
          htmltools::tags$p(
            class = "ic2025-spatial-table-note",
            "Os botões acima exportam a série temporal atual em .csv ou .rds."
          ),
          if (total_rows > table_row_cap) {
            htmltools::tags$p(
              class = "ic2025-spatial-table-note is-muted",
              paste0(
                "A visualização foi limitada às primeiras ",
                table_row_cap,
                " linhas (15 páginas de 15) para preservar desempenho. Os downloads usam a série completa do recorte atual."
              )
            )
          }
        ))
      }
      htmltools::tags$div(
        class = "ic2025-spatial-table-info",
        htmltools::tags$h4("Visão geral"),
        htmltools::tags$p(
          class = "ic2025-spatial-table-note",
          paste0(
            "A tabela resume o recorte atual em escala ",
            if (identical(current_scale(), "state")) "estadual" else "municipal",
            ". Selecione uma linha para focar o mapa nessa localidade. Mostrando ",
            shown_rows,
            " de ",
            total_rows,
            " linhas."
          )
        ),
        htmltools::tags$p(
          class = "ic2025-spatial-table-note",
          "Os botões acima exportam o recorte atual completo em .csv ou .rds."
        ),
        if (total_rows > table_row_cap) {
          htmltools::tags$p(
            class = "ic2025-spatial-table-note is-muted",
            paste0(
              "A visualização foi limitada às primeiras ",
              table_row_cap,
              " linhas (15 páginas de 15) para preservar desempenho. Os downloads usam o conjunto completo do recorte atual."
            )
          )
        }
      )
    })

    shiny::observeEvent(input$view_map, {
      shiny::updateTabsetPanel(session, "view_tabs", selected = "map")
      shinyjs::removeClass(selector = paste0("#", ns("view_table")), class = "is-active")
      shinyjs::removeClass(selector = paste0("#", ns("view_portrait")), class = "is-active")
      shinyjs::addClass(selector = paste0("#", ns("view_map")), class = "is-active")
      session$sendCustomMessage("ic2025-spatial-resize", list())
    })
    shiny::observeEvent(input$view_table, {
      shiny::updateTabsetPanel(session, "view_tabs", selected = "table")
      shinyjs::removeClass(selector = paste0("#", ns("view_map")), class = "is-active")
      shinyjs::removeClass(selector = paste0("#", ns("view_portrait")), class = "is-active")
      shinyjs::addClass(selector = paste0("#", ns("view_table")), class = "is-active")
    })
    shiny::observeEvent(input$view_portrait, {
      shiny::updateTabsetPanel(session, "view_tabs", selected = "portrait")
      shinyjs::removeClass(selector = paste0("#", ns("view_map")), class = "is-active")
      shinyjs::removeClass(selector = paste0("#", ns("view_table")), class = "is-active")
      shinyjs::addClass(selector = paste0("#", ns("view_portrait")), class = "is-active")
    })

    shiny::observe({
      view_now <- current_view_tab()
      shinyjs::toggle(
        selector = paste0("#", ns("help")),
        condition = identical(view_now, "map")
      )
      shinyjs::toggle(
        selector = paste0("#", ns("compare_panel")),
        condition = identical(view_now, "portrait")
      )
      shinyjs::toggle(
        selector = paste0("#", ns("frequency_panel")),
        condition = identical(view_now, "portrait")
      )
    })

    series_scope_cache_key <- function(scope) {
      type <- as.character(scope$type %||% "brasil")
      values <- sort(as.character(scope$value %||% ""))
      paste(type, paste(values, collapse = ","), sep = "::")
    }

    series_refresh_source <- shiny::reactive({
      period_now <- current_period()
      scope_now <- current_scope()
      list(
        variable = current_variable(),
        compare = current_compare_var(),
        start = format(period_now[[1]], "%Y-%m-%d"),
        end = format(period_now[[2]], "%Y-%m-%d"),
        frequency = spatial_scalar_string(input$frequency, "D"),
        scope = scope_now,
        scope_key = series_scope_cache_key(scope_now)
      )
    })

    series_refresh_fast <- shiny::debounce(
      series_refresh_source,
      millis = SPATIAL_ACTIVE_SERIES_REFRESH_MS
    )

    series_refresh_slow <- shiny::debounce(
      series_refresh_source,
      millis = SPATIAL_BACKGROUND_REFRESH_MS
    )

    quick_series_refresh_key <- shiny::reactive({
      if (identical(current_view_tab(), "map")) {
        series_refresh_fast()
      } else {
        series_refresh_slow()
      }
    })

    portrait_series_refresh_key <- shiny::reactive({
      if (identical(current_view_tab(), "portrait")) {
        series_refresh_fast()
      } else {
        series_refresh_slow()
      }
    })

    series_data_cached <- function(signature) {
      cache_key <- paste(
        signature$variable,
        signature$compare,
        signature$start,
        signature$end,
        signature$frequency,
        signature$scope_key,
        sep = "|"
      )
      cached <- series_store_get(cache_key)
      if (!is.null(cached)) {
        return(cached)
      }
      dat <- spatial_query_series_data(
        con = con,
        table_name = query_table_name,
        primary_code = signature$variable,
        secondary_code = signature$compare,
        start_date = signature$start,
        end_date = signature$end,
        frequency = signature$frequency,
        scope = signature$scope
      )
      series_store_set(cache_key, dat)
    }

    quick_series_data <- shiny::reactive({
      shiny::req(isTRUE(active()))
      quick_series_refresh_key()
      series_data_cached(isolate(series_refresh_source()))
    })

    portrait_series_data <- shiny::reactive({
      shiny::req(isTRUE(active()))
      portrait_series_refresh_key()
      series_data_cached(isolate(series_refresh_source()))
    })

    output$portrait_title <- shiny::renderUI({
      portrait_series_refresh_key()
      scope <- isolate(current_scope())
      alvo <- switch(
        scope$type,
        municipio = {
          hit <- meta$cidades[meta$cidades$code_muni %in% scope$value, , drop = FALSE]
          if (nrow(hit) == 0) "Município" else hit$rotulo[[1]]
        },
        uf = {
          hit <- meta$estados[meta$estados$uf %in% scope$value, , drop = FALSE]
          if (nrow(hit) == 0) scope$value else hit$rotulo[[1]]
        },
        regiao = scope$value,
        "Brasil"
      )
      htmltools::tags$span(
        paste0("Série temporal de ", alvo)
      )
    })
    shiny::outputOptions(output, "portrait_title", suspendWhenHidden = TRUE)

    output$portrait_plot <- plotly::renderPlotly({
      build_portrait_plot(portrait_series_data(), compact = FALSE)
    })
    shiny::outputOptions(output, "portrait_plot", suspendWhenHidden = TRUE)
    shiny::outputOptions(output, "quick_portrait_plot", suspendWhenHidden = FALSE)

    output$overlay_ui <- shiny::renderUI({
      mode <- overlay_mode()
      if (is.null(mode) || !nzchar(mode)) {
        return(NULL)
      }

      card_title <- switch(
        mode,
        settings = "Opções avançadas",
        info = spatial_var_info(current_variable())$label[[1]],
        "Painel"
      )

      card_body <- switch(
        mode,
        settings = spatial_settings_panel_ui(
          ns = ns,
          meta = meta,
          prefs = saved_preferences(),
          current_region = current_region(),
          current_state = current_state(),
          current_city = current_city()
        ),
        info = {
          info <- spatial_var_info(current_variable())
          period_now <- current_period()
          htmltools::tagList(
            htmltools::tags$p(
              paste0(
                "Este indicador é exibido na Visualização Espacial com ",
                if (identical(info$agg[[1]], "sum")) "soma" else "média",
                " no mapa e na série temporal, sempre respeitando o recorte geográfico atual."
              )
            ),
            htmltools::tags$ul(
              htmltools::tags$li(paste0("Cobertura disponível até ", spatial_format_date_pt(info$available_end[[1]]), ".")),
              htmltools::tags$li(paste0("Janela atualmente filtrada: ", spatial_format_date_pt(period_now[[1]]), " até ", spatial_format_date_pt(period_now[[2]]), ".")),
              htmltools::tags$li("O painel da direita resume o recorte atual ou a localidade clicada no mapa.")
            )
          )
        },
        htmltools::tags$p("Sem conteúdo disponível.")
      )

      htmltools::tags$div(
        id = ns("overlay_shell"),
        class = "ic2025-spatial-overlay",
        htmltools::tags$button(
          id = ns("overlay_backdrop"),
          type = "button",
          class = "ic2025-spatial-overlay-backdrop",
          `aria-label` = "Fechar painel"
        ),
        htmltools::tags$div(
          class = "ic2025-spatial-overlay-card",
          htmltools::tags$div(
            class = "ic2025-spatial-overlay-head",
            htmltools::tags$h3(class = "ic2025-spatial-overlay-title", card_title),
            shiny::actionButton(ns("overlay_close"), label = NULL, icon = shiny::icon("xmark"), class = "ic2025-spatial-overlay-close")
          ),
          htmltools::tags$div(class = "ic2025-spatial-overlay-body", card_body)
        )
      )
    })

    shiny::observeEvent(input$panel_info, {
      if (!isTRUE(active())) return(invisible(NULL))
      session$sendCustomMessage("ic2025-spatial-introjs-cleanup", list())
      overlay_mode("info")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$help, {
      if (!isTRUE(active())) return(invisible(NULL))
      session$sendCustomMessage("ic2025-spatial-introjs-cleanup", list())
      if (!identical(as.character(input$view_tabs %||% "map"), "map")) {
        shiny::updateTabsetPanel(session, "view_tabs", selected = "map")
        shinyjs::removeClass(selector = paste0("#", ns("view_table")), class = "is-active")
        shinyjs::removeClass(selector = paste0("#", ns("view_portrait")), class = "is-active")
        shinyjs::addClass(selector = paste0("#", ns("view_map")), class = "is-active")
        session$sendCustomMessage("ic2025-spatial-resize", list())
      }
      if (!isTRUE(tutorial_ready)) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Tutorial indisponível",
            htmltools::tags$p("O pacote do tour guiado não foi carregado nesta sessão. Reabra o app para tentar novamente."),
            easyClose = TRUE,
            footer = shiny::modalButton("Fechar")
          )
        )
        return(invisible(NULL))
      }
      session$sendCustomMessage(
        "ic2025-spatial-tutorial-mark-seen",
        list(storageKey = tutorial_storage_key)
      )
      shinyjs::delay(
        120,
        rintrojs::introjs(
          session,
          options = list(
            steps = spatial_tutorial_steps(ns),
            nextLabel = "Próximo",
            prevLabel = "Voltar",
            doneLabel = "Concluir"
          )
        )
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$coverage, {
      if (!isTRUE(active())) return(invisible(NULL))
      session$sendCustomMessage("ic2025-spatial-introjs-cleanup", list())
      overlay_mode("settings")
    }, ignoreInit = TRUE)

    shiny::observeEvent(list(input$overlay_close, input$overlay_backdrop), {
      overlay_mode(NULL)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$settings_region_default, {
      prefs <- saved_preferences()
      prefs$region_default <- spatial_scalar_string(input$settings_region_default, SPATIAL_ALL_REGION)
      persist_spatial_preferences(prefs)
      if (is.null(prefs$location_default)) {
        apply_spatial_preferences(prefs)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$settings_lock_region, {
      region_now <- spatial_scalar_string(input$settings_lock_region, SPATIAL_ALL_REGION)
      state_choices <- spatial_state_choices(meta, region_now)
      state_selected <- spatial_scalar_string(input$settings_lock_state, SPATIAL_ALL_STATE)
      if (!state_selected %in% unname(state_choices)) {
        state_selected <- SPATIAL_ALL_STATE
      }
      shiny::freezeReactiveValue(input, "settings_lock_state")
      shiny::updateSelectInput(session, "settings_lock_state", choices = state_choices, selected = state_selected)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$settings_lock_state, {
      state_now <- spatial_scalar_string(input$settings_lock_state, SPATIAL_ALL_STATE)
      city_choices <- spatial_city_choices(meta, state_now)
      city_selected <- spatial_scalar_string(input$settings_lock_city, SPATIAL_ALL_CITY)
      if (!city_selected %in% unname(city_choices)) {
        city_selected <- SPATIAL_ALL_CITY
      }
      shiny::freezeReactiveValue(input, "settings_lock_city")
      shiny::updateSelectInput(session, "settings_lock_city", choices = city_choices, selected = city_selected)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$settings_use_current, {
      shiny::freezeReactiveValue(input, "settings_lock_region")
      shiny::freezeReactiveValue(input, "settings_lock_state")
      shiny::freezeReactiveValue(input, "settings_lock_city")
      shiny::updateSelectInput(session, "settings_lock_region", choices = spatial_region_choices(meta), selected = current_region())
      shiny::updateSelectInput(session, "settings_lock_state", choices = spatial_state_choices(meta, current_region()), selected = current_state())
      shiny::updateSelectInput(session, "settings_lock_city", choices = spatial_city_choices(meta, current_state()), selected = current_city())
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$settings_save_location, {
      prefs <- saved_preferences()
      prefs$location_default <- spatial_normalize_saved_location(
        list(
          region = input$settings_lock_region,
          state = input$settings_lock_state,
          city = input$settings_lock_city
        ),
        meta = meta
      )
      persist_spatial_preferences(prefs)
      apply_spatial_preferences(prefs)
      shiny::showNotification("Localização padrão salva.", type = "default")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$settings_clear_default_location, {
      prefs <- saved_preferences()
      prefs$location_default <- NULL
      persist_spatial_preferences(prefs)
      apply_spatial_preferences(prefs)
      shiny::freezeReactiveValue(input, "settings_lock_region")
      shiny::freezeReactiveValue(input, "settings_lock_state")
      shiny::freezeReactiveValue(input, "settings_lock_city")
      shiny::updateSelectInput(session, "settings_lock_region", choices = spatial_region_choices(meta), selected = prefs$region_default)
      shiny::updateSelectInput(session, "settings_lock_state", choices = spatial_state_choices(meta, prefs$region_default), selected = SPATIAL_ALL_STATE)
      shiny::updateSelectInput(session, "settings_lock_city", choices = spatial_city_choices(meta, SPATIAL_ALL_STATE), selected = SPATIAL_ALL_CITY)
      shiny::showNotification("Localização padrão removida.", type = "default")
    }, ignoreInit = TRUE)
  })
}
