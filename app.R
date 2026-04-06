suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(fresh)
  library(plotly)
  library(DT)
  library(dplyr)
  library(stringr)
  library(tibble)
})

# Usa ragg no renderPlot quando disponível, melhorando nitidez sem inflar o
# tamanho visual dos gráficos dentro do dashboard.
options(shiny.useragg = TRUE)

#### IC2025 Theme Colors #####################################################
ic2025_theme_flatten <- function(x, prefix = NULL) {
  out <- list()
  nms <- names(x)
  for (idx in seq_along(x)) {
    key <- if (!is.null(nms) && nzchar(nms[[idx]])) nms[[idx]] else as.character(idx)
    full_key <- if (is.null(prefix) || !nzchar(prefix)) key else paste(prefix, key, sep = ".")
    value <- x[[idx]]
    if (is.list(value) && !is.null(names(value))) {
      out <- c(out, ic2025_theme_flatten(value, prefix = full_key))
    } else {
      out[[full_key]] <- as.character(value)
    }
  }
  out
}

cor_com_opacidade <- function(cor_hex, opacidade = 1, formato = c("css", "grafico")) {
  formato <- match.arg(formato)
  if (is.null(opacidade) || length(opacidade) == 0) {
    opacidade <- 1
  }
  opacidade <- suppressWarnings(as.numeric(opacidade))
  if (!is.finite(opacidade)) opacidade <- 1
  opacidade <- max(0, min(1, opacidade))
  if (identical(formato, "grafico")) {
    return(grDevices::adjustcolor(cor_hex, alpha.f = opacidade))
  }
  rgb <- grDevices::col2rgb(cor_hex)
  sprintf("rgba(%d, %d, %d, %.3f)", rgb[1], rgb[2], rgb[3], opacidade)
}

cor_css <- function(cor_hex, opacidade = 1) {
  cor_com_opacidade(cor_hex, opacidade = opacidade, formato = "css")
}

cor_grafico <- function(cor_hex, opacidade = 1) {
  cor_com_opacidade(cor_hex, opacidade = opacidade, formato = "grafico")
}

IC2025_THEME_COLORS <- list(
  # ######################## Dashboard principal ########################
  dashboard = list(
    # ---------- Estrutura global, header e navegação ----------
    header = "#0B5D49",
    header_top = "#0E7C66",
    header_bottom = "#0B5D49",
    header_nav_bg = "#10B981",
    header_nav_text = "#FFFFFF",
    header_hover_bg = cor_css("#FFFFFF", 0.10),
    header_logo_bg = "#0B5D49",
    header_logo_text = "#FFFFFF",
    header_border = cor_css("#FFFFFF", 0.08),
    header_nav_shadow = cor_css("#101827", 0.08),
    dropdown_border = cor_css("#101827", 0.08),
    dropdown_shadow = cor_css("#101827", 0.18),
    accent = "#10B981",
    accent_hover = "#0FA775",
    accent_dark = "#047857",
    accent_deep = "#0B5D49",
    accent_soft = "#A7F3D0",
    accent_border = "#18A87C",
    ink = "#101827",
    ink_mid = "#2D3B50",
    ink_soft = "#1F2937",
    muted = "#6B7280",
    muted_alt = "#6B7A90",
    muted_soft = "#8A94A6",
    focus = "#1F77B4",
    focus_ring = cor_css("#1F77B4", 0.16),
    surface = "#FFFFFF",
    surface_alt = "#F4F7FB",
    surface_soft = "#F9FAFB",
    surface_line = "#E5E7EB",
    transparent = cor_grafico("#000000", 0),
    border = "#D9E2EC",
    shadow = cor_css("#101827", 0.18),
    shadow_soft = cor_css("#101827", 0.08),
    shadow_medium = cor_css("#101827", 0.12),
    shadow_dark = cor_css("#000000", 0.18),
    grid = "#E6EDF5",
    sidebar_bg = "#064E3B",
    sidebar_border = cor_css("#FFFFFF", 0.08),
    sidebar_shadow = cor_css("#101827", 0.16),
    sidebar_label = cor_css("#FFFFFF", 0.58),
    sidebar_text = cor_css("#FFFFFF", 0.88),
    sidebar_icon = cor_css("#FFFFFF", 0.86),
    sidebar_hover_bg = cor_css("#FFFFFF", 0.10),
    sidebar_submenu_bg = cor_css("#FFFFFF", 0.05),
    sidebar_submenu_border = cor_css("#FFFFFF", 0.06),
    sidebar_submenu_shadow = cor_css("#000000", 0.10),
    sidebar_active_bg = sprintf("linear-gradient(90deg, %s, %s)", cor_css("#10B981", 0.36), cor_css("#10B981", 0.08)),
    sidebar_active_border = cor_css("#A7F3D0", 0.28),
    flat_active_bg = cor_css("#10B981", 0.26),
    flat_active_accent = cor_css("#A7F3D0", 0.9),
    page_glow_left = cor_css("#10B981", 0.18),
    page_glow_right = cor_css("#1F77B4", 0.10),
    box_outline = "#10B981",
    box_body_bg = "#FFFFFF",
    box_header_bg = "#FFFFFF",
    solid_header_bg = "#10B981",
    solid_header_text = "#FFFFFF",
    pill_text = "#315E52",
    pill_border = cor_css("#10B981", 0.18),
    pill_active_bg = sprintf("linear-gradient(180deg, %s, %s)", cor_css("#DFF4EB", 1), cor_css("#CCEBDF", 1)),
    pill_active_border = cor_css("#0B5D49", 0.95),
    pill_active_outline = cor_css("#0B5D49", 0.12),
    pill_active_text = "#0B5D49",
    tab_text = "#5B6B7F",
    table_head_bg = "#F7F9FC",
    loading_overlay_bg = sprintf("linear-gradient(180deg, %s 0%%, %s 100%%)", cor_css("#FFFFFF", 0.72), cor_css("#FFFFFF", 0.56)),
    loading_card_bg = cor_css("#FFFFFF", 0.94),
    loading_card_border = cor_css("#C3E8DF", 0.95),
    loading_card_shadow = cor_css("#0F4C3A", 0.10),
    loading_spinner_track = cor_css("#18A87C", 0.18),
    loading_spinner_head = "#18A87C",
    progress_track = "#E5E7EB",
    progress_fill = "#10B981",
    progress_text = "#4B5563"
  ),
  # ######################## Análise descritiva ########################
  desc = list(
    temp_mean = "#3A86FF",
    temp_min = "#8338EC",
    temp_max = "#FB5607",
    alt_pink = "#FF006E",
    alt_teal = "#2A9D8F",
    alt_gray = "#6B7280",
    violet = "#6D28D9"
  ),
  # ################ Simulações e avaliação de modelos ################
  model = list(
    # ---------- Séries principais e bandas ----------
    primary = "#1F77B4",
    primary_band = cor_css("#1F77B4", 0.20),
    black = "#111111",
    black_soft = cor_css("#111111", 0.55),
    seasonal_band = cor_css("#10B981", 0.18),
    seasonal_line = "#047857",
    transparent = cor_grafico("#000000", 0)
  ),
  # ######################## Aplicações e mapas ########################
  funcoes = list(
    # ---------- Sidebar, mapas auxiliares e caixas ----------
    sidebar_bg = "#064E3B",
    sidebar_hover = "#0E7C66",
    sidebar_accent = "#10B981",
    sidebar_light = "#34D399",
    sidebar_soft = "#A7F3D0",
    content_bg = "#ECF0F5",
    map_low = "#67B7E5",
    map_mid = "#F6E9C7",
    map_high = "#B51720",
    text = "#374151",
    text_muted = "#4B5563",
    box_bg = "#F4F7FB",
    box_border = "#D9E2EC",
    transparent = cor_grafico("#000000", 0),
    white = "#FFFFFF",
    black = "#000000"
  ),
  # ######################## Visualização espacial ########################
  spatial = list(
    # ---------- Base, superfícies e tipografia ----------
    bg = "#F4F0E4",
    canvas = "#F4F2EA",
    card = cor_css("#FCFAF4", 0.97),
    card_soft = cor_css("#FCFAF4", 0.96),
    card_strong = cor_css("#FCFAF4", 0.98),
    ink = "#0E0E0E",
    ink_soft = "#171717",
    ink_dim = cor_css("#111111", 0.52),
    ink_20 = cor_css("#111111", 0.2),
    muted = "#5F594F",
    line = "#131313",
    black = "#111111",
    black_true = "#000000",
    on_dark = "#FFFFFF",
    # ---------- Destaques, notas e textos de apoio ----------
    accent = "#F0CF69",
    accent_soft = cor_css("#F0CF69", 0.16),
    accent_soft_strong = cor_css("#F0CF69", 0.34),
    accent_border = cor_css("#D7B24D", 0.45),
    note = "#6B4D0A",
    note_border = "#8A7841",
    note_bg = "#E7E1CF",
    mini_label = "#8A7A4E",
    section_icon = "#E2B93D",
    popup_kicker = "#8D7A3C",
    popup_text = "#322F2A",
    overlay_text = "#2E2822",
    overlay_text_soft = "#3B352D",
    selection_text = "#25221D",
    link = "#2B73C5",
    # ---------- Legenda, grades e popups ----------
    legend_na = "#B9BCC5",
    legend_fallback = "#D7DCE6",
    grid = "#E8E3D2",
    legend_caption = "#4E4A43",
    popup_border = "#000000",
    grid_soft = cor_css("#E8E3D2", 0.38),
    scrollbar = cor_css("#000000", 0.18),
    hover_surface = cor_css("#FFFFFF", 0.92),
    white_14 = cor_css("#FFFFFF", 0.14),
    # ---------- Bordas, contornos e sombras ----------
    border_soft = cor_css("#111111", 0.12),
    border = cor_css("#111111", 0.18),
    border_medium = cor_css("#111111", 0.28),
    border_strong = cor_css("#111111", 0.72),
    border_hard = cor_css("#111111", 0.82),
    border_max = cor_css("#111111", 0.92),
    shadow_soft = cor_css("#111111", 0.08),
    shadow = cor_css("#111111", 0.12),
    shadow_medium = cor_css("#111111", 0.18),
    mapbox_shadow = cor_css("#000000", 0.1),
    transparent = cor_grafico("#000000", 0),
    # ---------- Controles, sliders e botões ----------
    disabled_bg = "#ECE9E0",
    slider_track = "#E5DFCF",
    slider_track_alt = "#E7E1CF",
    control_bg = "#FFFFFF",
    control_text = "#181818",
    control_border = "#121212",
    button_soft = "#F7F2DF",
    view_toggle_bg = "#0B0B0B",
    # ---------- Séries, histograma e paleta do mapa ----------
    series_primary = "#30547C",
    series_secondary = "#C17B2C",
    outline_light = "#F8F7F2",
    histogram_fill = "#8AA2D3",
    blue_0 = "#D6DCE9",
    blue_1 = "#B3C2E0",
    blue_2 = "#8AA2D3",
    blue_3 = "#667FBA",
    blue_4 = "#455D93",
    blue_5 = "#2B3757"
  )
)

#### IC2025 Theme Presets ####################################################
ic2025_theme_merge <- function(base, overrides) {
  out <- base
  if (!is.list(overrides) || length(overrides) == 0) {
    return(out)
  }
  for (nm in names(overrides)) {
    base_value <- out[[nm]]
    override_value <- overrides[[nm]]
    if (is.list(base_value) && !is.null(names(base_value)) && is.list(override_value) && !is.null(names(override_value))) {
      out[[nm]] <- ic2025_theme_merge(base_value, override_value)
    } else {
      out[[nm]] <- override_value
    }
  }
  out
}

ic2025_theme_juntar <- function(...) {
  partes <- Filter(function(x) is.list(x) && length(x) > 0, list(...))
  if (length(partes) == 0) {
    return(list())
  }
  Reduce(ic2025_theme_merge, partes, init = list())
}

ic2025_theme_make_dashboard <- function(
    header, header_top, header_bottom, accent, accent_soft, accent_border,
    focus, ink = "#102A43", muted = "#6B7280", surface = "#FFFFFF",
    surface_alt = "#F4F7FB", logo_text = "#FFFFFF") {
  list(
    dashboard = list(
      header = header,
      header_top = header_top,
      header_bottom = header_bottom,
      header_nav_bg = sprintf("linear-gradient(180deg, %s 0%%, %s 100%%)", header_top, header_bottom),
      header_nav_text = logo_text,
      header_hover_bg = cor_css(logo_text, 0.10),
      header_logo_bg = header,
      header_logo_text = logo_text,
      header_border = cor_css(logo_text, 0.08),
      header_nav_shadow = cor_css(ink, 0.08),
      dropdown_border = cor_css(ink, 0.08),
      dropdown_shadow = cor_css(ink, 0.18),
      accent = accent,
      accent_hover = header_top,
      accent_dark = header_bottom,
      accent_deep = header,
      accent_soft = accent_soft,
      accent_border = accent_border,
      ink = ink,
      ink_mid = ink,
      ink_soft = ink,
      muted = muted,
      muted_alt = muted,
      muted_soft = muted,
      focus = focus,
      focus_ring = cor_css(focus, 0.16),
      surface = surface,
      surface_alt = surface_alt,
      surface_soft = surface_alt,
      border = cor_css(ink, 0.12),
      shadow = cor_css(ink, 0.18),
      shadow_soft = cor_css(ink, 0.08),
      shadow_medium = cor_css(ink, 0.12),
      grid = cor_css(ink, 0.10),
      sidebar_bg = sprintf("linear-gradient(180deg, %s 0%%, %s 100%%)", header, header_bottom),
      sidebar_border = cor_css(logo_text, 0.08),
      sidebar_shadow = cor_css(ink, 0.16),
      sidebar_label = cor_css(logo_text, 0.58),
      sidebar_text = cor_css(logo_text, 0.88),
      sidebar_icon = cor_css(logo_text, 0.86),
      sidebar_hover_bg = cor_css(logo_text, 0.10),
      sidebar_submenu_bg = cor_css(logo_text, 0.05),
      sidebar_submenu_border = cor_css(logo_text, 0.06),
      sidebar_submenu_shadow = cor_css(ink, 0.10),
      sidebar_active_bg = sprintf("linear-gradient(90deg, %s, %s)", cor_css(accent, 0.36), cor_css(accent, 0.08)),
      sidebar_active_border = cor_css(accent_soft, 0.28),
      flat_active_bg = cor_css(accent, 0.26),
      flat_active_accent = cor_css(accent_soft, 0.9),
      page_glow_left = cor_css(accent, 0.18),
      page_glow_right = cor_css(focus, 0.10),
      box_outline = accent_border,
      box_body_bg = "#FFFFFF",
      box_header_bg = "#FFFFFF",
      solid_header_bg = sprintf("linear-gradient(180deg, %s 0%%, %s 100%%)", header_top, header_bottom),
      solid_header_text = logo_text,
      pill_text = header,
      pill_border = cor_css(accent, 0.18),
      pill_active_bg = sprintf("linear-gradient(180deg, %s, %s)", cor_css(accent_soft, 1), cor_css(accent_soft, 0.92)),
      pill_active_border = cor_css(header, 0.95),
      pill_active_outline = cor_css(header, 0.12),
      pill_active_text = header,
      tab_text = muted,
      table_head_bg = surface_alt,
      loading_overlay_bg = sprintf("linear-gradient(180deg, %s 0%%, %s 100%%)", cor_css("#FFFFFF", 0.72), cor_css(surface, 0.56)),
      loading_card_bg = cor_css("#FFFFFF", 0.94),
      loading_card_border = cor_css(accent_soft, 0.95),
      loading_card_shadow = cor_css(header, 0.10),
      loading_spinner_track = cor_css(accent, 0.18),
      loading_spinner_head = accent,
      progress_track = "#E5E7EB",
      progress_fill = accent,
      progress_text = muted
    )
  )
}

ic2025_theme_make_desc <- function(temp_mean, temp_min, temp_max, alt_pink, alt_teal, alt_gray, violet) {
  list(
    desc = list(
      temp_mean = temp_mean,
      temp_min = temp_min,
      temp_max = temp_max,
      alt_pink = alt_pink,
      alt_teal = alt_teal,
      alt_gray = alt_gray,
      violet = violet
    )
  )
}

ic2025_theme_make_model <- function(primary, seasonal_line, black = "#111111") {
  list(
    model = list(
      primary = primary,
      primary_band = cor_css(primary, 0.20),
      black = black,
      black_soft = cor_css(black, 0.55),
      seasonal_band = cor_css(seasonal_line, 0.18),
      seasonal_line = seasonal_line,
      transparent = cor_grafico("#000000", 0)
    )
  )
}

ic2025_theme_make_funcoes <- function(
    sidebar_bg, sidebar_hover, sidebar_accent, sidebar_light, sidebar_soft,
    map_low, map_mid, map_high, text = "#374151", text_muted = "#4B5563",
    content_bg = "#ECF0F5", box_bg = "#F4F7FB", box_border = "#D9E2EC") {
  list(
    funcoes = list(
      sidebar_bg = sidebar_bg,
      sidebar_hover = sidebar_hover,
      sidebar_accent = sidebar_accent,
      sidebar_light = sidebar_light,
      sidebar_soft = sidebar_soft,
      content_bg = content_bg,
      map_low = map_low,
      map_mid = map_mid,
      map_high = map_high,
      text = text,
      text_muted = text_muted,
      box_bg = box_bg,
      box_border = box_border,
      transparent = cor_grafico("#000000", 0),
      white = "#FFFFFF",
      black = "#000000"
    )
  )
}

ic2025_theme_make_spatial <- function(
    bg, canvas, card_base, ink, muted, accent, note, note_bg, link, blues,
    series_secondary = accent, legend_na = "#B9BCC5", outline_light = "#F8F7F2",
    view_toggle_bg = NULL) {
  stopifnot(length(blues) == 6)
  if (is.null(view_toggle_bg) || !nzchar(view_toggle_bg)) {
    view_toggle_bg <- ink
  }
  list(
    spatial = list(
      bg = bg,
      canvas = canvas,
      card = cor_css(card_base, 0.97),
      card_soft = cor_css(card_base, 0.96),
      card_strong = cor_css(card_base, 0.98),
      ink = ink,
      ink_soft = ink,
      ink_dim = cor_css(ink, 0.52),
      ink_20 = cor_css(ink, 0.2),
      muted = muted,
      line = ink,
      black = ink,
      black_true = "#000000",
      on_dark = "#FFFFFF",
      accent = accent,
      accent_soft = cor_css(accent, 0.16),
      accent_soft_strong = cor_css(accent, 0.34),
      accent_border = cor_css(accent, 0.45),
      note = note,
      note_border = note,
      note_bg = note_bg,
      mini_label = muted,
      section_icon = accent,
      popup_kicker = note,
      popup_text = ink,
      overlay_text = ink,
      overlay_text_soft = muted,
      selection_text = ink,
      link = link,
      legend_na = legend_na,
      legend_fallback = blues[[1]],
      grid = note_bg,
      legend_caption = muted,
      popup_border = "#000000",
      grid_soft = cor_css(note_bg, 0.38),
      scrollbar = cor_css("#000000", 0.18),
      hover_surface = cor_css("#FFFFFF", 0.92),
      white_14 = cor_css("#FFFFFF", 0.14),
      border_soft = cor_css(ink, 0.12),
      border = cor_css(ink, 0.18),
      border_medium = cor_css(ink, 0.28),
      border_strong = cor_css(ink, 0.72),
      border_hard = cor_css(ink, 0.82),
      border_max = cor_css(ink, 0.92),
      shadow_soft = cor_css(ink, 0.08),
      shadow = cor_css(ink, 0.12),
      shadow_medium = cor_css(ink, 0.18),
      mapbox_shadow = cor_css("#000000", 0.1),
      transparent = cor_grafico("#000000", 0),
      disabled_bg = note_bg,
      slider_track = note_bg,
      slider_track_alt = note_bg,
      control_bg = "#FFFFFF",
      control_text = ink,
      control_border = ink,
      button_soft = note_bg,
      view_toggle_bg = view_toggle_bg,
      series_primary = blues[[5]],
      series_secondary = series_secondary,
      outline_light = outline_light,
      histogram_fill = blues[[3]],
      blue_0 = blues[[1]],
      blue_1 = blues[[2]],
      blue_2 = blues[[3]],
      blue_3 = blues[[4]],
      blue_4 = blues[[5]],
      blue_5 = blues[[6]]
    )
  )
}

IC2025_THEME_REFERENCIAS <- list(
  uff_azul = "#005BAA",
  les_azul = "#005AAB",
  les_azul_claro = "#6496D9",
  uff_cinza = "#6D6E71",
  les_cinza = "#939493",
  marinho = "#102A43",
  anil = "#1D4E89",
  petroleo = "#0F3B66",
  cobalto = "#2563EB",
  ardosia = "#334155",
  gelo = "#EAF2FF",
  gelo_2 = "#F4F8FF",
  areia = "#F4F0E4",
  ouro = "#D4A72C",
  menta = "#2FA38A"
)

IC2025_THEME_PRESET_ORIGINAL_ID <- "atual_classica"
IC2025_THEME_PRESET_DEFAULT_ID <- "les_institucional"
IC2025_THEME_SIDEBAR_PRESET_DEFAULT_ID <- "les_grafite"
IC2025_THEME_LINK_TOPBAR_DEFAULT <- TRUE

IC2025_THEME_PRESET_LABELS <- c(
  atual_classica = "Atual clássica",
  atual_espacial_alinhada = "Atual + espacial alinhada",
  les_institucional = "LES institucional",
  les_uff_claro = "LES UFF claro",
  les_cobalto = "LES cobalto",
  les_niteroi = "LES Niterói",
  les_grafite = "LES grafite",
  les_noite = "LES noite",
  les_tecnico = "LES técnico",
  les_extensao = "LES extensão"
)

IC2025_THEME_PRESET_OVERRIDES <- list(
  atual_classica = list(),
  atual_espacial_alinhada = ic2025_theme_make_spatial(
    bg = "#F3F7FB",
    canvas = "#EEF6F4",
    card_base = "#FFFFFF",
    ink = "#102A43",
    muted = "#5B6B7F",
    accent = "#10B981",
    note = "#0B5D49",
    note_bg = "#DFF4EB",
    link = "#1F77B4",
    blues = c("#E4F4EF", "#BFE8D8", "#87D8B8", "#4DBE94", "#1E9872", "#0B5D49"),
    series_secondary = "#1F77B4",
    legend_na = "#C8D3DE",
    outline_light = "#F7FBF9",
    view_toggle_bg = "#0B5D49"
  ),
  les_institucional = ic2025_theme_juntar(
    ic2025_theme_make_dashboard(
      header = "#005BAA",
      header_top = "#1B6FC1",
      header_bottom = "#004A8B",
      accent = "#6496D9",
      accent_soft = "#DDEAFE",
      accent_border = "#8AB5E6",
      focus = "#0F3B66",
      ink = "#102A43",
      muted = "#61748A",
      surface_alt = "#F4F8FF"
    ),
    ic2025_theme_make_desc("#005AAB", "#6C86A6", "#0F3B66", "#6496D9", "#2FA38A", "#6D6E71", "#355C99"),
    ic2025_theme_make_model("#005AAB", "#2FA38A", black = "#0F3B66"),
    ic2025_theme_make_funcoes("#005BAA", "#1B6FC1", "#6496D9", "#8AB5E6", "#DDEAFE", "#E4EFFC", "#8EB6E6", "#0F3B66", content_bg = "#F4F8FF", box_bg = "#FFFFFF", box_border = "#D3E2F5"),
    ic2025_theme_make_spatial("#F6FAFF", "#F0F6FF", "#FFFFFF", "#102A43", "#61748A", "#005AAB", "#005AAB", "#EAF2FF", "#005BAA", c("#E6F0FB", "#C6DAF4", "#96BAE6", "#6496D9", "#2D73BF", "#005AAB"), series_secondary = "#6D6E71", legend_na = "#CDD8E6", outline_light = "#F7FAFF", view_toggle_bg = "#0F3B66")
  ),
  les_uff_claro = ic2025_theme_juntar(
    ic2025_theme_make_dashboard(
      header = "#3E7FC1",
      header_top = "#78AEE3",
      header_bottom = "#2A69AA",
      accent = "#005BAA",
      accent_soft = "#E3EFFC",
      accent_border = "#A5C6EA",
      focus = "#163E63",
      ink = "#16324F",
      muted = "#6E8094",
      surface_alt = "#F6FAFF"
    ),
    ic2025_theme_make_desc("#3E7FC1", "#8AAAD0", "#005BAA", "#97BCE8", "#2FA38A", "#7E8A97", "#5579B1"),
    ic2025_theme_make_model("#3E7FC1", "#2FA38A", black = "#163E63"),
    ic2025_theme_make_funcoes("#3E7FC1", "#5C98D6", "#005BAA", "#8CB7E8", "#E3EFFC", "#EFF5FD", "#B6CFEF", "#3E7FC1", content_bg = "#F7FBFF", box_bg = "#FFFFFF", box_border = "#DCE8F5"),
    ic2025_theme_make_spatial("#FAFCFF", "#F4F8FF", "#FFFFFF", "#16324F", "#70849A", "#3E7FC1", "#3E7FC1", "#EEF5FF", "#005BAA", c("#EFF5FD", "#D8E6F8", "#B8D1F1", "#8CB7E8", "#5B93D7", "#2A69AA"), series_secondary = "#2FA38A", legend_na = "#D3DDEA", outline_light = "#FBFDFF", view_toggle_bg = "#163E63")
  ),
  les_cobalto = ic2025_theme_juntar(
    ic2025_theme_make_dashboard(
      header = "#004DA8",
      header_top = "#2563EB",
      header_bottom = "#003E87",
      accent = "#60A5FA",
      accent_soft = "#DBEAFE",
      accent_border = "#93C5FD",
      focus = "#1D4ED8",
      ink = "#0F2A44",
      muted = "#5E738B",
      surface_alt = "#F5F9FF"
    ),
    ic2025_theme_make_desc("#2563EB", "#7C9AC7", "#0F2A44", "#60A5FA", "#14B8A6", "#6B7280", "#4F46E5"),
    ic2025_theme_make_model("#2563EB", "#14B8A6", black = "#0F2A44"),
    ic2025_theme_make_funcoes("#004DA8", "#2563EB", "#60A5FA", "#93C5FD", "#DBEAFE", "#E9F2FF", "#9FC4F8", "#1D4ED8", content_bg = "#F5F9FF", box_bg = "#FFFFFF", box_border = "#D6E4FB"),
    ic2025_theme_make_spatial("#F7FAFF", "#EEF4FF", "#FFFFFF", "#0F2A44", "#5E738B", "#2563EB", "#1D4ED8", "#E8F0FF", "#2563EB", c("#EAF1FF", "#C8DBFF", "#9EC1FF", "#6EA5F7", "#3D82E6", "#1D4ED8"), series_secondary = "#14B8A6", legend_na = "#CDD8EA", outline_light = "#F8FBFF", view_toggle_bg = "#0F2A44")
  ),
  les_niteroi = ic2025_theme_juntar(
    ic2025_theme_make_dashboard(
      header = "#0F3B66",
      header_top = "#186FAF",
      header_bottom = "#0E4F84",
      accent = "#2FA38A",
      accent_soft = "#D9F1EA",
      accent_border = "#79C8B8",
      focus = "#0F5C8D",
      ink = "#16324F",
      muted = "#5B7385",
      surface_alt = "#F3FAFB"
    ),
    ic2025_theme_make_desc("#186FAF", "#76A3C4", "#0F3B66", "#2FA38A", "#65BEC0", "#6D6E71", "#3E7FC1"),
    ic2025_theme_make_model("#186FAF", "#2FA38A", black = "#16324F"),
    ic2025_theme_make_funcoes("#0F3B66", "#186FAF", "#2FA38A", "#7CCDC6", "#D9F1EA", "#EAF7F5", "#9ED8D4", "#1D6E86", content_bg = "#F2FAFA", box_bg = "#FFFFFF", box_border = "#D7ECEA"),
    ic2025_theme_make_spatial("#F4FBFB", "#ECF7F8", "#FFFFFF", "#16324F", "#5B7385", "#2FA38A", "#0F5C8D", "#E7F4F8", "#186FAF", c("#EAF7F5", "#C8EAE5", "#9ED8D4", "#65BEC0", "#2F9BA3", "#1D6E86"), series_secondary = "#186FAF", legend_na = "#D2E0E3", outline_light = "#F7FCFC", view_toggle_bg = "#0F3B66")
  ),
  les_grafite = ic2025_theme_juntar(
    ic2025_theme_make_dashboard(
      header = "#243746",
      header_top = "#35556D",
      header_bottom = "#1A2732",
      accent = "#005AAB",
      accent_soft = "#E3ECF8",
      accent_border = "#9DB7D4",
      focus = "#4A6480",
      ink = "#1E293B",
      muted = "#64748B",
      surface_alt = "#F6F8FB"
    ),
    ic2025_theme_make_desc("#4A6480", "#8A9AAF", "#243746", "#005AAB", "#2FA38A", "#6D6E71", "#61758F"),
    ic2025_theme_make_model("#4A6480", "#2FA38A", black = "#1E293B"),
    ic2025_theme_make_funcoes("#243746", "#35556D", "#005AAB", "#8FB2D5", "#E3ECF8", "#EEF3F7", "#B4C5D6", "#314658", content_bg = "#F6F8FB", box_bg = "#FFFFFF", box_border = "#D8E1EA"),
    ic2025_theme_make_spatial("#F7F9FB", "#F1F4F8", "#FFFFFF", "#1E293B", "#64748B", "#005AAB", "#314658", "#EDF2F7", "#005AAB", c("#E7EDF2", "#CCD8E4", "#A7BACD", "#7E96B1", "#58708B", "#314658"), series_secondary = "#6D6E71", legend_na = "#D4DADF", outline_light = "#FAFBFC", view_toggle_bg = "#243746")
  ),
  les_noite = ic2025_theme_juntar(
    ic2025_theme_make_dashboard(
      header = "#0B1C2D",
      header_top = "#173A63",
      header_bottom = "#08131F",
      accent = "#60A5FA",
      accent_soft = "#E0EDFF",
      accent_border = "#93C5FD",
      focus = "#93C5FD",
      ink = "#102A43",
      muted = "#64748B",
      surface_alt = "#F3F7FF"
    ),
    ic2025_theme_make_desc("#60A5FA", "#93C5FD", "#173A63", "#7DD3FC", "#2FA38A", "#64748B", "#818CF8"),
    ic2025_theme_make_model("#60A5FA", "#2FA38A", black = "#0B1C2D"),
    ic2025_theme_make_funcoes("#0B1C2D", "#173A63", "#60A5FA", "#93C5FD", "#E0EDFF", "#EDF4FF", "#B8CFF7", "#173A63", content_bg = "#F4F7FC", box_bg = "#FFFFFF", box_border = "#D9E3F1"),
    ic2025_theme_make_spatial("#F5F8FF", "#EEF4FF", "#FFFFFF", "#102A43", "#64748B", "#60A5FA", "#173A63", "#EAF1FF", "#2563EB", c("#EAF1FF", "#CCDBF5", "#A8C1EE", "#7F9FE0", "#4C76C6", "#173A63"), series_secondary = "#2FA38A", legend_na = "#D0D8E6", outline_light = "#F8FAFF", view_toggle_bg = "#0B1C2D")
  ),
  les_tecnico = ic2025_theme_juntar(
    ic2025_theme_make_dashboard(
      header = "#1D3557",
      header_top = "#457B9D",
      header_bottom = "#16324F",
      accent = "#7AA5C6",
      accent_soft = "#E8F1F8",
      accent_border = "#B8CFE0",
      focus = "#005AAB",
      ink = "#223247",
      muted = "#667788",
      surface_alt = "#F6FAFC"
    ),
    ic2025_theme_make_desc("#457B9D", "#7AA5C6", "#1D3557", "#A8C5DD", "#2FA38A", "#6D6E71", "#56759F"),
    ic2025_theme_make_model("#457B9D", "#2FA38A", black = "#223247"),
    ic2025_theme_make_funcoes("#1D3557", "#457B9D", "#7AA5C6", "#A8C5DD", "#E8F1F8", "#EEF3F7", "#B4C5D6", "#5F7893", content_bg = "#F6FAFC", box_bg = "#FFFFFF", box_border = "#D7E1E8"),
    ic2025_theme_make_spatial("#F8FBFC", "#F1F6F8", "#FFFFFF", "#223247", "#667788", "#7AA5C6", "#1D3557", "#EDF3F6", "#005AAB", c("#EEF3F7", "#D7E1EA", "#B4C5D6", "#8BA2BB", "#5F7893", "#2E435B"), series_secondary = "#2FA38A", legend_na = "#D6DCE1", outline_light = "#FBFDFC", view_toggle_bg = "#1D3557")
  ),
  les_extensao = ic2025_theme_juntar(
    ic2025_theme_make_dashboard(
      header = "#005BAA",
      header_top = "#2FA38A",
      header_bottom = "#004A8B",
      accent = "#2FA38A",
      accent_soft = "#D9F1EA",
      accent_border = "#79C8B8",
      focus = "#005BAA",
      ink = "#102A43",
      muted = "#61748A",
      surface_alt = "#F3FAF8"
    ),
    ic2025_theme_make_desc("#005BAA", "#6496D9", "#004A8B", "#2FA38A", "#79C8B8", "#6D6E71", "#355C99"),
    ic2025_theme_make_model("#005BAA", "#2FA38A", black = "#102A43"),
    ic2025_theme_make_funcoes("#005BAA", "#2FA38A", "#2FA38A", "#79C8B8", "#D9F1EA", "#E8F6F1", "#A9DCCD", "#005AAB", content_bg = "#F3FAF8", box_bg = "#FFFFFF", box_border = "#D4EBE2"),
    ic2025_theme_make_spatial("#F4FBF9", "#EDF7F4", "#FFFFFF", "#102A43", "#61748A", "#2FA38A", "#005BAA", "#E7F4EF", "#005BAA", c("#E6F4F1", "#C5E7DF", "#93D3C3", "#5DB6A0", "#268B7C", "#005AAB"), series_secondary = "#6496D9", legend_na = "#D2DED9", outline_light = "#F8FCFB", view_toggle_bg = "#005BAA")
  )
)

ic2025_theme_keep_spatial_map_colours <- function(theme_list) {
  if (!is.list(theme_list$spatial)) {
    theme_list$spatial <- list()
  }
  spatial_keys <- c(
    "legend_na",
    "legend_fallback",
    "blue_0",
    "blue_1",
    "blue_2",
    "blue_3",
    "blue_4",
    "blue_5"
  )
  for (nm in spatial_keys) {
    theme_list$spatial[[nm]] <- IC2025_THEME_COLORS$spatial[[nm]]
  }
  theme_list
}

ic2025_theme_keep_desc_map_colours <- function(theme_list) {
  if (!is.list(theme_list$funcoes)) {
    theme_list$funcoes <- list()
  }
  desc_map_keys <- c("map_low", "map_mid", "map_high")
  for (nm in desc_map_keys) {
    theme_list$funcoes[[nm]] <- IC2025_THEME_COLORS$funcoes[[nm]]
  }
  theme_list
}

IC2025_THEME_PRESETS <- lapply(IC2025_THEME_PRESET_OVERRIDES, function(overrides) {
  ic2025_theme_keep_desc_map_colours(
    ic2025_theme_keep_spatial_map_colours(
      ic2025_theme_merge(IC2025_THEME_COLORS, overrides)
    )
  )
})

IC2025_THEME_COLORS_FLAT <- ic2025_theme_flatten(IC2025_THEME_COLORS)
IC2025_THEME_PRESETS_FLAT <- lapply(IC2025_THEME_PRESETS, ic2025_theme_flatten)

ic2025_theme_css_lines <- function(flat) {
  vapply(names(flat), function(key) {
    css_name <- gsub("[^a-zA-Z0-9-]+", "-", key)
    sprintf("--ic2025-%s: %s;", css_name, flat[[key]])
  }, character(1))
}

ic2025_theme_current_id <- function(default = IC2025_THEME_PRESET_DEFAULT_ID) {
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session)) {
    selected <- tryCatch(as.character(session$input$ui_theme_palette), error = function(e) NULL)
    if (!is.null(selected) && length(selected) > 0 && nzchar(selected[[1]]) && selected[[1]] %in% names(IC2025_THEME_PRESETS_FLAT)) {
      return(selected[[1]])
    }
  }
  default
}

ic2025_theme_current_flat <- function(default = IC2025_THEME_COLORS_FLAT) {
  preset_id <- ic2025_theme_current_id()
  preset_flat <- IC2025_THEME_PRESETS_FLAT[[preset_id]]
  if (is.null(preset_flat)) {
    preset_flat <- getOption("ic2025.theme.colors", default = default)
  }
  preset_flat
}

ic2025_theme_value <- function(key, default = IC2025_THEME_COLORS_FLAT[[as.character(key)]]) {
  vals <- ic2025_theme_current_flat()
  value <- vals[[as.character(key)]]
  if (is.null(value) || !nzchar(value)) default else value
}

ic2025_theme_css_tag <- function(preset_id = IC2025_THEME_PRESET_DEFAULT_ID) {
  flat <- IC2025_THEME_PRESETS_FLAT[[preset_id]]
  if (is.null(flat)) {
    flat <- IC2025_THEME_COLORS_FLAT
  }
  tags$style(HTML(paste0(":root{", paste(ic2025_theme_css_lines(flat), collapse = ""), "}")))
}

ic2025_theme_js_tag <- function() {
  tags$script(HTML(sprintf(
    "window.IC2025_THEME_PRESET_ORIGINAL=%s;window.IC2025_THEME_PRESET_DEFAULT=%s;window.IC2025_THEME_PRESET_LABELS=%s;window.IC2025_THEME_PRESETS=%s;window.IC2025_THEME_COLORS=%s;",
    jsonlite::toJSON(IC2025_THEME_PRESET_ORIGINAL_ID, auto_unbox = TRUE, pretty = FALSE),
    jsonlite::toJSON(IC2025_THEME_PRESET_DEFAULT_ID, auto_unbox = TRUE, pretty = FALSE),
    jsonlite::toJSON(IC2025_THEME_PRESET_LABELS, auto_unbox = TRUE, pretty = FALSE),
    jsonlite::toJSON(IC2025_THEME_PRESETS_FLAT, auto_unbox = TRUE, pretty = FALSE),
    jsonlite::toJSON(IC2025_THEME_PRESETS_FLAT[[IC2025_THEME_PRESET_DEFAULT_ID]], auto_unbox = TRUE, pretty = FALSE)
  )))
}

options(
  ic2025.theme.colors = IC2025_THEME_PRESETS_FLAT[[IC2025_THEME_PRESET_DEFAULT_ID]],
  ic2025.theme.preset.default = IC2025_THEME_PRESET_DEFAULT_ID
)
#### END IC2025 Theme Colors #################################################

# ----------------------------
# Diagnóstico de startup (opcional)
# Ligue via: options(ic2025.startup_diagnostics = TRUE)
# ----------------------------
STARTUP_DIAGNOSTICS <- isTRUE(getOption("ic2025.startup_diagnostics", FALSE))
STARTUP_DIAGNOSTICS_LOG_FILE <- as.character(
  getOption("ic2025.startup_diagnostics_log_file", "startup_diagnostics.log")
)

diag_mem_mb <- function() {
  g <- gc()
  if (!all(c("Ncells", "Vcells") %in% rownames(g)) || !("used" %in% colnames(g))) {
    return(NA_real_)
  }
  # Aproximação em MB (útil para comparar tendência entre etapas)
  ((as.numeric(g["Ncells", "used"]) * 56) + (as.numeric(g["Vcells", "used"]) * 8)) / (1024^2)
}

diag_log <- function(etapa, evento = "INFO", detalhe = "") {
  if (!isTRUE(STARTUP_DIAGNOSTICS)) return(invisible(NULL))
  linha <- sprintf(
    "[startup-diag] %s | %-5s | mem=%.1fMB | %s%s",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    evento,
    diag_mem_mb(),
    etapa,
    if (nzchar(detalhe)) paste0(" | ", detalhe) else ""
  )
  message(linha)
  try(
    cat(linha, "\n", file = STARTUP_DIAGNOSTICS_LOG_FILE, append = TRUE),
    silent = TRUE
  )
  invisible(NULL)
}

diag_step <- function(etapa, expr) {
  if (!isTRUE(STARTUP_DIAGNOSTICS)) {
    return(force(expr))
  }
  m0 <- diag_mem_mb()
  t0 <- proc.time()[["elapsed"]]
  diag_log(etapa, "IN")
  out <- withCallingHandlers(
    force(expr),
    warning = function(w) {
      diag_log(etapa, "WARN", conditionMessage(w))
    }
  )
  dt <- proc.time()[["elapsed"]] - t0
  dm <- diag_mem_mb() - m0
  diag_log(etapa, "OUT", sprintf("%.3fs | delta_mem=%+.1fMB", dt, dm))
  out
}

if (isTRUE(STARTUP_DIAGNOSTICS)) {
  try(
    cat(
      sprintf("\n===== STARTUP DIAGNOSTICS %s =====\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      file = STARTUP_DIAGNOSTICS_LOG_FILE,
      append = FALSE
    ),
    silent = TRUE
  )
  diag_log("Inicialização do app", "INFO")
}

# ----------------------------
# Fonte de dados do dashboard (switch central)
# "agregada": usa a base final agregada (RDS/DuckDB)
# "legacy": usa as 3 bases antigas (RJ/SP/Amazônia Legal)
# ----------------------------
DATA_SOURCE_MODE <- "agregada"
DATA_SOURCE_AGGREGATED_RDS <- "base_final_era5land_cams_internacoes_obitos_BR_20150101_20251231_FINAL_missing_padronizado.rds"
DATA_SOURCE_AGGREGATED_DUCKDB <- "base_final.duckdb" # base_final_so_capitais.duckdb
DATA_SOURCE_AGGREGATED_DUCKDB_URL <- "https://github.com/richardamarante/pdldglm-amazonia-pollution/releases/download/v1.0/base_final.duckdb"
DATA_SOURCE_AGGREGATED_DUCKDB_TIMEOUT <- 3600L
DATA_SOURCE_AGGREGATED_DUCKDB_TABLE <- "base_final_era5land_cams_internacoes_obitos_missing_padronizado"

# Backend da base agregada:
# "rds"    -> leitura tradicional do .rds
# "duckdb" -> consultas sob demanda no arquivo .duckdb (mais rápido para base grande)
DATA_STORAGE_BACKEND <- "duckdb"
ENABLE_SPATIAL_COMPACT_DESKTOP <- FALSE

# Estratégia de carga da base:
# "startup_once_no_cache": lê o(s) arquivo(s) uma vez no startup, ignora cache interno do funcoes.R
# "cached_on_demand": comportamento antigo (cache em memória sob demanda)
DATA_BASE_LOADING_MODE <- "startup_once_no_cache"

# Pré-carrega a(s) base(s) na inicialização do app para evitar latência na 1ª interação.
PRELOAD_BASE_ON_STARTUP <- TRUE
# Opcional: pré-carrega geometria do mapa descritivo no startup.
# FALSE evita atrasar abertura; TRUE evita latência na 1ª renderização do mapa.
PRELOAD_DESC_MAP_GEOMETRY_ON_STARTUP <- FALSE
# Liga/desliga totalmente os mapas da aba Análise Descritiva.
# FALSE remove a UI e desativa o bloco de renderização dos mapas.
ENABLE_DESC_MAPS <- TRUE
# TRUE  -> mantém os mapas da Descritiva aquecendo em background mesmo
#         quando a visão ativa é "Série Temporal"
# FALSE -> suspende os mapas ocultos
DESC_MAP_WARM_BACKGROUND <- isTRUE(getOption("ic2025.desc_map_warm_background", TRUE))
# Labels do mapa descritivo:
# FALSE -> não mostra "DF"
# TRUE  -> mostra "DF" junto com as demais siglas
DESC_MAP_SHOW_DF_LABEL <- FALSE
DESC_VIEW_SELECTOR_STYLE <- "outline" # "outline" ou "underline"
SHOW_SIM_SERIES_MODE_INPUT <- TRUE
SHOW_APP_SERIES_MODE_INPUT <- FALSE
SHOW_EVAL_SERIES_MODE_INPUT <- FALSE
ENABLE_EVAL_AUTO_CLIMA_SWITCH <- FALSE
SERIES_MODE_LABEL <- htmltools::HTML("Modo da série (<span style='font-weight:400;'>μ</span>)")
APP_FIXED_N_AMOSTRAS <- 1000L
APP_SAZONAL_DEFAULT_ENABLED <- FALSE
APP_SAZONAL_DEFAULT_PERIOD <- 365L
APP_SAZONAL_DEFAULT_ORDER <- 1L
APP_SAZONAL_DEFAULT_FD <- 0.98
APP_MAX_LAGS <- 16L
EVAL_DEFAULT_YEAR <- 2024L
EVAL_DEFAULT_LAGS <- 10L
EVAL_DEFAULT_LAG_RANGE <- c(8L, 12L)
EVAL_DEFAULT_SAZONAL_PERIOD <- 365L
EVAL_DEFAULT_SAZONAL_ORDER <- 1L
EVAL_DEFAULT_SAZONAL_FD_GRID <- c(0.97, 0.98, 0.99)
EVAL_DEFAULT_SAZONAL_FD_CHOICES <- c(0.95, 0.96, 0.97, 0.98, 0.99, 1.00)
DESC_MAP_DATA_CACHE_VERSION <- "v3"
DESC_MAP_PLOT_CACHE_VERSION <- "v6"
if (!DATA_SOURCE_MODE %in% c("agregada", "legacy")) {
  stop("DATA_SOURCE_MODE deve ser 'agregada' ou 'legacy'.")
}
if (!DATA_BASE_LOADING_MODE %in% c("startup_once_no_cache", "cached_on_demand")) {
  stop("DATA_BASE_LOADING_MODE deve ser 'startup_once_no_cache' ou 'cached_on_demand'.")
}
if (!DESC_VIEW_SELECTOR_STYLE %in% c("outline", "underline")) {
  stop("DESC_VIEW_SELECTOR_STYLE deve ser 'outline' ou 'underline'.")
}
if (!DATA_STORAGE_BACKEND %in% c("rds", "duckdb")) {
  stop("DATA_STORAGE_BACKEND deve ser 'rds' ou 'duckdb'.")
}
options(
  ic2025.data_mode = DATA_SOURCE_MODE,
  ic2025.agregada_rds = DATA_SOURCE_AGGREGATED_RDS,
  ic2025.storage_backend = DATA_STORAGE_BACKEND,
  ic2025.agregada_duckdb = DATA_SOURCE_AGGREGATED_DUCKDB,
  ic2025.agregada_duckdb_url = DATA_SOURCE_AGGREGATED_DUCKDB_URL,
  ic2025.agregada_duckdb_timeout = DATA_SOURCE_AGGREGATED_DUCKDB_TIMEOUT,
  ic2025.agregada_duckdb_table = DATA_SOURCE_AGGREGATED_DUCKDB_TABLE,
  ic2025.disable_rds_cache = identical(DATA_BASE_LOADING_MODE, "startup_once_no_cache"),
  ic2025.desc_map_show_df_label = isTRUE(DESC_MAP_SHOW_DF_LABEL)
)
options(
  ic2025.preloaded_base_agregada = NULL,
  ic2025.preloaded_bases_legacy = NULL,
  ic2025.preloaded_base_agregada_duckdb = FALSE
)

diag_step("source(funcoes.R)", {
  source("funcoes.R", local = TRUE, encoding = "UTF-8")
})
diag_step("source(spatial_app/spatial_module.R)", {
  source("spatial_app/spatial_module.R", local = TRUE, encoding = "UTF-8")
})
diag_step("bootstrap curbcut vendor packages", {
  spatial_activate_vendor_library(spatial_detect_project_root())
  spatial_ensure_curbcut_packages(spatial_detect_project_root())
})
ACTIVE_STORAGE_BACKEND <- diag_step("detectar backend ativo", {
  ic2025_storage_backend()
})
if (!identical(ACTIVE_STORAGE_BACKEND, DATA_STORAGE_BACKEND)) {
  warning(
    "Backend solicitado ('", DATA_STORAGE_BACKEND,
    "') não está disponível; usando '", ACTIVE_STORAGE_BACKEND, "'."
  )
}
options(ic2025.storage_backend = ACTIVE_STORAGE_BACKEND)

DESC_Y2_TEMP_BANDS_VALUE <- "__TEMP_MIN_MAX__"
DESC_TEMP_MEDIA_CANDIDATES <- c("Temperatura", "temp", "temperatura")
DESC_TEMP_MIN_CANDIDATES <- c("TemperaturaMin", "temperatura_min", "temperaturamin", "temp_min", "tempmin")
DESC_TEMP_MAX_CANDIDATES <- c("TemperaturaMax", "temperatura_max", "temperaturamax", "temp_max", "tempmax")

desc_pick_var_candidate <- function(vars, candidates) {
  vars <- as.character(vars %||% character(0))
  hit <- candidates[candidates %in% vars]
  if (length(hit) == 0) return(NA_character_)
  as.character(hit[[1]])
}

desc_temp_media_col <- function(vars) {
  desc_pick_var_candidate(vars, DESC_TEMP_MEDIA_CANDIDATES)
}

desc_temp_min_col <- function(vars) {
  desc_pick_var_candidate(vars, DESC_TEMP_MIN_CANDIDATES)
}

desc_temp_max_col <- function(vars) {
  desc_pick_var_candidate(vars, DESC_TEMP_MAX_CANDIDATES)
}

desc_is_temp_media_var <- function(v) {
  as.character(v %||% "") %in% DESC_TEMP_MEDIA_CANDIDATES
}

desc_is_temp_min_var <- function(v) {
  as.character(v %||% "") %in% DESC_TEMP_MIN_CANDIDATES
}

desc_is_temp_max_var <- function(v) {
  as.character(v %||% "") %in% DESC_TEMP_MAX_CANDIDATES
}

desc_can_use_temp_band_combo <- function(vars, y_sel = NULL) {
  y_val <- as.character(y_sel %||% "")
  if (!isTRUE(desc_is_temp_media_var(y_val))) {
    return(FALSE)
  }
  vars_lookup <- unique(c(as.character(vars %||% character(0)), desc_schema_vars_startup()))
  min_col <- desc_temp_min_col(vars_lookup)
  max_col <- desc_temp_max_col(vars_lookup)
  nzchar(min_col %||% "") || nzchar(max_col %||% "")
}

desc_temp_band_available <- function(vars) {
  vars_lookup <- unique(c(as.character(vars %||% character(0)), desc_schema_vars_startup()))
  min_col <- desc_temp_min_col(vars_lookup)
  max_col <- desc_temp_max_col(vars_lookup)
  nzchar(min_col %||% "") || nzchar(max_col %||% "")
}

desc_plot_color <- function(v, idx = 1L) {
  if (desc_is_temp_media_var(v)) return(ic2025_theme_value("desc.temp_mean"))
  if (desc_is_temp_min_var(v)) return(ic2025_theme_value("desc.temp_min"))
  if (desc_is_temp_max_var(v)) return(ic2025_theme_value("desc.temp_max"))
  pal <- c(
    ic2025_theme_value("desc.temp_mean"),
    ic2025_theme_value("desc.alt_pink"),
    ic2025_theme_value("desc.alt_teal"),
    ic2025_theme_value("desc.alt_gray")
  )
  pal[[min(max(as.integer(idx), 1L), length(pal))]]
}

desc_startup_label <- function(v) {
  vv <- as.character(v %||% "")
  dplyr::case_when(
    desc_is_temp_media_var(vv) ~ "Temperatura Média",
    desc_is_temp_min_var(vv) ~ "Temperatura Mínima",
    desc_is_temp_max_var(vv) ~ "Temperatura Máxima",
    vv %in% c("UmidRel", "umid", "UmidadeRelativa") ~ "Umidade Relativa (%)",
    vv %in% c("VelocVento", "vento") ~ "Velocidade do Vento",
    vv %in% c("SensTermica", "sens", "sens_termica") ~ "Sensação Térmica",
    vv %in% c("PM2p5", "pm25", "PM25") ~ "PM2.5",
    vv %in% c("PM10", "pm10") ~ "PM10",
    vv %in% c("CO", "co") ~ "CO",
    grepl("obit", tolower(vv)) ~ "Óbitos",
    grepl("interna|casos|resp|circ", tolower(vv)) ~ "Internações",
    TRUE ~ vv
  )
}

desc_startup_group <- function(v) {
  vv <- tolower(as.character(v %||% ""))
  if (!nzchar(vv)) return("Clima")
  if (vv %in% tolower(c("PM2p5", "pm25", "PM25", "PM10", "pm10", "CO", "co", "NO2", "no2", "SO2", "so2", "O3", "o3"))) {
    return("Qualidade do Ar")
  }
  if (grepl("casos|interna|obit|cid10|resp|circ", vv)) {
    return("Saúde Pública")
  }
  "Clima"
}

desc_schema_vars_startup <- function() {
  if (!(identical(DATA_SOURCE_MODE, "agregada") && identical(ACTIVE_STORAGE_BACKEND, "duckdb"))) {
    return(character(0))
  }
  info <- tryCatch(duckdb_info_agregada(use_cache = TRUE), error = function(e) NULL)
  if (!is.list(info) || length(info$campos) == 0) return(character(0))
  cols_excluir <- unique(c(
    info$col_cod, info$col_data, info$col_uf, info$col_nome,
    "CodigoMunicipio", "CodigoMunicipio6", "CodMunicipio", "codigo_municipio",
    "Ano", "Mes", "Dia",
    "RegiaoMunicipio", "EstadoMunicipio", "NomeMunicipio", "Municipio",
    "UFMunicipio", "uf", "UF", "abbrev_state", "estado", "regiao"
  ))
  vars <- setdiff(as.character(info$campos), cols_excluir)
  vars <- vars[!grepl("^(codigo|cod).*municip", tolower(vars))]
  vars <- vars[!grepl("^ibge", tolower(vars))]
  unique(vars[nzchar(vars)])
}

desc_y_default_startup <- function(vars) {
  vars <- as.character(vars %||% character(0))
  if (length(vars) == 0) return("__NONE__")
  dplyr::case_when(
    "UmidRel" %in% vars ~ "UmidRel",
    "umid" %in% vars ~ "umid",
    "UmidadeRelativa" %in% vars ~ "UmidadeRelativa",
    "Temperatura" %in% vars ~ "Temperatura",
    "temp" %in% vars ~ "temp",
    "SensTermica" %in% vars ~ "SensTermica",
    "VelocVento" %in% vars ~ "VelocVento",
    TRUE ~ vars[[1]]
  )
}

desc_y_startup_choices <- local({
  vars <- desc_schema_vars_startup()
  if (length(vars) == 0) return(c("Carregando..." = "__NONE__"))
  grupos <- vapply(vars, desc_startup_group, character(1))
  labels <- vapply(vars, desc_startup_label, character(1))
  ord_gr <- c("Clima", "Saúde Pública", "Qualidade do Ar")
  out <- list()
  for (g in ord_gr) {
    idx <- which(grupos == g)
    if (length(idx) == 0) next
    idx <- idx[order(labels[idx])]
    out[[g]] <- stats::setNames(vars[idx], labels[idx])
  }
  out
})
desc_y_startup_default <- desc_y_default_startup(desc_schema_vars_startup())
desc_y2_startup_choices <- local({
  vars <- desc_schema_vars_startup()
  out <- c("Nenhuma" = "__NONE__")
  if (isTRUE(desc_can_use_temp_band_combo(vars, desc_y_startup_default))) {
    out <- c(out, "Temperatura Mínima e Máxima" = DESC_Y2_TEMP_BANDS_VALUE)
  }
  out
})

precarregar_bases_dashboard <- function() {
  if (!isTRUE(PRELOAD_BASE_ON_STARTUP)) return(invisible(NULL))

  if (identical(DATA_SOURCE_MODE, "agregada")) {
    backend <- ic2025_storage_backend()
    if (identical(backend, "duckdb")) {
      message("[startup] Precarregando backend DuckDB da base agregada...")
      out <- try(diag_step("precarregar DuckDB agregada", {
        precarregar_base_agregada_duckdb()
      }), silent = TRUE)
      if (inherits(out, "try-error")) {
        warning("[startup] Falha ao preparar DuckDB: ", conditionMessage(attr(out, "condition")))
      } else {
        options(ic2025.preloaded_base_agregada_duckdb = TRUE)
        message("[startup] DuckDB pronto para consultas.")
      }
    } else {
      message("[startup] Precarregando base agregada (RDS)...")
      out <- try(diag_step("precarregar RDS agregada", {
        carregar_base_rds(DATA_SOURCE_AGGREGATED_RDS)
      }), silent = TRUE)
      if (inherits(out, "try-error")) {
        warning("[startup] Falha ao precarregar base agregada: ", conditionMessage(attr(out, "condition")))
      } else {
        if (identical(DATA_BASE_LOADING_MODE, "startup_once_no_cache")) {
          options(ic2025.preloaded_base_agregada = out)
          message("[startup] Base agregada precarregada em memória (startup_once_no_cache).")
        } else {
          message("[startup] Base agregada precarregada.")
        }
      }
    }
  } else {
    message("[startup] Precarregando bases legadas...")
    arqs <- c(
      rj = "resultados_tese/Aplicações/Rio de Janeiro/base_cidades_rj.rds",
      sp = "resultados_tese/Aplicações/São Paulo/base_cidades_sp.rds",
      amz = "resultados_tese/Aplicações/Amazônia Legal/base_cidades_amazonia_legal.rds"
    )
    preloaded <- list()
    for (arq in arqs) {
      out <- try(diag_step(paste0("precarregar legado: ", basename(arq)), {
        carregar_base_rds(arq)
      }), silent = TRUE)
      if (inherits(out, "try-error")) {
        warning("[startup] Falha ao precarregar ", arq, ": ", conditionMessage(attr(out, "condition")))
      } else if (identical(DATA_BASE_LOADING_MODE, "startup_once_no_cache")) {
        key <- names(arqs)[which(arqs == arq)][1]
        preloaded[[key]] <- out
      }
    }
    if (identical(DATA_BASE_LOADING_MODE, "startup_once_no_cache")) {
      options(ic2025.preloaded_bases_legacy = preloaded)
    }
    message("[startup] Bases legadas precarregadas.")
  }
  invisible(NULL)
}
diag_step("precarregar_bases_dashboard()", {
  precarregar_bases_dashboard()
})
if (isTRUE(ENABLE_DESC_MAPS) && isTRUE(PRELOAD_DESC_MAP_GEOMETRY_ON_STARTUP)) {
  diag_step("precarregar_geometria_mapa_brasil_refinado()", {
    tryCatch(
      {
        geo_pre <- carregar_geometria_mapa_brasil_refinado()
        if (is.list(geo_pre)) {
          options(ic2025.preloaded_desc_map_geo = geo_pre)
        }
        geo_pre
      },
      error = function(e) warning("[startup] Falha ao precarregar geometria do mapa: ", conditionMessage(e))
    )
  })
}

# Estilo interno do menu lateral: "rounded" (original) ou "flat" (reto)
MENU_STYLE <- "flat"
if (!MENU_STYLE %in% c("rounded", "flat")) {
  stop("MENU_STYLE deve ser 'rounded' ou 'flat'.")
}

asset_prefix <- paste0("ic2025_assets_", as.integer(Sys.time()))
if (dir.exists("www")) {
  shiny::addResourcePath(asset_prefix, normalizePath("www", winslash = "/", mustWork = TRUE))
}
if (dir.exists("cache_geo")) {
  shiny::addResourcePath("ic2025_cache_geo", normalizePath("cache_geo", winslash = "/", mustWork = TRUE))
}

asset_href <- function(file) {
  ver <- tryCatch(as.integer(file.info(file.path("www", file))$mtime), error = function(e) NA_integer_)
  if (!is.finite(ver)) ver <- as.integer(Sys.time())
  sprintf("%s/%s?v=%s", asset_prefix, file, ver)
}

desc_view_tab_type <- if (identical(DESC_VIEW_SELECTOR_STYLE, "underline")) "tabs" else "pills"
desc_view_main_class <- paste(
  "desc-view-switch",
  "desc-view-switch-primary",
  if (identical(DESC_VIEW_SELECTOR_STYLE, "underline")) "desc-view-switch-underline" else "desc-view-switch-outline"
)

cidades_ref <- diag_step("catalogo_cidades_tese()", {
  catalogo_cidades_tese()
})

carregar_catalogo_desc_geo <- function() {
  t0_diag <- proc.time()[["elapsed"]]
  m0_diag <- diag_mem_mb()
  diag_log("carregar_catalogo_desc_geo()", "IN")
  on.exit({
    dt <- proc.time()[["elapsed"]] - t0_diag
    dm <- diag_mem_mb() - m0_diag
    diag_log("carregar_catalogo_desc_geo()", "OUT", sprintf("%.3fs | delta_mem=%+.1fMB", dt, dm))
  }, add = TRUE)

  dir.create("cache_geo", showWarnings = FALSE, recursive = TRUE)
  arq_munis <- file.path("cache_geo", "geobr_municipality_2024.rds")
  arq_pop <- file.path("cache_geo", "sidra_pop_2024.rds")
  uf_meta <- tibble::tribble(
    ~uf, ~estado, ~regiao,
    "AC","Acre","Norte","AL","Alagoas","Nordeste","AP","Amapá","Norte","AM","Amazonas","Norte",
    "BA","Bahia","Nordeste","CE","Ceará","Nordeste","DF","Distrito Federal","Centro-Oeste",
    "ES","Espírito Santo","Sudeste","GO","Goiás","Centro-Oeste","MA","Maranhão","Nordeste",
    "MT","Mato Grosso","Centro-Oeste","MS","Mato Grosso do Sul","Centro-Oeste","MG","Minas Gerais","Sudeste",
    "PA","Pará","Norte","PB","Paraíba","Nordeste","PR","Paraná","Sul","PE","Pernambuco","Nordeste",
    "PI","Piauí","Nordeste","RJ","Rio de Janeiro","Sudeste","RN","Rio Grande do Norte","Nordeste",
    "RS","Rio Grande do Sul","Sul","RO","Rondônia","Norte","RR","Roraima","Norte","SC","Santa Catarina","Sul",
    "SP","São Paulo","Sudeste","SE","Sergipe","Nordeste","TO","Tocantins","Norte"
  )
  normalizar_munis <- function(obj) {
    if (!is.data.frame(obj)) return(NULL)
    nms <- names(obj)
    if (all(c("code_muni", "name_muni", "abbrev_state", "name_state", "name_region") %in% nms)) {
      out <- suppressWarnings(sf::st_drop_geometry(obj)) %>%
        transmute(
          code_muni = normalizar_codigo_municipio(code_muni),
          name_muni = as.character(name_muni),
          uf = as.character(abbrev_state),
          estado = as.character(name_state),
          regiao = as.character(name_region)
        ) %>%
        distinct(code_muni, .keep_all = TRUE)
      return(out)
    }
    if (all(c("code_muni", "name_muni", "uf", "estado", "regiao") %in% nms)) {
      out <- obj %>%
        transmute(
          code_muni = normalizar_codigo_municipio(code_muni),
          name_muni = as.character(name_muni),
          uf = as.character(uf),
          estado = as.character(estado),
          regiao = as.character(regiao),
          pop_2024 = if ("pop_2024" %in% nms) suppressWarnings(as.numeric(pop_2024)) else NA_real_
        ) %>%
        distinct(code_muni, .keep_all = TRUE)
      return(out)
    }
    NULL
  }
  cache_munis_valido <- function(obj) {
    nobj <- normalizar_munis(obj)
    is.data.frame(nobj) && nrow(nobj) > 5000 && all(c("code_muni", "name_muni", "uf", "estado", "regiao") %in% names(nobj))
  }
  cache_pop_valido <- function(obj) {
    is.data.frame(obj) && nrow(obj) > 5000 && all(c("code_muni", "pop_2024") %in% names(obj))
  }
  construir_munis_via_sidra <- function() {
    if (!requireNamespace("sidrar", quietly = TRUE)) return(NULL)
    p_new <- tryCatch(diag_step("catálogo geo: sidra municípios", {
      suppressWarnings(suppressMessages(
        sidrar::get_sidra(api = "/t/6579/n6/all/v/9324/p/2024")
      ))
    }), error = function(e) NULL)
    if (is.null(p_new) || !is.data.frame(p_new) || nrow(p_new) == 0) return(NULL)
    mun_txt <- as.character(p_new$`Município`)
    uf <- stringr::str_extract(mun_txt, "[A-Z]{2}$")
    name_muni <- stringr::str_trim(stringr::str_remove(mun_txt, "\\s*-\\s*[A-Z]{2}$"))
    out <- tibble(
      code_muni = as.integer(p_new$`Município (Código)`) %/% 10L,
      name_muni = name_muni,
      uf = uf,
      pop_2024 = suppressWarnings(as.numeric(p_new$Valor))
    ) %>%
      left_join(uf_meta, by = "uf") %>%
      filter(!is.na(code_muni), !is.na(uf), !is.na(estado), !is.na(regiao)) %>%
      distinct(code_muni, .keep_all = TRUE)
    if (nrow(out) < 5000) return(NULL)
    out
  }

  munis <- if (file.exists(arq_munis)) {
    tryCatch(diag_step("catálogo geo: ler cache municípios", {
      readRDS(arq_munis)
    }), error = function(e) NULL)
  } else if (requireNamespace("geobr", quietly = TRUE)) {
    m_new <- tryCatch(diag_step("catálogo geo: geobr municípios (sem cache local)", {
      suppressWarnings(suppressMessages(
        geobr::read_municipality(year = 2024, showProgress = FALSE, cache = FALSE)
      ))
    }), error = function(e) NULL)
    if (!is.null(m_new)) try(saveRDS(m_new, arq_munis), silent = TRUE)
    m_new
  } else {
    NULL
  }
  if (!cache_munis_valido(munis) && requireNamespace("geobr", quietly = TRUE)) {
    m_new <- tryCatch(diag_step("catálogo geo: geobr fallback municípios", {
      suppressWarnings(suppressMessages(
        geobr::read_municipality(year = 2024, showProgress = FALSE, cache = FALSE)
      ))
    }), error = function(e) NULL)
    if (cache_munis_valido(m_new)) {
      munis <- m_new
      try(saveRDS(munis, arq_munis), silent = TRUE)
    }
  }
  if (!cache_munis_valido(munis)) {
    mun_sidra <- construir_munis_via_sidra()
    if (!is.null(mun_sidra)) {
      munis <- mun_sidra
      try(saveRDS(munis, arq_munis), silent = TRUE)
    }
  }
  if (!cache_munis_valido(munis) && file.exists(arq_munis)) {
    try(unlink(arq_munis), silent = TRUE)
  }
  if (is.null(munis)) {
    return(list(ok = FALSE, msg = "Sem cache de municípios e pacote geobr indisponível.", munis = NULL, estados = NULL, estado_choices = NULL))
  }
  munis <- normalizar_munis(munis)
  if (is.null(munis) || nrow(munis) == 0) {
    return(list(ok = FALSE, msg = "Cache de municípios inválido.", munis = NULL, estados = NULL, estado_choices = NULL))
  }
  if ("estado" %in% names(munis)) {
    munis$estado <- dplyr::recode(
      as.character(munis$estado),
      "Amapa" = "Amapá",
      "Ceara" = "Ceará",
      "Espirito Santo" = "Espírito Santo",
      "Goias" = "Goiás",
      "Maranhao" = "Maranhão",
      "Para" = "Pará",
      "Paraiba" = "Paraíba",
      "Parana" = "Paraná",
      "Piaui" = "Piauí",
      "Rondonia" = "Rondônia",
      "Sao Paulo" = "São Paulo",
      .default = as.character(munis$estado)
    )
  }

  pop <- NULL
  if (file.exists(arq_pop)) {
    pop <- tryCatch(diag_step("catálogo geo: ler cache população", {
      readRDS(arq_pop)
    }), error = function(e) NULL)
  }
  if (!cache_pop_valido(pop) && requireNamespace("sidrar", quietly = TRUE)) {
    p_new <- tryCatch(diag_step("catálogo geo: sidra população", {
      suppressWarnings(suppressMessages(
        sidrar::get_sidra(api = "/t/6579/n6/all/v/9324/p/2024")
      ))
    }), error = function(e) NULL)
    if (!is.null(p_new)) {
      pop <- p_new %>%
        transmute(
          code_muni = as.integer(`Município (Código)`) %/% 10L,
          pop_2024 = suppressWarnings(as.numeric(Valor))
        ) %>%
        distinct(code_muni, .keep_all = TRUE)
      if (cache_pop_valido(pop)) try(saveRDS(pop, arq_pop), silent = TRUE)
    }
  }
  if (!is.null(pop)) {
    munis <- munis %>% left_join(pop, by = "code_muni")
  } else {
    munis <- munis %>% mutate(pop_2024 = NA_real_)
  }

  estados <- munis %>%
    distinct(uf, estado, regiao) %>%
    mutate(estado_rotulo = paste0(estado, " (", uf, ")")) %>%
    arrange(regiao, estado)

  reg_ord <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")
  estado_choices <- split(estados, factor(estados$regiao, levels = reg_ord)) %>%
    lapply(function(df) setNames(df$uf, df$estado_rotulo))
  estado_choices <- estado_choices[lengths(estado_choices) > 0]

  list(ok = TRUE, msg = NULL, munis = munis, estados = estados, estado_choices = estado_choices)
}

desc_geo <- diag_step("desc_geo <- carregar_catalogo_desc_geo()", {
  carregar_catalogo_desc_geo()
})
if (isTRUE(desc_geo$ok) && nrow(cidades_ref) > 0) {
  pop_ref <- desc_geo$munis %>%
    transmute(
      codigo = as.integer(code_muni),
      pop_2024 = suppressWarnings(as.numeric(pop_2024))
    ) %>%
    distinct(codigo, .keep_all = TRUE)

  cidades_ref <- cidades_ref %>%
    left_join(pop_ref, by = "codigo") %>%
    arrange(desc(is.finite(pop_2024)), desc(pop_2024), rotulo) %>%
    dplyr::select(-pop_2024)
}
codigos_com_dados_atuais <- unique(as.integer(cidades_ref$codigo))
cidade_default_key <- if ("manaus" %in% cidades_ref$chave) "manaus" else (cidades_ref$chave[[1]] %||% "manaus")
desc_region_all_value <- "__REGION__"
desc_region_all_label <- "Nenhuma"
desc_state_all_value <- "__STATE__"
desc_state_all_label <- "Nenhuma"
desc_city_all_value <- "__CITY__"
desc_city_all_label <- "Nenhuma"

capital_por_uf <- c(
  AC = "Rio Branco", AL = "Maceió", AP = "Macapá", AM = "Manaus", BA = "Salvador",
  CE = "Fortaleza", DF = "Brasília", ES = "Vitória", GO = "Goiânia", MA = "São Luís",
  MT = "Cuiabá", MS = "Campo Grande", MG = "Belo Horizonte", PA = "Belém", PB = "João Pessoa",
  PR = "Curitiba", PE = "Recife", PI = "Teresina", RJ = "Rio de Janeiro", RN = "Natal",
  RS = "Porto Alegre", RO = "Porto Velho", RR = "Boa Vista", SC = "Florianópolis",
  SP = "São Paulo", SE = "Aracaju", TO = "Palmas"
)

prep_estado_por_uf <- c(
  AC = "do", AL = "de", AP = "do", AM = "do", BA = "da",
  CE = "do", DF = "do", ES = "do", GO = "de", MA = "do",
  MT = "de", MS = "de", MG = "de", PA = "do", PB = "da",
  PR = "do", PE = "de", PI = "do", RJ = "do", RN = "do",
  RS = "do", RO = "de", RR = "de", SC = "de", SP = "de",
  SE = "de", TO = "do"
)

estado_com_preposicao <- function(uf, estado_nome = NULL) {
  uf_sel <- toupper(as.character(uf %||% ""))
  nm <- as.character(estado_nome %||% uf_sel)
  if (!nzchar(nm)) nm <- uf_sel
  prep <- as.character(prep_estado_por_uf[[uf_sel]] %||% "de")
  paste0(prep, " ", nm)
}

txt_norm <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  tolower(trimws(x))
}

desc_munis_uf <- function(uf) {
  uf_sel <- toupper(uf %||% "")
  if (!isTRUE(desc_geo$ok)) return(tibble())
  out <- desc_geo$munis %>%
    filter(.data$uf == !!uf_sel)
  if (length(codigos_com_dados_atuais) > 0) {
    out <- out %>% filter(.data$code_muni %in% codigos_com_dados_atuais)
  }
  out %>%
    arrange(desc(is.finite(pop_2024)), desc(pop_2024), name_muni) %>%
    mutate(nome_exibir = name_muni)
}

desc_regioes_disponiveis <- function() {
  if (!isTRUE(desc_geo$ok) || !is.data.frame(desc_geo$estados)) return(character())
  reg_ord <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")
  regs <- unique(as.character(desc_geo$estados$regiao))
  regs <- regs[!is.na(regs) & nzchar(trimws(regs))]
  ord <- match(regs, reg_ord)
  regs[order(is.na(ord), ord, regs)]
}

desc_regiao_choices <- function() {
  regs <- desc_regioes_disponiveis()
  c(
    setNames(desc_region_all_value, desc_region_all_label),
    setNames(regs, regs)
  )
}

desc_estados_regiao <- function(regiao) {
  reg_sel <- as.character(regiao %||% "")
  empty_est <- tibble::tibble(
    uf = character(),
    estado = character(),
    regiao = character(),
    estado_rotulo = character()
  )
  if (!isTRUE(desc_geo$ok) || !is.data.frame(desc_geo$estados)) return(empty_est)
  if (!nzchar(reg_sel) || identical(reg_sel, desc_region_all_value)) return(empty_est)
  desc_geo$estados %>%
    filter(.data$regiao == !!reg_sel) %>%
    arrange(.data$estado) %>%
    mutate(estado_rotulo = paste0(.data$estado, " (", .data$uf, ")"))
}

desc_estado_choices <- function(regiao) {
  est <- desc_estados_regiao(regiao)
  if (!all(c("uf", "estado") %in% names(est))) {
    return(c(setNames(desc_state_all_value, desc_state_all_label)))
  }
  c(
    setNames(desc_state_all_value, desc_state_all_label),
    setNames(as.character(est$uf), est$estado)
  )
}

desc_regiao_por_uf <- function(uf) {
  uf_sel <- toupper(as.character(uf %||% ""))
  if (!isTRUE(desc_geo$ok) || !is.data.frame(desc_geo$estados) || !nzchar(uf_sel)) return(NA_character_)
  hit <- desc_geo$estados %>% filter(.data$uf == !!uf_sel) %>% slice_head(n = 1)
  if (nrow(hit) == 0) return(NA_character_)
  as.character(hit$regiao[[1]])
}

ufs_da_regiao <- function(regiao) {
  est <- desc_estados_regiao(regiao)
  unique(as.character(est$uf))
}

desc_cidade_choices <- function(munis) {
  c(
    setNames(desc_city_all_value, desc_city_all_label),
    setNames(as.character(munis$code_muni), munis$nome_exibir)
  )
}

cidade_pertence_uf <- function(code_muni, uf) {
  if (!isTRUE(desc_geo$ok) || !is.finite(code_muni)) return(FALSE)
  any(desc_geo$munis$code_muni == as.integer(code_muni) & desc_geo$munis$uf == toupper(uf %||% ""))
}

capital_code_uf <- function(uf) {
  mun <- desc_munis_uf(uf)
  if (nrow(mun) == 0) return(NA_integer_)
  cap <- capital_por_uf[[toupper(uf %||% "")]]
  if (is.null(cap) || !nzchar(cap)) return(mun$code_muni[[1]])
  hit <- mun %>% filter(txt_norm(name_muni) == txt_norm(cap)) %>% pull(code_muni) %>% head(1)
  if (length(hit) == 0) return(mun$code_muni[[1]])
  hit[[1]]
}

desc_estado_default <- if (isTRUE(desc_geo$ok) && is.data.frame(desc_geo$estados) && "SP" %in% (desc_geo$estados$uf %||% character(0))) {
  "SP"
} else if (isTRUE(desc_geo$ok) && is.data.frame(desc_geo$estados) && nrow(desc_geo$estados) > 0) {
  as.character(desc_geo$estados$uf[[1]])
} else {
  "SP"
}
desc_regiao_default <- desc_regiao_por_uf(desc_estado_default)
if (!is.finite(nchar(desc_regiao_default %||% "")) || !nzchar(desc_regiao_default %||% "")) {
  regs0 <- desc_regioes_disponiveis()
  desc_regiao_default <- if (length(regs0) > 0) regs0[[1]] else desc_region_all_value
}
desc_cidade_default <- as.character(capital_code_uf(desc_estado_default))

ui <- dashboardPage(
  dashboardHeader(
    title = "Painel Nacional",
    tags$li(
      class = "dropdown user user-menu",
      tags$a(
        href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown",
        tags$img(src = asset_href("perfil_les.png"), class = "user-image", alt = "LES"),
        tags$span(class = "hidden-xs", "Laboratório de Estatística")
      ),
      tags$ul(
        class = "dropdown-menu",
        tags$li(
          class = "user-header",
          tags$img(src = asset_href("perfil_les.png"), class = "img-circle", alt = "LES"),
          tags$p("Laboratório de Estatística")
        ),
        tags$li(
          class = "user-body",
          tags$div(
            class = "user-theme-panel",
            tags$div(
              class = "user-tools-row",
              tags$a(
                href = "https://github.com/richardamarante",
                target = "_blank",
                rel = "noopener",
                class = "github-link",
                title = "Abrir GitHub",
                tags$i(class = "fa fa-github")
              ),
              tags$div(
                class = "user-switch-wrap",
                checkboxInput("ui_custom_theme", label = "", value = TRUE)
              )
            ),
            tags$div(
              class = "user-theme-palette-wrap",
              tags$div(class = "user-theme-panel-title", "Palheta do dashboard"),
              tags$div(class = "user-theme-panel-copy", "Troque e compare o app inteiro em tempo real."),
              selectInput(
                inputId = "ui_theme_palette",
                label = NULL,
                choices = setNames(names(IC2025_THEME_PRESET_LABELS), unname(IC2025_THEME_PRESET_LABELS)),
                selected = IC2025_THEME_PRESET_DEFAULT_ID,
                selectize = FALSE,
                width = "100%"
              )
            ),
            tags$div(
              class = "user-theme-palette-wrap user-theme-palette-wrap--secondary",
              tags$div(class = "user-theme-panel-title", "Palheta da sidebar principal"),
              tags$div(class = "user-theme-panel-copy", "Misture a cor da sidebar esquerda do dashboard principal com outra palheta."),
              selectInput(
                inputId = "ui_theme_sidebar_palette",
                label = NULL,
                choices = setNames(names(IC2025_THEME_PRESET_LABELS), unname(IC2025_THEME_PRESET_LABELS)),
                selected = IC2025_THEME_SIDEBAR_PRESET_DEFAULT_ID,
                selectize = FALSE,
                width = "100%"
              )
            ),
            tags$div(
              class = "user-theme-toggle-wrap",
              tags$div(class = "user-theme-panel-title", "Conectar quina da topbar"),
              tags$div(class = "user-theme-panel-copy", "Liga a área do “Dashboard” ao mesmo degradê da topbar. A palheta original ignora essa opção."),
              checkboxInput("ui_theme_link_topbar_corner", label = "Usar a quina conectada", value = IC2025_THEME_LINK_TOPBAR_DEFAULT)
            )
          )
        )
      )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Análise Descritiva", tabName = "desc", icon = icon("chart-line")),
      menuItem("Visualização Espacial", tabName = "spatial", icon = icon("map")),
      menuItem(
        "Modelagem",
        icon = icon("chart-area"),
        startExpanded = FALSE,
        menuSubItem("Simulação", tabName = "sim", icon = icon("flask")),
        menuSubItem("Comparação", tabName = "aval", icon = icon("balance-scale")),
        menuSubItem("Aplicação", tabName = "app", icon = icon("globe-americas"))
      )
    )
  ),
  dashboardBody(
    use_theme(tema_amazonia),
    tags$head(
      ic2025_theme_css_tag(),
      ic2025_theme_js_tag(),
      tags$link(rel = "stylesheet", href = "https://api.mapbox.com/mapbox-gl-js/v2.15.0/mapbox-gl.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = asset_href("profile.css")),
      tags$link(id = "custom-css-link", rel = "stylesheet", type = "text/css", href = asset_href("custom.css")),
      tags$link(rel = "stylesheet", type = "text/css", href = asset_href("spatial/spatial-shell.css")),
      if (isTRUE(ENABLE_SPATIAL_COMPACT_DESKTOP)) {
        tags$link(rel = "stylesheet", type = "text/css", href = asset_href("spatial/spatial-shell-compact.css"))
      },
      tags$script(src = "https://api.mapbox.com/mapbox-gl-js/v2.15.0/mapbox-gl.js"),
      tags$script(src = asset_href("spatial/spatial-shell.js")),
      if (MENU_STYLE == "flat") {
        tags$link(id = "custom-flat-css-link", rel = "stylesheet", type = "text/css", href = asset_href("custom-flat.css"))
      },
      tags$style(HTML("
        .desc-map-box > .box-header {
          padding-bottom: 1px;
        }
        .desc-map-box > .box-body {
          padding-top: 0;
        }
        .desc-map-output-wrap {
          position: relative;
          margin-top: -6px;
          height: 860px;
          display: flex;
          flex-direction: column;
        }
        .desc-map-loading-overlay {
          position: absolute;
          inset: 0;
          z-index: 8;
          display: flex;
          align-items: center;
          justify-content: center;
          pointer-events: none;
          background: var(--ic2025-dashboard-loading-overlay-bg);
          opacity: 1;
          visibility: visible;
          transition: opacity 0.18s ease, visibility 0.18s ease;
        }
        .desc-map-output-wrap.desc-map-ready .desc-map-loading-overlay {
          opacity: 0;
          visibility: hidden;
        }
        .desc-map-output-wrap.desc-map-force-loading .desc-map-loading-overlay {
          opacity: 1;
          visibility: visible;
        }
        .desc-map-output-wrap:not(.desc-map-ready) .desc-map-grid,
        .desc-map-output-wrap:not(.desc-map-ready) .desc-map-legend-wrap {
          opacity: 0;
          visibility: hidden;
        }
        .desc-map-output-wrap .desc-map-grid,
        .desc-map-output-wrap .desc-map-legend-wrap {
          transition: opacity 0.18s ease, visibility 0.18s ease;
        }
        .desc-map-loading-card {
          min-width: 220px;
          padding: 18px 24px 16px;
          border-radius: 18px;
          background: var(--ic2025-dashboard-loading-card-bg);
          border: 1px solid var(--ic2025-dashboard-loading-card-border);
          box-shadow: 0 12px 30px var(--ic2025-dashboard-loading-card-shadow);
          display: flex;
          flex-direction: column;
          align-items: center;
          gap: 10px;
        }
        .desc-map-loading-spinner {
          width: 34px;
          height: 34px;
          border-radius: 50%;
          border: 3px solid var(--ic2025-dashboard-loading-spinner-track);
          border-top-color: var(--ic2025-dashboard-loading-spinner-head);
          animation: desc-map-spin 0.8s linear infinite;
        }
        .desc-map-loading-text {
          font-size: 26px;
          line-height: 1;
          font-weight: 700;
          color: var(--ic2025-dashboard-muted);
          letter-spacing: -0.02em;
        }
        .desc-map-loading-subtext {
          font-size: 13px;
          line-height: 1.2;
          font-weight: 600;
          color: var(--ic2025-dashboard-muted-soft);
          text-align: center;
        }
        @keyframes desc-map-spin {
          to { transform: rotate(360deg); }
        }
        .desc-map-grid {
          flex: 0 0 720px;
          height: 720px;
          display: grid;
          gap: 0;
        }
        .desc-map-grid-3 {
          grid-template-columns: 1.15fr 1.15fr 1.05fr 0.90fr 0.90fr;
          grid-template-rows: 360px 360px;
        }
        .desc-map-grid-2 {
          grid-template-columns: 1.45fr 1fr;
          grid-template-rows: 720px;
          align-items: center;
        }
        .desc-map-grid-1 {
          grid-template-columns: 1fr;
          grid-template-rows: 720px;
          align-items: center;
          margin-top: -16px;
        }
        .desc-map-panel {
          min-width: 0;
          min-height: 0;
          overflow: hidden;
          position: relative;
        }
        .desc-map-grid-3 .desc-map-panel-brasil {
          grid-column: 1 / span 3;
          grid-row: 1 / span 2;
        }
        .desc-map-grid-3 .desc-map-panel-regiao {
          grid-column: 4 / span 2;
          grid-row: 1;
        }
        .desc-map-grid-3 .desc-map-panel-estado {
          grid-column: 4 / span 2;
          grid-row: 2;
        }
        .desc-map-grid-2 .desc-map-panel-brasil {
          grid-column: 1;
          grid-row: 1;
        }
        .desc-map-grid-2 .desc-map-panel-regiao {
          grid-column: 2;
          grid-row: 1;
        }
        .desc-map-grid-1 .desc-map-panel-brasil {
          grid-column: 1;
          grid-row: 1;
        }
        .desc-map-panel > .shiny-plot-output,
        .desc-map-legend-wrap > .shiny-plot-output {
          width: 100% !important;
          height: 100% !important;
        }
        .desc-map-panel .shiny-plot-output {
          min-height: 0 !important;
        }
        .desc-map-grid-3 .desc-map-panel-brasil { height: 100%; }
        .desc-map-grid-3 .desc-map-panel-regiao { height: 100%; }
        .desc-map-grid-3 .desc-map-panel-estado { height: 100%; }
        .desc-map-grid-2 .desc-map-panel-brasil { height: 100%; }
        .desc-map-grid-2 .desc-map-panel-regiao { height: 100%; }
        .desc-map-grid-1 .desc-map-panel-brasil { height: 100%; }
        .desc-map-legend-wrap {
          flex: 0 0 78px;
          height: 78px;
          min-height: 78px;
          display: flex;
          align-items: flex-start;
          justify-content: center;
          margin-top: -2px;
        }
      ")),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('desc-map-loading-state', function(message) {
          var wrap = document.getElementById('desc_map_wrap');
          if (!wrap) return;
          var active = !!(message && message.active);
          wrap.classList.toggle('desc-map-force-loading', active);
          if (active) {
            wrap.classList.remove('desc-map-ready');
          }
        });
        $(document).on('click mousedown touchstart',
          '.navbar-nav > .user-menu > .dropdown-menu',
          function(e){ e.stopPropagation(); }
        );
        (function(){
          var descMapReadyTimer = null;
          function syncDescMapLoading(){
            var wrap = document.getElementById('desc_map_wrap');
            if (!wrap) return;
            var panels = wrap.querySelectorAll('.desc-map-panel .shiny-plot-output');
            if (!panels || !panels.length) {
              wrap.classList.remove('desc-map-ready');
              if (descMapReadyTimer) {
                clearTimeout(descMapReadyTimer);
                descMapReadyTimer = null;
              }
              return;
            }
            var allLoaded = true;
            panels.forEach(function(el){
              var img = el.querySelector('img');
              var ok = !!(
                img &&
                img.getAttribute('src') &&
                img.complete &&
                Number(img.naturalWidth || 0) > 0 &&
                Number(el.clientWidth || 0) > 80 &&
                Number(el.clientHeight || 0) > 80
              );
              if (!ok) allLoaded = false;
            });
            if (!allLoaded) {
              wrap.classList.remove('desc-map-ready');
              if (descMapReadyTimer) {
                clearTimeout(descMapReadyTimer);
                descMapReadyTimer = null;
              }
              return;
            }
            if (descMapReadyTimer) clearTimeout(descMapReadyTimer);
            descMapReadyTimer = setTimeout(function(){
              wrap.classList.remove('desc-map-force-loading');
              wrap.classList.add('desc-map-ready');
            }, 180);
          }
          function forceDescMapEagerLoad(){
            var root = document.getElementById('desc_map_wrap');
            if (!root) return;
            var apply = function(){
              root.querySelectorAll('img').forEach(function(img){
                try {
                  img.setAttribute('loading', 'eager');
                  img.setAttribute('fetchpriority', 'high');
                  img.decoding = 'sync';
                } catch(e) {}
              });
              syncDescMapLoading();
            };
            apply();
            if (!root.__descMapObserverAttached) {
              try {
                var obs = new MutationObserver(function(){ apply(); });
                obs.observe(root, { childList: true, subtree: true, attributes: true, attributeFilter: ['src'] });
                root.__descMapObserverAttached = true;
              } catch(e) {}
            }
          }
          document.addEventListener('shiny:value', function(evt){
            var id = evt && evt.target && evt.target.id ? evt.target.id : '';
            if (id === 'desc_map_brasil' || id === 'desc_map_regiao' || id === 'desc_map_estado' || id === 'desc_map_layout') {
              forceDescMapEagerLoad();
            }
          });
          document.addEventListener('shiny:outputinvalidated', function(evt){
            var id = evt && evt.target && evt.target.id ? evt.target.id : '';
            if (id === 'desc_map_brasil' || id === 'desc_map_regiao' || id === 'desc_map_estado' || id === 'desc_map_layout') {
              var wrap = document.getElementById('desc_map_wrap');
              if (wrap) wrap.classList.remove('desc-map-ready');
            }
          });
          function themeVarName(key){
            return '--ic2025-' + String(key || '').replace(/[^a-zA-Z0-9-]+/g, '-');
          }
          function resolveThemePalette(paletteId){
            var presets = window.IC2025_THEME_PRESETS || {};
            var fallbackId = window.IC2025_THEME_PRESET_DEFAULT || 'atual_classica';
            return presets[paletteId] || presets[fallbackId] || window.IC2025_THEME_COLORS || {};
          }
          function cloneThemeObject(obj){
            if (!obj || typeof obj !== 'object') return {};
            return JSON.parse(JSON.stringify(obj));
          }
          function isOriginalThemePalette(paletteId){
            return (paletteId || '') === (window.IC2025_THEME_PRESET_ORIGINAL || 'atual_classica');
          }
          function currentThemeSidebarPaletteId(){
            var node = document.getElementById('ui_theme_sidebar_palette');
            if (node && node.value) return node.value;
            return currentThemePaletteId();
          }
          function currentThemeTopbarCornerLinked(){
            var node = document.getElementById('ui_theme_link_topbar_corner');
            return !!(node && node.checked);
          }
          function syncThemeOptionAvailability(paletteId){
            var original = isOriginalThemePalette(paletteId);
            var sidebarNode = document.getElementById('ui_theme_sidebar_palette');
            var linkedNode = document.getElementById('ui_theme_link_topbar_corner');
            var panel = document.querySelector('.user-theme-panel');
            if (sidebarNode) sidebarNode.disabled = original;
            if (linkedNode) linkedNode.disabled = original;
            if (panel) panel.setAttribute('data-theme-original', original ? 'true' : 'false');
          }
          function buildAppliedThemePalette(paletteId){
            var mainId = paletteId || currentThemePaletteId();
            var palette = cloneThemeObject(resolveThemePalette(mainId));
            if (!palette['dashboard.header_logo_bg'] && palette['dashboard.accent_deep']) {
              palette['dashboard.header_logo_bg'] = palette['dashboard.accent_deep'];
            }
            if (!isOriginalThemePalette(mainId)) {
              var sidebarPalette = resolveThemePalette(currentThemeSidebarPaletteId());
              [
                'sidebar_bg',
                'sidebar_border',
                'sidebar_shadow',
                'sidebar_label',
                'sidebar_text',
                'sidebar_icon',
                'sidebar_hover_bg',
                'sidebar_submenu_bg',
                'sidebar_submenu_border',
                'sidebar_submenu_shadow',
                'sidebar_active_bg',
                'sidebar_active_border',
                'flat_active_bg',
                'flat_active_accent'
              ].forEach(function(key){
                var flatKey = 'dashboard.' + key;
                if (Object.prototype.hasOwnProperty.call(sidebarPalette, flatKey)) {
                  palette[flatKey] = sidebarPalette[flatKey];
                }
              });
              if (currentThemeTopbarCornerLinked() && palette['dashboard.header_nav_bg']) {
                palette['dashboard.header_logo_bg'] = palette['dashboard.header_nav_bg'];
              }
            }
            return palette;
          }
          function syncThemeSidebarPaletteValue(nextPaletteId, previousPaletteId){
            var node = document.getElementById('ui_theme_sidebar_palette');
            if (!node) return;
            var userEdited = node.getAttribute('data-user-edited') === 'true';
            var shouldMirror = !userEdited || !node.value || node.value === (previousPaletteId || '');
            if (shouldMirror) {
              node.value = nextPaletteId || '';
              node.setAttribute('data-user-edited', 'false');
            }
          }
          function primeThemeSidebarCustomDefault(){
            var node = document.getElementById('ui_theme_sidebar_palette');
            if (!node || !node.value) return;
            if (node.value !== currentThemePaletteId()) {
              node.setAttribute('data-user-edited', 'true');
            }
          }
          function applyThemePalette(paletteId){
            var activePaletteId = paletteId || currentThemePaletteId();
            var palette = buildAppliedThemePalette(activePaletteId);
            var root = document.documentElement;
            Object.keys(palette).forEach(function(key){
              root.style.setProperty(themeVarName(key), palette[key]);
            });
            window.IC2025_THEME_COLORS = palette;
            syncThemeOptionAvailability(activePaletteId);
            document.body.setAttribute('data-ic2025-theme-palette', activePaletteId || (window.IC2025_THEME_PRESET_DEFAULT || 'atual_classica'));
            document.body.setAttribute('data-ic2025-theme-sidebar-palette', currentThemeSidebarPaletteId());
            document.body.setAttribute('data-ic2025-theme-linked-logo', (!isOriginalThemePalette(activePaletteId) && currentThemeTopbarCornerLinked()) ? 'true' : 'false');
          }
          function currentThemePaletteId(){
            var node = document.getElementById('ui_theme_palette');
            if (node && node.value) return node.value;
            return window.IC2025_THEME_PRESET_DEFAULT || 'atual_classica';
          }
          function applyThemeToggle(on){
            var link = document.getElementById('custom-css-link');
            var flatLink = document.getElementById('custom-flat-css-link');
            if (link) link.disabled = !on;
            if (flatLink) flatLink.disabled = !on;
          }
          function resizeAllPlotly(){
            if(!(window.Plotly && Plotly.Plots && Plotly.Plots.resize)) return;
            document.querySelectorAll('.js-plotly-plot').forEach(function(el){
              if (el && el.offsetParent !== null) {
                try { Plotly.Plots.resize(el); } catch(e) {}
              }
            });
          }
          $(function(){
            applyThemeToggle($('#ui_custom_theme').prop('checked') === true);
            primeThemeSidebarCustomDefault();
            syncThemeSidebarPaletteValue(currentThemePaletteId(), '');
            applyThemePalette(currentThemePaletteId());
            forceDescMapEagerLoad();
            setTimeout(resizeAllPlotly, 100);
            setTimeout(resizeAllPlotly, 400);
          });
          $(document).on('click mousedown', '.user-theme-panel, .user-theme-panel *', function(evt){
            evt.stopPropagation();
          });
          $(document).on('change', '#ui_custom_theme', function(){
            applyThemeToggle($(this).prop('checked') === true);
            forceDescMapEagerLoad();
            setTimeout(resizeAllPlotly, 80);
            setTimeout(resizeAllPlotly, 250);
          });
          $(document).on('change', '#ui_theme_palette', function(){
            var previousPaletteId = document.body.getAttribute('data-ic2025-theme-palette') || (window.IC2025_THEME_PRESET_DEFAULT || 'atual_classica');
            var nextPaletteId = $(this).val();
            syncThemeSidebarPaletteValue(nextPaletteId, previousPaletteId);
            applyThemePalette(nextPaletteId);
            forceDescMapEagerLoad();
            setTimeout(resizeAllPlotly, 80);
            setTimeout(resizeAllPlotly, 260);
          });
          $(document).on('change', '#ui_theme_sidebar_palette', function(){
            this.setAttribute('data-user-edited', 'true');
            applyThemePalette(currentThemePaletteId());
            forceDescMapEagerLoad();
            setTimeout(resizeAllPlotly, 80);
            setTimeout(resizeAllPlotly, 260);
          });
          $(document).on('change', '#ui_theme_link_topbar_corner', function(){
            applyThemePalette(currentThemePaletteId());
            forceDescMapEagerLoad();
            setTimeout(resizeAllPlotly, 80);
            setTimeout(resizeAllPlotly, 260);
          });
          $(document).on('shiny:reconnected shiny:connected', function(){
            applyThemeToggle($('#ui_custom_theme').prop('checked') === true);
            primeThemeSidebarCustomDefault();
            syncThemeSidebarPaletteValue(currentThemePaletteId(), '');
            applyThemePalette(currentThemePaletteId());
            forceDescMapEagerLoad();
            setTimeout(resizeAllPlotly, 120);
            setTimeout(resizeAllPlotly, 420);
          });
          $(document).on('shown.bs.tab shown.bs.dropdown shown.bs.collapse', function(){
            forceDescMapEagerLoad();
            setTimeout(resizeAllPlotly, 80);
            setTimeout(resizeAllPlotly, 260);
          });
          document.addEventListener('shiny:value', function(evt){
            var id = evt && evt.target && evt.target.id ? evt.target.id : '';
            if (id === 'app_mu' || id === 'app_beta' || id === 'app_sazonal') {
              setTimeout(resizeAllPlotly, 60);
              setTimeout(resizeAllPlotly, 220);
            }
          });
          $(document).on('shiny:value shiny:visualchange', function(){ forceDescMapEagerLoad(); });
          window.addEventListener('resize', function(){ setTimeout(resizeAllPlotly, 40); });
          window.addEventListener('orientationchange', function(){ setTimeout(resizeAllPlotly, 80); });
        })();
        Shiny.addCustomMessageHandler('plotly-resize', function(_) {
          if (window.Plotly && Plotly.Plots && Plotly.Plots.resize) {
            document.querySelectorAll('.js-plotly-plot').forEach(function(el){
              if (el && el.offsetParent !== null) {
                try { Plotly.Plots.resize(el); } catch(e) {}
              }
            });
          }
          setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 50);
          setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 220);
        });
        Shiny.addCustomMessageHandler('eval-inline-progress', function(msg) {
          var wrap = document.getElementById('eval-inline-progress-wrap');
          var bar = document.getElementById('eval-inline-progress-bar');
          var txt = document.getElementById('eval-inline-progress-text');
          if (!wrap || !bar || !txt) return;
          if (!msg || msg.show === false) {
            wrap.style.display = 'none';
            return;
          }
          var done = Number(msg.done || 0);
          var total = Number(msg.total || 1);
          var pct = Math.max(0, Math.min(100, Math.round((done / Math.max(1, total)) * 100)));
          wrap.style.display = 'block';
          bar.style.width = pct + '%';
          txt.textContent = (msg.label || 'Processando...') + ' (' + done + '/' + total + ')';
        });
        Shiny.addCustomMessageHandler('eval-param-lock', function(msg) {
          var wrap = document.getElementById('eval-param-panel');
          if (!wrap) return;
          var lock = !!(msg && msg.lock);
          wrap.style.opacity = lock ? '0.7' : '1';
          wrap.style.pointerEvents = lock ? 'none' : 'auto';
          wrap.querySelectorAll('select').forEach(function(el){
            if (el.selectize) {
              if (lock) el.selectize.disable();
              else el.selectize.enable();
            } else {
              el.disabled = lock;
            }
          });
          wrap.querySelectorAll('input, textarea, button').forEach(function(el){
            if (!el.classList.contains('selectized')) el.disabled = lock;
          });
          wrap.querySelectorAll('.js-range-slider').forEach(function(el){
            var irs = $(el).data('ionRangeSlider');
            if (irs && irs.update) irs.update({ disable: lock });
          });
        });
      "))
    ),
    tabItems(
      tabItem(
        tabName = "spatial",
        spatial_app_ui("spatial_app")
      ),
      tabItem(
        tabName = "desc",
        # fluidRow(
        #   valueBoxOutput("desc_n", width = 3),
        #   valueBoxOutput("desc_med", width = 3),
        #   valueBoxOutput("desc_max", width = 3),
        #   valueBoxOutput("desc_cidades", width = 3)
        # ),
        fluidRow(
          box(
            title = "Filtros", width = 12, status = "primary", solidHeader = TRUE, class = "box-overflow-visible",
            tags$div(
              id = "desc-param-panel",
              fluidRow(
                column(
                  2,
                  selectizeInput(
                    "desc_regiao", "Região",
                    choices = if (isTRUE(desc_geo$ok)) desc_regiao_choices() else c("Nenhuma" = desc_region_all_value),
                    selected = if (isTRUE(desc_geo$ok)) desc_regiao_default else desc_region_all_value,
                    options = list(dropdownParent = "body")
                  )
                ),
                column(
                  2,
                  selectizeInput(
                    "desc_estado", "Estado",
                    choices = if (isTRUE(desc_geo$ok)) desc_estado_choices(desc_regiao_default) else c("Sao Paulo (SP)" = "SP", "Rio de Janeiro (RJ)" = "RJ"),
                    selected = if (isTRUE(desc_geo$ok)) desc_estado_default else "SP",
                    options = list(dropdownParent = "body")
                  )
                ),
                column(
                  2,
                  selectizeInput(
                    "desc_cidade", "Cidade",
                    choices = if (isTRUE(desc_geo$ok)) {
                      m0 <- desc_munis_uf(desc_estado_default)
                      if (nrow(m0) == 0) m0 <- desc_geo$munis %>% filter(.data$uf == desc_estado_default) %>% mutate(nome_exibir = name_muni)
                      desc_cidade_choices(m0)
                    } else {
                      c(
                        setNames(desc_city_all_value, desc_city_all_label),
                        setNames(as.character(cidades_ref$codigo), cidades_ref$rotulo)
                      )
                    },
                    selected = if (isTRUE(desc_geo$ok)) {
                      m0 <- desc_munis_uf(desc_estado_default)
                      if (nrow(m0) == 0) {
                        desc_cidade_default
                      } else {
                        cap <- desc_cidade_default
                        if (cap %in% as.character(m0$code_muni)) cap else as.character(m0$code_muni[[1]])
                      }
                    } else {
                      as.character(cidades_ref$codigo[[1]])
                    },
                    options = list(dropdownParent = "body")
                  )
                ),
                column(3, dateRangeInput("desc_periodo", "Período", start = as.Date("2015-01-01"), end = as.Date("2015-01-31"), format = "dd/mm/yyyy", separator = " até ")),
                column(3, selectizeInput("desc_agreg", "Agregação", choices = c("Diária" = "D", "Semanal" = "W", "Mensal" = "M"), selected = "M", options = list(dropdownParent = "body")))
              ),
              fluidRow(
                column(
                  6,
                  selectizeInput(
                    "desc_y", "Variável principal",
                    choices = desc_y_startup_choices,
                    selected = desc_y_startup_default,
                    options = list(dropdownParent = "body")
                  )
                ),
                column(
                  6,
                  selectizeInput(
                    "desc_y2", "Variável secundária",
                    choices = desc_y2_startup_choices,
                    selected = "__NONE__",
                    options = list(dropdownParent = "body")
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            12,
            tags$div(
              class = desc_view_main_class,
              tabsetPanel(
                id = "desc_view",
                type = desc_view_tab_type,
                selected = "series",
                tabPanel(
                  title = "Série Temporal",
                  value = "series",
                  fluidRow(
                    box(title = uiOutput("desc_series_title"), width = 12, plotlyOutput("desc_plot", height = "520px"))
                  )
                ),
                if (isTRUE(ENABLE_DESC_MAPS)) {
                  tabPanel(
                    title = "Mapas",
                    value = "maps",
                    fluidRow(
                      box(
                        title = uiOutput("desc_map_title"),
                        width = 12,
                        class = "desc-map-box",
                        tags$div(
                          id = "desc_map_wrap",
                          class = "desc-map-output-wrap",
                          tags$div(
                            class = "desc-map-loading-overlay",
                            tags$div(
                              class = "desc-map-loading-card",
                              tags$div(class = "desc-map-loading-spinner"),
                              tags$div(class = "desc-map-loading-text", "Carregando..."),
                              tags$div(class = "desc-map-loading-subtext", "Montando os painéis do mapa")
                            )
                          ),
                          uiOutput("desc_map_layout"),
                          tags$div(class = "desc-map-legend-wrap", plotOutput("desc_map_legend", height = "78px", width = "100%"))
                        )
                      )
                    )
                  )
                }
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "sim",
        fluidRow(
          box(
            title = "Configuração da simulação", width = 4, status = "primary", solidHeader = TRUE, class = "box-overflow-visible",
            tags$div(
              id = "sim-param-panel",
              selectizeInput(
              "sim_tipo", "Modelo",
              choices = c(
                "Modelo Linear Dinâmico" = "dlm",
                "Modelo Linear Dinâmico Generalizado" = "dglm",
                "Modelo Linear Dinâmico Generalizado de Defasagem Polinomial" = "pdldglm"
              ),
              selected = "pdldglm",
              options = list(
                dropdownParent = "body",
                render = I("{
                  item: function(item, escape) {
                    var opt = (this.options && this.options[item.value]) ? this.options[item.value] : item;
                    var full = opt.text || opt.label || item.text || item.label || '';
                    var txt = full;
                    if (item.value === 'pdldglm') {
                      txt = 'Modelo Linear Dinâmico Generalizado de Defasagem';
                    }
                    return '<div title=\"' + escape(full) + '\">' + escape(txt) + '</div>';
                  },
                  option: function(item, escape) {
                    var full = item.text || item.label || '';
                    return '<div>' + escape(full) + '</div>';
                  }
                }")
              )
              ),
              if (isTRUE(SHOW_SIM_SERIES_MODE_INPUT)) {
              selectizeInput(
              "sim_modo", SERIES_MODE_LABEL,
              choices = c("Suavizado" = "suavizado", "Filtrado" = "filtrado", "1-step ahead" = "one_step"),
              selected = "suavizado",
              options = list(dropdownParent = "body")
              )
              },
              conditionalPanel(
              "input.sim_tipo == 'pdldglm'",
              numericInput("sim_n_total", "Tamanho da série", value = 365, min = 150, max = 5000, step = 1),
              sliderInput("sim_lags", "Janela de defasagem (lags)", min = 0, max = 60, value = 16, step = 1),
              selectizeInput("sim_d", "Grau do polinômio", choices = c(2, 3), selected = 2, options = list(dropdownParent = "body")),
              numericInput("sim_namostras", "Número de amostras", value = 1000, min = 0, max = 30000, step = 100),
              numericInput("sim_seed", "Seed", value = 82, min = 1, step = 1),
              tags$hr(),
              numericInput("sim_x0", "Valor inicial de X (x0)", value = 30, step = 0.1),
              numericInput("sim_wx", "Variância de evolução de X (Wx)", value = 10, min = 1e-06, step = 0.001),
              numericInput("sim_alpha1", "Nível inicial log (alpha1)", value = 1.80, step = 0.01),
              numericInput("sim_walpha", "Variância de evolução de alpha (W_alpha)", value = 0.002, min = 1e-06, step = 0.001),
              uiOutput("sim_eta_ui")
              ),
              conditionalPanel(
              "input.sim_tipo == 'dlm'",
              numericInput("sim_dlm_n", "Tamanho da série", value = 201, min = 50, max = 5000, step = 1),
              numericInput("sim_dlm_seed", "Seed", value = 5, min = 1, step = 1),
              numericInput("sim_dlm_m0", "Média inicial (m0)", value = 20, step = 0.1),
              numericInput("sim_dlm_c0", "Variância inicial (C0)", value = 4, min = 0.0001, step = 0.1),
              numericInput("sim_dlm_w", "Variância de evolução (W)", value = 0.5, min = 0.0001, step = 0.01),
              checkboxInput("sim_dlm_v_conhecida", "Variância observacional conhecida (Vt)", TRUE),
              conditionalPanel(
                "input.sim_dlm_v_conhecida == true",
                numericInput("sim_dlm_v", "Variância observacional (Vt)", value = 2, min = 0.0001, step = 0.1)
              )
              ),
              conditionalPanel(
              "input.sim_tipo == 'dglm'",
              numericInput("sim_dglm_n", "Tamanho da série", value = 201, min = 50, max = 5000, step = 1),
              numericInput("sim_dglm_seed", "Seed", value = 5, min = 1, step = 1),
              numericInput("sim_dglm_m0", "Nível inicial log (m0)", value = log(60), step = 0.01),
              numericInput("sim_dglm_c0", "Variância inicial (C0)", value = 0.2, min = 0.0001, step = 0.01),
              numericInput("sim_dglm_w", "Variância de evolução (W)", value = 0.001, min = 1e-06, step = 0.001)
              ),
              conditionalPanel(
              "input.sim_tipo == 'dlm' || input.sim_tipo == 'dglm'",
              textInput("sim_deltas", "Fatores de Desconto (separados por vírgula)", value = "0.90,0.95,0.99")
              )
            )
          ),
          column(
            width = 8,
            box(title = "Ajuste de mu", width = 12, plotlyOutput("sim_fit_mu", height = "420px")),
            conditionalPanel(
              "input.sim_tipo == 'pdldglm'",
              box(title = "Curva de beta", width = 12, plotlyOutput("sim_fit_beta", height = "420px"))
            )
          )
        ),
        fluidRow(
          box(title = "Série simulada", width = 12, plotlyOutput("sim_series", height = "380px")),
          conditionalPanel(
            "input.sim_tipo == 'pdldglm'",
            box(title = "Beta verdadeiro", width = 12, plotlyOutput("sim_beta_true", height = "380px"))
          )
        )
      ),
      tabItem(
        tabName = "app",
        # fluidRow(
        #   valueBoxOutput("app_tau", width = 6),
        #   valueBoxOutput("app_obs", width = 6)
        # ),
        fluidRow(
          box(
            title = "Parâmetros do Modelo", width = 4, status = "success", solidHeader = TRUE, class = "box-overflow-visible",
            tags$div(
              id = "app-param-panel",
              selectizeInput("app_cidade", "Cidade", choices = NULL, selected = NULL, options = list(dropdownParent = "body")),
              selectizeInput("app_ano", "Ano", choices = NULL, options = list(dropdownParent = "body")),
              dateRangeInput("app_periodo", "Período", start = as.Date(sprintf("%d-01-01", EVAL_DEFAULT_YEAR)), end = as.Date(sprintf("%d-12-31", EVAL_DEFAULT_YEAR)), format = "dd/mm/yyyy", separator = " até "),
              selectizeInput("app_modelo", "Modelo", choices = c("PDLDGLM" = "pdldglm", "PDLDGLM c/ covariável" = "clima", "PDLDGLM c/ duo" = "duo"), selected = "clima", options = list(dropdownParent = "body")),
              sliderInput("app_lags", "Janela de defasagem (lags)", min = 0, max = APP_MAX_LAGS, value = min(10L, APP_MAX_LAGS), step = 1),
              selectizeInput("app_d", "Grau do polinômio", choices = c(2, 3), selected = 2, options = list(dropdownParent = "body")),
              numericInput("app_fd", "Fator de desconto", value = 0.98, min = 0.7, max = 1.0, step = 0.01),
              conditionalPanel(
                "input.app_modelo != 'duo'",
                checkboxInput("app_usar_sazonal", "Incluir componente sazonal harmônico", value = APP_SAZONAL_DEFAULT_ENABLED)
              ),
              conditionalPanel(
                "input.app_modelo != 'duo' && input.app_usar_sazonal == true",
                numericInput("app_periodo_sazonal", "Período sazonal", value = APP_SAZONAL_DEFAULT_PERIOD, min = 2, step = 1),
                numericInput("app_ordem_sazonal", "Ordem harmônica", value = APP_SAZONAL_DEFAULT_ORDER, min = 1, step = 1),
                numericInput("app_fd_sazonal", "Fator de desconto sazonal", value = APP_SAZONAL_DEFAULT_FD, min = 0.7, max = 1.0, step = 0.005)#,
                #tags$div(
                  #style = "margin-top: 6px; font-size: 12px; line-height: 1.35; color: var(--ic2025-dashboard-muted);",
                  #HTML("Na formulação dinâmica, a sazonalidade entra como um bloco harmônico adicional. O período deve ser informado em número de observações; para série diária com ciclo anual, use <b>365</b>.")
                #)
              ),
              conditionalPanel(
                "input.app_modelo == 'clima' || input.app_modelo == 'duo'",
                selectizeInput("app_covar", "Covariável", choices = c("Temperatura Média (°C)" = "temp", "Umidade Relativa (%)" = "umid"), selected = "temp", options = list(dropdownParent = "body"))
              ),
              conditionalPanel(
                "input.app_modelo == 'clima'",
                numericInput("app_lag_covar", "Defasagem da covariável", value = 0, min = 0, max = 30, step = 1),
                sliderInput("app_perc", "Faixa de percentil", min = 0, max = 1, value = c(0.85, 1.00), step = 0.05),
                checkboxInput("app_show_tau", "Mostrar efeito climático (tau) na curva", value = FALSE)
              ),
              conditionalPanel(
                "input.app_modelo == 'duo'",
                sliderInput("app_lags_covar", "Janela da covariável (lags)", min = 2, max = APP_MAX_LAGS, value = min(10L, APP_MAX_LAGS), step = 1)
              ),
              if (isTRUE(SHOW_APP_SERIES_MODE_INPUT)) {
                selectizeInput("app_modo", SERIES_MODE_LABEL, choices = c("Suavizado" = "suavizado", "Filtrado" = "filtrado", "1-step ahead" = "one_step"), selected = "suavizado", options = list(dropdownParent = "body"))
              }
            )
          ),
          column(
            width = 8,
            box(title = "Ajuste de mu", width = 12, plotlyOutput("app_mu", height = "420px")),
            box(title = "Curva de beta", width = 12, plotlyOutput("app_beta", height = "420px")),
            conditionalPanel(
              "input.app_modelo == 'duo'",
              box(title = "Curva da covariável", width = 12, plotlyOutput("app_tau_curve", height = "420px"))
            ),
            conditionalPanel(
              "input.app_modelo != 'duo' && input.app_usar_sazonal == true",
              box(title = "Componente sazonal harmônico", width = 12, plotlyOutput("app_sazonal", height = "360px"))
            )
          )
        ),
        fluidRow(
          box(title = "Série da variável principal", width = 12, plotlyOutput("app_x_series", height = "320px"))
        ),
        conditionalPanel(
          "input.app_modelo == 'clima' || input.app_modelo == 'duo'",
          fluidRow(
            box(title = "Série da covariável", width = 12, plotlyOutput("app_covar_series", height = "320px"))
          )
        )
      ),
      tabItem(
        tabName = "aval",
        fluidRow(
          box(
            title = "Parâmetros da Avaliação", width = 4, status = "success", solidHeader = TRUE, class = "box-overflow-visible",
            tags$div(
              id = "eval-param-panel",
              selectizeInput("eval_cidade", "Cidade", choices = NULL, selected = NULL, options = list(dropdownParent = "body")),
              selectizeInput("eval_ano", "Ano", choices = NULL, options = list(dropdownParent = "body")),
              dateRangeInput("eval_periodo", "Período", start = as.Date(sprintf("%d-01-01", EVAL_DEFAULT_YEAR)), end = as.Date(sprintf("%d-12-31", EVAL_DEFAULT_YEAR)), format = "dd/mm/yyyy", separator = " até "),
              checkboxInput("eval_buscar_lags", "Buscar automaticamente a melhor janela de defasagem (lags)", value = FALSE),
              tags$div(
                class = if (isTRUE(ENABLE_EVAL_AUTO_CLIMA_SWITCH)) NULL else "ui-disabled-lite",
                checkboxInput("eval_buscar_clima", "Buscar automaticamente a melhor covariável climática e ℓ", value = FALSE)
              ),
              checkboxInput("eval_buscar_sazonal", "Buscar automaticamente o melhor componente sazonal harmônico", value = FALSE),
              uiOutput("eval_lags_ui"),
              uiOutput("eval_sazonal_ui"),
              tags$div(
                id = "eval-inline-progress-wrap",
                style = "display:none; margin-top:10px; margin-bottom:6px;",
                tags$div(
                  style = "height:10px; border-radius:8px; background:var(--ic2025-dashboard-progress-track); overflow:hidden;",
                  tags$div(
                    id = "eval-inline-progress-bar",
                    style = "height:10px; width:0%; background:var(--ic2025-dashboard-progress-fill); transition:width .18s linear;"
                  )
                ),
                tags$div(
                  id = "eval-inline-progress-text",
                  style = "margin-top:6px; font-size:12px; color:var(--ic2025-dashboard-progress-text);"
                )
              ),
              conditionalPanel(
                "input.eval_buscar_lags == true || input.eval_buscar_clima == true || input.eval_buscar_sazonal == true",
                actionButton(
                  "eval_run_button",
                  "Iniciar Busca de Modelos",
                  class = "btn-success btn-block",
                  style = "margin-top: 6px; margin-bottom: 10px;"
                )
              ),
              selectizeInput("eval_m1", "Métrica prioritária #1", choices = NULL, selected = NULL, options = list(dropdownParent = "body")),
              selectizeInput("eval_m2", "Métrica prioritária #2", choices = NULL, selected = NULL, options = list(dropdownParent = "body")),
              selectizeInput("eval_m3", "Métrica prioritária #3", choices = NULL, selected = NULL, options = list(dropdownParent = "body")),
              if (isTRUE(SHOW_EVAL_SERIES_MODE_INPUT)) {
                selectizeInput("eval_modo", SERIES_MODE_LABEL, choices = c("Suavizado" = "suavizado", "Filtrado" = "filtrado", "1-step ahead" = "one_step"), selected = "suavizado", options = list(dropdownParent = "body"))
              }
            )
          ),
          tabBox(
            id = "eval_top_tabs",
            title = "Top 3 Modelos",
            width = 8,
            tabPanel(
              "Modelo 1",
              fluidRow(
                box(title = "Ajuste de mu", width = 12, plotlyOutput("eval_mu_1", height = "420px"))
              ),
              fluidRow(
                box(title = "Curva de beta", width = 12, plotlyOutput("eval_beta_1", height = "420px"))
              ),
              fluidRow(
                box(width = 12, status = "primary", solidHeader = FALSE, DTOutput("eval_metrics_1"))
              )
            ),
            tabPanel(
              "Modelo 2",
              fluidRow(
                box(title = "Ajuste de mu", width = 12, plotlyOutput("eval_mu_2", height = "420px"))
              ),
              fluidRow(
                box(title = "Curva de beta", width = 12, plotlyOutput("eval_beta_2", height = "420px"))
              ),
              fluidRow(
                box(width = 12, status = "primary", solidHeader = FALSE, DTOutput("eval_metrics_2"))
              )
            ),
            tabPanel(
              "Modelo 3",
              fluidRow(
                box(title = "Ajuste de mu", width = 12, plotlyOutput("eval_mu_3", height = "420px"))
              ),
              fluidRow(
                box(title = "Curva de beta", width = 12, plotlyOutput("eval_beta_3", height = "420px"))
              ),
              fluidRow(
                box(width = 12, status = "primary", solidHeader = FALSE, DTOutput("eval_metrics_3"))
              )
            )
          )
        )
      )
    )
  )
)

diag_log("Definições globais concluídas (pronto para iniciar server)", "INFO")

server <- function(input, output, session) {
  desc_inicio_padrao <- as.Date("2015-01-01")
  desc_boot_t0 <- Sys.time()
  desc_boot_sec <- 4
  tema_palheta_ativa <- reactive({
    selected <- as.character(input$ui_theme_palette)
    if (!length(selected) || !nzchar(selected[[1]]) || !(selected[[1]] %in% names(IC2025_THEME_PRESETS_FLAT))) {
      IC2025_THEME_PRESET_DEFAULT_ID
    } else {
      selected[[1]]
    }
  })
  plotly_layout_base <- new.env(parent = emptyenv())
  makeActiveBinding("paper_bgcolor", function() ic2025_theme_value("dashboard.surface"), plotly_layout_base)
  makeActiveBinding("plot_bgcolor", function() ic2025_theme_value("dashboard.surface"), plotly_layout_base)
  makeActiveBinding("font", function() {
    list(family = "Manrope, Segoe UI, Arial, sans-serif", size = 13, color = ic2025_theme_value("dashboard.ink_soft"))
  }, plotly_layout_base)
  makeActiveBinding("margin", function() {
    list(t = 34, r = 18, b = 52, l = 62)
  }, plotly_layout_base)
  makeActiveBinding("legend", function() {
    list(orientation = "h", x = 0, y = 1.10, font = list(size = 12))
  }, plotly_layout_base)
  makeActiveBinding("hoverlabel", function() {
    list(
      bgcolor = ic2025_theme_value("dashboard.ink"),
      bordercolor = ic2025_theme_value("dashboard.ink"),
      font = list(color = ic2025_theme_value("dashboard.surface"), size = 12)
    )
  }, plotly_layout_base)
  makeActiveBinding("xaxis", function() {
    list(
      showgrid = TRUE,
      gridcolor = ic2025_theme_value("dashboard.grid"),
      zeroline = FALSE,
      tickfont = list(size = 12),
      title = list(standoff = 8)
    )
  }, plotly_layout_base)
  makeActiveBinding("yaxis", function() {
    list(
      showgrid = TRUE,
      gridcolor = ic2025_theme_value("dashboard.grid"),
      zeroline = FALSE,
      tickfont = list(size = 12),
      title = list(standoff = 10)
    )
  }, plotly_layout_base)
  spatial_tab_ativa <- reactive({
    identical(input$tabs %||% "", "spatial")
  })
  spatial_refresh_tick <- reactiveVal(0L)
  observeEvent(input$tabs, {
    if (identical(input$tabs %||% "", "spatial")) {
      spatial_refresh_tick(spatial_refresh_tick() + 1L)
    }
  }, ignoreInit = TRUE)
  observe({
    session$sendCustomMessage(
      "ic2025-spatial-shell",
      list(active = isTRUE(spatial_tab_ativa()))
    )
  })
  observeEvent(input[["spatial_app-back"]], {
    shinydashboard::updateTabItems(session, "tabs", selected = "desc")
  }, ignoreInit = TRUE)
  spatial_app_server("spatial_app", active = spatial_tab_ativa, refresh = reactive(spatial_refresh_tick()))

  # Cache em RAM da aba descritiva (não persiste em arquivo).
  # Não persiste em arquivo e vive apenas durante a sessão do app.
  desc_none_cache <- new.env(parent = emptyenv())
  desc_data_cache <- new.env(parent = emptyenv())
  desc_map_geo_cache <- new.env(parent = emptyenv())
  desc_map_data_cache <- new.env(parent = emptyenv())
  desc_map_panel_cache <- new.env(parent = emptyenv())
  desc_map_cache_meta <- new.env(parent = emptyenv())
  desc_map_cache_meta$data_order <- character()
  desc_map_cache_meta$panel_order <- character()
  desc_cache_fetch <- function(env, key, loader, store_if = function(x) !is.null(x), max_n = Inf, order_name = NULL) {
    if (exists(key, envir = env, inherits = FALSE)) {
      return(get(key, envir = env, inherits = FALSE))
    }
    out <- loader()
    if (isTRUE(store_if(out))) {
      assign(key, out, envir = env)
      if (!is.null(order_name) && exists(order_name, envir = desc_map_cache_meta, inherits = FALSE)) {
        ord <- get(order_name, envir = desc_map_cache_meta, inherits = FALSE)
        ord <- c(setdiff(ord, key), key)
        if (is.finite(max_n) && length(ord) > max_n) {
          drop_keys <- ord[seq_len(length(ord) - max_n)]
          for (k in drop_keys) {
            if (exists(k, envir = env, inherits = FALSE)) rm(list = k, envir = env)
          }
          ord <- tail(ord, max_n)
        }
        assign(order_name, ord, envir = desc_map_cache_meta)
      }
    }
    out
  }
  get_desc_map_geo <- function() {
    desc_cache_fetch(
      env = desc_map_geo_cache,
      key = "geo",
      loader = function() {
        geo_pre <- getOption("ic2025.preloaded_desc_map_geo", NULL)
        if (is.list(geo_pre) && all(c("municipios", "estados", "labels") %in% names(geo_pre))) {
          return(geo_pre)
        }
        carregar_geometria_mapa_brasil_refinado()
      }
    )
  }
  desc_none_cache_fetch <- function(key, loader) {
    if (exists(key, envir = desc_none_cache, inherits = FALSE)) {
      return(get(key, envir = desc_none_cache, inherits = FALSE))
    }
    out <- loader()
    if (is.data.frame(out) && nrow(out) > 0) {
      assign(key, out, envir = desc_none_cache)
    }
    out
  }
  desc_data_cache_fetch <- function(key, loader) {
    if (exists(key, envir = desc_data_cache, inherits = FALSE)) {
      return(get(key, envir = desc_data_cache, inherits = FALSE))
    }
    out <- loader()
    if (is.data.frame(out)) {
      assign(key, out, envir = desc_data_cache)
    }
    out
  }
  rotulo_nome_generico_desc <- function(v) {
    v <- as.character(v %||% "")
    if (!nzchar(v)) return("")
    txt <- gsub("_", " ", v)
    txt <- gsub("\\.", " ", txt)
    txt <- gsub("([a-z])([A-Z])", "\\1 \\2", txt)
    txt <- stringr::str_squish(txt)
    stringr::str_to_sentence(txt, locale = "pt")
  }

  rotulo_var_desc <- function(v) {
    dplyr::case_when(
      v %in% c("CasosCID10J", "Casos_Resp", "Internacoes_Resp", "InternacoesResp") ~ "Internações por Doenças do Aparelho Respiratório",
      v %in% c("CasosCID10I", "Internacoes_Circ", "InternacoesCirc") ~ "Internações por Doenças do Aparelho Circulatório",
      v %in% c("ObitosCID10J", "Obitos_Resp", "ObitosResp") ~ "Óbitos por Doenças do Aparelho Respiratório",
      v %in% c("ObitosCID10I", "Obitos_Circ", "ObitosCirc") ~ "Óbitos por Doenças do Aparelho Circulatório",
      v %in% c("PM2p5", "pm25") ~ "PM2.5 (\u00b5g/m\u00b3)",
      v %in% c("PM10", "pm10") ~ "PM10 (\u00b5g/m\u00b3)",
      desc_is_temp_media_var(v) ~ "Temperatura Média (\u00b0C)",
      desc_is_temp_min_var(v) ~ "Temperatura Mínima (\u00b0C)",
      desc_is_temp_max_var(v) ~ "Temperatura Máxima (\u00b0C)",
      v %in% c("UmidRel", "umid", "umidade_relativa", "UmidadeRelativa") ~ "Umidade Relativa (%)",
      v %in% c("VelocVento", "veloc_vento", "vento", "VelocidadeVento", "velocidade_vento") ~ "Velocidade do Vento (km/h)",
      v %in% c("SensTermica", "sens_termica", "SensacaoTermica", "sensacao_termica", "sensacaoTermica") ~ "Sensação Térmica (\u00b0C)",
      v %in% c("CO", "co") ~ "CO (ppm)",
      TRUE ~ rotulo_nome_generico_desc(v)
    )
  }

  rotulo_curto_desc <- function(v) {
    dplyr::case_when(
      v %in% c("CasosCID10J", "Casos_Resp", "CasosCID10I", "Internacoes_Resp", "InternacoesResp", "Internacoes_Circ", "InternacoesCirc") ~ "Internações",
      v %in% c("ObitosCID10J", "Obitos_Resp", "ObitosCID10I", "ObitosResp", "Obitos_Circ", "ObitosCirc") ~ "Óbitos",
      TRUE ~ rotulo_var_desc(v)
    )
  }

  grupo_var_desc <- function(v) {
    v0 <- tolower(as.character(v %||% ""))
    if (!nzchar(v0)) return("Clima")

    eh_pol <- v0 %in% tolower(c("PM2p5", "pm25", "PM25", "PM10", "pm10", "CO", "co", "NO2", "no2", "SO2", "so2", "O3", "o3"))
    eh_hosp <- grepl("casos|interna|obit|cid10|resp|circ", v0)

    if (isTRUE(eh_pol)) return("Qualidade do Ar")
    if (isTRUE(eh_hosp)) return("Saúde Pública")
    "Clima"
  }

  montar_choices_desc_agrupadas <- function(vars, incluir_none = FALSE) {
    vars <- as.character(vars %||% character(0))
    vars <- vars[nzchar(vars)]
    if (length(vars) == 0) {
      if (isTRUE(incluir_none)) return(c("Nenhuma" = "__NONE__"))
      return(character(0))
    }

    labels <- vapply(vars, rotulo_var_desc, character(1))
    grupos <- vapply(vars, grupo_var_desc, character(1))
    ordem_grupos <- c("Clima", "Saúde Pública", "Qualidade do Ar")
    ordem_clima <- c(
      "Sensação Térmica (\u00b0C)",
      "Temperatura Média (\u00b0C)",
      "Temperatura Mínima (\u00b0C)",
      "Temperatura Máxima (\u00b0C)",
      "Umidade Relativa (%)",
      "Velocidade do Vento (km/h)"
    )

    out <- list()
    if (isTRUE(incluir_none)) {
      out[["Geral"]] <- c("Nenhuma" = "__NONE__")
    }

    for (g in ordem_grupos) {
      idx <- which(grupos == g)
      if (length(idx) == 0) next
      if (identical(g, "Clima")) {
        rk <- match(labels[idx], ordem_clima)
        if (anyNA(rk)) {
          rk[is.na(rk)] <- length(ordem_clima) + seq_len(sum(is.na(rk)))
        }
        idx <- idx[order(rk, labels[idx])]
      } else {
        idx <- idx[order(labels[idx])]
      }
      out[[g]] <- setNames(vars[idx], labels[idx])
    }
    out
  }

  desc_y2_choices <- function(vars_sec, vars_all, y_sel) {
    out <- montar_choices_desc_agrupadas(vars_sec, incluir_none = TRUE)
    if (isTRUE(desc_can_use_temp_band_combo(vars_all, y_sel))) {
      geral <- out[["Geral"]]
      if (is.null(geral)) geral <- c()
      if (!(DESC_Y2_TEMP_BANDS_VALUE %in% unname(geral))) {
        geral <- c(geral, "Temperatura Mínima e Máxima" = DESC_Y2_TEMP_BANDS_VALUE)
      }
      out[["Geral"]] <- geral
    }
    out
  }

  plot_placeholder <- function(msg) {
    plot_ly(type = "scatter", mode = "lines") %>%
      layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations = list(list(
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          text = as.character(msg %||% "Sem dados disponíveis."),
          showarrow = FALSE, align = "center",
          font = list(size = 14, color = ic2025_theme_value("dashboard.muted"))
        )),
        paper_bgcolor = ic2025_theme_value("dashboard.surface"),
        plot_bgcolor = ic2025_theme_value("dashboard.surface")
      )
  }

  info_modelagem_df <- function(df) {
    if (!is.data.frame(df) || nrow(df) == 0) {
      msg_load <- attr(df, "load_error", exact = TRUE)
      if (!is.character(msg_load) || length(msg_load) == 0 || !nzchar(msg_load[[1]] %||% "")) {
        msg_load <- "Sem dados para a cidade/período selecionados."
      }
      return(list(
        ok = FALSE,
        has_resp = FALSE,
        has_pm25 = FALSE,
        covars = character(0),
        msg = msg_load[[1]]
      ))
    }
    has_resp <- ("Casos_Resp" %in% names(df)) && any(is.finite(suppressWarnings(as.numeric(df$Casos_Resp))))
    has_pm25 <- ("pm25" %in% names(df)) && any(is.finite(suppressWarnings(as.numeric(df$pm25))))
    covars_all <- c("temp", "umid", "VelocVento", "SensTermica")
    covars <- covars_all[vapply(covars_all, function(v) {
      (v %in% names(df)) && any(is.finite(suppressWarnings(as.numeric(df[[v]]))))
    }, logical(1))]

    faltantes <- c()
    if (!has_resp) faltantes <- c(faltantes, rotulo_var_desc("Casos_Resp"))
    if (!has_pm25) faltantes <- c(faltantes, rotulo_var_desc("pm25"))
    msg <- if (length(faltantes) > 0) {
      paste0(
        "A base atual não contém ",
        paste(faltantes, collapse = " e "),
        " para rodar os modelos nesta aba."
      )
    } else {
      ""
    }

    list(
      ok = TRUE,
      has_resp = has_resp,
      has_pm25 = has_pm25,
      covars = covars,
      msg = msg
    )
  }

  bases_cache <- reactiveValues(rj = NULL, sp = NULL, amz = NULL, br = NULL)
  model_cache <- reactiveValues(
    dados = new.env(parent = emptyenv()),
    ajuste = new.env(parent = emptyenv())
  )
  sim_cache <- reactiveValues(
    sim = new.env(parent = emptyenv()),
    fit = new.env(parent = emptyenv())
  )
  eval_cache <- reactiveValues(
    fit = new.env(parent = emptyenv()),
    clima = new.env(parent = emptyenv()),
    sazonal = new.env(parent = emptyenv())
  )
  cidades_named <- setNames(cidades_ref$chave, cidades_ref$rotulo)
  sim_tab_ativa <- reactive({
    identical(input$tabs %||% "", "sim")
  })
  app_batch_updating <- reactiveVal(FALSE)
  app_programmatic_update <- reactiveVal(FALSE)
  app_primeira_execucao <- reactiveVal(TRUE)
  app_tab_ativa <- reactive({
    identical(input$tabs %||% "", "app")
  })
  periodo_modelagem_por_ano <- function(ano) {
    ano_i <- suppressWarnings(as.integer(ano))
    if (length(ano_i) == 0 || !is.finite(ano_i[[1]])) {
      return(c(as.Date(NA), as.Date(NA)))
    }
    ano_i <- as.integer(ano_i[[1]])
    c(as.Date(sprintf("%d-01-01", ano_i)), as.Date(sprintf("%d-12-31", ano_i)))
  }
  periodo_modelagem_limites <- function(anos) {
    anos_i <- suppressWarnings(as.integer(anos))
    anos_i <- anos_i[is.finite(anos_i)]
    if (length(anos_i) == 0) {
      return(c(as.Date(NA), as.Date(NA)))
    }
    c(as.Date(sprintf("%d-01-01", min(anos_i))), as.Date(sprintf("%d-12-31", max(anos_i))))
  }
  normalizar_periodo_modelagem <- function(periodo, fallback_ano = NA_integer_) {
    vals <- tryCatch(as.Date(periodo), error = function(e) as.Date(c(NA, NA)))
    if (length(vals) < 2 || any(is.na(vals[1:2]))) {
      return(periodo_modelagem_por_ano(fallback_ano))
    }
    c(min(vals[1:2]), max(vals[1:2]))
  }
  formatar_periodo_modelagem <- function(data_ini, data_fim) {
    di <- tryCatch(as.Date(data_ini), error = function(e) as.Date(NA))
    df <- tryCatch(as.Date(data_fim), error = function(e) as.Date(NA))
    if (is.na(di) || is.na(df)) return("NA")
    paste0(format(di, "%d/%m/%Y"), " até ", format(df, "%d/%m/%Y"))
  }
  configurar_xaxis_periodo_modelagem <- function(datas, periodo_ref = NULL) {
    xaxis_out <- modifyList(plotly_layout_base$xaxis, list(title = "Data"))
    datas_ref <- tryCatch(as.Date(datas), error = function(e) as.Date(character()))
    datas_ref <- datas_ref[!is.na(datas_ref)]
    if (length(datas_ref) == 0) return(xaxis_out)

    dr <- range(datas_ref)
    if (!all(is.finite(dr))) return(xaxis_out)
    span <- as.numeric(dr[2] - dr[1])
    if (!is.finite(span) || span > 370) return(xaxis_out)

    periodo_norm <- tryCatch(as.Date(periodo_ref), error = function(e) as.Date(c(NA, NA)))
    if (length(periodo_norm) >= 2 && all(!is.na(periodo_norm[1:2]))) {
      periodo_norm <- c(min(periodo_norm[1:2]), max(periodo_norm[1:2]))
    } else {
      periodo_norm <- dr
    }

    cobre_ano_calendario <- identical(format(periodo_norm[[1]], "%m-%d"), "01-01") &&
      identical(format(periodo_norm[[2]], "%m-%d"), "12-31") &&
      identical(format(periodo_norm[[1]], "%Y"), format(periodo_norm[[2]], "%Y"))

    if (isTRUE(cobre_ano_calendario)) {
      ano_ref <- as.integer(format(periodo_norm[[1]], "%Y"))
      tickvals <- seq(as.Date(sprintf("%d-01-01", ano_ref)), by = "month", length.out = 12)
      return(modifyList(
        xaxis_out,
        list(
          tickmode = "array",
          tickvals = tickvals,
          ticktext = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"),
          range = c(as.Date(sprintf("%d-01-01", ano_ref)), as.Date(sprintf("%d-12-31", ano_ref)))
        )
      ))
    }

    modifyList(
      xaxis_out,
      list(
        tickformat = "%d/%m/%Y",
        range = c(periodo_norm[[1]], periodo_norm[[2]])
      )
    )
  }
  nucleo_state <- reactiveVal(NULL)
  deps_inited_flag <- new.env(parent = emptyenv())
  deps_inited_flag$value <- FALSE
  deps_status <- status_dependencias_pdldglm()
  deps_ok <- all(deps_status$instalado)
  sim_deps_inited <- reactiveVal(FALSE)

  # Server-side selectize para listas grandes de cidades (evita freeze e warning).
  observe({
    updateSelectizeInput(
      session, "app_cidade",
      choices = cidades_named,
      selected = cidade_default_key,
      server = TRUE
    )
    updateSelectizeInput(
      session, "eval_cidade",
      choices = cidades_named,
      selected = cidade_default_key,
      server = TRUE
    )
  })

  get_nucleo <- reactive({
    st <- nucleo_state()
    if (is.null(st)) {
      st <- carregar_nucleo_pdldglm("resultados_tese/Aplicações/PDLDGLM_Geral_deploy.R")
      nucleo_state(st)
    }
    st
  })

  get_base <- function(id) {
    if (identical(DATA_SOURCE_MODE, "agregada")) {
      if (is.null(bases_cache$br)) {
        pre <- getOption("ic2025.preloaded_base_agregada", NULL)
        if (is.data.frame(pre) && nrow(pre) > 0) {
          bases_cache$br <- pre
        } else {
          bases_cache$br <- carregar_base_rds(DATA_SOURCE_AGGREGATED_RDS)
        }
      }
      return(bases_cache$br)
    }
    if (identical(id, "rj")) {
      if (is.null(bases_cache$rj)) {
        pre <- getOption("ic2025.preloaded_bases_legacy", NULL)
        if (is.list(pre) && is.data.frame(pre$rj) && nrow(pre$rj) > 0) {
          bases_cache$rj <- pre$rj
        } else {
          bases_cache$rj <- carregar_base_rds("resultados_tese/Aplicações/Rio de Janeiro/base_cidades_rj.rds")
        }
      }
      return(bases_cache$rj)
    }
    if (identical(id, "sp")) {
      if (is.null(bases_cache$sp)) {
        pre <- getOption("ic2025.preloaded_bases_legacy", NULL)
        if (is.list(pre) && is.data.frame(pre$sp) && nrow(pre$sp) > 0) {
          bases_cache$sp <- pre$sp
        } else {
          bases_cache$sp <- carregar_base_rds("resultados_tese/Aplicações/São Paulo/base_cidades_sp.rds")
        }
      }
      return(bases_cache$sp)
    }
    if (is.null(bases_cache$amz)) {
      pre <- getOption("ic2025.preloaded_bases_legacy", NULL)
      if (is.list(pre) && is.data.frame(pre$amz) && nrow(pre$amz) > 0) {
        bases_cache$amz <- pre$amz
      } else {
        bases_cache$amz <- carregar_base_rds("resultados_tese/Aplicações/Amazônia Legal/base_cidades_amazonia_legal.rds")
      }
    }
    bases_cache$amz
  }

  perc_lado_para_faixa <- function(perc, lado) {
    p <- suppressWarnings(as.numeric(perc))
    if (!is.finite(p)) p <- 0.85
    p <- min(max(p, 0.01), 0.99)
    if (identical(lado, "abaixo")) {
      return(c(0, p))
    }
    c(p, 1)
  }

  faixa_para_perc_lado <- function(faixa, covar = "temp") {
    r <- suppressWarnings(as.numeric(faixa))
    if (length(r) < 2 || any(!is.finite(r))) {
      return(if (identical(covar, "umid")) list(perc = 0.15, lado = "abaixo", perc_sup = NULL) else list(perc = 0.85, lado = "acima", perc_sup = NULL))
    }
    r <- sort(r[1:2])
    r[1] <- min(max(r[1], 0), 1)
    r[2] <- min(max(r[2], 0), 1)

    # Interpretação fiel ao modelo:
    # [p, 1] -> lado = acima, perc = p
    # [0, p] -> lado = abaixo, perc = p
    if (r[1] <= 0 && r[2] < 1) return(list(perc = min(max(r[2], 0.01), 0.99), lado = "abaixo", perc_sup = NULL))
    if (r[2] >= 1 && r[1] > 0) return(list(perc = min(max(r[1], 0.01), 0.99), lado = "acima", perc_sup = NULL))

    # Intervalo interno [p_inf, p_sup]
    list(
      perc = min(max(r[1], 0.01), 0.99),
      lado = "acima",
      perc_sup = min(max(r[2], 0.01), 0.99)
    )
  }

  base_id_por_uf <- function(uf) {
    if (identical(DATA_SOURCE_MODE, "agregada")) return("br")
    uf <- toupper(uf %||% "")
    if (uf == "RJ") return("rj")
    if (uf == "SP") return("sp")
    if (uf %in% c("AC", "AP", "AM", "MA", "MT", "PA", "RO", "RR", "TO")) return("amz")
    NA_character_
  }

  desc_base_vazia <- function() {
    tibble(
      CodigoMunicipio = integer(),
      Data = as.Date(character()),
      Ano = integer()
    )
  }

  normalizar_desc_colunas <- function(colunas) {
    cols <- as.character(colunas %||% character(0))
    cols <- cols[nzchar(cols)]
    unique(sort(cols))
  }

  assinatura_desc_colunas <- function(colunas) {
    cols <- normalizar_desc_colunas(colunas)
    if (length(cols) == 0) return("__ALL__")
    paste(cols, collapse = ",")
  }

  safe_input_chr <- function(id, fallback = "") {
    out <- tryCatch(input[[id]], error = function(e) fallback)
    if (is.null(out) || length(out) == 0) return(as.character(fallback %||% ""))
    as.character(out[[1]])
  }

  desc_regiao_inteiro <- reactive({
    identical(safe_input_chr("desc_regiao", ""), desc_region_all_value)
  })

  desc_estado_inteiro <- reactive({
    identical(safe_input_chr("desc_estado", ""), desc_state_all_value)
  })

  desc_cidade_inteiro <- reactive({
    identical(safe_input_chr("desc_cidade", ""), desc_city_all_value)
  })

  desc_cidade_efetiva <- reactive({
    if (isTRUE(desc_cidade_inteiro()) || isTRUE(desc_estado_inteiro())) return(NA_integer_)
    cod_in <- normalizar_codigo_municipio(suppressWarnings(as.integer(safe_input_chr("desc_cidade", NA_character_))))[1]
    if (is.finite(cod_in)) return(cod_in)
    as.integer(capital_code_uf(toupper(safe_input_chr("desc_estado", desc_estado_default))))
  })

  desc_selects_pending <- reactive({
    vals <- c(
      safe_input_chr("desc_regiao", ""),
      safe_input_chr("desc_estado", ""),
      safe_input_chr("desc_cidade", "")
    )
    any(!nzchar(vals))
  })

  desc_view_active <- reactive({
    view <- safe_input_chr("desc_view", "series")
    if (!nzchar(view)) "series" else view
  })

  observeEvent(
    list(
      input$desc_view,
      input$desc_regiao,
      input$desc_estado,
      input$desc_cidade,
      input$desc_periodo,
      input$desc_y,
      input$tabs
    ),
    {
      if (identical(as.character(input$tabs %||% ""), "desc") && identical(desc_view_active(), "maps")) {
        session$sendCustomMessage("desc-map-loading-state", list(active = TRUE))
      }
      invisible(NULL)
    },
    ignoreInit = TRUE
  )

  desc_plot_cache_sig <- reactiveVal(NULL)
  desc_plot_cache_obj <- reactiveVal(NULL)
  desc_map_render_cache <- reactiveValues(
    br_sig = NULL, br_plot = NULL,
    reg_sig = NULL, reg_plot = NULL,
    uf_sig = NULL, uf_plot = NULL
  )

  output$desc_series_title <- renderUI({
    reg_sel <- as.character(input$desc_regiao %||% desc_regiao_default)
    uf_sel <- toupper(input$desc_estado %||% desc_estado_default)

    if (isTRUE(desc_regiao_inteiro())) {
      return("Série temporal - Brasil")
    }

    if (isTRUE(desc_estado_inteiro())) {
      return(paste0("Série temporal - Região ", reg_sel))
    }

    if (isTRUE(desc_cidade_inteiro())) {
      estado_nm <- uf_sel
      if (isTRUE(desc_geo$ok) && is.data.frame(desc_geo$estados)) {
        est <- desc_geo$estados %>% filter(.data$uf == !!uf_sel) %>% slice_head(n = 1)
        if (nrow(est) == 1 && nzchar(est$estado[[1]] %||% "")) estado_nm <- est$estado[[1]]
      }
      return(paste0("Série temporal - Estado ", estado_com_preposicao(uf_sel, estado_nm)))
    }

    cod <- desc_cidade_efetiva()
    if (!is.finite(cod)) return("Série temporal")

    if (isTRUE(desc_geo$ok)) {
      meta_city <- desc_geo$munis %>%
        filter(.data$code_muni == cod) %>%
        slice_head(n = 1)
      if (nrow(meta_city) == 1) {
        return(paste0("Série temporal - ", meta_city$name_muni[[1]], " (", meta_city$uf[[1]], ")"))
      }
    }

    meta_ref <- cidades_ref %>% filter(.data$codigo == cod) %>% slice_head(n = 1)
    if (nrow(meta_ref) == 1) {
      return(paste0("Série temporal - ", meta_ref$rotulo[[1]]))
    }

    "Série temporal"
  })

  if (TRUE) {
  desc_map_periodo_label <- reactive({
    p <- input$desc_periodo
    p_ini <- if (length(p) >= 1) as.Date(p[[1]]) else as.Date(NA)
    p_fim <- if (length(p) >= 2) as.Date(p[[2]]) else as.Date(NA)
    if (!is.na(p_ini) && !is.na(p_fim)) {
      paste0(
        "Período: ",
        format(min(p_ini, p_fim), "%d/%m/%Y"),
        " até ",
        format(max(p_ini, p_fim), "%d/%m/%Y")
      )
    } else {
      "Período completo disponível"
    }
  })

  desc_estado_foco_mapa <- reactive({
    uf_sel <- toupper(as.character(input$desc_estado %||% ""))
    if (nzchar(uf_sel) && !identical(uf_sel, desc_state_all_value)) {
      return(uf_sel)
    }
    reg_sel <- as.character(input$desc_regiao %||% "")
    est_reg <- desc_estados_regiao(reg_sel)
    if (nrow(est_reg) > 0) {
      return(as.character(est_reg$uf[[1]]))
    }
    desc_estado_default
  })

  desc_regiao_foco_mapa <- reactive({
    reg_sel <- as.character(input$desc_regiao %||% "")
    if (nzchar(reg_sel) && !identical(reg_sel, desc_region_all_value)) {
      return(reg_sel)
    }
    desc_regiao_por_uf(desc_estado_foco_mapa()) %||% desc_regiao_default
  })

  desc_estado_foco_nome <- reactive({
    uf_sel <- desc_estado_foco_mapa()
    if (isTRUE(desc_geo$ok) && is.data.frame(desc_geo$estados)) {
      hit <- desc_geo$estados %>% filter(.data$uf == !!uf_sel) %>% slice_head(n = 1)
      if (nrow(hit) == 1 && nzchar(hit$estado[[1]] %||% "")) {
        return(as.character(hit$estado[[1]]))
      }
    }
    as.character(uf_sel)
  })

  desc_regiao_foco_ufs <- reactive({
    reg_sel <- desc_regiao_foco_mapa()
    ufs <- ufs_da_regiao(reg_sel)
    ufs[!is.na(ufs) & nzchar(ufs)]
  })

  desc_meta_municipio <- function(code_muni) {
    cod <- normalizar_codigo_municipio(code_muni)[1]
    if (!is.finite(cod) || !isTRUE(desc_geo$ok)) return(NULL)
    hit <- desc_geo$munis %>% filter(.data$code_muni == cod) %>% slice_head(n = 1)
    if (nrow(hit) == 0) return(NULL)
    hit
  }

  desc_capital_foco_code <- reactive({
    as.integer(capital_code_uf(desc_estado_foco_mapa()))
  })

  desc_municipio_destaque <- reactive({
    uf_sel <- desc_estado_foco_mapa()
    munis <- desc_munis_uf(uf_sel)
    if (nrow(munis) == 0) {
      return(list(code = NA_integer_, fallback = TRUE))
    }
    cap <- as.integer(capital_code_uf(uf_sel))
    sel <- desc_cidade_efetiva()
    if (is.finite(sel) && sel %in% munis$code_muni && !identical(as.integer(sel), cap)) {
      return(list(code = as.integer(sel), fallback = FALSE))
    }
    alt <- munis %>% filter(.data$code_muni != !!cap) %>% slice_head(n = 1)
    if (nrow(alt) == 1) {
      return(list(code = as.integer(alt$code_muni[[1]]), fallback = TRUE))
    }
    if (is.finite(sel) && sel %in% munis$code_muni) {
      return(list(code = as.integer(sel), fallback = FALSE))
    }
    list(code = as.integer(cap), fallback = TRUE)
  })

  rotulo_var_desc_mapa <- function(v) {
    vv <- as.character(v %||% "")
    dplyr::case_when(
      desc_is_temp_min_var(vv) ~ "Temperatura Mínima Média (\u00b0C)",
      desc_is_temp_max_var(vv) ~ "Temperatura Máxima Média (\u00b0C)",
      TRUE ~ rotulo_var_desc(vv)
    )
  }

  output$desc_map_title <- renderUI({
    y_sel <- safe_input_chr("desc_y", desc_select_sig$y_selected %||% "__NONE__")
    titulo <- if (!nzchar(y_sel) || identical(y_sel, "__NONE__")) {
      "Mapas"
    } else {
      paste0("Mapas - ", rotulo_var_desc_mapa(y_sel))
    }
    tags$div(
      tags$div(titulo),
      tags$div(
        desc_map_periodo_label(),
        style = "font-size: 12px; font-weight: 500; color: var(--ic2025-dashboard-muted); margin-top: 0;"
      )
    )
  })

  output$desc_map_layout <- renderUI({
    if (isTRUE(desc_regiao_inteiro())) {
      tags$div(
        class = "desc-map-grid desc-map-grid-1",
        tags$div(class = "desc-map-panel desc-map-panel-brasil", plotOutput("desc_map_brasil", height = "100%"))
      )
    } else if (isTRUE(desc_estado_inteiro())) {
      tags$div(
        class = "desc-map-grid desc-map-grid-2",
        tags$div(class = "desc-map-panel desc-map-panel-brasil", plotOutput("desc_map_brasil", height = "100%")),
        tags$div(class = "desc-map-panel desc-map-panel-regiao", plotOutput("desc_map_regiao", height = "100%"))
      )
    } else {
      tags$div(
        class = "desc-map-grid desc-map-grid-3",
        tags$div(class = "desc-map-panel desc-map-panel-brasil", plotOutput("desc_map_brasil", height = "100%")),
        tags$div(class = "desc-map-panel desc-map-panel-regiao", plotOutput("desc_map_regiao", height = "100%")),
        tags$div(class = "desc-map-panel desc-map-panel-estado", plotOutput("desc_map_estado", height = "100%"))
      )
    }
  })

  desc_map_medias_fetch <- function(y_sel, data_ini = NULL, data_fim = NULL) {
    if (!nzchar(y_sel) || identical(y_sel, "__NONE__")) {
      return(tibble::tibble(CodigoMunicipio = integer(), valor = numeric()))
    }
    data_ini_chr <- if (is.null(data_ini)) "__MIN__" else as.character(as.Date(data_ini))
    data_fim_chr <- if (is.null(data_fim)) "__MAX__" else as.character(as.Date(data_fim))
    cache_key <- paste(
      "desc-map-data",
      DESC_MAP_DATA_CACHE_VERSION,
      ACTIVE_STORAGE_BACKEND,
      y_sel,
      data_ini_chr,
      data_fim_chr,
      sep = "|"
    )

    desc_cache_fetch(
      env = desc_map_data_cache,
      key = cache_key,
      loader = function() {
        tryCatch(
          carregar_base_agregada_media_municipio_periodo(
            var_col = y_sel,
            data_ini = data_ini,
            data_fim = data_fim,
            duckdb_no_cache = FALSE
          ),
          error = function(e) tibble::tibble(CodigoMunicipio = integer(), valor = numeric())
        )
      },
      store_if = function(x) is.data.frame(x),
      max_n = 24,
      order_name = "data_order"
    )
  }

  desc_map_params_sig <- reactive({
    if (!isTRUE(DESC_MAP_WARM_BACKGROUND)) {
      req(identical(desc_view_active(), "maps"))
    }
    y_sel <- safe_input_chr("desc_y", desc_select_sig$y_selected %||% "__NONE__")
    p <- input$desc_periodo
    p_ini <- if (length(p) >= 1) as.Date(p[[1]]) else as.Date(NA)
    p_fim <- if (length(p) >= 2) as.Date(p[[2]]) else as.Date(NA)
    if (!is.na(p_ini) && !is.na(p_fim)) {
      data_ini <- min(p_ini, p_fim)
      data_fim <- max(p_ini, p_fim)
    } else {
      data_ini <- NULL
      data_fim <- NULL
    }

    list(
      tab = as.character(input$tabs %||% ""),
      y_sel = y_sel,
      data_ini = data_ini,
      data_fim = data_fim,
      uf_focus = desc_estado_foco_mapa(),
      estado_nome_focus = desc_estado_foco_nome(),
      reg_focus = desc_regiao_foco_mapa(),
      reg_ufs = desc_regiao_foco_ufs()
    )
  })

  desc_map_params_rv <- reactiveVal(NULL)
  desc_map_data_rv <- reactiveVal(NULL)
  desc_map_brasil_rv <- reactiveVal(NULL)
  desc_map_regiao_rv <- reactiveVal(NULL)
  desc_map_estado_rv <- reactiveVal(NULL)
  desc_map_sig_store <- new.env(parent = emptyenv())
  desc_map_sig_store$params <- NULL
  desc_map_sig_store$data <- NULL
  desc_map_sig_store$br <- NULL
  desc_map_sig_store$reg <- NULL
  desc_map_sig_store$uf <- NULL

  observeEvent(desc_map_params_sig(), {
    sig <- desc_map_params_sig()
    if (!identical(sig$tab, "desc")) {
      return(invisible(NULL))
    }
    session$onFlushed(function() {
      if (!identical(desc_map_sig_store$params, sig)) {
        desc_map_sig_store$params <- sig
        desc_map_params_rv(sig)
      }

      sig_data <- list(
        tab = sig$tab,
        y_sel = sig$y_sel,
        data_ini = sig$data_ini,
        data_fim = sig$data_fim
      )
      if (!identical(desc_map_sig_store$data, sig_data)) {
        desc_map_sig_store$data <- sig_data
        desc_map_data_rv(sig_data)
      }

      sig_br <- sig_data
      if (!identical(desc_map_sig_store$br, sig_br)) {
        desc_map_sig_store$br <- sig_br
        desc_map_brasil_rv(sig_br)
      }

      sig_reg <- list(
        tab = sig$tab,
        y_sel = sig$y_sel,
        data_ini = sig$data_ini,
        data_fim = sig$data_fim,
        reg_focus = sig$reg_focus,
        reg_ufs = sig$reg_ufs
      )
      if (!identical(desc_map_sig_store$reg, sig_reg)) {
        desc_map_sig_store$reg <- sig_reg
        desc_map_regiao_rv(sig_reg)
      }

      sig_uf <- list(
        tab = sig$tab,
        y_sel = sig$y_sel,
        data_ini = sig$data_ini,
        data_fim = sig$data_fim,
        uf_focus = sig$uf_focus,
        estado_nome_focus = sig$estado_nome_focus
      )
      if (!identical(desc_map_sig_store$uf, sig_uf)) {
        desc_map_sig_store$uf <- sig_uf
        desc_map_estado_rv(sig_uf)
      }
    }, once = TRUE)
    invisible(NULL)
  }, ignoreNULL = FALSE, priority = -100)

  desc_map_data_sig <- reactive({
    desc_map_data_rv()
  })

  observeEvent(tema_palheta_ativa(), {
    desc_plot_cache_sig(NULL)
    desc_plot_cache_obj(NULL)
    desc_map_render_cache$br_sig <- NULL
    desc_map_render_cache$br_plot <- NULL
    desc_map_render_cache$reg_sig <- NULL
    desc_map_render_cache$reg_plot <- NULL
    desc_map_render_cache$uf_sig <- NULL
    desc_map_render_cache$uf_plot <- NULL
    desc_map_sig_store$params <- NULL
    desc_map_sig_store$data <- NULL
    desc_map_sig_store$br <- NULL
    desc_map_sig_store$reg <- NULL
    desc_map_sig_store$uf <- NULL
    spatial_refresh_tick(spatial_refresh_tick() + 1L)
    session$sendCustomMessage("plotly-resize", list())
    invisible(NULL)
  }, ignoreInit = TRUE)

  desc_map_medias <- reactive({
    sig <- desc_map_data_sig()
    if (is.null(sig) || !identical(sig$tab, "desc")) {
      return(tibble::tibble(CodigoMunicipio = integer(), valor = numeric()))
    }
    desc_map_medias_fetch(
      y_sel = sig$y_sel,
      data_ini = sig$data_ini,
      data_fim = sig$data_fim
    )
  })

  carregar_desc_base <- function(colunas = NULL) {
    cols_sel <- normalizar_desc_colunas(colunas)
    cols_param <- if (length(cols_sel) > 0) cols_sel else NULL
    col_key <- assinatura_desc_colunas(cols_sel)

    if (identical(DATA_SOURCE_MODE, "agregada")) {
      # Na inicialização, os selects podem passar por estados transitórios
      # (vazios) antes de estabilizar. Evita consultas inválidas nesse momento.
      reg_sel0 <- safe_input_chr("desc_regiao", desc_regiao_default)
      est_sel0 <- safe_input_chr("desc_estado", desc_estado_default)
      cid_sel0 <- safe_input_chr("desc_cidade", desc_cidade_default)
      if (!nzchar(reg_sel0) || !nzchar(est_sel0) || !nzchar(cid_sel0)) {
        return(desc_base_vazia())
      }
      cache_key <- paste(
        "desc-base",
        DATA_SOURCE_MODE,
        ACTIVE_STORAGE_BACKEND,
        reg_sel0,
        est_sel0,
        cid_sel0,
        col_key,
        sep = "|"
      )
      df <- desc_data_cache_fetch(cache_key, function() tryCatch({
        if (isTRUE(desc_regiao_inteiro())) {
          key <- paste(
            "desc-none",
            DATA_SOURCE_MODE,
            ACTIVE_STORAGE_BACKEND,
            "BR",
            as.character(input$desc_regiao %||% ""),
            as.character(input$desc_estado %||% ""),
            as.character(input$desc_cidade %||% ""),
            col_key,
            sep = "|"
          )
          desc_none_cache_fetch(key, function() {
            carregar_base_agregada_media_diaria(
              ufs = NULL,
              colunas = cols_param,
              duckdb_no_cache = FALSE
            )
          })
        } else if (isTRUE(desc_estado_inteiro())) {
          reg_sel <- as.character(input$desc_regiao %||% desc_regiao_default)
          ufs_sel <- ufs_da_regiao(reg_sel)
          key <- paste(
            "desc-none",
            DATA_SOURCE_MODE,
            ACTIVE_STORAGE_BACKEND,
            "REG",
            reg_sel,
            as.character(input$desc_estado %||% ""),
            as.character(input$desc_cidade %||% ""),
            col_key,
            sep = "|"
          )
          desc_none_cache_fetch(key, function() {
            carregar_base_agregada_media_diaria(
              ufs = ufs_sel,
              colunas = cols_param,
              duckdb_no_cache = FALSE
            )
          })
        } else if (isTRUE(desc_cidade_inteiro())) {
          uf_sel <- toupper(input$desc_estado %||% desc_estado_default)
          key <- paste(
            "desc-none",
            DATA_SOURCE_MODE,
            ACTIVE_STORAGE_BACKEND,
            "UF",
            as.character(input$desc_regiao %||% ""),
            uf_sel,
            as.character(input$desc_cidade %||% ""),
            col_key,
            sep = "|"
          )
          desc_none_cache_fetch(key, function() {
            carregar_base_agregada_estado_media_diaria(
              uf = uf_sel,
              colunas = cols_param,
              duckdb_no_cache = FALSE
            )
          })
        } else {
          cod <- desc_cidade_efetiva()
          if (!is.finite(cod)) return(desc_base_vazia())
          carregar_base_agregada_slice(
            codigo_municipio = cod,
            colunas = cols_param,
            duckdb_no_cache = FALSE
          )
        }
      }, error = function(e) {
        if (inherits(e, "shiny.silent.error")) {
          return(NULL)
        }
        msg <- tryCatch(conditionMessage(e), error = function(...) "")
        msg <- paste(msg, collapse = " ")
        if (!nzchar(trimws(msg))) {
          msg <- paste0("erro sem mensagem (classe: ", paste(class(e), collapse = "/"), ")")
        }
        warning("[desc/base_desc] ", msg)
        NULL
      }))
      if (is.null(df) || nrow(df) == 0) return(desc_base_vazia())
      return(df)
    }

    base_id <- base_id_por_uf(input$desc_estado %||% desc_estado_default)
    if (is.na(base_id) || !nzchar(base_id)) return(desc_base_vazia())
    cache_key <- paste(
      "desc-base",
      DATA_SOURCE_MODE,
      ACTIVE_STORAGE_BACKEND,
      as.character(input$desc_regiao %||% ""),
      as.character(input$desc_estado %||% ""),
      as.character(input$desc_cidade %||% ""),
      col_key,
      sep = "|"
    )
    df <- desc_data_cache_fetch(cache_key, function() get_base(base_id))
    if (is.null(df) || nrow(df) == 0) return(desc_base_vazia())
    if ("CodigoMunicipio" %in% names(df)) {
      df$CodigoMunicipio <- normalizar_codigo_municipio(df$CodigoMunicipio)
    }
    df <- df %>% filter(CodigoMunicipio %in% codigos_com_dados_atuais)

    if (length(cols_sel) > 0) {
      keep <- unique(c(cols_sel, "CodigoMunicipio", "Data", "Ano", "Mes", "Dia"))
      keep <- keep[keep %in% names(df)]
      if (length(keep) > 0) {
        df <- df[, keep, drop = FALSE]
      }
    }
    df
  }

  base_desc_catalog <- reactive({
    carregar_desc_base(colunas = NULL)
  })

  carregar_desc_base_periodo <- function() {
    # Para atualizar apenas o range de datas da cidade, não precisamos puxar
    # todas as colunas da base no caminho DuckDB.
    if (!(identical(DATA_SOURCE_MODE, "agregada") && identical(ACTIVE_STORAGE_BACKEND, "duckdb"))) {
      return(base_desc_catalog())
    }
    if (isTRUE(desc_cidade_inteiro()) || isTRUE(desc_estado_inteiro())) {
      return(base_desc_catalog())
    }

    cod <- desc_cidade_efetiva()
    if (!is.finite(cod)) {
      return(desc_base_vazia())
    }

    cache_key <- paste(
      "desc-periodo-base",
      DATA_SOURCE_MODE,
      ACTIVE_STORAGE_BACKEND,
      safe_input_chr("desc_regiao", desc_regiao_default),
      safe_input_chr("desc_estado", desc_estado_default),
      safe_input_chr("desc_cidade", desc_cidade_default),
      sep = "|"
    )

    df <- desc_data_cache_fetch(cache_key, function() tryCatch({
      carregar_base_agregada_slice(
        codigo_municipio = cod,
        colunas = character(0),
        duckdb_no_cache = FALSE
      )
    }, error = function(e) NULL))

    if (is.null(df) || nrow(df) == 0 || !("Data" %in% names(df))) {
      return(desc_base_vazia())
    }
    if ("CodigoMunicipio" %in% names(df)) {
      df$CodigoMunicipio <- normalizar_codigo_municipio(df$CodigoMunicipio)
    }
    df
  }

  observeEvent(input$desc_regiao, {
    if (!isTRUE(desc_geo$ok)) return()
    reg_sel <- as.character(input$desc_regiao %||% desc_regiao_default)

    if (identical(reg_sel, desc_region_all_value)) {
      freezeReactiveValue(input, "desc_estado")
      updateSelectizeInput(
        session, "desc_estado",
        choices = desc_estado_choices(desc_region_all_value),
        selected = desc_state_all_value,
        server = FALSE
      )
      return(invisible(NULL))
    }

    est_reg <- desc_estados_regiao(reg_sel)
    if (nrow(est_reg) == 0) {
      freezeReactiveValue(input, "desc_estado")
      updateSelectizeInput(
        session, "desc_estado",
        choices = desc_estado_choices(reg_sel),
        selected = desc_state_all_value,
        server = FALSE
      )
      return(invisible(NULL))
    }

    est_atual <- as.character(input$desc_estado %||% "")
    if (identical(est_atual, desc_state_all_value)) {
      est_atual <- desc_state_all_value
    } else if (!(est_atual %in% as.character(est_reg$uf))) {
      est_atual <- as.character(est_reg$uf[[1]])
    }

    freezeReactiveValue(input, "desc_estado")
    updateSelectizeInput(
      session, "desc_estado",
      choices = desc_estado_choices(reg_sel),
      selected = est_atual,
      server = FALSE
    )
  }, ignoreInit = TRUE)

  observeEvent(input$desc_estado, {
    uf_sel <- toupper(input$desc_estado %||% desc_estado_default)
    if (!isTRUE(desc_geo$ok)) return()
    cidade_atual <- as.character(input$desc_cidade %||% desc_city_all_value)

    if (isTRUE(desc_regiao_inteiro())) {
      freezeReactiveValue(input, "desc_cidade")
      updateSelectizeInput(
        session, "desc_cidade",
        choices = c(setNames(desc_city_all_value, desc_city_all_label)),
        selected = desc_city_all_value,
        server = FALSE
      )
      return(invisible(NULL))
    }

    if (isTRUE(desc_estado_inteiro())) {
      freezeReactiveValue(input, "desc_cidade")
      updateSelectizeInput(
        session, "desc_cidade",
        choices = c(setNames(desc_city_all_value, desc_city_all_label)),
        selected = desc_city_all_value,
        server = FALSE
      )
      return(invisible(NULL))
    }

    reg_uf <- desc_regiao_por_uf(uf_sel)
    reg_sel <- as.character(input$desc_regiao %||% desc_regiao_default)
    if (!identical(reg_sel, reg_uf) && nzchar(reg_uf %||% "")) {
      freezeReactiveValue(input, "desc_regiao")
      updateSelectizeInput(session, "desc_regiao", selected = reg_uf, server = FALSE)
      return(invisible(NULL))
    }

    cidades_uf <- desc_munis_uf(uf_sel)
    if (nrow(cidades_uf) == 0) {
      freezeReactiveValue(input, "desc_cidade")
      updateSelectizeInput(
        session, "desc_cidade",
        choices = c(setNames(desc_city_all_value, desc_city_all_label)),
        selected = desc_city_all_value,
        server = FALSE
      )
      return(invisible(NULL))
    }

    sel <- as.character(capital_code_uf(uf_sel))
    if (identical(cidade_atual, desc_city_all_value)) {
      sel <- desc_city_all_value
    } else if (cidade_atual %in% as.character(cidades_uf$code_muni)) {
      sel <- cidade_atual
    } else if (!(sel %in% as.character(cidades_uf$code_muni))) {
      sel <- as.character(cidades_uf$code_muni[[1]])
    }

    updateSelectizeInput(
      session, "desc_cidade",
      choices = desc_cidade_choices(cidades_uf),
      selected = sel,
      server = FALSE
    )
  }, ignoreInit = TRUE)

  observeEvent(input$desc_cidade, {
    req(!is.null(input$desc_cidade), nzchar(input$desc_cidade))

    if (isTRUE(desc_geo$ok) && !isTRUE(desc_cidade_inteiro()) && !isTRUE(desc_estado_inteiro())) {
      cod <- normalizar_codigo_municipio(suppressWarnings(as.integer(input$desc_cidade)))[1]
      meta_city <- desc_geo$munis %>% filter(code_muni == cod) %>% slice_head(n = 1)
      if (nrow(meta_city) == 1 && !identical(toupper(input$desc_estado %||% ""), meta_city$uf[[1]])) {
        updateSelectizeInput(session, "desc_estado", selected = meta_city$uf[[1]], server = FALSE)
      }
    }

    df <- carregar_desc_base_periodo()
    if (nrow(df) == 0) return()
    df_city <- if (isTRUE(desc_cidade_inteiro())) {
      df
    } else {
      cod <- desc_cidade_efetiva()
      if (!is.finite(cod)) return()
      df %>% filter(CodigoMunicipio == cod)
    }

    set_periodo_se_mudou <- function(end_target) {
      end_target <- as.Date(end_target)
      if (!is.finite(end_target) || is.na(end_target)) end_target <- desc_inicio_padrao
      start_target <- desc_inicio_padrao

      atual <- input$desc_periodo
      atual_ini <- if (length(atual) >= 1) as.Date(atual[[1]]) else as.Date(NA)
      atual_fim <- if (length(atual) >= 2) as.Date(atual[[2]]) else as.Date(NA)

      mudou <- !isTRUE(all.equal(atual_ini, start_target)) || !isTRUE(all.equal(atual_fim, end_target))
      if (!isTRUE(mudou)) return(invisible(NULL))

      freezeReactiveValue(input, "desc_periodo")
      updateDateRangeInput(
        session, "desc_periodo",
        start = start_target, end = end_target,
        min = start_target, max = end_target
      )
      invisible(NULL)
    }

    if (nrow(df_city) > 0) {
      dr <- range(df_city$Data, na.rm = TRUE)
      set_periodo_se_mudou(as.Date(dr[[2]]))
    } else if (nrow(df) > 0) {
      dr <- range(df$Data, na.rm = TRUE)
      set_periodo_se_mudou(as.Date(dr[[2]]))
    } else {
      set_periodo_se_mudou(desc_inicio_padrao)
    }
  }, ignoreInit = FALSE)

  desc_vars_disponiveis <- reactive({
    if (identical(DATA_SOURCE_MODE, "agregada") && identical(ACTIVE_STORAGE_BACKEND, "duckdb")) {
      info <- tryCatch(duckdb_info_agregada(), error = function(e) NULL)
      if (is.list(info) && length(info$campos) > 0) {
        cols_excluir <- unique(c(
          info$col_cod, info$col_data, info$col_uf, info$col_nome,
          "CodigoMunicipio", "CodigoMunicipio6", "CodMunicipio", "codigo_municipio",
          "Ano", "Mes", "Dia",
          "RegiaoMunicipio", "EstadoMunicipio", "NomeMunicipio", "Municipio",
          "UFMunicipio", "uf", "UF", "abbrev_state", "estado", "regiao"
        ))
        vars <- setdiff(as.character(info$campos), cols_excluir)
        vars <- vars[!grepl("^(codigo|cod).*municip", tolower(vars))]
        vars <- vars[!grepl("^ibge", tolower(vars))]
        vars <- vars[nzchar(vars)]
        if (length(vars) > 0) return(unique(vars))
      }
    }

    df <- base_desc_catalog()
    if (nrow(df) == 0) return(character())
    cod <- desc_cidade_efetiva()
    if (
      !isTRUE(desc_regiao_inteiro()) &&
      !isTRUE(desc_estado_inteiro()) &&
      !isTRUE(desc_cidade_inteiro()) &&
      is.finite(cod) &&
      "CodigoMunicipio" %in% names(df)
    ) {
      df <- df %>% filter(CodigoMunicipio == cod)
    }
    if (nrow(df) == 0) return(character())
    vars <- names(df)[vapply(df, is.numeric, logical(1))]
    vars <- setdiff(vars, c("CodigoMunicipio", "CodigoMunicipio6", "CodMunicipio", "codigo_municipio", "Ano", "Mes", "Dia"))
    vars <- vars[!grepl("^(codigo|cod).*municip", tolower(vars))]
    vars <- vars[!grepl("^ibge", tolower(vars))]
    vars <- vars[vapply(vars, function(v) {
      x <- suppressWarnings(as.numeric(df[[v]]))
      any(is.finite(x))
    }, logical(1))]
    unique(vars)
  })

  desc_select_sig <- reactiveValues(
    y_choices = NULL, y_selected = NULL,
    y2_choices = NULL, y2_selected = NULL
  )

  desc_y2_raw_atual <- function() {
    y2_raw <- safe_input_chr("desc_y2", desc_select_sig$y2_selected %||% "__NONE__")
    y2_mem <- as.character(desc_select_sig$y2_selected %||% "__NONE__")
    if (identical(y2_raw, "__NONE__") && !identical(y2_mem, "__NONE__")) {
      return(y2_mem)
    }
    y2_raw
  }

  observeEvent(input$desc_y, {
    desc_select_sig$y_selected <- safe_input_chr("desc_y", desc_select_sig$y_selected %||% "__NONE__")
  }, ignoreInit = TRUE)

  observeEvent(input$desc_y2, {
    desc_select_sig$y2_selected <- safe_input_chr("desc_y2", desc_select_sig$y2_selected %||% "__NONE__")
  }, ignoreInit = TRUE)

  assinatura_choices <- function(ch) {
    if (is.list(ch)) {
      parts <- unlist(lapply(names(ch), function(grp) {
        vals <- ch[[grp]]
        vals <- as.character(vals)
        nm <- names(vals)
        if (is.null(nm)) nm <- rep("", length(vals))
        paste0("[", grp %||% "", "]", nm, "=>", vals)
      }), use.names = FALSE)
      return(paste(parts, collapse = "||"))
    }
    ch <- as.character(ch)
    nm <- names(ch)
    if (is.null(nm)) nm <- rep("", length(ch))
    paste0(nm, "=>", unname(ch), collapse = "||")
  }

  desc_y_default_from_vars <- function(vars) {
    vars <- as.character(vars %||% character(0))
    if (length(vars) == 0) return("__NONE__")
    dplyr::case_when(
      "UmidRel" %in% vars ~ "UmidRel",
      "umid" %in% vars ~ "umid",
      "UmidadeRelativa" %in% vars ~ "UmidadeRelativa",
      "Temperatura" %in% vars ~ "Temperatura",
      "temp" %in% vars ~ "temp",
      "SensTermica" %in% vars ~ "SensTermica",
      "VelocVento" %in% vars ~ "VelocVento",
      TRUE ~ vars[[1]]
    )
  }

  desc_y2_validado <- function(y2_in, vars_ok, y_in) {
    y2_chr <- as.character(y2_in %||% "__NONE__")
    if (identical(y2_chr, DESC_Y2_TEMP_BANDS_VALUE)) {
      if (isTRUE(desc_is_temp_media_var(y_in)) && isTRUE(desc_temp_band_available(vars_ok))) {
        return(y2_chr)
      }
      return("__NONE__")
    }
    if (!identical(y2_chr, "__NONE__") && !(y2_chr %in% setdiff(vars_ok, y_in))) {
      return("__NONE__")
    }
    y2_chr
  }

  desc_plot_vars <- function(y, y2, vars_pool) {
    vars_pool <- as.character(vars_pool %||% character(0))
    out <- c(as.character(y %||% ""))
    if (identical(as.character(y2 %||% "__NONE__"), DESC_Y2_TEMP_BANDS_VALUE)) {
      out <- c(out, desc_temp_min_col(vars_pool), desc_temp_max_col(vars_pool))
    } else {
      out <- c(out, as.character(y2 %||% "__NONE__"))
    }
    out <- unique(out)
    out <- out[nzchar(out) & !is.na(out)]
    out[out %in% vars_pool]
  }

  observe({
    vars <- desc_vars_disponiveis()

    if (length(vars) == 0) {
      y_prev <- as.character(desc_select_sig$y_selected %||% "__NONE__")
      y2_prev <- as.character(desc_select_sig$y2_selected %||% "__NONE__")
      if (nzchar(y_prev) && !identical(y_prev, "__NONE__")) {
        return()
      }
      ch_y <- c("Sem variável disponível para a cidade/período atual" = "__NONE__")
      sig_y <- assinatura_choices(ch_y)
      sel_y <- "__NONE__"
      if (!identical(desc_select_sig$y_choices, sig_y) || !identical(desc_select_sig$y_selected, sel_y)) {
        freezeReactiveValue(input, "desc_y")
        updateSelectizeInput(session, "desc_y", choices = ch_y, selected = sel_y, server = FALSE)
        desc_select_sig$y_choices <- sig_y
        desc_select_sig$y_selected <- sel_y
      }

      ch_y2 <- c("Nenhuma" = "__NONE__")
      sig_y2 <- assinatura_choices(ch_y2)
      sel_y2 <- "__NONE__"
      if (!identical(desc_select_sig$y2_choices, sig_y2) || !identical(desc_select_sig$y2_selected, sel_y2)) {
        freezeReactiveValue(input, "desc_y2")
        updateSelectizeInput(session, "desc_y2", choices = ch_y2, selected = sel_y2, server = FALSE)
        desc_select_sig$y2_choices <- sig_y2
        desc_select_sig$y2_selected <- sel_y2
      }
      return()
    }

    y_atual <- safe_input_chr("desc_y", desc_select_sig$y_selected %||% "")
    y_sel <- dplyr::case_when(
      nzchar(y_atual) && y_atual %in% vars ~ y_atual,
      TRUE ~ desc_y_default_from_vars(vars)
    )

    ch_y <- montar_choices_desc_agrupadas(vars, incluir_none = FALSE)
    sig_y <- assinatura_choices(ch_y)
    if (!identical(desc_select_sig$y_choices, sig_y) || !identical(desc_select_sig$y_selected, y_sel)) {
      freezeReactiveValue(input, "desc_y")
      updateSelectizeInput(session, "desc_y", choices = ch_y, selected = y_sel, server = FALSE)
      desc_select_sig$y_choices <- sig_y
      desc_select_sig$y_selected <- y_sel
    }

    vars_sec <- setdiff(vars, y_sel)
    y2_sel <- desc_y2_validado(
      desc_y2_raw_atual(),
      vars,
      y_sel
    )
    ch_y2 <- desc_y2_choices(vars_sec, vars, y_sel)
    sig_y2 <- assinatura_choices(ch_y2)
    if (!identical(desc_select_sig$y2_choices, sig_y2) || !identical(desc_select_sig$y2_selected, y2_sel)) {
      freezeReactiveValue(input, "desc_y2")
      updateSelectizeInput(session, "desc_y2", choices = ch_y2, selected = y2_sel, server = FALSE)
      desc_select_sig$y2_choices <- sig_y2
      desc_select_sig$y2_selected <- y2_sel
    }
  })

  desc_inputs <- reactive({
    vars_ok <- desc_vars_disponiveis()
    if (length(vars_ok) == 0) {
      return(list(
        regiao_inteiro = isTRUE(desc_regiao_inteiro()),
        estado_inteiro = isTRUE(desc_estado_inteiro()),
        cidade_inteiro = isTRUE(desc_cidade_inteiro()),
        pre_agregado_diario = identical(DATA_SOURCE_MODE, "agregada") && (
          isTRUE(desc_regiao_inteiro()) ||
          isTRUE(desc_estado_inteiro()) ||
          isTRUE(desc_cidade_inteiro())
        ),
        cidade = desc_cidade_efetiva(),
        periodo = input$desc_periodo,
        agreg = input$desc_agreg %||% "M",
        y = "__NONE__",
        y2 = "__NONE__"
      ))
    }
    y_in <- safe_input_chr("desc_y", desc_select_sig$y_selected %||% "")
    if (!nzchar(y_in) || !(y_in %in% vars_ok)) y_in <- desc_y_default_from_vars(vars_ok)
    y2_in <- desc_y2_validado(
      desc_y2_raw_atual(),
      vars_ok,
      y_in
    )
    list(
      regiao_inteiro = isTRUE(desc_regiao_inteiro()),
      estado_inteiro = isTRUE(desc_estado_inteiro()),
      cidade_inteiro = isTRUE(desc_cidade_inteiro()),
      pre_agregado_diario = identical(DATA_SOURCE_MODE, "agregada") && (
        isTRUE(desc_regiao_inteiro()) ||
        isTRUE(desc_estado_inteiro()) ||
        isTRUE(desc_cidade_inteiro())
      ),
      cidade = desc_cidade_efetiva(),
      periodo = input$desc_periodo,
      agreg = input$desc_agreg %||% "M",
      y = y_in,
      y2 = y2_in
    )
  })

  base_desc_plot <- reactive({
    d_in <- desc_inputs()
    cols <- desc_plot_vars(d_in$y, d_in$y2, desc_vars_disponiveis())
    if (length(cols) == 0) return(desc_base_vazia())
    carregar_desc_base(colunas = cols)
  })

  desc_filtrado <- reactive({
    d_in <- desc_inputs()
    df <- base_desc_plot()
    if (nrow(df) == 0) return(df[0, , drop = FALSE])
    if (!isTRUE(d_in$regiao_inteiro) && !isTRUE(d_in$estado_inteiro) && !isTRUE(d_in$cidade_inteiro)) {
      if (!is.finite(d_in$cidade)) return(df[0, , drop = FALSE])
      df <- df %>% filter(CodigoMunicipio == d_in$cidade)
    }
    if (!is.null(d_in$periodo) && length(d_in$periodo) == 2) {
      p1 <- as.Date(d_in$periodo[[1]])
      p2 <- as.Date(d_in$periodo[[2]])
      if (!is.na(p1) && !is.na(p2)) {
        lo <- min(p1, p2)
        hi <- max(p1, p2)
        df <- df %>% filter(Data >= lo, Data <= hi)
      }
    }
    df %>% arrange(Data)
  })

  desc_plot_df <- reactive({
    d_in <- desc_inputs()
    df <- desc_filtrado()
    req(nrow(df) > 0)
    y <- d_in$y
    req(!identical(y, "__NONE__"))
    req(nzchar(y), y %in% names(df))
    y2 <- d_in$y2
    if (identical(y2, "__NONE__")) y2 <- NULL
    plot_vars <- desc_plot_vars(y, y2, names(df))
    req(length(plot_vars) > 0, y %in% plot_vars)

    # Consolida diariamente apenas quando o dataset ainda não veio agregado.
    if (isTRUE(d_in$cidade_inteiro) && !isTRUE(d_in$pre_agregado_diario)) {
      keep_state <- plot_vars[plot_vars %in% names(df)]
      req(length(keep_state) > 0)
      df <- df %>%
        group_by(Data) %>%
        summarise(
          across(all_of(keep_state), ~ {
            z <- suppressWarnings(as.numeric(.x))
            if (all(!is.finite(z))) NA_real_ else mean(z, na.rm = TRUE)
          }),
          .groups = "drop"
        ) %>%
        arrange(Data)
    }

    freq <- d_in$agreg
    if (!identical(freq, "D")) {
      cut_breaks <- switch(freq, W = "week", M = "month", "day")
      keep <- plot_vars[plot_vars %in% names(df)]
      df <- df %>%
        mutate(Periodo = as.Date(as.character(cut(Data, breaks = cut_breaks)))) %>%
        group_by(Periodo) %>%
        summarise(across(all_of(keep), ~ mean(suppressWarnings(as.numeric(.x)))), .groups = "drop") %>%
        rename(Data = Periodo)
    }

    periodo_label <- if (identical(freq, "W")) {
      data_ini <- df$Data
      data_fim <- data_ini + 6
      paste0(format(data_ini, "%d/%m/%Y"), " - ", format(data_fim, "%d/%m/%Y"))
    } else if (identical(freq, "M")) {
      data_ini <- df$Data
      data_fim <- as.Date(vapply(
        data_ini,
        function(d) as.character(seq.Date(d, by = "month", length.out = 2)[2] - 1),
        character(1)
      ))
      paste0(format(data_ini, "%d/%m/%Y"), " - ", format(data_fim, "%d/%m/%Y"))
    } else {
      format(df$Data, "%d/%m/%Y")
    }

    primary_raw <- suppressWarnings(as.numeric(df[[y]]))
    req(any(is.finite(primary_raw)))
    shared_temp_scale <- identical(as.character(y2 %||% ""), DESC_Y2_TEMP_BANDS_VALUE) && desc_is_temp_media_var(y)

    normalize_max <- function(z) {
      mx <- max(z, na.rm = TRUE)
      if (!is.finite(mx) || mx == 0) return(z)
      z / mx
    }

    series <- lapply(seq_along(plot_vars), function(i) {
      v <- plot_vars[[i]]
      raw <- suppressWarnings(as.numeric(df[[v]]))
      list(
        var = v,
        raw = raw,
        int = all(is.na(raw) | abs(raw - round(raw)) < 1e-9),
        color = desc_plot_color(v, i),
        width = if (identical(v, y)) 2.6 else 2.3,
        dash = if (isTRUE(shared_temp_scale) && !identical(v, y)) "dash" else "solid"
      )
    })

    normalizado_auto <- length(series) > 1 && !isTRUE(shared_temp_scale)
    series <- lapply(series, function(s) {
      s$plot <- if (isTRUE(normalizado_auto)) normalize_max(s$raw) else s$raw
      s$label <- rotulo_var_desc(s$var)
      s$short <- rotulo_curto_desc(s$var)
      s
    })

    list(
      df = df,
      y = y,
      y2 = y2,
      series = series,
      normalizado_auto = normalizado_auto,
      shared_temp_scale = shared_temp_scale,
      periodo_label = periodo_label
    )
  })

  desc_plot_render_sig <- reactive({
    p <- input$desc_periodo
    list(
      regiao = safe_input_chr("desc_regiao", ""),
      estado = safe_input_chr("desc_estado", ""),
      cidade = safe_input_chr("desc_cidade", ""),
      data_ini = if (length(p) >= 1) as.character(as.Date(p[[1]])) else NA_character_,
      data_fim = if (length(p) >= 2) as.character(as.Date(p[[2]])) else NA_character_,
      agreg = safe_input_chr("desc_agreg", "M"),
      y = safe_input_chr("desc_y", desc_select_sig$y_selected %||% "__NONE__"),
      y2 = desc_y2_raw_atual()
    )
  })

  build_desc_plot_object <- function() {
    if (isTRUE(desc_selects_pending())) {
      invalidateLater(250, session)
      return(plot_placeholder("Carregando série temporal..."))
    }
    df_msg <- tryCatch(desc_filtrado(), error = function(e) NULL)
    if (is.null(df_msg)) {
      invalidateLater(250, session)
      return(plot_placeholder("Carregando série temporal..."))
    }
    if (nrow(df_msg) == 0) {
      booting <- as.numeric(difftime(Sys.time(), desc_boot_t0, units = "secs")) < desc_boot_sec
      pending <- isTRUE(desc_selects_pending())
      if (isTRUE(booting) || isTRUE(pending)) invalidateLater(300, session)
      return(
        plot_ly(type = "scatter", mode = "lines") %>%
          layout(
            xaxis = modifyList(plotly_layout_base$xaxis, list(title = "Data")),
            yaxis = modifyList(plotly_layout_base$yaxis, list(title = "")),
            annotations = list(list(
              text = if (isTRUE(booting) || isTRUE(pending)) "Carregando..." else "Sem dados para a cidade e período selecionados.",
              x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
              font = list(size = 15, color = ic2025_theme_value("dashboard.muted"))
            )),
            paper_bgcolor = plotly_layout_base$paper_bgcolor,
            plot_bgcolor = plotly_layout_base$plot_bgcolor,
            font = plotly_layout_base$font,
            margin = plotly_layout_base$margin
          )
      )
    }

    d_in <- desc_inputs()
    if (identical(d_in$y %||% "__NONE__", "__NONE__")) {
      return(
        plot_ly(type = "scatter", mode = "lines") %>%
          layout(
            xaxis = modifyList(plotly_layout_base$xaxis, list(title = "Data")),
            yaxis = modifyList(plotly_layout_base$yaxis, list(title = "")),
            annotations = list(list(
              text = "Sem variável numérica disponível para a cidade selecionada.",
              x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
              font = list(size = 15, color = ic2025_theme_value("dashboard.muted"))
            )),
            paper_bgcolor = plotly_layout_base$paper_bgcolor,
            plot_bgcolor = plotly_layout_base$plot_bgcolor,
            font = plotly_layout_base$font,
            margin = plotly_layout_base$margin
          )
      )
    }

    o <- tryCatch(desc_plot_df(), error = function(e) e)
    if (inherits(o, "error")) {
      return(
        plot_ly(type = "scatter", mode = "lines") %>%
          layout(
            xaxis = modifyList(plotly_layout_base$xaxis, list(title = "Data")),
            yaxis = modifyList(plotly_layout_base$yaxis, list(title = "")),
            annotations = list(list(
              text = "Sem dados para a variável selecionada no período atual.",
              x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE,
              font = list(size = 15, color = ic2025_theme_value("dashboard.muted"))
            )),
            paper_bgcolor = plotly_layout_base$paper_bgcolor,
            plot_bgcolor = plotly_layout_base$plot_bgcolor,
            font = plotly_layout_base$font,
            margin = plotly_layout_base$margin
          )
      )
    }
    req(!is.null(o$y), nzchar(o$y))
    req(is.list(o$series), length(o$series) > 0)
    fmt_num <- function(v, inteiro = FALSE) {
      ifelse(
        is.finite(v),
        if (isTRUE(inteiro)) formatC(round(v), format = "f", digits = 0) else formatC(v, format = "f", digits = 2),
        "NA"
      )
    }
    p <- plot_ly(o$df, x = ~Data)
    for (s in o$series) {
      txt_s <- if (isTRUE(o$normalizado_auto)) {
        paste0(
          "Período: ", o$periodo_label,
          "<br>", s$short, ": ", fmt_num(s$raw, isTRUE(s$int)),
          "<br>", s$short, " (padronizado): ", fmt_num(s$plot, FALSE)
        )
      } else {
        paste0(
          "Período: ", o$periodo_label,
          "<br>", s$short, ": ", fmt_num(s$raw, isTRUE(s$int))
        )
      }
      p <- p %>% add_lines(
        y = s$plot,
        name = s$label,
        line = list(color = s$color, width = s$width, dash = s$dash),
        text = txt_s,
        hoverinfo = "text"
      )
    }
    y_title <- if (isTRUE(o$normalizado_auto)) {
      "Indice relativo (max = 1)"
    } else if (isTRUE(o$shared_temp_scale)) {
      "Temperatura (\u00b0C)"
    } else {
      o$series[[1]]$short
    }
    p %>% layout(
      xaxis = modifyList(plotly_layout_base$xaxis, list(title = "Data")),
      yaxis = modifyList(plotly_layout_base$yaxis, list(title = y_title)),
      paper_bgcolor = plotly_layout_base$paper_bgcolor,
      plot_bgcolor = plotly_layout_base$plot_bgcolor,
      font = plotly_layout_base$font,
      margin = plotly_layout_base$margin,
      legend = plotly_layout_base$legend
    )
  }

  output$desc_plot <- renderPlotly({
    sig <- desc_plot_render_sig()
    cache_sig <- isolate(desc_plot_cache_sig())
    cache_obj <- isolate(desc_plot_cache_obj())
    if (identical(cache_sig, sig) && !is.null(cache_obj)) {
      return(cache_obj)
    }
    obj <- build_desc_plot_object()
    desc_plot_cache_sig(sig)
    desc_plot_cache_obj(obj)
    obj
  })

  desc_map_booting_now <- function() {
    as.numeric(difftime(Sys.time(), desc_boot_t0, units = "secs")) < desc_boot_sec
  }

  desc_map_base_key_parts <- function(y_sel, data_ini = NULL, data_fim = NULL) {
    data_ini_chr <- if (is.null(data_ini)) "__MIN__" else as.character(as.Date(data_ini))
    data_fim_chr <- if (is.null(data_fim)) "__MAX__" else as.character(as.Date(data_fim))
    c(
      DESC_MAP_PLOT_CACHE_VERSION,
      ACTIVE_STORAGE_BACKEND,
      y_sel,
      data_ini_chr,
      data_fim_chr
    )
  }

  desc_map_geo_safe <- function() {
    geo_err <- NULL
    geo_obj <- tryCatch(
      get_desc_map_geo(),
      error = function(e) {
        geo_err <<- conditionMessage(e)
        NULL
      }
    )
    list(obj = geo_obj, err = geo_err)
  }

  desc_map_brasil_sig <- reactive({
    desc_map_brasil_rv()
  })

  desc_map_regiao_sig <- reactive({
    desc_map_regiao_rv()
  })

  desc_map_estado_sig <- reactive({
    desc_map_estado_rv()
  })

  desc_map_brasil_fetch <- function(y_sel, data_ini = NULL, data_fim = NULL, panel_title = "Brasil") {
    geo_info <- desc_map_geo_safe()
    if (is.null(geo_info$obj)) stop(as.character(geo_info$err %||% "Geometria do mapa indisponível."))
    medias <- desc_map_medias_fetch(y_sel = y_sel, data_ini = data_ini, data_fim = data_fim)
    if (!is.data.frame(medias) || nrow(medias) == 0) return(NULL)
    base_key_parts <- desc_map_base_key_parts(y_sel = y_sel, data_ini = data_ini, data_fim = data_fim)
    var_label <- rotulo_var_desc(y_sel)
    desc_cache_fetch(
      env = desc_map_panel_cache,
      key = paste(c("br", base_key_parts, if (nzchar(panel_title %||% "")) panel_title else "__NO_TITLE__"), collapse = "|"),
        loader = function() {
          p1_tmp <- plot_mapa_brasil_refinado_ggplot(
            geo_info = geo_info$obj,
            medias_df = medias,
            var_col = y_sel,
            var_label = var_label,
            periodo_label = NULL,
            geo_scope = "brasil",
            show_legend = TRUE,
            show_state_labels = TRUE,
            panel_title = panel_title,
            legend_position = "bottom"
          )
        list(
          plot = p1_tmp + ggplot2::theme(legend.position = "none"),
          legend = extrair_legend_grob_ggplot(p1_tmp, preferred = "bottom")
        )
      },
      store_if = function(x) is.list(x) && all(c("plot", "legend") %in% names(x)),
      max_n = 12,
      order_name = "panel_order"
    )
  }

  desc_map_regiao_fetch <- function(y_sel, data_ini = NULL, data_fim = NULL, reg_focus = NULL, reg_ufs = NULL) {
    geo_info <- desc_map_geo_safe()
    if (is.null(geo_info$obj)) stop(as.character(geo_info$err %||% "Geometria do mapa indisponível."))
    medias <- desc_map_medias_fetch(y_sel = y_sel, data_ini = data_ini, data_fim = data_fim)
    if (!is.data.frame(medias) || nrow(medias) == 0) return(NULL)
    base_key_parts <- desc_map_base_key_parts(y_sel = y_sel, data_ini = data_ini, data_fim = data_fim)
    var_label <- rotulo_var_desc(y_sel)
    desc_cache_fetch(
      env = desc_map_panel_cache,
      key = paste(c("reg", base_key_parts, reg_focus), collapse = "|"),
      loader = function() {
        plot_mapa_brasil_refinado_ggplot(
          geo_info = geo_info$obj,
          medias_df = medias,
          var_col = y_sel,
          var_label = var_label,
          periodo_label = NULL,
          geo_scope = "regiao",
          uf_focus = reg_ufs,
          show_legend = FALSE,
          show_state_labels = TRUE,
          panel_title = paste0("Região ", reg_focus)
        )
      },
      store_if = function(x) inherits(x, "ggplot"),
      max_n = 12,
      order_name = "panel_order"
    )
  }

  desc_map_estado_fetch <- function(y_sel, data_ini = NULL, data_fim = NULL, uf_focus = NULL, estado_nome_focus = NULL) {
    geo_info <- desc_map_geo_safe()
    if (is.null(geo_info$obj)) stop(as.character(geo_info$err %||% "Geometria do mapa indisponível."))
    medias <- desc_map_medias_fetch(y_sel = y_sel, data_ini = data_ini, data_fim = data_fim)
    if (!is.data.frame(medias) || nrow(medias) == 0) return(NULL)
    base_key_parts <- desc_map_base_key_parts(y_sel = y_sel, data_ini = data_ini, data_fim = data_fim)
    var_label <- rotulo_var_desc(y_sel)
    desc_cache_fetch(
      env = desc_map_panel_cache,
      key = paste(c("uf", base_key_parts, uf_focus, estado_nome_focus), collapse = "|"),
      loader = function() {
        plot_mapa_brasil_refinado_ggplot(
          geo_info = geo_info$obj,
          medias_df = medias,
          var_col = y_sel,
          var_label = var_label,
          periodo_label = NULL,
          geo_scope = "uf",
          uf_focus = uf_focus,
          show_legend = FALSE,
          show_state_labels = FALSE,
          panel_title = paste0("Estado ", estado_com_preposicao(uf_focus, estado_nome_focus))
        )
      },
      store_if = function(x) inherits(x, "ggplot"),
      max_n = 12,
      order_name = "panel_order"
    )
  }

  output$desc_map_brasil <- renderPlot({
    req(identical(as.character(input$tabs %||% ""), "desc"))
    if (!isTRUE(DESC_MAP_WARM_BACKGROUND)) {
      req(identical(desc_view_active(), "maps"))
    }
    sig <- desc_map_brasil_sig()
    if (is.null(sig)) {
      invalidateLater(250, session)
      return(mapa_placeholder_ggplot("Carregando..."))
    }
    y_sel <- as.character(sig$y_sel %||% "__NONE__")
    if (!nzchar(y_sel) || identical(y_sel, "__NONE__")) {
      return(mapa_placeholder_ggplot("Sem variável principal disponível para mapear."))
    }
    if (isTRUE(desc_map_booting_now())) {
      invalidateLater(350, session)
      return(mapa_placeholder_ggplot("Carregando..."))
    }
    if (identical(isolate(desc_map_render_cache$br_sig), sig) && !is.null(isolate(desc_map_render_cache$br_plot))) {
      return(isolate(desc_map_render_cache$br_plot))
    }
    panel_title_br <- if (isTRUE(desc_regiao_inteiro())) NULL else "Brasil"
    tryCatch({
      out <- desc_map_brasil_fetch(
        y_sel = y_sel,
        data_ini = sig$data_ini,
        data_fim = sig$data_fim,
        panel_title = panel_title_br
      )
      if (is.null(out)) {
        mapa_placeholder_ggplot("Sem dados para montar o mapa no período selecionado.")
      } else {
        desc_map_render_cache$br_sig <- sig
        desc_map_render_cache$br_plot <- out$plot
        out$plot
      }
    }, error = function(e) {
      warning("[desc/map/brasil] ", conditionMessage(e))
      mapa_placeholder_ggplot("Falha ao montar o mapa.")
    })
  }, res = 120, bg = ic2025_theme_value("dashboard.transparent"))

  output$desc_map_regiao <- renderPlot({
    req(identical(as.character(input$tabs %||% ""), "desc"))
    if (!isTRUE(DESC_MAP_WARM_BACKGROUND)) {
      req(identical(desc_view_active(), "maps"))
    }
    req(!isTRUE(desc_regiao_inteiro()))
    sig <- desc_map_regiao_sig()
    if (is.null(sig)) {
      invalidateLater(250, session)
      return(mapa_placeholder_ggplot("Carregando..."))
    }
    y_sel <- as.character(sig$y_sel %||% "__NONE__")
    if (!nzchar(y_sel) || identical(y_sel, "__NONE__")) {
      return(mapa_placeholder_ggplot("Sem variável principal disponível para mapear."))
    }
    if (isTRUE(desc_map_booting_now())) {
      invalidateLater(350, session)
      return(mapa_placeholder_ggplot("Carregando..."))
    }
    if (identical(isolate(desc_map_render_cache$reg_sig), sig) && !is.null(isolate(desc_map_render_cache$reg_plot))) {
      return(isolate(desc_map_render_cache$reg_plot))
    }
    tryCatch({
      out <- desc_map_regiao_fetch(
        y_sel = y_sel,
        data_ini = sig$data_ini,
        data_fim = sig$data_fim,
        reg_focus = sig$reg_focus,
        reg_ufs = sig$reg_ufs
      )
      if (is.null(out)) {
        mapa_placeholder_ggplot("Sem dados para a região selecionada.")
      } else {
        desc_map_render_cache$reg_sig <- sig
        desc_map_render_cache$reg_plot <- out
        out
      }
    }, error = function(e) {
      warning("[desc/map/regiao] ", conditionMessage(e))
      mapa_placeholder_ggplot("Falha ao montar o mapa.")
    })
  }, res = 120, bg = ic2025_theme_value("dashboard.transparent"))

  output$desc_map_estado <- renderPlot({
    req(identical(as.character(input$tabs %||% ""), "desc"))
    if (!isTRUE(DESC_MAP_WARM_BACKGROUND)) {
      req(identical(desc_view_active(), "maps"))
    }
    req(!isTRUE(desc_regiao_inteiro()), !isTRUE(desc_estado_inteiro()))
    sig <- desc_map_estado_sig()
    if (is.null(sig)) {
      invalidateLater(250, session)
      return(mapa_placeholder_ggplot("Carregando..."))
    }
    y_sel <- as.character(sig$y_sel %||% "__NONE__")
    if (!nzchar(y_sel) || identical(y_sel, "__NONE__")) {
      return(mapa_placeholder_ggplot("Sem variável principal disponível para mapear."))
    }
    if (isTRUE(desc_map_booting_now())) {
      invalidateLater(350, session)
      return(mapa_placeholder_ggplot("Carregando..."))
    }
    if (identical(isolate(desc_map_render_cache$uf_sig), sig) && !is.null(isolate(desc_map_render_cache$uf_plot))) {
      return(isolate(desc_map_render_cache$uf_plot))
    }
    tryCatch({
      out <- desc_map_estado_fetch(
        y_sel = y_sel,
        data_ini = sig$data_ini,
        data_fim = sig$data_fim,
        uf_focus = sig$uf_focus,
        estado_nome_focus = sig$estado_nome_focus
      )
      if (is.null(out)) {
        mapa_placeholder_ggplot("Sem dados para o estado selecionado.")
      } else {
        desc_map_render_cache$uf_sig <- sig
        desc_map_render_cache$uf_plot <- out
        out
      }
    }, error = function(e) {
      warning("[desc/map/estado] ", conditionMessage(e))
      mapa_placeholder_ggplot("Falha ao montar o mapa.")
    })
  }, res = 120, bg = ic2025_theme_value("dashboard.transparent"))

  output$desc_map_legend <- renderPlot({
    req(identical(as.character(input$tabs %||% ""), "desc"))
    if (!isTRUE(DESC_MAP_WARM_BACKGROUND)) {
      req(identical(desc_view_active(), "maps"))
    }
    sig <- desc_map_brasil_sig()
    if (is.null(sig)) {
      invalidateLater(250, session)
      return(invisible(NULL))
    }
    y_sel <- as.character(sig$y_sel %||% "__NONE__")
    if (!nzchar(y_sel) || identical(y_sel, "__NONE__")) {
      return(invisible(NULL))
    }
    if (isTRUE(desc_map_booting_now())) {
      invalidateLater(350, session)
      return(invisible(NULL))
    }
    panel_title_br <- if (isTRUE(desc_regiao_inteiro())) NULL else "Brasil"
    tryCatch({
      out <- desc_map_brasil_fetch(
        y_sel = y_sel,
        data_ini = sig$data_ini,
        data_fim = sig$data_fim,
        panel_title = panel_title_br
      )
      grid::grid.newpage()
      if (!is.null(out) && !is.null(out$legend)) {
        grid::grid.draw(out$legend)
      }
      invisible(NULL)
    }, error = function(e) {
      warning("[desc/map/legend] ", conditionMessage(e))
      invisible(NULL)
    })
  }, res = 120, bg = ic2025_theme_value("dashboard.transparent"))
  # output$desc_n <- renderValueBox({
  #   df <- desc_filtrado()
  #   valueBox(nrow(df), "Observações", icon = icon("database"), color = "teal")
  # })
  #
  # output$desc_med <- renderValueBox({
  #   df <- desc_filtrado()
  #   y <- input$desc_y %||% "Casos_Resp"
  #   med <- suppressWarnings(mean(as.numeric(df[[y]]), na.rm = TRUE))
  #   valueBox(formatar_numero(med, 2), "Média", icon = icon("chart-area"), color = "green")
  # })
  #
  # output$desc_max <- renderValueBox({
  #   df <- desc_filtrado()
  #   y <- input$desc_y %||% "Casos_Resp"
  #   mx <- suppressWarnings(max(as.numeric(df[[y]]), na.rm = TRUE))
  #   valueBox(formatar_numero(mx, 2), "Máximo", icon = icon("arrow-up"), color = "olive")
  # })
  #
  # output$desc_cidades <- renderValueBox({
  #   valueBox(nrow(cidades_ref), "Cidades disponíveis", icon = icon("city"), color = "light-blue")
  # })

  output$sim_eta_ui <- renderUI({
    dsel <- suppressWarnings(as.integer(input$sim_d %||% 2L))
    if (!is.finite(dsel) || !(dsel %in% c(2L, 3L))) dsel <- 2L
    n_eta <- dsel + 1L
    defaults <- if (identical(dsel, 3L)) {
      c(-1.663, 0.506, 0.143, -0.011) / 1600
    } else {
      c(-8.47, 5.17, -0.36) / 1600
    }
    steps <- 10^-(seq_len(n_eta))

    inputs <- lapply(seq_len(n_eta), function(i) {
      idx <- i - 1L
      id <- paste0("sim_eta_", idx)
      cur <- input[[id]]
      val <- if (!is.null(cur) && is.finite(as.numeric(cur))) as.numeric(cur) else defaults[[i]]
      numericInput(
        inputId = id,
        label = paste0("Coeficiente de tendência eta", idx),
        value = val,
        step = steps[[i]]
      )
    })

    tagList(inputs)
  })

  parse_deltas <- function(txt) {
    vals <- suppressWarnings(as.numeric(trimws(unlist(strsplit(txt %||% "", ",")))))
    vals <- vals[is.finite(vals)]
    vals <- unique(vals)
    vals <- vals[vals > 0 & vals <= 1]
    if (length(vals) == 0) vals <- c(0.90, 0.95, 0.99)
    vals
  }
  modo_to_lag <- function(modo = c("suavizado", "filtrado", "one_step")) {
    modo <- match.arg(modo)
    switch(modo, suavizado = -1L, filtrado = 0L, one_step = 1L)
  }
  normalizar_modo_sim <- function(modo, tipo = c("pdldglm", "dlm", "dglm")) {
    tipo <- match.arg(tipo)
    valid <- c("suavizado", "filtrado", "one_step")
    m <- as.character(modo %||% "")
    if (!(m %in% valid)) {
      return(if (identical(tipo, "pdldglm")) "suavizado" else "one_step")
    }
    if (identical(tipo, "dlm") && identical(m, "suavizado")) return("one_step")
    m
  }
  ajustar_faixa_plot <- function(y, mu, lo, hi) {
    y <- as.numeric(y); mu <- as.numeric(mu); lo <- as.numeric(lo); hi <- as.numeric(hi)
    centrais <- c(y, mu)
    centrais <- centrais[is.finite(centrais) & centrais >= 0]
    if (length(centrais) < 10) {
      return(list(y_plot = y, mu_plot = mu, lo_plot = lo, hi_plot = hi, clipped = FALSE, ub = NA_real_))
    }
    # teto visual baseado apenas em dados + estimativas centrais
    ub <- as.numeric(stats::quantile(centrais, probs = 0.995, na.rm = TRUE, type = 8))
    mx_ic <- max(c(lo, hi), na.rm = TRUE)
    if (!is.finite(ub) || ub <= 0 || !is.finite(mx_ic) || mx_ic <= (6 * ub)) {
      return(list(y_plot = y, mu_plot = mu, lo_plot = lo, hi_plot = hi, clipped = FALSE, ub = ub))
    }
    list(
      y_plot = pmin(y, ub),
      mu_plot = pmin(mu, ub),
      lo_plot = pmin(lo, ub),
      hi_plot = pmin(hi, ub),
      clipped = TRUE,
      ub = ub
    )
  }

  observeEvent(input$sim_tipo, {
    tipo <- input$sim_tipo %||% "pdldglm"
    if (identical(tipo, "pdldglm")) {
      updateSelectizeInput(
        session, "sim_modo",
        choices = c("Suavizado" = "suavizado", "Filtrado" = "filtrado", "1-step ahead" = "one_step"),
        selected = "suavizado",
        server = TRUE
      )
      updateNumericInput(session, "sim_n_total", value = 365)
      updateSliderInput(session, "sim_lags", value = 16)
      updateSelectizeInput(session, "sim_d", selected = 2)
      updateNumericInput(session, "sim_namostras", value = 1000)
      updateNumericInput(session, "sim_seed", value = 82)
      updateNumericInput(session, "sim_x0", value = 30)
      updateNumericInput(session, "sim_wx", value = 10)
      updateNumericInput(session, "sim_alpha1", value = 1.80)
      updateNumericInput(session, "sim_walpha", value = 0.002)
    } else if (identical(tipo, "dlm")) {
      updateSelectizeInput(
        session, "sim_modo",
        choices = c("Filtrado" = "filtrado", "1-step ahead" = "one_step"),
        selected = "one_step",
        server = TRUE
      )
      updateNumericInput(session, "sim_dlm_n", value = 201)
      updateNumericInput(session, "sim_dlm_seed", value = 5)
      updateNumericInput(session, "sim_dlm_m0", value = 20)
      updateNumericInput(session, "sim_dlm_c0", value = 4)
      updateNumericInput(session, "sim_dlm_w", value = 0.5)
      updateCheckboxInput(session, "sim_dlm_v_conhecida", value = TRUE)
      updateNumericInput(session, "sim_dlm_v", value = 2)
      updateTextInput(session, "sim_deltas", value = "0.90,0.95,0.99")
    } else if (identical(tipo, "dglm")) {
      updateSelectizeInput(
        session, "sim_modo",
        choices = c("Suavizado" = "suavizado", "Filtrado" = "filtrado", "1-step ahead" = "one_step"),
        selected = "one_step",
        server = TRUE
      )
      updateNumericInput(session, "sim_dglm_n", value = 201)
      updateNumericInput(session, "sim_dglm_seed", value = 5)
      updateNumericInput(session, "sim_dglm_m0", value = log(60))
      updateNumericInput(session, "sim_dglm_c0", value = 0.2)
      updateNumericInput(session, "sim_dglm_w", value = 0.001)
      updateTextInput(session, "sim_deltas", value = "0.90,0.95,0.99")
    }
    session$sendCustomMessage("plotly-resize", list())
  }, ignoreInit = TRUE)

  observeEvent(input$tabs, {
    if (identical(input$tabs %||% "", "sim")) {
      session$sendCustomMessage("plotly-resize", list())
    }
  }, ignoreInit = TRUE)

  sim_inputs <- reactive({
    tipo <- input$sim_tipo %||% "pdldglm"
    if (identical(tipo, "pdldglm")) {
      dsel <- suppressWarnings(as.integer(input$sim_d %||% 2L))
      if (!is.finite(dsel) || !(dsel %in% c(2L, 3L))) dsel <- 2L
      n_eta <- dsel + 1L
      eta_vals <- vapply(seq_len(n_eta), function(i) {
        as.numeric(input[[paste0("sim_eta_", i - 1L)]] %||% NA_real_)
      }, numeric(1))
      return(list(
        tipo = "pdldglm",
        modo = normalizar_modo_sim(input$sim_modo, "pdldglm"),
        n_total = as.integer(input$sim_n_total),
        lags = as.integer(input$sim_lags),
        d = dsel,
        n_amostras = as.integer(input$sim_namostras %||% 1000L),
        seed = as.integer(input$sim_seed),
        x0 = as.numeric(input$sim_x0),
        wx = as.numeric(input$sim_wx),
        alpha1 = as.numeric(input$sim_alpha1),
        walpha = as.numeric(input$sim_walpha),
        eta_vals = eta_vals,
        fit = TRUE
      ))
    }
    if (identical(tipo, "dlm")) {
      return(list(
        tipo = "dlm",
        modo = normalizar_modo_sim(input$sim_modo, "dlm"),
        n = as.integer(input$sim_dlm_n),
        seed = as.integer(input$sim_dlm_seed),
        m0 = as.numeric(input$sim_dlm_m0),
        c0 = as.numeric(input$sim_dlm_c0),
        w = as.numeric(input$sim_dlm_w),
        v_known = isTRUE(input$sim_dlm_v_conhecida),
        v = as.numeric(input$sim_dlm_v),
        deltas = parse_deltas(input$sim_deltas)
      ))
    }
    list(
      tipo = "dglm",
      modo = normalizar_modo_sim(input$sim_modo, "dglm"),
      n = as.integer(input$sim_dglm_n),
      seed = as.integer(input$sim_dglm_seed),
      m0 = as.numeric(input$sim_dglm_m0),
      c0 = as.numeric(input$sim_dglm_c0),
      w = as.numeric(input$sim_dglm_w),
      deltas = parse_deltas(input$sim_deltas)
    )
  })
  sim_run <- reactive({
    req(sim_tab_ativa())
    p_in <- sim_inputs()
    nucleo_boot <- get_nucleo()
    req(nucleo_boot$ok)
    tipo <- p_in$tipo %||% "pdldglm"

    if (identical(tipo, "dlm")) {
      set.seed(p_in$seed)
      n <- p_in$n
      theta <- numeric(n)
      y <- numeric(n)
      theta0 <- rnorm(1, mean = p_in$m0, sd = sqrt(p_in$c0))
      theta[1] <- theta0 + rnorm(1, 0, sqrt(p_in$w))
      v_use <- if (isTRUE(p_in$v_known)) p_in$v else 2
      y[1] <- theta[1] + rnorm(1, 0, sqrt(v_use))
      if (n >= 2) {
        for (t in 2:n) {
          theta[t] <- theta[t - 1] + rnorm(1, 0, sqrt(p_in$w))
          y[t] <- theta[t] + rnorm(1, 0, sqrt(v_use))
        }
      }

      req(exists("LM_nivel_sim", envir = nucleo_boot$env, inherits = FALSE))
      lm_fun <- get("LM_nivel_sim", envir = nucleo_boot$env, inherits = FALSE)
      fit_obj <- do.call(lm_fun, list(
        y = y,
        deltas = p_in$deltas,
        Vt = if (isTRUE(p_in$v_known)) p_in$v else NULL,
        m0 = p_in$m0,
        C0 = p_in$c0,
        nivel_ic = 0.95,
        ic_style = "dashed"
      ))
      fits <- fit_obj$fits
      pred_df_mode <- do.call(rbind, lapply(seq_along(fits), function(i) {
        f <- fits[[i]]
        center <- if (identical(p_in$modo, "one_step")) f$ft else f$mt
        varv <- if (identical(p_in$modo, "one_step")) f$Qt else f$Ct
        if (is.null(center) || is.null(varv)) {
          center <- f$ft
          varv <- f$Qt
        }
        center <- as.numeric(center)
        varv <- pmax(as.numeric(varv), 1e-12)
        crit <- if (!is.null(f$nt)) stats::qt(0.975, df = pmax(as.numeric(f$nt), 1)) else stats::qnorm(0.975)
        if (length(crit) == 1L) crit <- rep(crit, length(center))
        data.frame(
          t = seq_along(center),
          delta = p_in$deltas[[i]],
          ft = center,
          lo = center - crit * sqrt(varv),
          hi = center + crit * sqrt(varv)
        )
      }))

      return(list(
        type = "dlm",
        sim = list(y = y, theta = theta, n_total = n),
        fit = fit_obj,
        pred_df = pred_df_mode,
        fit_status = "DLM executado."
      ))
    }

    if (identical(tipo, "dglm")) {
      if (!isTRUE(sim_deps_inited())) {
        inicializar_dependencias_pdldglm()
        sim_deps_inited(TRUE)
      }
      set.seed(p_in$seed)
      n <- p_in$n
      theta <- numeric(n)
      mu <- numeric(n)
      y <- integer(n)
      theta0 <- rnorm(1, mean = p_in$m0, sd = sqrt(p_in$c0))
      theta[1] <- theta0 + rnorm(1, 0, sqrt(p_in$w))
      mu[1] <- exp(theta[1])
      y[1] <- rpois(1, lambda = mu[1])
      if (n >= 2) {
        for (t in 2:n) {
          theta[t] <- theta[t - 1] + rnorm(1, 0, sqrt(p_in$w))
          mu[t] <- exp(theta[t])
          y[t] <- rpois(1, lambda = mu[t])
        }
      }

      fit_obj <- DGLM_poisson_nivel_sim1(
        y = y,
        deltas = p_in$deltas,
        pred_cred = 0.95,
        m0 = 0,
        C0 = 1000,
        lag_coef = modo_to_lag(p_in$modo),
        ic_style = "dashed",
        alpha_ic = 0.45,
        alpha_lines = 1.00,
        lw_ic = 0.90,
        lw_main = 1.00
      )

      return(list(
        type = "dglm",
        sim = list(y = y, theta = theta, mu = mu, n_total = n),
        fit = fit_obj,
        pred_df = fit_obj$pred_df,
        fit_status = "DGLM executado."
      ))
    }

    req(exists("sim_PDLDGLM_poisson", envir = nucleo_boot$env, inherits = FALSE))

    eta <- as.numeric(p_in$eta_vals)
    dsel <- p_in$d
    if (length(eta) != (dsel + 1) || any(!is.finite(eta))) {
      eta <- if (identical(dsel, 3L)) {
        c(-1.663, 0.506, 0.143, -0.011) / 1600
      } else {
        c(-8.47, 5.17, -0.36) / 1600
      }
    }

    sim_key <- paste(
      p_in$n_total, p_in$lags, dsel, p_in$seed,
      signif(p_in$x0, 10), signif(p_in$wx, 10), signif(p_in$alpha1, 10), signif(p_in$walpha, 10),
      paste(signif(eta, 10), collapse = ","),
      sep = "|"
    )
    if (exists(sim_key, envir = sim_cache$sim, inherits = FALSE)) {
      sim_obj <- get(sim_key, envir = sim_cache$sim, inherits = FALSE)
    } else {
      sim_fun <- get("sim_PDLDGLM_poisson", envir = nucleo_boot$env, inherits = FALSE)
      alpha1 <- p_in$alpha1
      req(is.finite(alpha1))
      sim_obj <- do.call(sim_fun, list(
        n_total = p_in$n_total,
        lags = p_in$lags,
        d = dsel,
        seed = p_in$seed,
        x0 = p_in$x0,
        Wx = p_in$wx,
        alpha1 = alpha1,
        W_alpha = p_in$walpha,
        eta_raw = eta
      ))
      assign(sim_key, sim_obj, envir = sim_cache$sim)
    }

    fit_obj <- NULL
    fit_status <- "Atualização imediata. Ajuste desativado."
    if (isTRUE(p_in$fit) && isTRUE(deps_ok) && exists("PDLDGLM", envir = nucleo_boot$env, inherits = FALSE)) {
      fit_key <- paste(sim_key, p_in$lags, dsel, "fd=0.98", paste0("n=", p_in$n_amostras %||% 1000L), sep = "|")
      if (exists(fit_key, envir = sim_cache$fit, inherits = FALSE)) {
        fit_obj <- get(fit_key, envir = sim_cache$fit, inherits = FALSE)
      } else {
        if (!isTRUE(sim_deps_inited())) {
          inicializar_dependencias_pdldglm()
          sim_deps_inited(TRUE)
        }
        fit_fun <- get("PDLDGLM", envir = nucleo_boot$env, inherits = FALSE)
        fit_obj <- do.call(fit_fun, list(
          Y = sim_obj$y_full,
          X = sim_obj$x_full,
          data = sim_obj$data_full,
          lags = p_in$lags,
          d = dsel,
          fd_nivel = 0.98,
          n_amostras = as.integer(p_in$n_amostras %||% 1000L)
        ))
        assign(fit_key, fit_obj, envir = sim_cache$fit)
      }
      fit_status <- "Atualização imediata. Ajuste executado."
    }

    list(type = "pdldglm", sim = sim_obj, fit = fit_obj, fit_status = fit_status)
  })

  output$sim_status <- renderText({
    x <- sim_run(); req(!is.null(x))
    if (identical(x$type, "dlm")) {
      s <- x$sim
      return(paste0(
        "n_total: ", s$n_total,
        "\nTipo: DLM (normal)",
        "\n", x$fit_status
      ))
    }
    if (identical(x$type, "dglm")) {
      s <- x$sim
      return(paste0(
        "n_total: ", s$n_total,
        "\nTipo: DGLM (Poisson)",
        "\n", x$fit_status
      ))
    }
    s <- x$sim
    paste0("n_total: ", s$n_total,
           "\nlags: ", s$lags,
           "\nd: ", s$d,
           "\nT efetivo: ", s$T_eff,
           "\n", x$fit_status)
  })

  output$sim_params <- renderDT({
    x <- sim_run(); req(!is.null(x))
    if (identical(x$type, "dlm")) {
      s <- x$sim
      tb <- tibble(
        parametro = c("n_total", "media(Y)", "sd(Y)"),
        valor = c(s$n_total, mean(s$y, na.rm = TRUE), sd(s$y, na.rm = TRUE))
      )
      return(DT::datatable(tb, options = list(dom = "t"), rownames = FALSE))
    }
    if (identical(x$type, "dglm")) {
      s <- x$sim
      tb <- tibble(
        parametro = c("n_total", "media(Y)", "media(mu)"),
        valor = c(s$n_total, mean(s$y, na.rm = TRUE), mean(s$mu, na.rm = TRUE))
      )
      return(DT::datatable(tb, options = list(dom = "t"), rownames = FALSE))
    }
    s <- x$sim
    tb <- tibble(
      parametro = c("x0", "Wx", "alpha1 (log)", "mu0 = exp(alpha1)", "W_alpha"),
      valor = c(s$x0, s$Wx, s$alpha1, exp(s$alpha1), s$W_alpha)
    )
    DT::datatable(tb, options = list(dom = "t"), rownames = FALSE)
  })

  output$sim_series <- renderPlotly({
    x <- sim_run(); req(!is.null(x))
    s <- x$sim
    if (identical(x$type, "dlm")) {
      df <- tibble(t = seq_along(s$y), Y = as.numeric(s$y), Theta = as.numeric(s$theta))
      return(
        plot_ly(df, x = ~t) %>%
          add_lines(
            y = ~Y, name = "Dados", line = list(color = ic2025_theme_value("model.black"), width = 2),
            hovertemplate = "Tempo: %{x}<br>Dados: %{y:.4f}<extra></extra>"
          ) %>%
          add_lines(
            y = ~Theta, name = "Nível verdadeiro", line = list(color = ic2025_theme_value("model.primary"), width = 2.2),
            hovertemplate = "Tempo: %{x}<br>Nível verdadeiro: %{y:.4f}<extra></extra>"
          ) %>%
          layout(
            xaxis = modifyList(plotly_layout_base$xaxis, list(title = "Tempo")),
            yaxis = modifyList(plotly_layout_base$yaxis, list(title = "Resposta")),
            paper_bgcolor = plotly_layout_base$paper_bgcolor,
            plot_bgcolor = plotly_layout_base$plot_bgcolor,
            font = plotly_layout_base$font,
            margin = plotly_layout_base$margin,
            legend = plotly_layout_base$legend
          )
      )
    }
    if (identical(x$type, "dglm")) {
      df <- tibble(t = seq_along(s$y), Y = as.numeric(s$y), Mu = as.numeric(s$mu))
      return(
        plot_ly(df, x = ~t) %>%
          add_lines(
            y = ~Y, name = "Dados", line = list(color = ic2025_theme_value("model.black"), width = 2),
            hovertemplate = "Tempo: %{x}<br>Dados: %{y:.4f}<extra></extra>"
          ) %>%
          add_lines(
            y = ~Mu, name = "mu verdadeiro", line = list(color = ic2025_theme_value("model.primary"), width = 2.2),
            hovertemplate = "Tempo: %{x}<br>mu verdadeiro: %{y:.4f}<extra></extra>"
          ) %>%
          layout(
            xaxis = modifyList(plotly_layout_base$xaxis, list(title = "Tempo")),
            yaxis = modifyList(plotly_layout_base$yaxis, list(title = "Resposta")),
            paper_bgcolor = plotly_layout_base$paper_bgcolor,
            plot_bgcolor = plotly_layout_base$plot_bgcolor,
            font = plotly_layout_base$font,
            margin = plotly_layout_base$margin,
            legend = plotly_layout_base$legend
          )
      )
    }
    df <- tibble(t = seq_along(s$y_full), Y = as.numeric(s$y_full), Mu = as.numeric(s$mu_full))
    plot_ly(df, x = ~t) %>%
      add_lines(
        y = ~Y, name = "Dados", line = list(color = ic2025_theme_value("model.black"), width = 2),
        hovertemplate = "Tempo: %{x}<br>Dados: %{y:.4f}<extra></extra>"
      ) %>%
      add_lines(
        y = ~Mu, name = "mu verdadeiro", line = list(color = ic2025_theme_value("model.primary"), width = 2.2),
        hovertemplate = "Tempo: %{x}<br>mu verdadeiro: %{y:.4f}<extra></extra>"
      ) %>%
      layout(
        xaxis = modifyList(plotly_layout_base$xaxis, list(title = "Tempo")),
        yaxis = modifyList(plotly_layout_base$yaxis, list(title = "Resposta")),
        paper_bgcolor = plotly_layout_base$paper_bgcolor,
        plot_bgcolor = plotly_layout_base$plot_bgcolor,
        font = plotly_layout_base$font,
        margin = plotly_layout_base$margin,
        legend = plotly_layout_base$legend
      )
  })

  output$sim_beta_true <- renderPlotly({
    req((input$sim_tipo %||% "pdldglm") == "pdldglm")
    x <- sim_run(); req(!is.null(x))
    s <- x$sim
    df <- tibble(lag = 0:s$lags, beta = as.numeric(s$beta_true))
    plot_ly(
      df, x = ~lag, y = ~beta, type = "scatter", mode = "lines+markers",
      line = list(color = ic2025_theme_value("model.black"), width = 2), marker = list(color = ic2025_theme_value("model.black")),
      hovertemplate = "Lag: %{x}<br>Beta verdadeiro: %{y:.6f}<extra></extra>"
    ) %>%
      layout(
        xaxis = modifyList(plotly_layout_base$xaxis, list(title = "Lags", tickmode = "linear", tick0 = 0, dtick = 1)),
        yaxis = modifyList(plotly_layout_base$yaxis, list(title = "Beta verdadeiro")),
        paper_bgcolor = plotly_layout_base$paper_bgcolor,
        plot_bgcolor = plotly_layout_base$plot_bgcolor,
        font = plotly_layout_base$font,
        margin = plotly_layout_base$margin,
        legend = plotly_layout_base$legend
      )
  })
  output$sim_fit_mu <- renderPlotly({
    x <- sim_run(); req(!is.null(x))
    if (identical(x$type, "dlm") || identical(x$type, "dglm")) {
      df <- x$pred_df
      req(!is.null(df), nrow(df) > 0)
      deltas <- sort(unique(df$delta))
      p <- plot_ly(type = "scatter", mode = "lines")
      for (dlt in deltas) {
        dfk <- df[df$delta == dlt, , drop = FALSE]
        adj <- ajustar_faixa_plot(y = x$sim$y[dfk$t], mu = dfk$ft, lo = dfk$lo, hi = dfk$hi)
        dfk$ft_plot <- adj$mu_plot
        dfk$lo_plot <- adj$lo_plot
        dfk$hi_plot <- adj$hi_plot
        p <- p %>%
          add_lines(
            data = dfk, x = ~t, y = ~lo_plot, inherit = FALSE, showlegend = FALSE,
            line = list(dash = "dash", width = 1.1),
            name = paste0("delta=", dlt, " (IC)"),
            hovertemplate = paste0("Tempo: %{x}<br>IC inf (delta=", dlt, "): %{y:.4f}<extra></extra>")
          ) %>%
          add_lines(
            data = dfk, x = ~t, y = ~hi_plot, inherit = FALSE, showlegend = FALSE,
            line = list(dash = "dash", width = 1.1),
            name = paste0("delta=", dlt, " (IC)"),
            hovertemplate = paste0("Tempo: %{x}<br>IC sup (delta=", dlt, "): %{y:.4f}<extra></extra>")
          ) %>%
          add_lines(
            data = dfk, x = ~t, y = ~ft_plot, inherit = FALSE,
            name = paste0("delta=", dlt),
            line = list(width = 2.1),
            hovertemplate = paste0("Tempo: %{x}<br>Ajuste (delta=", dlt, "): %{y:.4f}<extra></extra>")
          )
      }
      adj_dados <- ajustar_faixa_plot(y = x$sim$y, mu = x$sim$y, lo = x$sim$y, hi = x$sim$y)
      return(
        p %>%
          add_lines(
            data = tibble(t = seq_along(x$sim$y), Y = adj_dados$y_plot), x = ~t, y = ~Y,
            name = "Dados", line = list(color = ic2025_theme_value("model.black"), width = 1.6), inherit = FALSE,
            hovertemplate = "Tempo: %{x}<br>Dados: %{y:.4f}<extra></extra>"
          ) %>%
          layout(
            xaxis = modifyList(plotly_layout_base$xaxis, list(title = "Tempo")),
            yaxis = modifyList(plotly_layout_base$yaxis, list(title = "Resposta", rangemode = "tozero")),
            paper_bgcolor = plotly_layout_base$paper_bgcolor,
            plot_bgcolor = plotly_layout_base$plot_bgcolor,
            font = plotly_layout_base$font,
            margin = plotly_layout_base$margin,
            legend = plotly_layout_base$legend
          )
      )
    }
    if (is.null(x$fit)) return(plot_ly(type = "scatter", mode = "lines") %>% layout(title = "Ajuste não executado"))
    modo <- normalizar_modo_sim(input$sim_modo, if (identical(x$type, "pdldglm")) "pdldglm" else if (identical(x$type, "dlm")) "dlm" else "dglm")
    mu_alt <- NULL
    if (!identical(modo, "suavizado") && !is.null(x$fit$ajuste1)) {
      lag_sel <- lag_modo_modelagem(modo)
      co <- try(stats::coef(x$fit$ajuste1, lag = lag_sel, eval.pred = TRUE, eval.metric = TRUE, pred.cred = 0.95), silent = TRUE)
      if (!inherits(co, "try-error")) {
        mu_alt <- extrair_mu_ic_kdglm(co)
      }
    }
    if (!is.null(mu_alt)) {
      y <- tail(as.numeric(x$sim$y_full), nrow(mu_alt))
      t_idx <- seq_along(y)
      df <- tibble(t = t_idx, Y = y, Mu = as.numeric(mu_alt$mu), Lo = as.numeric(mu_alt$lo), Hi = as.numeric(mu_alt$hi))
    } else {
      y <- tail(as.numeric(x$sim$y_full), length(x$fit$mu_media))
      t_idx <- seq_along(y)
      df <- tibble(t = t_idx, Y = y, Mu = as.numeric(x$fit$mu_media), Lo = as.numeric(x$fit$mu_ic_inf), Hi = as.numeric(x$fit$mu_ic_sup))
    }
    adj <- ajustar_faixa_plot(df$Y, df$Mu, df$Lo, df$Hi)
    df$Y_plot <- adj$y_plot
    df$Mu_plot <- adj$mu_plot
    df$Lo_plot <- adj$lo_plot
    df$Hi_plot <- adj$hi_plot
    df$txt_dados <- sprintf("Tempo: %s<br>Dados: %.4f", df$t, df$Y)
    df$txt_mu <- sprintf(
      "Tempo: %s<br>Estimativa: %.4f<br>IC 95%%: [%.4f, %.4f]",
      df$t, df$Mu, df$Lo, df$Hi
    )

    plot_ly(df, x = ~t) %>%
      add_ribbons(
        ymin = ~Lo_plot, ymax = ~Hi_plot, name = "IC 95%",
        fillcolor = ic2025_theme_value("model.primary_band"), line = list(color = ic2025_theme_value("dashboard.transparent")),
        hoverinfo = "skip"
      ) %>%
      add_lines(
        y = ~Y_plot, name = "Dados", line = list(color = ic2025_theme_value("model.black_soft"), width = 1.7),
        text = ~txt_dados, hoverinfo = "text"
      ) %>%
      add_lines(
        y = ~Mu_plot, name = "Estimativas", line = list(color = ic2025_theme_value("model.primary"), width = 2.8),
        text = ~txt_mu, hoverinfo = "text"
      ) %>%
      layout(
        xaxis = modifyList(plotly_layout_base$xaxis, list(title = "Tempo")),
        yaxis = modifyList(plotly_layout_base$yaxis, list(title = "Resposta", rangemode = "tozero")),
        paper_bgcolor = plotly_layout_base$paper_bgcolor,
        plot_bgcolor = plotly_layout_base$plot_bgcolor,
        font = plotly_layout_base$font,
        margin = plotly_layout_base$margin,
        legend = plotly_layout_base$legend
      )
  })

  output$sim_fit_beta <- renderPlotly({
    req((input$sim_tipo %||% "pdldglm") == "pdldglm")
    x <- sim_run(); req(!is.null(x))
    if (is.null(x$fit)) return(plot_ly(type = "scatter", mode = "lines") %>% layout(title = "Ajuste não executado"))

    b <- x$fit
    df <- tibble(
      lag = seq_along(b$beta_media) - 1L,
      rr = as.numeric(b$beta_media),
      lo = as.numeric(b$beta_ic_inf),
      hi = as.numeric(b$beta_ic_sup)
    )
    df$txt_rr <- sprintf(
      "Lag: %s<br>RR: %.4f<br>IC 95%%: [%.4f, %.4f]",
      df$lag, df$rr, df$lo, df$hi
    )

    plot_ly(df, x = ~lag) %>%
      add_ribbons(
        ymin = ~lo, ymax = ~hi, name = "IC 95%",
        fillcolor = ic2025_theme_value("model.primary_band"), line = list(color = ic2025_theme_value("dashboard.transparent")),
        hoverinfo = "skip"
      ) %>%
      add_lines(
        y = ~rr, name = "RR", line = list(color = ic2025_theme_value("model.primary"), width = 2.3),
        text = ~txt_rr, hoverinfo = "text"
      ) %>%
      add_markers(
        y = ~rr, name = "RR", marker = list(color = ic2025_theme_value("model.primary"), size = 6), showlegend = FALSE,
        text = ~txt_rr, hoverinfo = "text"
      ) %>%
      add_lines(
        y = rep(1, nrow(df)), name = "RR=1",
        line = list(color = ic2025_theme_value("model.black_soft"), width = 1.4),
        showlegend = FALSE,
        hoverinfo = "skip"
      ) %>%
      layout(
        xaxis = modifyList(plotly_layout_base$xaxis, list(title = "Lags", tickmode = "linear", tick0 = 0, dtick = 1)),
        yaxis = modifyList(plotly_layout_base$yaxis, list(title = "Risco Relativo")),
        paper_bgcolor = plotly_layout_base$paper_bgcolor,
        plot_bgcolor = plotly_layout_base$plot_bgcolor,
        font = plotly_layout_base$font,
        margin = plotly_layout_base$margin,
        legend = plotly_layout_base$legend
      )
  })
  outputOptions(output, "sim_series", suspendWhenHidden = TRUE)
  outputOptions(output, "sim_beta_true", suspendWhenHidden = TRUE)
  outputOptions(output, "sim_fit_beta", suspendWhenHidden = TRUE)
  outputOptions(output, "desc_plot", suspendWhenHidden = FALSE)
  outputOptions(output, "desc_map_brasil", suspendWhenHidden = !isTRUE(DESC_MAP_WARM_BACKGROUND))
  outputOptions(output, "desc_map_regiao", suspendWhenHidden = !isTRUE(DESC_MAP_WARM_BACKGROUND))
  outputOptions(output, "desc_map_estado", suspendWhenHidden = !isTRUE(DESC_MAP_WARM_BACKGROUND))
  outputOptions(output, "desc_map_legend", suspendWhenHidden = !isTRUE(DESC_MAP_WARM_BACKGROUND))
  }

  carregar_df_modelagem_cache <- function(cidade, data_ini, data_fim) {
    di <- tryCatch(as.Date(data_ini), error = function(e) as.Date(NA))
    df <- tryCatch(as.Date(data_fim), error = function(e) as.Date(NA))
    if (is.na(di) || is.na(df)) {
      return(tibble())
    }
    dr <- sort(c(di, df))
    di <- dr[[1]]
    df <- dr[[2]]
    ano1 <- suppressWarnings(as.integer(format(di, "%Y")))
    ano2 <- suppressWarnings(as.integer(format(df, "%Y")))
    data_key <- paste(cidade, as.character(di), as.character(df), sep = "|")
    if (exists(data_key, envir = model_cache$dados, inherits = FALSE)) {
      return(get(data_key, envir = model_cache$dados, inherits = FALSE))
    }
    out <- tryCatch(
      carregar_base_modelagem_local(cidade, ano1, ano2),
      error = function(e) e
    )
    if (inherits(out, "error")) {
      empty <- tibble()
      attr(empty, "load_error") <- conditionMessage(out)
      return(empty)
    }
    if (is.data.frame(out) && "Data" %in% names(out)) {
      out <- out %>%
        mutate(Data = as.Date(.data$Data)) %>%
        filter(.data$Data >= di, .data$Data <= df) %>%
        arrange(.data$Data)
    }
    assign(data_key, out, envir = model_cache$dados)
    out
  }

  app_data_info <- reactive({
    cidade <- input$app_cidade %||% cidade_default_key
    ano <- suppressWarnings(as.integer(input$app_ano))
    periodo <- normalizar_periodo_modelagem(input$app_periodo, fallback_ano = ano)
    df <- carregar_df_modelagem_cache(cidade, periodo[[1]], periodo[[2]])
    inf <- info_modelagem_df(df)
    list(df = df, info = inf, cidade = cidade, ano = ano, periodo = periodo)
  })

  observe({
    x <- app_data_info()
    covars <- x$info$covars
    covar_choices <- if (length(covars) > 0) {
      setNames(covars, vapply(covars, rotulo_var_desc, character(1)))
    } else {
      c("Sem covariável climática disponível" = "__NONE__")
    }
    covar_sel <- input$app_covar %||% "temp"
    if (!(covar_sel %in% as.character(unname(covar_choices)))) {
      covar_sel <- if ("temp" %in% covars) "temp" else if (length(covars) > 0) covars[[1]] else "__NONE__"
    }
    updateSelectizeInput(
      session, "app_covar",
      choices = covar_choices,
      selected = covar_sel,
      server = FALSE
    )

    if ((input$app_modelo %||% "clima") %in% c("clima", "duo") && identical(covar_sel, "__NONE__")) {
      updateSelectizeInput(session, "app_modelo", selected = "pdldglm", server = FALSE)
    }
  })

  observeEvent(input$app_modelo, {
    if (identical(input$app_modelo %||% "", "duo") && isTRUE(input$app_usar_sazonal)) {
      updateCheckboxInput(session, "app_usar_sazonal", value = FALSE)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$app_cidade, {
    app_batch_updating(TRUE)
    app_programmatic_update(TRUE)
    on.exit(app_programmatic_update(FALSE), add = TRUE)

    chave <- input$app_cidade %||% cidade_default_key
    anos <- anos_disponiveis_cidade(chave)
    req(length(anos) > 0)
    p <- preset_modelagem_cidade(chave)

    ano_sel <- if (!is.na(p$ano) && p$ano %in% anos) p$ano else max(anos)
    limites <- periodo_modelagem_limites(anos)
    periodo_sel <- periodo_modelagem_por_ano(ano_sel)

    updateSelectizeInput(session, "app_ano", choices = as.character(rev(anos)), selected = as.character(ano_sel), server = FALSE)
    updateDateRangeInput(session, "app_periodo", start = periodo_sel[[1]], end = periodo_sel[[2]], min = limites[[1]], max = limites[[2]])
    updateSelectizeInput(session, "app_modelo", selected = p$modelo, server = FALSE)
    updateSliderInput(
      session, "app_lags",
      min = 0,
      max = APP_MAX_LAGS,
      value = max(0L, min(APP_MAX_LAGS, as.integer(p$lags %||% 10L)))
    )
    updateSelectizeInput(session, "app_d", selected = as.character(p$d), server = FALSE)
    updateNumericInput(session, "app_fd", value = p$fd)
    updateCheckboxInput(session, "app_usar_sazonal", value = isTRUE(p$usar_sazonal %||% APP_SAZONAL_DEFAULT_ENABLED))
    updateNumericInput(session, "app_periodo_sazonal", value = as.integer(p$periodo_sazonal %||% APP_SAZONAL_DEFAULT_PERIOD))
    updateNumericInput(session, "app_ordem_sazonal", value = as.integer(p$ordem_sazonal %||% APP_SAZONAL_DEFAULT_ORDER))
    updateNumericInput(session, "app_fd_sazonal", value = as.numeric(p$fd_sazonal %||% APP_SAZONAL_DEFAULT_FD))
    updateSelectizeInput(session, "app_covar", selected = p$covar, server = FALSE)
    updateNumericInput(session, "app_lag_covar", value = p$lag_covar)
    updateSliderInput(session, "app_lags_covar", value = max(2L, min(APP_MAX_LAGS, as.integer(p$lags %||% 10L))))
    updateSliderInput(session, "app_perc", value = perc_lado_para_faixa(p$perc, p$lado))
    if (isTRUE(SHOW_APP_SERIES_MODE_INPUT)) {
      updateSelectizeInput(session, "app_modo", selected = "suavizado", server = FALSE)
    }

    session$onFlushed(function() {
      app_batch_updating(FALSE)
    }, once = TRUE)
  }, ignoreInit = FALSE)

  observeEvent(input$app_ano, {
    if (isTRUE(app_batch_updating())) return(invisible(NULL))
    chave <- input$app_cidade %||% cidade_default_key
    anos <- anos_disponiveis_cidade(chave)
    ano_sel <- suppressWarnings(as.integer(input$app_ano))
    if (length(ano_sel) == 0 || !is.finite(ano_sel[[1]])) return(invisible(NULL))
    periodo_sel <- periodo_modelagem_por_ano(ano_sel[[1]])
    limites <- periodo_modelagem_limites(anos)
    freezeReactiveValue(input, "app_periodo")
    updateDateRangeInput(session, "app_periodo", start = periodo_sel[[1]], end = periodo_sel[[2]], min = limites[[1]], max = limites[[2]])
    invisible(NULL)
  }, ignoreInit = TRUE)

  app_inputs_now <- reactive({
    to_int1 <- function(v, default = NA_integer_) {
      out <- suppressWarnings(as.integer(v))
      if (length(out) == 0 || !is.finite(out[[1]])) return(as.integer(default))
      as.integer(out[[1]])
    }
    to_num1 <- function(v, default = NA_real_) {
      out <- suppressWarnings(as.numeric(v))
      if (length(out) == 0 || !is.finite(out[[1]])) return(as.numeric(default))
      as.numeric(out[[1]])
    }
    to_chr1 <- function(v, default = "") {
      out <- as.character(v)
      if (length(out) == 0 || !nzchar(out[[1]] %||% "")) return(as.character(default))
      as.character(out[[1]])
    }
    list(
      cidade = to_chr1(input$app_cidade, cidade_default_key),
      ano = to_int1(input$app_ano, NA_integer_),
      periodo = normalizar_periodo_modelagem(input$app_periodo, fallback_ano = to_int1(input$app_ano, NA_integer_)),
      modelo = to_chr1(input$app_modelo, "pdldglm"),
      lags = max(0L, min(APP_MAX_LAGS, to_int1(input$app_lags, 10L))),
      d = to_int1(input$app_d, 2L),
      fd = to_num1(input$app_fd, 0.98),
      usar_sazonal = isTRUE(input$app_usar_sazonal),
      periodo_sazonal = to_int1(input$app_periodo_sazonal, APP_SAZONAL_DEFAULT_PERIOD),
      ordem_sazonal = to_int1(input$app_ordem_sazonal, APP_SAZONAL_DEFAULT_ORDER),
      fd_sazonal = to_num1(input$app_fd_sazonal, APP_SAZONAL_DEFAULT_FD),
      center = FALSE,
      dp = TRUE,
      n_amostras = as.integer(APP_FIXED_N_AMOSTRAS),
      covar = to_chr1(input$app_covar, "__NONE__"),
      lag_covar = to_int1(input$app_lag_covar, 0L),
      lags_covar = max(2L, min(APP_MAX_LAGS, to_int1(input$app_lags_covar, 10L))),
      perc_faixa = as.numeric(input$app_perc %||% c(0.85, 1.00)),
      modo = to_chr1(input$app_modo, "suavizado")
    )
  })
  app_perc_deb <- debounce(reactive(as.numeric(input$app_perc %||% c(0.85, 1.00))), millis = 2000)

  app_evento_rapido <- reactive({
    list(
      cidade = input$app_cidade %||% cidade_default_key,
      ano = input$app_ano,
      periodo = tryCatch(as.character(as.Date(input$app_periodo)), error = function(e) c(NA_character_, NA_character_)),
      modelo = input$app_modelo,
      lags = input$app_lags,
      d = input$app_d,
      fd = input$app_fd,
      usar_sazonal = input$app_usar_sazonal,
      periodo_sazonal = input$app_periodo_sazonal,
      ordem_sazonal = input$app_ordem_sazonal,
      fd_sazonal = input$app_fd_sazonal,
      covar = input$app_covar,
      lag_covar = input$app_lag_covar,
      lags_covar = input$app_lags_covar,
      perc_faixa = input$app_perc,
      batch = app_batch_updating()
    )
  })

  observeEvent(input$app_covar, {
    cov <- input$app_covar %||% "__NONE__"
    if (identical(cov, "__NONE__")) return()
    perc_atual <- sort(as.numeric(input$app_perc %||% c(0.85, 1.00)))
    if (length(perc_atual) < 2 || any(!is.finite(perc_atual))) {
      app_batch_updating(TRUE)
      app_programmatic_update(TRUE)
      on.exit(app_programmatic_update(FALSE), add = TRUE)
      if (identical(cov, "umid")) {
        updateSliderInput(session, "app_perc", value = c(0, 0.15))
      } else {
        updateSliderInput(session, "app_perc", value = c(0.85, 1.00))
      }
      session$onFlushed(function() {
        app_batch_updating(FALSE)
      }, once = TRUE)
    }
  }, ignoreInit = FALSE)

  observeEvent(input$app_periodo_sazonal, {
    periodo <- suppressWarnings(as.integer(input$app_periodo_sazonal %||% APP_SAZONAL_DEFAULT_PERIOD))
    if (length(periodo) == 0 || !is.finite(periodo[[1]]) || periodo[[1]] <= 1L) {
      return(invisible(NULL))
    }
    periodo <- as.integer(periodo[[1]])
    ordem_max <- max(1L, floor(periodo / 2))
    ordem_atual <- suppressWarnings(as.integer(input$app_ordem_sazonal %||% APP_SAZONAL_DEFAULT_ORDER))
    if (length(ordem_atual) == 0 || !is.finite(ordem_atual[[1]]) || ordem_atual[[1]] < 1L) {
      ordem_atual <- APP_SAZONAL_DEFAULT_ORDER
    } else {
      ordem_atual <- as.integer(ordem_atual[[1]])
    }
    ordem_nova <- max(1L, min(ordem_atual, ordem_max))
    updateNumericInput(session, "app_ordem_sazonal", value = ordem_nova, min = 1, max = ordem_max)
    invisible(NULL)
  }, ignoreInit = FALSE)

  app_fit_inputs <- eventReactive(
    list(app_evento_rapido(), app_perc_deb()),
    {
      req(!isTRUE(app_batch_updating()))
      p_in <- app_inputs_now()
      if (isTRUE(app_primeira_execucao())) {
        p_in$n_amostras <- as.integer(APP_FIXED_N_AMOSTRAS)
        p_in$perc_faixa <- as.numeric(input$app_perc %||% c(0.85, 1.00))
        app_primeira_execucao(FALSE)
      } else {
        p_in$n_amostras <- as.integer(APP_FIXED_N_AMOSTRAS)
        p_in$perc_faixa <- as.numeric(app_perc_deb())
      }
      p_in
    },
    ignoreInit = FALSE
  )

  app_fit_core <- reactive({
    p_in <- app_fit_inputs()
    nucleo_boot <- get_nucleo()
    req(nucleo_boot$ok)
    req(isTRUE(deps_ok))
    if (!isTRUE(deps_inited_flag$value)) {
      inicializar_dependencias_pdldglm()
      deps_inited_flag$value <- TRUE
    }

    p_in$data_ini <- as.Date(p_in$periodo[[1]])
    p_in$data_fim <- as.Date(p_in$periodo[[2]])
    if (is.na(p_in$data_ini) || is.na(p_in$data_fim)) {
      return(list(ready = FALSE, msg = "Período inválido para modelagem.", df = tibble(), fit = NULL, modelo = p_in$modelo, params = p_in))
    }

    if (identical(p_in$modelo, "duo") && isTRUE(p_in$usar_sazonal)) {
      return(list(
        ready = FALSE,
        msg = "O modelo PDLDGLM c/ duo ainda não possui versão sazonal nesta aba.",
        df = tibble(), fit = NULL, modelo = p_in$modelo, params = p_in
      ))
    }

    if (isTRUE(p_in$usar_sazonal)) {
      if (!is.finite(p_in$periodo_sazonal) || p_in$periodo_sazonal <= 1 || p_in$periodo_sazonal != round(p_in$periodo_sazonal)) {
        return(list(
          ready = FALSE,
          msg = "O período sazonal deve ser um inteiro maior que 1, medido em número de observações.",
          df = tibble(), fit = NULL, modelo = p_in$modelo, params = p_in
        ))
      }
      if (!is.finite(p_in$ordem_sazonal) || p_in$ordem_sazonal < 1 || p_in$ordem_sazonal != round(p_in$ordem_sazonal)) {
        return(list(
          ready = FALSE,
          msg = "A ordem harmônica deve ser um inteiro maior ou igual a 1.",
          df = tibble(), fit = NULL, modelo = p_in$modelo, params = p_in
        ))
      }
      if (p_in$ordem_sazonal > floor(p_in$periodo_sazonal / 2)) {
        return(list(
          ready = FALSE,
          msg = paste0(
            "A ordem harmônica deve ser menor ou igual a floor(periodo_sazonal/2) = ",
            floor(p_in$periodo_sazonal / 2), "."
          ),
          df = tibble(), fit = NULL, modelo = p_in$modelo, params = p_in
        ))
      }
    if (!is.finite(p_in$fd_sazonal) || p_in$fd_sazonal <= 0 || p_in$fd_sazonal > 1) {
        return(list(
          ready = FALSE,
          msg = "O fator de desconto sazonal deve estar no intervalo (0, 1].",
          df = tibble(), fit = NULL, modelo = p_in$modelo, params = p_in
        ))
      }
    }
    df <- carregar_df_modelagem_cache(p_in$cidade, p_in$data_ini, p_in$data_fim)
    inf <- info_modelagem_df(df)
    if (!isTRUE(inf$ok)) {
      return(list(ready = FALSE, msg = inf$msg, df = tibble(), fit = NULL, modelo = p_in$modelo, params = p_in))
    }
    if (!isTRUE(inf$has_resp) || !isTRUE(inf$has_pm25)) {
      return(list(ready = FALSE, msg = inf$msg, df = df, fit = NULL, modelo = p_in$modelo, params = p_in))
    }

    df <- df %>% filter(is.finite(Casos_Resp), is.finite(pm25), !is.na(Data))
    if (nrow(df) <= (p_in$lags + 20)) {
      return(list(
        ready = FALSE,
        msg = paste0("Dados insuficientes após filtros (n=", nrow(df), "). Ajuste lags/período/cidade."),
        df = df, fit = NULL, modelo = p_in$modelo, params = p_in
      ))
    }

    modelo <- p_in$modelo
    pl <- faixa_para_perc_lado(p_in$perc_faixa, p_in$covar)
    p_in$perc <- pl$perc
    p_in$lado <- pl$lado
    p_in$perc_sup <- pl$perc_sup
    fit_key <- paste(
      p_in$cidade, as.character(p_in$data_ini), as.character(p_in$data_fim), modelo, p_in$lags, p_in$d, p_in$fd,
      paste0("saz=", isTRUE(p_in$usar_sazonal)),
      p_in$periodo_sazonal,
      p_in$ordem_sazonal,
      sprintf("%.4f", p_in$fd_sazonal),
      "center=FALSE", "dp=TRUE", p_in$n_amostras, p_in$covar, p_in$lag_covar, p_in$lags_covar,
      sprintf("%.4f", p_in$perc), p_in$lado, sprintf("%.4f", p_in$perc_sup %||% NA_real_), sep = "|"
    )
    if (exists(fit_key, envir = model_cache$ajuste, inherits = FALSE)) {
      fit <- get(fit_key, envir = model_cache$ajuste, inherits = FALSE)
    } else {
      if (identical(modelo, "pdldglm")) {
        fit_fun <- get(
          if (isTRUE(p_in$usar_sazonal)) "PDLDGLM_sazonal" else "PDLDGLM",
          envir = nucleo_boot$env,
          inherits = FALSE
        )
        fit_args <- list(
          Y = df$Casos_Resp,
          X = df$pm25,
          data = df$Data,
          lags = p_in$lags,
          d = p_in$d,
          fd_nivel = p_in$fd,
          padronizar_center = p_in$center,
          padronizar_dp = p_in$dp,
          n_amostras = p_in$n_amostras
        )
        if (isTRUE(p_in$usar_sazonal)) {
          fit_args$periodo_sazonal <- p_in$periodo_sazonal
          fit_args$ordem_sazonal <- p_in$ordem_sazonal
          fit_args$fd_sazonal <- p_in$fd_sazonal
        }
        fit <- do.call(fit_fun, fit_args)
      } else if (identical(modelo, "clima")) {
        covar <- p_in$covar
        if (!isTRUE(covar %in% inf$covars)) {
          return(list(
            ready = FALSE,
            msg = "Não há covariável climática disponível para o modelo com clima nesta cidade/período.",
            df = df, fit = NULL, modelo = modelo, params = p_in
          ))
        }
        df <- df %>% filter(is.finite(.data[[covar]]))
        if (nrow(df) <= (p_in$lags + 20)) {
          return(list(
            ready = FALSE,
            msg = paste0("Dados insuficientes para a covariável climática selecionada (", rotulo_var_desc(covar), ")."),
            df = df, fit = NULL, modelo = modelo, params = p_in
          ))
        }
        fit_fun <- get(
          if (isTRUE(p_in$usar_sazonal)) "PDLDGLM_clima_sazonal" else "PDLDGLM_clima",
          envir = nucleo_boot$env,
          inherits = FALSE
        )
        fit_args <- list(
          Y = df$Casos_Resp,
          X = df$pm25,
          covar = df[[covar]],
          data = df$Data,
          lags = p_in$lags,
          lag_covar = p_in$lag_covar,
          d = p_in$d,
          perc = p_in$perc,
          perc_sup = p_in$perc_sup,
          lado = p_in$lado,
          fd_nivel = p_in$fd,
          padronizar_center = p_in$center,
          padronizar_dp = p_in$dp,
          n_amostras = p_in$n_amostras
        )
        if (isTRUE(p_in$usar_sazonal)) {
          fit_args$periodo_sazonal <- p_in$periodo_sazonal
          fit_args$ordem_sazonal <- p_in$ordem_sazonal
          fit_args$fd_sazonal <- p_in$fd_sazonal
        }
        fit <- do.call(fit_fun, fit_args)
      } else if (identical(modelo, "duo")) {
        covar <- p_in$covar
        if (!isTRUE(covar %in% inf$covars)) {
          return(list(
            ready = FALSE,
            msg = "Não há covariável disponível para o modelo PDLDGLM c/ duo nesta cidade/período.",
            df = df, fit = NULL, modelo = modelo, params = p_in
          ))
        }
        if (!is.finite(p_in$lags) || p_in$lags < 2L) {
          return(list(
            ready = FALSE,
            msg = "No modelo PDLDGLM c/ duo, a janela de defasagem do poluente deve ser pelo menos 2.",
            df = df, fit = NULL, modelo = modelo, params = p_in
          ))
        }
        if (!is.finite(p_in$lags_covar) || p_in$lags_covar < 2L) {
          return(list(
            ready = FALSE,
            msg = "No modelo PDLDGLM c/ duo, a janela da covariável deve ser pelo menos 2.",
            df = df, fit = NULL, modelo = modelo, params = p_in
          ))
        }
        df <- df %>% filter(is.finite(.data[[covar]]))
        if (nrow(df) <= (max(p_in$lags, p_in$lags_covar) + 20)) {
          return(list(
            ready = FALSE,
            msg = paste0("Dados insuficientes para a covariável selecionada (", rotulo_var_desc(covar), ")."),
            df = df, fit = NULL, modelo = modelo, params = p_in
          ))
        }
        fit_fun <- get("PDLDGLM_Duo", envir = nucleo_boot$env, inherits = FALSE)
        fit <- do.call(fit_fun, list(
          Y = df$Casos_Resp,
          X = df$pm25,
          covar = df[[covar]],
          data = df$Data,
          lags = p_in$lags,
          lags_covar = p_in$lags_covar,
          d = p_in$d,
          fd_nivel = p_in$fd,
          padronizar_center = p_in$center,
          padronizar_dp = p_in$dp,
          n_amostras = p_in$n_amostras
        ))
      }
      assign(fit_key, fit, envir = model_cache$ajuste)
    }

    list(ready = TRUE, df = df, fit = fit, modelo = modelo, params = p_in)
  })

  app_run <- reactive({
    req(app_tab_ativa())
    x_core <- app_fit_core()
    if (!isTRUE(x_core$ready)) {
      return(list(
        ready = FALSE,
        df = x_core$df %||% tibble(),
        fit = NULL,
        mu_alt = NULL,
        mu_msg = x_core$msg %||% "Modelagem indisponível para a base/cidade/período atual.",
        modelo = x_core$modelo %||% (input$app_modelo %||% "pdldglm"),
        params = x_core$params %||% app_inputs_now()
      ))
    }
    fit <- x_core$fit
    modelo <- x_core$modelo
    p_in <- x_core$params
    modo <- input$app_modo %||% "suavizado"
    mu_alt <- NULL
    mu_msg <- "Atualização imediata. Suavizado (default)."

    if (identical(modelo, "pdldglm") && !identical(modo, "suavizado") && !is.null(fit$ajuste1)) {
      lag_sel <- lag_modo_modelagem(modo)
      co <- try(stats::coef(fit$ajuste1, lag = lag_sel, eval.pred = TRUE, eval.metric = TRUE, pred.cred = 0.95), silent = TRUE)
      if (!inherits(co, "try-error")) {
        mu_alt <- extrair_mu_ic_kdglm(co)
        mu_msg <- paste0("Atualização imediata. Modo: ", modo)
      }
    }

    list(ready = TRUE, df = x_core$df, fit = fit, mu_alt = mu_alt, mu_msg = mu_msg, modelo = modelo, params = p_in)
  })

  output$app_status <- renderText({
    x <- app_run(); req(!is.null(x))
    if (!isTRUE(x$ready)) {
      return(paste0("Status: ", x$mu_msg))
    }
    d <- x$df
    tau_txt <- "NA"
    if (!is.null(x$fit$tau_media)) {
      if (length(x$fit$tau_media) == 1L) {
        tau_txt <- paste(format(round(c(x$fit$tau_media, x$fit$tau_ic_inf, x$fit$tau_ic_sup), 4), nsmall = 4), collapse = " | ")
      } else {
        tau_txt <- "Curva da covariável disponível no objeto ajustado"
      }
    }
    paste0(
      "Observações usadas: ", nrow(d),
      "\nPeríodo: ", format(min(d$Data), "%d/%m/%Y"), " a ", format(max(d$Data), "%d/%m/%Y"),
      "\n", x$mu_msg,
      "\nSazonalidade harmônica: ",
      if (isTRUE(x$params$usar_sazonal)) {
        paste0(
          "ativa | período=", x$params$periodo_sazonal,
          " | ordem=", x$params$ordem_sazonal,
          " | fd_sazonal=", format(round(x$params$fd_sazonal, 3), nsmall = 3)
        )
      } else {
        "desativada"
      },
      "\nTau (RR, IC inf, IC sup): ", tau_txt
    )
  })

  output$app_params <- renderDT({
    x <- app_run(); req(!is.null(x))
    if (!isTRUE(x$ready)) {
      tb <- tibble(
        parametro = c("status", "mensagem"),
        valor = c("indisponível", x$mu_msg %||% "Modelagem indisponível")
      )
      return(DT::datatable(tb, options = list(dom = "t"), rownames = FALSE))
    }
    p_in <- x$params
    tb <- tibble(
      parametro = c(
        "cidade", "ano_preset", "periodo", "modelo", "lags", "d",
        "fd_nivel", "usa_sazonal",
        "periodo_sazonal", "ordem_sazonal", "fd_sazonal",
        "lag_covar", "lags_covar", "perc", "perc_sup", "lado", "n_amostras"
      ),
      valor = c(
        p_in$cidade, p_in$ano, formatar_periodo_modelagem(p_in$data_ini, p_in$data_fim), p_in$modelo, p_in$lags, p_in$d,
        p_in$fd, isTRUE(p_in$usar_sazonal),
        if (isTRUE(p_in$usar_sazonal)) p_in$periodo_sazonal else NA,
        if (isTRUE(p_in$usar_sazonal)) p_in$ordem_sazonal else NA,
        if (isTRUE(p_in$usar_sazonal)) p_in$fd_sazonal else NA,
        p_in$lag_covar %||% NA, p_in$lags_covar %||% NA, p_in$perc %||% NA, p_in$perc_sup %||% NA, p_in$lado %||% NA, p_in$n_amostras
      )
    )
    DT::datatable(tb, options = list(dom = "t"), rownames = FALSE)
  })

  output$app_mu <- renderPlotly({
    x <- app_run(); req(!is.null(x))
    if (!isTRUE(x$ready)) {
      return(plot_placeholder(x$mu_msg))
    }
    fit <- x$fit

    if (!is.null(x$mu_alt)) {
      mu <- x$mu_alt
      y <- tail(x$df$Casos_Resp, nrow(mu))
      dt <- tail(x$df$Data, nrow(mu))
      dfm <- tibble(Data = dt, Y = y, Mu = mu$mu, Lo = mu$lo, Hi = mu$hi)
    } else {
      y <- tail(x$df$Casos_Resp, length(fit$mu_media))
      dt <- tail(x$df$Data, length(fit$mu_media))
      dfm <- tibble(Data = dt, Y = y, Mu = as.numeric(fit$mu_media), Lo = as.numeric(fit$mu_ic_inf), Hi = as.numeric(fit$mu_ic_sup))
    }
    plot_mu_padrao_app(dfm, periodo_ref = x$params$periodo)
  })

  output$app_beta <- renderPlotly({
    x <- app_run(); req(!is.null(x))
    if (!isTRUE(x$ready)) {
      return(plot_placeholder(x$mu_msg))
    }
    b <- x$fit
    stats_beta <- estatisticas_efetivas_poluente_app(
      df = x$df,
      modelo = x$modelo,
      lags = x$params$lags,
      lag_covar = x$params$lag_covar %||% 0L,
      lags_covar = x$params$lags_covar %||% x$params$lags
    )
    rr <- as.numeric(b$beta_media)
    lo <- as.numeric(b$beta_ic_inf)
    hi <- as.numeric(b$beta_ic_sup)
    dfb <- tibble(lag = seq_along(rr) - 1L, rr = rr, lo = lo, hi = hi)
    dfb$txt_rr <- sprintf(
      "Lag: %s<br>RR: %.4f<br>IC 95%%: [%.4f, %.4f]%s",
      dfb$lag, dfb$rr, dfb$lo, dfb$hi,
      montar_sufixo_hover_stats(
        media_efetiva = stats_beta$media,
        media_rotulo = "Média do PM2.5 (\u00b5g/m\u00b3)",
        sd_efetivo = stats_beta$sd,
        sd_rotulo = "Desvio Padrão do PM2.5 (\u00b5g/m\u00b3)"
      )
    )

    p <- plot_ly(dfb, x = ~lag) %>%
      add_ribbons(
        ymin = ~lo, ymax = ~hi, name = "IC 95%",
        fillcolor = ic2025_theme_value("model.primary_band"), line = list(color = ic2025_theme_value("dashboard.transparent")),
        hoverinfo = "skip"
      ) %>%
      add_lines(
        y = ~rr, name = "RR", line = list(color = ic2025_theme_value("model.black"), width = 2.3),
        text = ~txt_rr, hoverinfo = "text"
      ) %>%
      add_markers(
        y = ~rr, name = "RR", marker = list(color = ic2025_theme_value("model.black"), size = 6), showlegend = FALSE,
        text = ~txt_rr, hoverinfo = "text"
      ) %>%
      add_lines(
        y = rep(1, nrow(dfb)), name = "RR=1",
        line = list(color = ic2025_theme_value("model.black_soft"), width = 1.4),
        showlegend = FALSE,
        hoverinfo = "skip"
      )

    if (isTRUE(input$app_show_tau) && identical(x$modelo, "clima") &&
        !is.null(b$tau_media) && !is.null(b$tau_ic_inf) && !is.null(b$tau_ic_sup)) {
      tau <- as.numeric(b$tau_media)
      tau_lo <- as.numeric(b$tau_ic_inf)
      tau_hi <- as.numeric(b$tau_ic_sup)
      lag_tau <- as.numeric(x$params$lag_covar %||% 0)
      if (is.finite(tau) && is.finite(tau_lo) && is.finite(tau_hi) && is.finite(lag_tau)) {
        stats_tau <- estatisticas_efetivas_covar_app(
          df = x$df,
          covar_nome = x$params$covar,
          modelo = "clima",
          lags = x$params$lags,
          lag_covar = x$params$lag_covar %||% 0L
        )
        p <- p %>% add_markers(
          data = tibble(lag = lag_tau, tau = tau),
          x = ~lag, y = ~tau,
          name = "Efeito climático (tau)",
          marker = list(color = ic2025_theme_value("desc.violet"), size = 10, symbol = "diamond"),
          error_y = list(
            type = "data",
            array = pmax(0, tau_hi - tau),
            arrayminus = pmax(0, tau - tau_lo),
            visible = TRUE,
            thickness = 1.5,
            width = 6,
            color = ic2025_theme_value("desc.violet")
          ),
          hovertemplate = paste0(
            "Lag covariável: %{x}<br>",
            "Tau (RR): %{y:.4f}<br>",
            "IC 95%: [", sprintf("%.4f", tau_lo), ", ", sprintf("%.4f", tau_hi), "]",
            montar_sufixo_hover_stats(
              media_efetiva = stats_tau$media,
              media_rotulo = paste0("Média de ", rotulo_var_desc(x$params$covar)),
              sd_efetivo = stats_tau$sd,
              sd_rotulo = paste0("Desvio Padrão de ", rotulo_var_desc(x$params$covar))
            ),
            "<extra></extra>"
          ),
          inherit = FALSE
        )
      }
    }

    p %>% layout(
      xaxis = modifyList(plotly_layout_base$xaxis, list(title = "Lags", tickmode = "linear", tick0 = 0, dtick = 1)),
      yaxis = modifyList(plotly_layout_base$yaxis, list(title = "Risco Relativo")),
      paper_bgcolor = plotly_layout_base$paper_bgcolor,
      plot_bgcolor = plotly_layout_base$plot_bgcolor,
      font = plotly_layout_base$font,
      margin = plotly_layout_base$margin,
      legend = plotly_layout_base$legend
    )
  })

  output$app_sazonal <- renderPlotly({
    x <- app_run(); req(!is.null(x))
    if (!isTRUE(x$ready)) {
      return(plot_placeholder(x$mu_msg))
    }
    if (!isTRUE(x$params$usar_sazonal)) {
      return(plot_placeholder("Componente sazonal harmônico desativado para o ajuste atual."))
    }
    fit <- x$fit
    if (is.null(fit$sazonal_rr_media) || is.null(fit$sazonal_rr_ic_inf) || is.null(fit$sazonal_rr_ic_sup)) {
      return(plot_placeholder("O ajuste atual não retornou a trajetória do componente sazonal."))
    }

    n_saz <- length(fit$sazonal_rr_media)
    req(n_saz > 0)
    df_sazonal <- tibble(
      Data = tail(x$df$Data, n_saz),
      RR = as.numeric(fit$sazonal_rr_media),
      Lo = as.numeric(fit$sazonal_rr_ic_inf),
      Hi = as.numeric(fit$sazonal_rr_ic_sup)
    )

    plot_sazonal_padrao_app(df_sazonal, periodo_ref = x$params$periodo)
  })

  output$app_tau_curve <- renderPlotly({
    x <- app_run(); req(!is.null(x))
    if (!isTRUE(x$ready)) {
      return(plot_placeholder(x$mu_msg))
    }
    if (!identical(x$modelo, "duo")) {
      return(plot_placeholder("A curva da covariável só está disponível para o modelo PDLDGLM c/ duo."))
    }
    fit <- x$fit
    if (is.null(fit$tau_media) || is.null(fit$tau_ic_inf) || is.null(fit$tau_ic_sup)) {
      return(plot_placeholder("O ajuste atual não retornou a curva da covariável."))
    }

    plot_curva_lag_rr_app(
      rr = fit$tau_media,
      lo = fit$tau_ic_inf,
      hi = fit$tau_ic_sup,
      x_titulo = "Lags da covariável",
      y_titulo = "Risco Relativo da covariável",
      nome_curva = "RR da covariável",
      cor_linha = ic2025_theme_value("desc.violet"),
      cor_faixa = cor_css(ic2025_theme_value("desc.violet"), 0.18),
      media_efetiva = media_efetiva_covar_app(
        df = x$df,
        covar_nome = x$params$covar,
        modelo = "duo",
        lags = x$params$lags,
        lags_covar = x$params$lags_covar %||% x$params$lags
      ),
      media_rotulo = paste0("Média de ", rotulo_var_desc(x$params$covar)),
      sd_efetivo = sd_efetivo_covar_app(
        df = x$df,
        covar_nome = x$params$covar,
        modelo = "duo",
        lags = x$params$lags,
        lags_covar = x$params$lags_covar %||% x$params$lags
      ),
      sd_rotulo = paste0("Desvio Padrão de ", rotulo_var_desc(x$params$covar))
    )
  })

  output$app_x_series <- renderPlotly({
    x <- app_run(); req(!is.null(x))
    if (!isTRUE(x$ready)) {
      return(plot_placeholder(x$mu_msg))
    }
    plot_serie_variavel_app(
      df_serie = x$df,
      coluna = "pm25",
      rotulo_y = rotulo_var_desc("pm25"),
      periodo_ref = x$params$periodo,
      cor_linha = ic2025_theme_value("model.primary")
    )
  })

  output$app_covar_series <- renderPlotly({
    x <- app_run(); req(!is.null(x))
    if (!isTRUE(x$ready)) {
      return(plot_placeholder(x$mu_msg))
    }
    if (!identical(x$modelo, "clima") && !identical(x$modelo, "duo")) {
      return(plot_placeholder("A série da covariável só está disponível para modelos com covariável."))
    }
    covar <- as.character(x$params$covar %||% "")
    if (!nzchar(covar) || !(covar %in% names(x$df))) {
      return(plot_placeholder("A covariável selecionada não está disponível no recorte atual."))
    }
    plot_serie_variavel_app(
      df_serie = x$df,
      coluna = covar,
      rotulo_y = rotulo_var_desc(covar),
      periodo_ref = x$params$periodo,
      cor_linha = ic2025_theme_value("desc.violet")
    )
  })

  eval_metric_labels <- c(
    beta_snr = "Relação Sinal-Ruído dos Betas (estimativa / incerteza)",
    beta_cv = "Coeficiente de Variação (CV)",
    is = "Interval Score (IS)",
    lvp = "Log-Verossimilhança Preditiva (LVP)",
    waic = "Watanabe-Akaike Information Criterion (WAIC)",
    mase = "Erro Médio Absoluto Escalado (MASE)",
    mae = "Erro Médio Absoluto (MAE)",
    epd = "Expected Posterior Deviation (EPD)"
  )
  eval_metric_codes <- names(eval_metric_labels)
  eval_metric_default1 <- "beta_snr"
  eval_metric_none <- "__none__"
  eval_metric_none_label <- "Nenhuma"
  eval_metric_dir <- c(
    waic = "min",
    is = "min",
    lvp = "max",
    mase = "min",
    mae = "min",
    epd = "min",
    beta_snr = "max",
    beta_cv = "min"
  )
  eval_select_state <- reactiveVal(list())

  normalizar_prioridades_metrica <- function(m1, m2, m3) {
    pick_obrigatoria <- function(v, used) {
      if (!is.character(v) || length(v) == 0 || is.na(v) || !(v %in% eval_metric_codes) || (v %in% used)) {
        cand <- unique(c(eval_metric_default1, setdiff(eval_metric_codes, eval_metric_default1)))
        cand <- setdiff(cand, used)
        return(if (length(cand) > 0) cand[[1]] else eval_metric_codes[[1]])
      }
      v
    }
    pick_opcional <- function(v, used) {
      if (!is.character(v) || length(v) == 0 || is.na(v) || identical(v, eval_metric_none)) {
        return(eval_metric_none)
      }
      if (!(v %in% eval_metric_codes) || (v %in% used)) {
        return(eval_metric_none)
      }
      v
    }
    a1 <- pick_obrigatoria(m1, character(0))
    a2 <- pick_opcional(m2, a1)
    used_3 <- c(a1, if (!identical(a2, eval_metric_none)) a2 else character(0))
    a3 <- pick_opcional(m3, used_3)
    c(a1, a2, a3)
  }

  ord_com_tolerancia <- function(v, dir = c("min", "max"), tol_rel = 0.01) {
    dir <- match.arg(dir)
    v <- as.numeric(v)
    ord <- rep(Inf, length(v))
    ok <- is.finite(v)
    if (!any(ok)) return(ord)
    best <- if (identical(dir, "min")) min(v[ok], na.rm = TRUE) else max(v[ok], na.rm = TRUE)
    escala <- max(abs(best), 1)
    passo <- max(tol_rel * escala, 1e-12)
    dist <- if (identical(dir, "min")) (v - best) / passo else (best - v) / passo
    dist[!is.finite(dist)] <- Inf
    dist[dist < 0] <- 0
    ord[ok] <- floor(dist[ok] + 1e-12)
    ord
  }

  contar_inversoes_tendencia_rr <- function(rr, eps = 1e-4) {
    rr <- as.numeric(rr)
    rr <- rr[is.finite(rr) & rr > 0]
    if (length(rr) < 3) return(0L)
    d1 <- diff(log(rr))
    s <- sign(d1)
    s[abs(d1) <= eps] <- 0
    s <- s[s != 0]
    if (length(s) < 2) return(0L)
    as.integer(sum(s[-1] != s[-length(s)]))
  }

  contar_blocos_true <- function(flag) {
    flag <- as.logical(flag)
    if (length(flag) == 0) return(0L)
    r <- rle(flag)
    as.integer(sum(r$values %in% TRUE))
  }

  linha_avaliacao_erro <- function(
      fit_key, lags_i, d_i, fd_i,
      covar = NA_character_, perc = NA_real_, lado = NA_character_, lag_covar = NA_integer_,
      usar_sazonal = FALSE, periodo_sazonal = NA_integer_, ordem_sazonal = NA_integer_, fd_sazonal = NA_real_,
      modelo = "pdldglm",
      tau_rr = NA_real_, tau_lo = NA_real_, tau_hi = NA_real_
  ) {
    tibble(
      fit_key = fit_key,
      modelo = modelo,
      covar = covar,
      perc = perc,
      lado = lado,
      lag_covar = lag_covar,
      usar_sazonal = isTRUE(usar_sazonal),
      periodo_sazonal = periodo_sazonal,
      ordem_sazonal = ordem_sazonal,
      fd_sazonal = fd_sazonal,
      tau_rr = tau_rr,
      tau_lo = tau_lo,
      tau_hi = tau_hi,
      lags = lags_i,
      d = d_i,
      fd = fd_i,
      inversoes_tendencia = NA_integer_,
      passa_filtro_forma = FALSE,
      n_lags_significativos = NA_integer_,
      proporcao_lags_significativos = NA_real_,
      blocos_significativos = NA_integer_,
      passa_filtro_signif = FALSE,
      passa_filtro_minsig = FALSE,
      passa_filtro_pico = FALSE,
      passa_filtro_cauda = FALSE,
      passa_filtro_prop = FALSE,
      waic = NA_real_,
      is = NA_real_,
      lvp = NA_real_,
      mase = NA_real_,
      mae = NA_real_,
      epd = NA_real_,
      beta_snr = NA_real_,
      beta_icv = NA_real_,
      beta_cv = NA_real_
    )
  }

  avaliar_linha_modelo <- function(
      fit_i, fit_key, x_full, lags_i, d_i, fd_i, n_draws_metricas,
      covar = NA_character_, perc = NA_real_, lado = NA_character_, lag_covar = NA_integer_,
      usar_sazonal = FALSE, periodo_sazonal = NA_integer_, ordem_sazonal = NA_integer_, fd_sazonal = NA_real_,
      modelo = "pdldglm",
      tau_rr = NA_real_, tau_lo = NA_real_, tau_hi = NA_real_
  ) {
    mets <- calc_metricas_avaliacao(
      fit = fit_i,
      x_full = x_full,
      lags = lags_i,
      d = d_i,
      n_draws = n_draws_metricas
    )

    rr_med <- as.numeric(fit_i$beta_media)
    rr_lo <- as.numeric(fit_i$beta_ic_inf)
    inv_tend <- contar_inversoes_tendencia_rr(rr_med)
    passa_forma <- is.finite(inv_tend) && (as.integer(inv_tend) <= (as.integer(d_i) - 1L))

    # Regra de cauda/significância: efeito só conta quando IC inteiro > 1.
    # Se ocorrer significativo -> não significativo -> significativo, descarta.
    sig_pos <- is.finite(rr_lo) & (rr_lo > (1 + 1e-8))
    n_sig_pos <- as.integer(sum(sig_pos, na.rm = TRUE))
    n_blocos_sig <- contar_blocos_true(sig_pos)
    passa_filtro_sig <- if (n_sig_pos <= 1L) TRUE else (n_blocos_sig <= 1L)
    # Regra mínima: exige ao menos 1 lag com efeito positivo significativo.
    passa_filtro_minsig <- is.finite(n_sig_pos) && (n_sig_pos >= 1L)

    n_lags_rr <- as.integer(length(rr_med))
    if (!is.finite(n_lags_rr) || n_lags_rr <= 0L) {
      stop("RR vazio para combinacao avaliada")
    }
    # Filtro 1: descarta pico na borda final (últimos 2 lags).
    idx_pico <- if (n_lags_rr > 0L && any(is.finite(rr_med))) {
      suppressWarnings(as.integer(which.max(replace(rr_med, !is.finite(rr_med), -Inf))))
    } else {
      NA_integer_
    }
    passa_filtro_pico <- if (!is.finite(idx_pico) || n_lags_rr < 3L) TRUE else (idx_pico < (n_lags_rr - 1L))

    # Filtro 3: na cauda, precisa haver ao menos um lag não-significativo.
    k_tail <- max(2L, floor(0.25 * n_lags_rr))
    ini_tail <- max(1L, n_lags_rr - k_tail + 1L)
    rr_lo_tail <- rr_lo[seq.int(ini_tail, n_lags_rr)]
    passa_filtro_cauda <- if (length(rr_lo_tail) == 0L) {
      TRUE
    } else {
      any(is.finite(rr_lo_tail) & (rr_lo_tail <= (1 + 1e-8)))
    }

    # Filtro 4 (ajustado): descarta se >=70% dos lags forem significativos.
    prop_sig_pos <- if (n_lags_rr > 0L) n_sig_pos / n_lags_rr else 0
    passa_filtro_prop <- is.finite(prop_sig_pos) && (prop_sig_pos < 0.70)

    tibble(
      fit_key = fit_key,
      modelo = modelo,
      covar = covar,
      perc = perc,
      lado = lado,
      lag_covar = lag_covar,
      usar_sazonal = isTRUE(usar_sazonal),
      periodo_sazonal = periodo_sazonal,
      ordem_sazonal = ordem_sazonal,
      fd_sazonal = fd_sazonal,
      tau_rr = tau_rr,
      tau_lo = tau_lo,
      tau_hi = tau_hi,
      lags = lags_i,
      d = d_i,
      fd = fd_i,
      inversoes_tendencia = as.integer(inv_tend),
      passa_filtro_forma = as.logical(passa_forma),
      n_lags_significativos = n_sig_pos,
      proporcao_lags_significativos = as.numeric(prop_sig_pos),
      blocos_significativos = n_blocos_sig,
      passa_filtro_signif = as.logical(passa_filtro_sig),
      passa_filtro_minsig = as.logical(passa_filtro_minsig),
      passa_filtro_pico = as.logical(passa_filtro_pico),
      passa_filtro_cauda = as.logical(passa_filtro_cauda),
      passa_filtro_prop = as.logical(passa_filtro_prop),
      waic = mets$waic,
      is = mets$is,
      lvp = mets$lvp,
      mase = mets$mase,
      mae = mets$mae,
      epd = mets$epd,
      beta_snr = mets$beta_snr,
      beta_icv = mets$beta_icv,
      beta_cv = mets$beta_cv
    )
  }

  montar_grid_clima <- function(covars_disponiveis = c("temp", "umid")) {
    covs <- unique(as.character(covars_disponiveis))
    covs <- covs[!is.na(covs) & nzchar(trimws(covs))]
    if (length(covs) == 0) return(tibble(covar = character(), perc = numeric(), lado = character(), lag_covar = integer()))

    blocos <- lapply(covs, function(cv) {
      if (identical(cv, "temp")) {
        return(tibble(covar = cv, perc = c(0.10, 0.15), lado = c("abaixo", "abaixo")))
      }
      if (identical(cv, "umid")) {
        return(tibble(covar = cv, perc = c(0.85, 0.90), lado = c("acima", "acima")))
      }
      # Covariáveis sem regra histórica explícita: testa caudas baixa e alta.
      tibble(covar = cv, perc = c(0.15, 0.85), lado = c("abaixo", "acima"))
    })
    base <- bind_rows(blocos)
    if (nrow(base) == 0) return(tibble(covar = character(), perc = numeric(), lado = character(), lag_covar = integer()))

    idx_grid <- expand.grid(idx = seq_len(nrow(base)), lag_covar = 0:5, stringsAsFactors = FALSE)
    tibble(
      covar = base$covar[idx_grid$idx],
      perc = as.numeric(base$perc[idx_grid$idx]),
      lado = as.character(base$lado[idx_grid$idx]),
      lag_covar = as.integer(idx_grid$lag_covar)
    )
  }

  ordens_sazonais_avaliacao <- function(df, periodo_sazonal = EVAL_DEFAULT_SAZONAL_PERIOD) {
    periodo <- suppressWarnings(as.integer(periodo_sazonal))
    if (length(periodo) == 0 || !is.finite(periodo[[1]]) || periodo[[1]] <= 1L) {
      return(1L)
    }
    periodo <- as.integer(periodo[[1]])
    if (!is.data.frame(df) || nrow(df) == 0) {
      return(1L)
    }
    n_ciclos <- floor(nrow(df) / periodo)
    if (!is.finite(n_ciclos) || n_ciclos <= 1L) {
      return(1L)
    }
    seq_len(min(2L, n_ciclos))
  }

  normalizar_ordens_sazonais_avaliacao <- function(x, ordens_disponiveis = 1L) {
    ordens_ok <- suppressWarnings(as.integer(ordens_disponiveis))
    ordens_ok <- sort(unique(ordens_ok[is.finite(ordens_ok) & ordens_ok >= 1L]))
    if (length(ordens_ok) == 0) ordens_ok <- 1L

    ordens_sel <- suppressWarnings(as.integer(x))
    ordens_sel <- sort(unique(ordens_sel[is.finite(ordens_sel) & ordens_sel %in% ordens_ok]))
    if (length(ordens_sel) > 0) return(ordens_sel)

    ordens_def <- ordens_ok[ordens_ok %in% EVAL_DEFAULT_SAZONAL_ORDER]
    if (length(ordens_def) == 0) ordens_def <- ordens_ok[[1]]
    as.integer(ordens_def)
  }

  normalizar_fd_sazonal_grid <- function(x, fallback = EVAL_DEFAULT_SAZONAL_FD_GRID, choices = EVAL_DEFAULT_SAZONAL_FD_CHOICES) {
    fd_ok <- sort(unique(as.numeric(choices)))
    fd_ok <- fd_ok[is.finite(fd_ok) & fd_ok > 0 & fd_ok <= 1]
    if (length(fd_ok) == 0) fd_ok <- c(0.97, 0.98, 0.99, 1.00)

    fd_sel <- sort(unique(as.numeric(x)))
    fd_sel <- fd_sel[is.finite(fd_sel) & fd_sel > 0 & fd_sel <= 1]
    fd_sel <- fd_sel[fd_sel %in% fd_ok]
    if (length(fd_sel) > 0) return(as.numeric(fd_sel))

    fd_def <- sort(unique(as.numeric(fallback)))
    fd_def <- fd_def[is.finite(fd_def) & fd_def > 0 & fd_def <= 1]
    fd_def <- fd_def[fd_def %in% fd_ok]
    if (length(fd_def) > 0) return(as.numeric(fd_def))

    as.numeric(fd_ok)
  }

  montar_grid_sazonal_avaliacao <- function(
      df,
      periodo_sazonal = EVAL_DEFAULT_SAZONAL_PERIOD,
      ordens_sazonais = EVAL_DEFAULT_SAZONAL_ORDER,
      fd_grid = EVAL_DEFAULT_SAZONAL_FD_GRID
  ) {
    ordens_disp <- ordens_sazonais_avaliacao(df, periodo_sazonal = periodo_sazonal)
    ordens_sel <- normalizar_ordens_sazonais_avaliacao(ordens_sazonais, ordens_disponiveis = ordens_disp)
    fd_vals <- normalizar_fd_sazonal_grid(fd_grid)
    expand.grid(
      periodo_sazonal = as.integer(periodo_sazonal),
      ordem_sazonal = as.integer(ordens_sel),
      fd_sazonal = as.numeric(fd_vals),
      stringsAsFactors = FALSE
    ) %>%
      tibble::as_tibble()
  }

  escolher_candidato_clima_tau <- function(tab_cand) {
    if (!is.data.frame(tab_cand) || nrow(tab_cand) == 0) return(tab_cand)
    tab <- tab_cand %>%
      mutate(
        tau_rr_num = suppressWarnings(as.numeric(.data$tau_rr)),
        tau_lo_num = suppressWarnings(as.numeric(.data$tau_lo)),
        tau_hi_num = suppressWarnings(as.numeric(.data$tau_hi)),
        tau_sig = is.finite(.data$tau_lo_num) & (.data$tau_lo_num > (1 + 1e-8)),
        tau_gap = ifelse(is.finite(.data$tau_lo_num), .data$tau_lo_num - 1, -Inf),
        tau_width = ifelse(
          is.finite(.data$tau_hi_num) & is.finite(.data$tau_lo_num),
          pmax(.data$tau_hi_num - .data$tau_lo_num, 0),
          Inf
        ),
        tau_rr_ord = ifelse(is.finite(.data$tau_rr_num), .data$tau_rr_num, -Inf)
      ) %>%
      arrange(
        desc(.data$tau_sig),
        desc(.data$tau_gap),
        desc(.data$tau_rr_ord),
        .data$tau_width,
        .data$lags,
        .data$d,
        desc(.data$fd)
      )
    tab %>% slice_head(n = 1)
  }

  rankear_final_clima_tau <- function(tab_in, pri_in) {
    if (!is.data.frame(tab_in) || nrow(tab_in) == 0) return(tab_in)
    tab_out <- tab_in %>%
      mutate(
        tau_rr_num = suppressWarnings(as.numeric(.data$tau_rr)),
        tau_lo_num = suppressWarnings(as.numeric(.data$tau_lo)),
        tau_hi_num = suppressWarnings(as.numeric(.data$tau_hi)),
        tau_sig = is.finite(.data$tau_lo_num) & (.data$tau_lo_num > (1 + 1e-8)),
        tau_gap = ifelse(is.finite(.data$tau_lo_num), .data$tau_lo_num - 1, -Inf),
        tau_width = ifelse(
          is.finite(.data$tau_hi_num) & is.finite(.data$tau_lo_num),
          pmax(.data$tau_hi_num - .data$tau_lo_num, 0),
          Inf
        ),
        tau_rr_ord = ifelse(is.finite(.data$tau_rr_num), .data$tau_rr_num, -Inf)
      )

    tab_out$ord1 <- 0
    tab_out$ord2 <- 0
    tab_out$ord3 <- 0
    for (k in seq_along(pri_in)) {
      mk <- pri_in[[k]]
      if (identical(mk, eval_metric_none)) {
        tab_out[[paste0("ord", k)]] <- 0
        next
      }
      vv <- tab_out[[mk]]
      tab_out[[paste0("ord", k)]] <- ord_com_tolerancia(vv, dir = eval_metric_dir[[mk]], tol_rel = 0.01)
    }

    tab_out %>%
      arrange(
        desc(.data$tau_sig),
        desc(.data$tau_gap),
        desc(.data$tau_rr_ord),
        .data$tau_width,
        .data$ord1,
        .data$ord2,
        .data$ord3,
        .data$lags,
        .data$d,
        desc(.data$fd)
      )
  }

  calc_metricas_avaliacao <- function(fit, x_full, lags, d, n_draws = 1000L) {
    co <- fit$kdglm_coef
    D <- co$data
    y <- as.numeric(D$Observation)
    mu <- if ("Prediction" %in% names(D)) as.numeric(D$Prediction) else as.numeric(exp(co$lambda.mean[1, ]))

    lim_mu <- 1e6
    explodiu <- any(!is.finite(mu)) || any(mu < 0) || any(mu > lim_mu) || any(!is.finite(y))
    if (explodiu) {
      return(list(
        waic = NA_real_, is = NA_real_,
        lvp = NA_real_, mase = NA_real_, mae = NA_real_, epd = NA_real_,
        beta_snr = NA_real_, beta_icv = NA_real_, beta_cv = NA_real_
      ))
    }

    mae <- mean(abs(y - mu), na.rm = TRUE)
    denom <- mean(abs(diff(y)), na.rm = TRUE)
    mase <- if (is.finite(denom) && denom > 0) mae / denom else NA_real_
    lvp <- sum(stats::dpois(y, lambda = mu, log = TRUE), na.rm = TRUE)
    if (all(c("C.I.lower", "C.I.upper") %in% names(D))) {
      lo <- as.numeric(D$`C.I.lower`)
      hi <- as.numeric(D$`C.I.upper`)
      alpha <- 0.05
      is_vec <- (hi - lo) + ifelse(y < lo, (2 / alpha) * (lo - y), ifelse(y > hi, (2 / alpha) * (y - hi), 0))
      is_sc <- sum(is_vec, na.rm = TRUE)
    } else {
      is_sc <- NA_real_
    }

    if (!is.null(co$ft) && !is.null(co$Qt)) {
      eta_mean <- as.numeric(co$ft[1, ])
      eta_var <- as.numeric(co$Qt[1, 1, ])
    } else {
      eta_mean <- as.numeric(co$lambda.mean[1, ])
      eta_var <- as.numeric(co$lambda.cov[1, 1, ])
    }
    eta_var[!is.finite(eta_var) | eta_var <= 1e-10] <- 1e-10
    n_t <- min(length(y), length(eta_mean), length(eta_var))
    y <- y[seq_len(n_t)]
    eta_mean <- eta_mean[seq_len(n_t)]
    eta_var <- eta_var[seq_len(n_t)]

    n_draws <- as.integer(max(200L, min(5000L, n_draws)))
    eta_draws <- matrix(
      stats::rnorm(n_draws * n_t, mean = rep(eta_mean, each = n_draws), sd = rep(sqrt(eta_var), each = n_draws)),
      nrow = n_draws,
      ncol = n_t
    )
    lambda_draws <- exp(eta_draws)
    y_mat <- matrix(y, nrow = n_draws, ncol = n_t, byrow = TRUE)
    ll_draws <- stats::dpois(y_mat, lambda = lambda_draws, log = TRUE)

    lppd <- sum(log(colMeans(exp(ll_draws), na.rm = TRUE)), na.rm = TRUE)
    p_waic <- sum(apply(ll_draws, 2, stats::var, na.rm = TRUE), na.rm = TRUE)
    waic <- -2 * (lppd - p_waic)

    mu_pred <- colMeans(lambda_draws, na.rm = TRUE)
    var_pred <- apply(lambda_draws, 2, stats::var, na.rm = TRUE) + mu_pred
    epd <- sum((y - mu_pred)^2 + var_pred, na.rm = TRUE)
    # Sinal-Ruído do Beta:
    # |média do efeito| / largura média do IC.
    # Usamos a escala log-RR para efeito e incerteza ficarem comparáveis.
    beta_snr <- NA_real_
    beta_icv <- NA_real_
    beta_cv <- NA_real_
    rr <- as.numeric(fit$beta_media)
    lo <- as.numeric(fit$beta_ic_inf)
    hi <- as.numeric(fit$beta_ic_sup)
    ok <- is.finite(rr) & is.finite(lo) & is.finite(hi) & rr > 0 & lo > 0 & hi > 0 & hi >= lo
    if (any(ok)) {
      efeito_snr <- mean(abs(log(rr[ok])), na.rm = TRUE)
      ruido_snr <- mean(log(hi[ok]) - log(lo[ok]), na.rm = TRUE)
      if (is.finite(efeito_snr) && is.finite(ruido_snr) && ruido_snr > 0) {
        beta_snr <- efeito_snr / ruido_snr
      }

      # CV na escala log-RR para manter unidade consistente com o SNR.
      sd_log_rr <- rep(NA_real_, length(rr))
      if (!is.null(fit$beta_sd)) {
        rr_safe <- pmax(rr, 1e-12)
        sd_log_rr <- pmax(as.numeric(fit$beta_sd), 0) / rr_safe
      }
      ok_icv <- is.finite(rr) & rr > 0 & is.finite(sd_log_rr) & sd_log_rr >= 0
      if (any(ok_icv)) {
        efeito_icv <- mean(abs(log(rr[ok_icv])), na.rm = TRUE)
        ruido_icv <- mean(sd_log_rr[ok_icv], na.rm = TRUE)
        if (is.finite(efeito_icv) && is.finite(ruido_icv) && efeito_icv > 0 && ruido_icv > 0) {
          # NÃO APAGAR: ICV mantido no código para referência futura, sem uso na UI/ranking.
          # beta_icv <- efeito_icv / ruido_icv
          beta_cv <- ruido_icv / efeito_icv
        }
      }
    }

    list(
      waic = waic,
      is = is_sc,
      lvp = lvp,
      mase = mase,
      mae = mae,
      epd = epd,
      beta_snr = beta_snr,
      beta_icv = beta_icv,
      beta_cv = beta_cv
    )
  }

  plot_mu_padrao_app <- function(dfm, periodo_ref = NULL) {
    xaxis_mu <- configurar_xaxis_periodo_modelagem(dfm$Data, periodo_ref = periodo_ref)
    dfm$txt_dados <- sprintf("Data: %s<br>Dados: %.4f", format(as.Date(dfm$Data), "%d/%m/%Y"), dfm$Y)
    dfm$txt_mu <- sprintf(
      "Data: %s<br>Estimativa: %.4f<br>IC 95%%: [%.4f, %.4f]",
      format(as.Date(dfm$Data), "%d/%m/%Y"), dfm$Mu, dfm$Lo, dfm$Hi
    )

    plot_ly(dfm, x = ~Data) %>%
      add_ribbons(
        ymin = ~Lo, ymax = ~Hi, name = "IC 95%",
        fillcolor = ic2025_theme_value("model.primary_band"), line = list(color = ic2025_theme_value("dashboard.transparent")),
        hoverinfo = "skip"
      ) %>%
      add_lines(
        y = ~Y, name = "Dados", line = list(color = ic2025_theme_value("model.black_soft"), width = 1.7),
        text = ~txt_dados, hoverinfo = "text"
      ) %>%
      add_lines(
        y = ~Mu, name = "Estimativas", line = list(color = ic2025_theme_value("model.primary"), width = 2.8),
        text = ~txt_mu, hoverinfo = "text"
      ) %>%
      layout(
        xaxis = xaxis_mu,
        yaxis = modifyList(plotly_layout_base$yaxis, list(title = "Internações")),
        paper_bgcolor = plotly_layout_base$paper_bgcolor,
        plot_bgcolor = plotly_layout_base$plot_bgcolor,
        font = plotly_layout_base$font,
        margin = plotly_layout_base$margin,
        legend = plotly_layout_base$legend
      )
  }

  plot_serie_variavel_app <- function(df_serie, coluna, rotulo_y, periodo_ref = NULL, cor_linha = NULL) {
    req(is.data.frame(df_serie), coluna %in% names(df_serie), "Data" %in% names(df_serie))
    if (is.null(cor_linha)) {
      cor_linha <- ic2025_theme_value("model.black")
    }
    dados <- df_serie %>%
      transmute(
        Data = as.Date(.data$Data),
        valor = suppressWarnings(as.numeric(.data[[coluna]]))
      ) %>%
      filter(is.finite(.data$valor), !is.na(.data$Data))
    req(nrow(dados) > 0)

    media_serie <- mean(dados$valor, na.rm = TRUE)
    sd_serie <- stats::sd(dados$valor, na.rm = TRUE)
    sufixo_stats <- montar_sufixo_hover_stats(
      media_efetiva = media_serie,
      media_rotulo = paste0("Média de ", rotulo_y),
      sd_efetivo = sd_serie,
      sd_rotulo = paste0("Desvio Padrão de ", rotulo_y)
    )

    dados$txt <- sprintf(
      "Data: %s<br>%s: %.4f%s",
      format(dados$Data, "%d/%m/%Y"),
      rotulo_y,
      dados$valor,
      sufixo_stats
    )

    plot_ly(dados, x = ~Data) %>%
      add_lines(
        y = ~valor,
        name = rotulo_y,
        line = list(color = cor_linha, width = 2.3),
        text = ~txt,
        hoverinfo = "text"
      ) %>%
      layout(
        xaxis = configurar_xaxis_periodo_modelagem(dados$Data, periodo_ref = periodo_ref),
        yaxis = modifyList(plotly_layout_base$yaxis, list(title = rotulo_y)),
        paper_bgcolor = plotly_layout_base$paper_bgcolor,
        plot_bgcolor = plotly_layout_base$plot_bgcolor,
        font = plotly_layout_base$font,
        margin = plotly_layout_base$margin,
        legend = list(orientation = "h", x = 0, y = 1.08)
      )
  }

  montar_sufixo_hover_stats <- function(media_efetiva = NA_real_, media_rotulo = NULL, sd_efetivo = NA_real_, sd_rotulo = NULL) {
    linhas <- character(0)
    if (is.finite(media_efetiva) && is.character(media_rotulo) && length(media_rotulo) > 0 && nzchar(media_rotulo[[1]])) {
      linhas <- c(linhas, sprintf("%s: %.4f", media_rotulo[[1]], media_efetiva))
    }
    if (is.finite(sd_efetivo) && is.character(sd_rotulo) && length(sd_rotulo) > 0 && nzchar(sd_rotulo[[1]])) {
      linhas <- c(linhas, sprintf("%s: %.4f", sd_rotulo[[1]], sd_efetivo))
    }
    if (length(linhas) == 0) return("")
    paste0("<br>────────────────────<br>", paste(linhas, collapse = "<br>"))
  }

  estatisticas_efetivas_poluente_app <- function(df, modelo = "pdldglm", lags = 0L, lag_covar = 0L, lags_covar = NULL) {
    if (!is.data.frame(df) || !"pm25" %in% names(df)) return(list(media = NA_real_, sd = NA_real_))
    x <- suppressWarnings(as.numeric(df$pm25))
    x <- x[is.finite(x)]
    if (length(x) == 0) return(list(media = NA_real_, sd = NA_real_))
    modelo <- as.character(modelo %||% "pdldglm")
    modelo <- if (length(modelo) == 0) "pdldglm" else modelo[[1]]
    normalizar_int <- function(x, padrao = 0L) {
      x <- suppressWarnings(as.integer(x))
      if (length(x) == 0 || !is.finite(x[[1]])) return(as.integer(padrao))
      as.integer(x[[1]])
    }

    lags <- normalizar_int(lags, 0L)
    lag_covar <- normalizar_int(lag_covar, 0L)
    lags_covar <- normalizar_int(lags_covar, lags)

    ini <- if (identical(modelo, "duo")) {
      max(lags, lags_covar) + 1L
    } else if (identical(modelo, "clima")) {
      max(lags + 1L, lag_covar + 1L)
    } else {
      lags + 1L
    }
    ini <- max(1L, min(length(x), ini))
    x_eff <- x[ini:length(x)]
    list(
      media = mean(x_eff, na.rm = TRUE),
      sd = stats::sd(x_eff, na.rm = TRUE)
    )
  }

  estatisticas_efetivas_covar_app <- function(df, covar_nome, modelo = "clima", lags = 0L, lag_covar = 0L, lags_covar = NULL) {
    if (!is.data.frame(df)) return(list(media = NA_real_, sd = NA_real_))
    covar_chr <- as.character(covar_nome %||% "")
    if (length(covar_chr) == 0) return(list(media = NA_real_, sd = NA_real_))
    covar_chr <- covar_chr[[1]]
    if (!nzchar(covar_chr) || !(covar_chr %in% names(df))) return(list(media = NA_real_, sd = NA_real_))
    z <- suppressWarnings(as.numeric(df[[covar_chr]]))
    z <- z[is.finite(z)]
    if (length(z) == 0) return(list(media = NA_real_, sd = NA_real_))
    modelo <- as.character(modelo %||% "clima")
    modelo <- if (length(modelo) == 0) "clima" else modelo[[1]]
    normalizar_int <- function(x, padrao = 0L) {
      x <- suppressWarnings(as.integer(x))
      if (length(x) == 0 || !is.finite(x[[1]])) return(as.integer(padrao))
      as.integer(x[[1]])
    }

    lags <- normalizar_int(lags, 0L)
    lag_covar <- normalizar_int(lag_covar, 0L)
    lags_covar <- normalizar_int(lags_covar, lags)

    ini <- if (identical(modelo, "duo")) {
      max(lags, lags_covar) + 1L
    } else {
      max(lags + 1L, lag_covar + 1L)
    }
    ini <- max(1L, min(length(z), ini))
    z_eff <- z[ini:length(z)]
    list(
      media = mean(z_eff, na.rm = TRUE),
      sd = stats::sd(z_eff, na.rm = TRUE)
    )
  }

  sd_efetivo_poluente_app <- function(df, modelo = "pdldglm", lags = 0L, lag_covar = 0L, lags_covar = NULL) {
    estatisticas_efetivas_poluente_app(
      df = df,
      modelo = modelo,
      lags = lags,
      lag_covar = lag_covar,
      lags_covar = lags_covar
    )$sd
  }

  media_efetiva_poluente_app <- function(df, modelo = "pdldglm", lags = 0L, lag_covar = 0L, lags_covar = NULL) {
    estatisticas_efetivas_poluente_app(
      df = df,
      modelo = modelo,
      lags = lags,
      lag_covar = lag_covar,
      lags_covar = lags_covar
    )$media
  }

  sd_efetivo_covar_app <- function(df, covar_nome, modelo = "clima", lags = 0L, lag_covar = 0L, lags_covar = NULL) {
    estatisticas_efetivas_covar_app(
      df = df,
      covar_nome = covar_nome,
      modelo = modelo,
      lags = lags,
      lag_covar = lag_covar,
      lags_covar = lags_covar
    )$sd
  }

  media_efetiva_covar_app <- function(df, covar_nome, modelo = "clima", lags = 0L, lag_covar = 0L, lags_covar = NULL) {
    estatisticas_efetivas_covar_app(
      df = df,
      covar_nome = covar_nome,
      modelo = modelo,
      lags = lags,
      lag_covar = lag_covar,
      lags_covar = lags_covar
    )$media
  }

  plot_curva_lag_rr_app <- function(rr, lo, hi, x_titulo = "Lags", y_titulo = "Risco Relativo", nome_curva = "RR", cor_linha = NULL, cor_faixa = NULL, media_efetiva = NA_real_, media_rotulo = "Média do PM2.5 (\u00b5g/m\u00b3)", sd_efetivo = NA_real_, sd_rotulo = "Desvio Padrão do PM2.5 (\u00b5g/m\u00b3)") {
    if (is.null(cor_linha)) {
      cor_linha <- ic2025_theme_value("model.black")
    }
    if (is.null(cor_faixa)) {
      cor_faixa <- ic2025_theme_value("model.primary_band")
    }
    dfb <- tibble(
      lag = seq_along(rr) - 1L,
      rr = as.numeric(rr),
      lo = as.numeric(lo),
      hi = as.numeric(hi)
    )
    sufixo_stats <- montar_sufixo_hover_stats(
      media_efetiva = media_efetiva,
      media_rotulo = media_rotulo,
      sd_efetivo = sd_efetivo,
      sd_rotulo = sd_rotulo
    )
    dfb$txt_rr <- sprintf("Lag: %s<br>RR: %.4f<br>IC 95%%: [%.4f, %.4f]%s", dfb$lag, dfb$rr, dfb$lo, dfb$hi, sufixo_stats)

    plot_ly(dfb, x = ~lag) %>%
      add_ribbons(
        ymin = ~lo, ymax = ~hi, name = "IC 95%",
        fillcolor = cor_faixa, line = list(color = ic2025_theme_value("dashboard.transparent")),
        hoverinfo = "skip"
      ) %>%
      add_lines(
        y = ~rr, name = nome_curva, line = list(color = cor_linha, width = 2.3),
        text = ~txt_rr, hoverinfo = "text"
      ) %>%
      add_markers(
        y = ~rr, name = nome_curva, marker = list(color = cor_linha, size = 6), showlegend = FALSE,
        text = ~txt_rr, hoverinfo = "text"
      ) %>%
      add_lines(
        y = rep(1, nrow(dfb)), name = "RR=1",
        line = list(color = ic2025_theme_value("model.black_soft"), width = 1.4),
        showlegend = FALSE, hoverinfo = "skip"
      ) %>%
      layout(
        xaxis = modifyList(plotly_layout_base$xaxis, list(title = x_titulo, tickmode = "linear", tick0 = 0, dtick = 1)),
        yaxis = modifyList(plotly_layout_base$yaxis, list(title = y_titulo)),
        paper_bgcolor = plotly_layout_base$paper_bgcolor,
        plot_bgcolor = plotly_layout_base$plot_bgcolor,
        font = plotly_layout_base$font,
        margin = plotly_layout_base$margin,
        legend = plotly_layout_base$legend
      )
  }

  plot_beta_padrao_app <- function(fit_obj, media_efetiva = NA_real_, media_rotulo = "Média do PM2.5 (\u00b5g/m\u00b3)", sd_efetivo = NA_real_, sd_rotulo = "Desvio Padrão do PM2.5 (\u00b5g/m\u00b3)") {
    plot_curva_lag_rr_app(
      rr = fit_obj$beta_media,
      lo = fit_obj$beta_ic_inf,
      hi = fit_obj$beta_ic_sup,
      x_titulo = "Lags",
      y_titulo = "Risco Relativo",
      nome_curva = "RR",
      media_efetiva = media_efetiva,
      media_rotulo = media_rotulo,
      sd_efetivo = sd_efetivo,
      sd_rotulo = sd_rotulo
    )
  }

  plot_sazonal_padrao_app <- function(df_sazonal, periodo_ref = NULL) {
    xaxis_sazonal <- configurar_xaxis_periodo_modelagem(df_sazonal$Data, periodo_ref = periodo_ref)
    df_sazonal$txt_rr <- sprintf(
      "Data: %s<br>Efeito sazonal (RR): %.4f<br>IC 95%%: [%.4f, %.4f]",
      format(as.Date(df_sazonal$Data), "%d/%m/%Y"),
      df_sazonal$RR,
      df_sazonal$Lo,
      df_sazonal$Hi
    )

    plot_ly(df_sazonal, x = ~Data) %>%
      add_ribbons(
        ymin = ~Lo, ymax = ~Hi, name = "IC 95%",
        fillcolor = ic2025_theme_value("model.seasonal_band"), line = list(color = ic2025_theme_value("dashboard.transparent")),
        hoverinfo = "skip"
      ) %>%
      add_lines(
        y = ~RR, name = "Efeito sazonal",
        line = list(color = ic2025_theme_value("model.seasonal_line"), width = 2.7),
        text = ~txt_rr, hoverinfo = "text"
      ) %>%
      add_lines(
        y = rep(1, nrow(df_sazonal)), name = "RR=1",
        line = list(color = ic2025_theme_value("model.black_soft"), width = 1.3),
        showlegend = FALSE, hoverinfo = "skip"
      ) %>%
      layout(
        xaxis = xaxis_sazonal,
        yaxis = modifyList(plotly_layout_base$yaxis, list(title = "Efeito sazonal (RR)")),
        paper_bgcolor = plotly_layout_base$paper_bgcolor,
        plot_bgcolor = plotly_layout_base$plot_bgcolor,
        font = plotly_layout_base$font,
        margin = plotly_layout_base$margin,
        legend = plotly_layout_base$legend
      )
  }

  observeEvent(input$eval_cidade, {
    if (isTRUE(eval_calc_running())) return()
    chave <- input$eval_cidade %||% cidade_default_key
    anos <- anos_disponiveis_cidade(chave)
    req(length(anos) > 0)
    ano_sel <- if (EVAL_DEFAULT_YEAR %in% anos) EVAL_DEFAULT_YEAR else max(anos)
    limites <- periodo_modelagem_limites(anos)
    periodo_sel <- periodo_modelagem_por_ano(ano_sel)
    updateSelectizeInput(session, "eval_ano", choices = as.character(rev(anos)), selected = as.character(ano_sel), server = FALSE)
    updateDateRangeInput(session, "eval_periodo", start = periodo_sel[[1]], end = periodo_sel[[2]], min = limites[[1]], max = limites[[2]])
    if (isTRUE(input$eval_buscar_lags)) {
      updateSliderInput(session, "eval_lags", value = EVAL_DEFAULT_LAG_RANGE)
    } else {
      updateSliderInput(session, "eval_lags", value = EVAL_DEFAULT_LAGS)
    }
  }, ignoreInit = FALSE)

  output$eval_lags_ui <- renderUI({
    if (isTRUE(input$eval_buscar_lags)) {
      val <- suppressWarnings(as.integer(isolate(input$eval_lags)))
      if (length(val) < 2 || any(!is.finite(val))) {
        val <- EVAL_DEFAULT_LAG_RANGE
      } else {
        val <- c(min(val), max(val))
      }
      val[1] <- max(6L, min(16L, val[1]))
      val[2] <- max(val[1], min(16L, val[2]))
      sliderInput("eval_lags", "Intervalo de defasagem (lags)", min = 6, max = 16, value = val, step = 1)
    } else {
      val <- suppressWarnings(as.integer(isolate(input$eval_lags)))
      if (length(val) == 0 || !is.finite(val[1])) val <- EVAL_DEFAULT_LAGS
      val <- max(6L, min(16L, val[1]))
      sliderInput("eval_lags", "Janela de defasagem (lags)", min = 6, max = 16, value = val, step = 1)
    }
  })

  output$eval_sazonal_ui <- renderUI({
    if (!isTRUE(input$eval_buscar_sazonal)) return(NULL)
    cidade <- input$eval_cidade %||% cidade_default_key
    ano <- suppressWarnings(as.integer(input$eval_ano))
    periodo_sel <- normalizar_periodo_modelagem(input$eval_periodo, fallback_ano = ano)
    df_periodo <- carregar_df_modelagem_cache(cidade, periodo_sel[[1]], periodo_sel[[2]])
    periodo_sazonal <- suppressWarnings(as.integer(input$eval_periodo_sazonal %||% EVAL_DEFAULT_SAZONAL_PERIOD))
    if (length(periodo_sazonal) == 0 || !is.finite(periodo_sazonal[[1]]) || periodo_sazonal[[1]] <= 1L) {
      periodo_sazonal <- EVAL_DEFAULT_SAZONAL_PERIOD
    } else {
      periodo_sazonal <- as.integer(periodo_sazonal[[1]])
    }
    ordens_disp <- ordens_sazonais_avaliacao(df_periodo, periodo_sazonal = periodo_sazonal)
    ordens_sel <- normalizar_ordens_sazonais_avaliacao(input$eval_ordem_sazonal, ordens_disponiveis = ordens_disp)
    fd_sazonais_sel <- normalizar_fd_sazonal_grid(input$eval_fd_sazonal)
    fd_choices <- sort(unique(as.numeric(EVAL_DEFAULT_SAZONAL_FD_CHOICES)))
    fd_choices <- fd_choices[is.finite(fd_choices) & fd_choices > 0 & fd_choices <= 1]
    tags$div(
      style = "margin-top:8px; margin-bottom:10px; padding:10px 12px; border:1px solid var(--ic2025-dashboard-surface-line); border-radius:10px; background:var(--ic2025-dashboard-surface-soft);",
      numericInput("eval_periodo_sazonal", "Período sazonal", value = periodo_sazonal, min = 2, step = 1),
      selectizeInput(
        "eval_ordem_sazonal", "Ordens harmônicas",
        choices = stats::setNames(as.character(ordens_disp), as.character(ordens_disp)),
        selected = as.character(ordens_sel),
        multiple = TRUE,
        options = list(
          dropdownParent = "body",
          plugins = list("remove_button")
        )
      ),
      selectizeInput(
        "eval_fd_sazonal", "Fatores de desconto sazonais",
        choices = stats::setNames(sprintf("%.2f", fd_choices), sprintf("%.2f", fd_choices)),
        selected = sprintf("%.2f", fd_sazonais_sel),
        multiple = TRUE,
        options = list(
          dropdownParent = "body",
          plugins = list("remove_button")
        )
      ),
      tags$div(
        style = "font-size:12px; color:var(--ic2025-dashboard-muted); line-height:1.35;",
        paste0(
          "Com o recorte atual (", formatar_periodo_modelagem(periodo_sel[[1]], periodo_sel[[2]]),
          "), a busca vai testar as ordens harmônicas e os fatores de desconto sazonais selecionados acima."
        )
      )
    )
  })

  observeEvent(input$eval_ano, {
    if (isTRUE(eval_calc_running())) return(invisible(NULL))
    chave <- input$eval_cidade %||% cidade_default_key
    anos <- anos_disponiveis_cidade(chave)
    ano_sel <- suppressWarnings(as.integer(input$eval_ano))
    if (length(ano_sel) == 0 || !is.finite(ano_sel[[1]])) return(invisible(NULL))
    periodo_sel <- periodo_modelagem_por_ano(ano_sel[[1]])
    limites <- periodo_modelagem_limites(anos)
    freezeReactiveValue(input, "eval_periodo")
    updateDateRangeInput(session, "eval_periodo", start = periodo_sel[[1]], end = periodo_sel[[2]], min = limites[[1]], max = limites[[2]])
    invisible(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$eval_buscar_lags, {
    if (isTRUE(eval_calc_running())) return()
    if (isTRUE(input$eval_buscar_lags)) {
      updateSliderInput(session, "eval_lags", value = EVAL_DEFAULT_LAG_RANGE)
    } else {
      updateSliderInput(session, "eval_lags", value = EVAL_DEFAULT_LAGS)
    }
  }, ignoreInit = TRUE)

  maybe_update_eval_select <- function(id, choices_vec, selected_val) {
    st <- eval_select_state()
    sig <- list(
      choices_names = names(choices_vec) %||% character(0),
      choices_values = as.character(unname(choices_vec)),
      selected = as.character(selected_val %||% "")
    )
    prev <- st[[id]]
    if (!identical(prev, sig)) {
      updateSelectizeInput(session, id, choices = choices_vec, selected = selected_val, server = FALSE)
      st[[id]] <- sig
      eval_select_state(st)
    }
  }

  observe({
    if (isTRUE(eval_calc_running())) return()
    sel <- normalizar_prioridades_metrica(input$eval_m1, input$eval_m2, input$eval_m3)
    c1 <- setNames(eval_metric_codes, eval_metric_labels[eval_metric_codes])
    maybe_update_eval_select("eval_m1", c1, sel[[1]])

    c2 <- c(sel[[2]], eval_metric_none, setdiff(eval_metric_codes, sel[[1]]))
    c2 <- unique(c2[c2 %in% c(eval_metric_none, eval_metric_codes)])
    c2_named <- setNames(c2, c(`__none__` = eval_metric_none_label, eval_metric_labels)[c2])
    maybe_update_eval_select("eval_m2", c2_named, sel[[2]])

    used_3 <- c(sel[[1]], if (!identical(sel[[2]], eval_metric_none)) sel[[2]] else character(0))
    c3 <- c(sel[[3]], eval_metric_none, setdiff(eval_metric_codes, used_3))
    c3 <- unique(c3[c3 %in% c(eval_metric_none, eval_metric_codes)])
    c3_named <- setNames(c3, c(`__none__` = eval_metric_none_label, eval_metric_labels)[c3])
    maybe_update_eval_select("eval_m3", c3_named, sel[[3]])
  })

  set_eval_progress_inline <- function(done = 0L, total = 1L, label = "Processando...", show = TRUE) {
    session$sendCustomMessage("eval-inline-progress", list(
      done = as.integer(done),
      total = as.integer(total),
      label = as.character(label),
      show = isTRUE(show)
    ))
  }
  set_eval_params_lock <- function(lock = FALSE) {
    session$sendCustomMessage("eval-param-lock", list(lock = isTRUE(lock)))
  }
  eval_calc_running <- reactiveVal(FALSE)
  eval_cancel_requested <- reactiveVal(FALSE)
  eval_view_data <- reactiveVal(NULL)

  request_cancel_eval <- function() {
    if (isTRUE(eval_calc_running())) {
      eval_cancel_requested(TRUE)
    }
  }

  # Navegar entre abas NÃO cancela. Alterar parâmetros de outras abas cancela avaliação em curso.
  observeEvent(
    list(
      input$desc_regiao, input$desc_estado, input$desc_cidade, input$desc_periodo, input$desc_agreg, input$desc_y, input$desc_y2,
      input$sim_tipo, input$sim_modo, input$sim_n_total, input$sim_lags, input$sim_d, input$sim_namostras, input$sim_seed,
      input$sim_x0, input$sim_wx, input$sim_alpha1, input$sim_walpha,
      input$sim_eta_0, input$sim_eta_1, input$sim_eta_2, input$sim_eta_3,
      input$sim_dlm_n, input$sim_dlm_seed, input$sim_dlm_m0, input$sim_dlm_c0, input$sim_dlm_w,
      input$sim_dlm_v_conhecida, input$sim_dlm_v, input$sim_dglm_n, input$sim_dglm_seed, input$sim_dglm_m0,
      input$sim_dglm_c0, input$sim_dglm_w, input$sim_deltas,
      input$app_cidade, input$app_ano, input$app_periodo, input$app_modelo, input$app_lags, input$app_lags_covar, input$app_d, input$app_fd,
      input$app_usar_sazonal, input$app_periodo_sazonal, input$app_ordem_sazonal, input$app_fd_sazonal,
      input$app_covar, input$app_lag_covar, input$app_perc, input$app_show_tau
    ),
    {
      request_cancel_eval()
    },
    ignoreInit = TRUE
  )

  eval_core <- reactive({
    eval_calc_running(TRUE)
    on.exit(eval_calc_running(FALSE), add = TRUE)
    eval_cancel_requested(FALSE)
    set_eval_params_lock(TRUE)
    on.exit(set_eval_params_lock(FALSE), add = TRUE)

    nucleo_boot <- get_nucleo()
    req(nucleo_boot$ok)
    req(isTRUE(deps_ok))
    if (!isTRUE(deps_inited_flag$value)) {
      inicializar_dependencias_pdldglm()
      deps_inited_flag$value <- TRUE
    }

    cidade <- input$eval_cidade %||% cidade_default_key
    ano <- as.integer(input$eval_ano)
    req(is.finite(ano))
    periodo_sel <- normalizar_periodo_modelagem(input$eval_periodo, fallback_ano = ano)
    data_ini <- as.Date(periodo_sel[[1]])
    data_fim <- as.Date(periodo_sel[[2]])
    req(!is.na(data_ini), !is.na(data_fim))
    lags_in <- suppressWarnings(as.integer(input$eval_lags %||% 10L))
    if (length(lags_in) == 0 || !any(is.finite(lags_in))) lags_in <- 10L
    lags <- as.integer(lags_in[[1]])
    req(is.finite(lags), lags >= 6L, lags <= 16L)
    buscar_lags <- isTRUE(input$eval_buscar_lags)
    buscar_clima <- isTRUE(input$eval_buscar_clima)
    buscar_sazonal <- isTRUE(input$eval_buscar_sazonal)
    buscar_clima_solicitado <- buscar_clima
    periodo_sazonal_busca <- suppressWarnings(as.integer(input$eval_periodo_sazonal %||% EVAL_DEFAULT_SAZONAL_PERIOD))
    if (length(periodo_sazonal_busca) == 0 || !is.finite(periodo_sazonal_busca[[1]]) || periodo_sazonal_busca[[1]] <= 1L) {
      periodo_sazonal_busca <- EVAL_DEFAULT_SAZONAL_PERIOD
    } else {
      periodo_sazonal_busca <- as.integer(periodo_sazonal_busca[[1]])
    }
    ordens_sazonais_busca <- normalizar_ordens_sazonais_avaliacao(input$eval_ordem_sazonal, ordens_disponiveis = 1L)
    fd_sazonal_busca <- normalizar_fd_sazonal_grid(input$eval_fd_sazonal)
    if (isTRUE(buscar_lags)) {
      if (length(lags_in) >= 2 && all(is.finite(lags_in[1:2]))) {
        lo <- max(6L, min(16L, min(lags_in[1:2])))
        hi <- max(lo, min(16L, max(lags_in[1:2])))
        lag_grid <- lo:hi
      } else {
        lo <- max(6L, lags - 2L)
        hi <- min(16L, lags + 2L)
        lag_grid <- lo:hi
      }
    } else {
      lag_grid <- lags
    }
    lag_grid <- as.integer(lag_grid[is.finite(lag_grid)])
    lag_grid <- lag_grid[lag_grid >= 6L & lag_grid <= 16L]
    req(length(lag_grid) > 0)
    n_amostras <- as.integer(input$eval_namostras %||% 1000L)
    n_amostras <- max(200L, min(30000L, n_amostras))
    if (isTRUE(buscar_lags)) {
      # Triagem automática de janela: reduz custo computacional.
      n_amostras <- 500L
    }
    n_draws_metricas <- max(200L, min(5000L, n_amostras))

    df <- carregar_df_modelagem_cache(cidade, data_ini, data_fim)
    inf <- info_modelagem_df(df)
    if (!isTRUE(inf$ok)) {
      return(list(
        df = tibble(),
        ano = ano,
        periodo = periodo_sel,
        data_ini = data_ini,
        data_fim = data_fim,
        lags = lags,
        lags_busca = lag_grid,
        buscar_lags = buscar_lags,
        buscar_clima = FALSE,
        buscar_sazonal = FALSE,
        buscar_clima_solicitado = buscar_clima_solicitado,
        periodo_sazonal_busca = periodo_sazonal_busca,
        ordens_sazonais_busca = ordens_sazonais_busca,
        fd_sazonal_busca = fd_sazonal_busca,
        n_amostras = n_amostras,
        total_base = 0L,
        total_sazonal = 0L,
        total_clima = 0L,
        total_combinado = 0L,
        ranking_raw = tibble(),
        unavailable = TRUE,
        unavailable_msg = inf$msg %||% "Sem dados para a cidade/período selecionados.",
        aviso = NULL,
        clima_covars = character(),
        clima_grid = tibble(),
        sazonal_grid = tibble()
      ))
    }
    if (!isTRUE(inf$has_resp) || !isTRUE(inf$has_pm25)) {
      return(list(
        df = df,
        ano = ano,
        periodo = periodo_sel,
        data_ini = data_ini,
        data_fim = data_fim,
        lags = lags,
        lags_busca = lag_grid,
        buscar_lags = buscar_lags,
        buscar_clima = FALSE,
        buscar_sazonal = FALSE,
        buscar_clima_solicitado = buscar_clima_solicitado,
        periodo_sazonal_busca = periodo_sazonal_busca,
        ordens_sazonais_busca = ordens_sazonais_busca,
        fd_sazonal_busca = fd_sazonal_busca,
        n_amostras = n_amostras,
        total_base = 0L,
        total_sazonal = 0L,
        total_clima = 0L,
        total_combinado = 0L,
        ranking_raw = tibble(),
        unavailable = TRUE,
        unavailable_msg = inf$msg,
        aviso = NULL,
        clima_covars = inf$covars %||% character(),
        clima_grid = tibble(),
        sazonal_grid = tibble()
      ))
    }
    df <- df %>% filter(is.finite(Casos_Resp), is.finite(pm25), !is.na(Data))
    if (nrow(df) <= (max(lag_grid) + 20)) {
      return(list(
        df = df,
        ano = ano,
        periodo = periodo_sel,
        data_ini = data_ini,
        data_fim = data_fim,
        lags = lags,
        lags_busca = lag_grid,
        buscar_lags = buscar_lags,
        buscar_clima = FALSE,
        buscar_sazonal = FALSE,
        buscar_clima_solicitado = buscar_clima_solicitado,
        periodo_sazonal_busca = periodo_sazonal_busca,
        ordens_sazonais_busca = ordens_sazonais_busca,
        fd_sazonal_busca = fd_sazonal_busca,
        n_amostras = n_amostras,
        total_base = 0L,
        total_sazonal = 0L,
        total_clima = 0L,
        total_combinado = 0L,
        ranking_raw = tibble(),
        unavailable = TRUE,
        unavailable_msg = paste0("Dados insuficientes após filtros (n=", nrow(df), ")."),
        aviso = NULL,
        clima_covars = inf$covars %||% character(),
        clima_grid = tibble(),
        sazonal_grid = tibble()
      ))
    }

    aviso_eval <- NULL
    clima_covars <- inf$covars %||% character()
    clima_grid <- if (isTRUE(buscar_clima)) montar_grid_clima(clima_covars) else tibble()
    sazonal_grid <- if (isTRUE(buscar_sazonal)) {
      ordens_sazonais_busca <- normalizar_ordens_sazonais_avaliacao(
        input$eval_ordem_sazonal,
        ordens_disponiveis = ordens_sazonais_avaliacao(df, periodo_sazonal = periodo_sazonal_busca)
      )
      fd_sazonal_busca <- normalizar_fd_sazonal_grid(input$eval_fd_sazonal)
      montar_grid_sazonal_avaliacao(
        df,
        periodo_sazonal = periodo_sazonal_busca,
        ordens_sazonais = ordens_sazonais_busca,
        fd_grid = fd_sazonal_busca
      )
    } else {
      tibble()
    }
    if (isTRUE(buscar_clima) && nrow(clima_grid) == 0) {
      buscar_clima <- FALSE
      aviso_eval <- "Busca climática foi desativada: não há covariáveis climáticas disponíveis para esta cidade/período."
    }

    combos <- expand.grid(
      lags = lag_grid,
      d = c(2L, 3L),
      fd = c(0.95, 0.96, 0.97, 0.98, 0.99),
      stringsAsFactors = FALSE
    )
    resultados <- vector("list", nrow(combos))
    total_iter <- nrow(combos)
    total_sazonal_esperado <- if (isTRUE(buscar_sazonal)) as.integer(3L * nrow(sazonal_grid)) else 0L
    total_clima_esperado <- if (isTRUE(buscar_clima)) as.integer(3L * nrow(clima_grid)) else 0L
    total_combinado <- total_iter + total_sazonal_esperado + total_clima_esperado

    fit_fun <- get("PDLDGLM", envir = nucleo_boot$env, inherits = FALSE)

    # Evita flash de mensagem inicial; a barra aparece a partir da 1a combinação testada.
    set_eval_progress_inline(0L, 1L, "", FALSE)
    on.exit(set_eval_progress_inline(0L, 1L, "", FALSE), add = TRUE)

    for (i in seq_len(total_iter)) {
      if (isTRUE(eval_cancel_requested())) {
        stop(structure(list(message = "Avaliação cancelada para priorizar ação em outra aba."), class = c("eval_cancelled", "error", "condition")))
      }
      lags_i <- as.integer(combos$lags[[i]])
      d_i <- as.integer(combos$d[[i]])
      fd_i <- as.numeric(combos$fd[[i]])
      fit_key <- paste(cidade, as.character(data_ini), as.character(data_fim), lags_i, d_i, sprintf("%.3f", fd_i), n_amostras, sep = "|")
      resultados[[i]] <- tryCatch({
        if (exists(fit_key, envir = eval_cache$fit, inherits = FALSE)) {
          fit_i <- get(fit_key, envir = eval_cache$fit, inherits = FALSE)
        } else {
          fit_i <- do.call(fit_fun, list(
            Y = df$Casos_Resp,
            X = df$pm25,
            data = df$Data,
            lags = lags_i,
            d = d_i,
            fd_nivel = fd_i,
            padronizar_center = FALSE,
            padronizar_dp = TRUE,
            n_amostras = n_amostras
          ))
          assign(fit_key, fit_i, envir = eval_cache$fit)
        }
        avaliar_linha_modelo(
          fit_i = fit_i,
          fit_key = fit_key,
          x_full = df$pm25,
          lags_i = lags_i,
          d_i = d_i,
          fd_i = fd_i,
          n_draws_metricas = n_draws_metricas,
          modelo = "pdldglm"
        )
      }, error = function(e) {
        linha_avaliacao_erro(
          fit_key = fit_key,
          lags_i = lags_i,
          d_i = d_i,
          fd_i = fd_i,
          modelo = "pdldglm"
        )
      })

      if (isTRUE(buscar_lags)) {
        lbl_i <- sprintf("Testando janela de lags=%d, d=%d, fd=%.2f", lags_i, d_i, fd_i)
      } else {
        lbl_i <- sprintf("Testando lags=%d, d=%d, fd=%.2f", lags_i, d_i, fd_i)
      }
      set_eval_progress_inline(
        done = i,
        total = total_combinado,
        label = lbl_i,
        show = TRUE
      )
    }
    set_eval_progress_inline(
      done = total_iter,
      total = total_combinado,
      label = "Montando ranking final...",
      show = TRUE
    )

    list(
      df = df,
      ano = ano,
      periodo = periodo_sel,
      data_ini = data_ini,
      data_fim = data_fim,
      lags = lags,
      lags_busca = lag_grid,
      buscar_lags = buscar_lags,
      buscar_clima = buscar_clima,
      buscar_sazonal = buscar_sazonal,
      buscar_clima_solicitado = buscar_clima_solicitado,
      periodo_sazonal_busca = periodo_sazonal_busca,
      ordens_sazonais_busca = ordens_sazonais_busca,
      fd_sazonal_busca = fd_sazonal_busca,
      n_amostras = n_amostras,
      total_base = total_iter,
      total_sazonal = total_sazonal_esperado,
      total_clima = total_clima_esperado,
      total_combinado = total_combinado,
      ranking_raw = bind_rows(resultados),
      unavailable = FALSE,
      unavailable_msg = "",
      aviso = aviso_eval,
      clima_covars = clima_covars,
      clima_grid = clima_grid,
      sazonal_grid = sazonal_grid
    )
  })

  eval_result_vazio <- function() {
    list(
      df = tibble(),
      ano = NA_integer_,
      periodo = c(as.Date(NA), as.Date(NA)),
      data_ini = as.Date(NA),
      data_fim = as.Date(NA),
      lags = NA_integer_,
      lags_busca = integer(),
      buscar_lags = FALSE,
      buscar_clima = FALSE,
      buscar_sazonal = FALSE,
      buscar_clima_solicitado = FALSE,
      periodo_sazonal_busca = NA_integer_,
      ordens_sazonais_busca = integer(),
      fd_sazonal_busca = numeric(),
      n_amostras = NA_integer_,
      total_base = NA_integer_,
      total_sazonal = NA_integer_,
      total_clima = NA_integer_,
      total_combinado = NA_integer_,
      ranking_raw = tibble(),
      unavailable = FALSE,
      unavailable_msg = "",
      aviso = NULL,
      clima_covars = character(),
      clima_grid = tibble(),
      sazonal_grid = tibble(),
      aplicar_filtro_forma = TRUE,
      prioridades = normalizar_prioridades_metrica(input$eval_m1, input$eval_m2, input$eval_m3),
      ranking = tibble(),
      top = tibble()
    )
  }

  processar_eval_run <- function(x) {
    aviso_run <- x$aviso %||% NULL
    if (is.null(x)) {
      prev <- isolate(eval_view_data())
      if (!is.null(prev)) return(prev)
      set_eval_progress_inline(0L, 1L, "", FALSE)
      return(eval_result_vazio())
    }
    pri <- normalizar_prioridades_metrica(input$eval_m1, input$eval_m2, input$eval_m3)
    tab <- x$ranking_raw
    if (isTRUE(x$unavailable)) {
      set_eval_progress_inline(0L, 1L, "", FALSE)
      return(utils::modifyList(
        x,
        list(
          aplicar_filtro_forma = TRUE,
          prioridades = pri,
          ranking = tibble(),
          top = tibble()
        )
      ))
    }
    aplicar_filtro_forma <- TRUE
    filtrar_tab_admissivel <- function(tab_in) {
      tab_in %>%
        filter(
          .data$passa_filtro_forma %in% TRUE,
          .data$passa_filtro_signif %in% TRUE,
          .data$passa_filtro_minsig %in% TRUE,
          .data$passa_filtro_pico %in% TRUE,
          .data$passa_filtro_cauda %in% TRUE,
          .data$passa_filtro_prop %in% TRUE
        )
    }
    tab <- filtrar_tab_admissivel(tab)
    if (nrow(tab) == 0) {
      set_eval_progress_inline(0L, 1L, "", FALSE)
      return(utils::modifyList(
        x,
        list(
          aplicar_filtro_forma = aplicar_filtro_forma,
          prioridades = pri,
          ranking = tab,
          top = tab
        )
      ))
    }
    rankear_tab <- function(tab_in, pri_in) {
      tab_out <- tab_in
      tab_out$ord1 <- 0
      tab_out$ord2 <- 0
      tab_out$ord3 <- 0
      for (k in seq_along(pri_in)) {
        mk <- pri_in[[k]]
        if (identical(mk, eval_metric_none)) {
          tab_out[[paste0("ord", k)]] <- 0
          next
        }
        vv <- tab_out[[mk]]
        tab_out[[paste0("ord", k)]] <- ord_com_tolerancia(vv, dir = eval_metric_dir[[mk]], tol_rel = 0.01)
      }
      tab_out %>%
        arrange(
          .data$ord1,
          .data$ord2,
          .data$ord3,
          .data$lags,
          .data$d,
          desc(.data$fd),
          .data$ordem_sazonal,
          desc(.data$fd_sazonal)
        )
    }

    if (isTRUE(x$buscar_sazonal %||% FALSE) || isTRUE(x$buscar_clima %||% FALSE)) {
      eval_calc_running(TRUE)
      on.exit(eval_calc_running(FALSE), add = TRUE)
      set_eval_params_lock(TRUE)
      on.exit(set_eval_params_lock(FALSE), add = TRUE)
      eval_cancel_requested(FALSE)
    }

    n_draws_metricas <- max(200L, min(5000L, as.integer(x$n_amostras %||% 1000L)))

    if (isTRUE(x$buscar_sazonal %||% FALSE)) {
      nucleo_boot <- get_nucleo()
      if (isTRUE(nucleo_boot$ok) && exists("PDLDGLM_sazonal", envir = nucleo_boot$env, inherits = FALSE)) {
        fit_fun_sazonal <- get("PDLDGLM_sazonal", envir = nucleo_boot$env, inherits = FALSE)
        tab_base <- rankear_tab(tab, pri) %>% slice_head(n = 3L)
        sazonal_grid <- x$sazonal_grid
        if (!is.data.frame(sazonal_grid) || nrow(sazonal_grid) == 0) {
          sazonal_grid <- montar_grid_sazonal_avaliacao(
            x$df,
            periodo_sazonal = x$periodo_sazonal_busca %||% EVAL_DEFAULT_SAZONAL_PERIOD,
            ordens_sazonais = x$ordens_sazonais_busca %||% EVAL_DEFAULT_SAZONAL_ORDER,
            fd_grid = x$fd_sazonal_busca %||% EVAL_DEFAULT_SAZONAL_FD_GRID
          )
        }
        escolhidos <- vector("list", nrow(tab_base))
        total_sazonal <- nrow(tab_base) * nrow(sazonal_grid)
        progresso_sazonal <- 0L
        base_offset <- as.integer(x$total_base %||% 0L)
        total_combinado <- as.integer(x$total_combinado %||% (base_offset + total_sazonal + as.integer(x$total_clima %||% 0L)))
        total_combinado <- max(total_combinado, base_offset + total_sazonal + as.integer(x$total_clima %||% 0L), 1L)
        set_eval_progress_inline(
          done = base_offset,
          total = total_combinado,
          label = "Iniciando busca sazonal...",
          show = TRUE
        )

        for (ii in seq_len(nrow(tab_base))) {
          if (isTRUE(eval_cancel_requested())) break
          rb <- tab_base[ii, , drop = FALSE]
          cache_key <- paste(
            rb$fit_key[[1]],
            "sazonal-auto-v1",
            "n=", as.integer(x$n_amostras %||% 1000L),
            sep = "|"
          )

          if (exists(cache_key, envir = eval_cache$sazonal, inherits = FALSE)) {
            cand <- get(cache_key, envir = eval_cache$sazonal, inherits = FALSE)
          } else {
            lags_i <- as.integer(rb$lags[[1]])
            d_i <- as.integer(rb$d[[1]])
            fd_i <- as.numeric(rb$fd[[1]])
            cand_list <- vector("list", nrow(sazonal_grid))

            for (jj in seq_len(nrow(sazonal_grid))) {
              if (isTRUE(eval_cancel_requested())) break
              periodo_saz_j <- as.integer(sazonal_grid$periodo_sazonal[[jj]])
              ordem_saz_j <- as.integer(sazonal_grid$ordem_sazonal[[jj]])
              fd_saz_j <- as.numeric(sazonal_grid$fd_sazonal[[jj]])
              fit_key_saz <- paste(
                rb$fit_key[[1]],
                "sazonal",
                paste0("P", periodo_saz_j),
                paste0("K", ordem_saz_j),
                sprintf("fdsaz%.3f", fd_saz_j),
                sep = "|"
              )

              cand_list[[jj]] <- tryCatch({
                if (exists(fit_key_saz, envir = eval_cache$fit, inherits = FALSE)) {
                  fit_saz <- get(fit_key_saz, envir = eval_cache$fit, inherits = FALSE)
                } else {
                  fit_saz <- do.call(fit_fun_sazonal, list(
                    Y = x$df$Casos_Resp,
                    X = x$df$pm25,
                    data = x$df$Data,
                    lags = lags_i,
                    d = d_i,
                    fd_nivel = fd_i,
                    padronizar_center = FALSE,
                    padronizar_dp = TRUE,
                    n_amostras = as.integer(x$n_amostras %||% 1000L),
                    periodo_sazonal = periodo_saz_j,
                    ordem_sazonal = ordem_saz_j,
                    fd_sazonal = fd_saz_j
                  ))
                  assign(fit_key_saz, fit_saz, envir = eval_cache$fit)
                }
                avaliar_linha_modelo(
                  fit_i = fit_saz,
                  fit_key = fit_key_saz,
                  x_full = x$df$pm25,
                  lags_i = lags_i,
                  d_i = d_i,
                  fd_i = fd_i,
                  n_draws_metricas = n_draws_metricas,
                  usar_sazonal = TRUE,
                  periodo_sazonal = periodo_saz_j,
                  ordem_sazonal = ordem_saz_j,
                  fd_sazonal = fd_saz_j,
                  modelo = "pdldglm"
                )
              }, error = function(e) {
                linha_avaliacao_erro(
                  fit_key = fit_key_saz,
                  lags_i = lags_i,
                  d_i = d_i,
                  fd_i = fd_i,
                  usar_sazonal = TRUE,
                  periodo_sazonal = periodo_saz_j,
                  ordem_sazonal = ordem_saz_j,
                  fd_sazonal = fd_saz_j,
                  modelo = "pdldglm"
                )
              })
              progresso_sazonal <- progresso_sazonal + 1L
              set_eval_progress_inline(
                done = base_offset + progresso_sazonal,
                total = total_combinado,
                label = sprintf(
                  "Testando sazonal: ordem=%d, fator sazonal=%.2f (base %d/%d)",
                  ordem_saz_j,
                  fd_saz_j,
                  ii,
                  nrow(tab_base)
                ),
                show = TRUE
              )
            }

            cand <- filtrar_tab_admissivel(bind_rows(cand_list))
            assign(cache_key, cand, envir = eval_cache$sazonal)
          }

          if (nrow(cand) == 0) {
            escolhidos[[ii]] <- NULL
          } else {
            escolhidos[[ii]] <- rankear_tab(cand, pri) %>% slice_head(n = 1)
          }
        }
        if (!isTRUE(eval_cancel_requested())) {
          tab <- bind_rows(escolhidos)
          if (nrow(tab) == 0) {
            set_eval_progress_inline(0L, 1L, "", FALSE)
            aviso_run <- "A busca sazonal foi executada, mas nenhum modelo sazonal passou pelos filtros para a configuração atual."
            return(utils::modifyList(
              x,
              list(
                aviso = aviso_run,
                aplicar_filtro_forma = aplicar_filtro_forma,
                prioridades = pri,
                ranking = tibble(),
                top = tibble()
              )
            ))
          }
          set_eval_progress_inline(
            done = base_offset + total_sazonal,
            total = total_combinado,
            label = "Busca sazonal concluída. Preparando próximo refinamento...",
            show = TRUE
          )
        }
      }
    }

    if (isTRUE(x$buscar_clima %||% FALSE)) {
      nucleo_boot <- get_nucleo()
      if (isTRUE(nucleo_boot$ok)) {
        tab_base <- rankear_tab(tab, pri) %>% slice_head(n = 3L)
        clima_grid <- x$clima_grid
        if (!is.data.frame(clima_grid) || nrow(clima_grid) == 0) {
          clima_grid <- montar_grid_clima(x$clima_covars %||% c("temp", "umid"))
        }
        escolhidos <- vector("list", nrow(tab_base))
        total_clima <- nrow(tab_base) * nrow(clima_grid)
        progresso_clima <- 0L
        base_offset <- as.integer(x$total_base %||% 0L) + as.integer(x$total_sazonal %||% 0L)
        total_combinado <- as.integer(x$total_combinado %||% (base_offset + total_clima))
        total_combinado <- max(total_combinado, base_offset + total_clima, 1L)
        set_eval_progress_inline(
          done = base_offset,
          total = total_combinado,
          label = "Iniciando busca climática...",
          show = TRUE
        )

        for (ii in seq_len(nrow(tab_base))) {
          if (isTRUE(eval_cancel_requested())) break
          rb <- tab_base[ii, , drop = FALSE]
          cache_key <- paste(
            rb$fit_key[[1]],
            "clima-auto-v1",
            "n=", as.integer(x$n_amostras %||% 1000L),
            sep = "|"
          )

          if (exists(cache_key, envir = eval_cache$clima, inherits = FALSE)) {
            cand <- get(cache_key, envir = eval_cache$clima, inherits = FALSE)
          } else {
            lags_i <- as.integer(rb$lags[[1]])
            d_i <- as.integer(rb$d[[1]])
            fd_i <- as.numeric(rb$fd[[1]])
            usar_sazonal_rb <- isTRUE(rb$usar_sazonal[[1]] %||% FALSE)
            periodo_sazonal_rb <- suppressWarnings(as.integer(rb$periodo_sazonal[[1]] %||% NA_integer_))
            ordem_sazonal_rb <- suppressWarnings(as.integer(rb$ordem_sazonal[[1]] %||% NA_integer_))
            fd_sazonal_rb <- suppressWarnings(as.numeric(rb$fd_sazonal[[1]] %||% NA_real_))
            fit_fun_clima_nome <- if (isTRUE(usar_sazonal_rb)) "PDLDGLM_clima_sazonal" else "PDLDGLM_clima"
            if (!exists(fit_fun_clima_nome, envir = nucleo_boot$env, inherits = FALSE)) {
              next
            }
            fit_fun_clima <- get(fit_fun_clima_nome, envir = nucleo_boot$env, inherits = FALSE)
            cand_list <- vector("list", nrow(clima_grid))

            for (jj in seq_len(nrow(clima_grid))) {
              if (isTRUE(eval_cancel_requested())) break
              covar_j <- as.character(clima_grid$covar[[jj]])
              perc_j <- as.numeric(clima_grid$perc[[jj]])
              lado_j <- as.character(clima_grid$lado[[jj]])
              lag_cov_j <- as.integer(clima_grid$lag_covar[[jj]])
              fit_key_clima <- paste(
                rb$fit_key[[1]],
                "clima",
                if (isTRUE(usar_sazonal_rb)) {
                  paste0(
                    "P", periodo_sazonal_rb,
                    "_K", ordem_sazonal_rb,
                    "_fdsaz", sprintf("%.3f", fd_sazonal_rb)
                  )
                } else {
                  "sem_sazonal"
                },
                covar_j,
                sprintf("p%.2f", perc_j),
                lado_j,
                paste0("lagcov", lag_cov_j),
                sep = "|"
              )

              cand_list[[jj]] <- tryCatch({
                if (exists(fit_key_clima, envir = eval_cache$fit, inherits = FALSE)) {
                  fit_clima <- get(fit_key_clima, envir = eval_cache$fit, inherits = FALSE)
                } else {
                  df_cov <- x$df %>% filter(is.finite(.data[[covar_j]]))
                  if (nrow(df_cov) <= (lags_i + 20L)) stop("Sem dados suficientes para ajuste com covariável.")
                  args_fit_clima <- list(
                    Y = df_cov$Casos_Resp,
                    X = df_cov$pm25,
                    covar = df_cov[[covar_j]],
                    data = df_cov$Data,
                    lags = lags_i,
                    lag_covar = lag_cov_j,
                    d = d_i,
                    perc = perc_j,
                    perc_sup = NULL,
                    lado = lado_j,
                    fd_nivel = fd_i,
                    padronizar_center = FALSE,
                    padronizar_dp = TRUE,
                    n_amostras = as.integer(x$n_amostras %||% 1000L)
                  )
                  if (isTRUE(usar_sazonal_rb)) {
                    args_fit_clima$periodo_sazonal <- periodo_sazonal_rb
                    args_fit_clima$ordem_sazonal <- ordem_sazonal_rb
                    args_fit_clima$fd_sazonal <- fd_sazonal_rb
                  }
                  fit_clima <- do.call(fit_fun_clima, args_fit_clima)
                  assign(fit_key_clima, fit_clima, envir = eval_cache$fit)
                }
                tau_m <- suppressWarnings(as.numeric(fit_clima$tau_media %||% NA_real_))
                tau_l <- suppressWarnings(as.numeric(fit_clima$tau_ic_inf %||% NA_real_))
                tau_u <- suppressWarnings(as.numeric(fit_clima$tau_ic_sup %||% NA_real_))
                avaliar_linha_modelo(
                  fit_i = fit_clima,
                  fit_key = fit_key_clima,
                  x_full = x$df$pm25,
                  lags_i = lags_i,
                  d_i = d_i,
                  fd_i = fd_i,
                  n_draws_metricas = n_draws_metricas,
                  covar = covar_j,
                  perc = perc_j,
                  lado = lado_j,
                  lag_covar = lag_cov_j,
                  usar_sazonal = usar_sazonal_rb,
                  periodo_sazonal = periodo_sazonal_rb,
                  ordem_sazonal = ordem_sazonal_rb,
                  fd_sazonal = fd_sazonal_rb,
                  modelo = "clima",
                  tau_rr = tau_m,
                  tau_lo = tau_l,
                  tau_hi = tau_u
                )
              }, error = function(e) {
                linha_avaliacao_erro(
                  fit_key = fit_key_clima,
                  lags_i = lags_i,
                  d_i = d_i,
                  fd_i = fd_i,
                  covar = covar_j,
                  perc = perc_j,
                  lado = lado_j,
                  lag_covar = lag_cov_j,
                  usar_sazonal = usar_sazonal_rb,
                  periodo_sazonal = periodo_sazonal_rb,
                  ordem_sazonal = ordem_sazonal_rb,
                  fd_sazonal = fd_sazonal_rb,
                  modelo = "clima"
                )
              })
              progresso_clima <- progresso_clima + 1L
              set_eval_progress_inline(
                done = base_offset + progresso_clima,
                total = total_combinado,
                label = sprintf(
                  "Testando covariável=%s, percentil=%s, ℓ=%d (base %d/%d)",
                  rotulo_var_desc(covar_j),
                  ifelse(identical(lado_j, "abaixo"),
                         paste0("< p", sprintf("%02d", as.integer(round(100 * perc_j)))),
                         paste0("> p", sprintf("%02d", as.integer(round(100 * perc_j))))),
                  lag_cov_j,
                  ii,
                  nrow(tab_base)
                ),
                show = TRUE
              )
            }

            cand <- filtrar_tab_admissivel(bind_rows(cand_list))
            assign(cache_key, cand, envir = eval_cache$clima)
          }

          if (nrow(cand) == 0) {
            escolhidos[[ii]] <- rb
          } else {
            escolhidos[[ii]] <- escolher_candidato_clima_tau(cand)
          }
        }
        if (!isTRUE(eval_cancel_requested())) {
          tab <- bind_rows(escolhidos)
          set_eval_progress_inline(
            done = total_combinado,
            total = total_combinado,
            label = "Montando ranking final...",
            show = TRUE
          )
        }
      }
    }

    if (isTRUE(x$buscar_clima %||% FALSE)) {
      tab <- rankear_final_clima_tau(tab, pri)
    } else {
      tab <- rankear_tab(tab, pri)
    }
    top <- tab %>% slice_head(n = 3) %>% mutate(posicao = row_number())
    set_eval_progress_inline(0L, 1L, "", FALSE)

    utils::modifyList(
      x,
      list(
        aviso = aviso_run,
        aplicar_filtro_forma = aplicar_filtro_forma,
        prioridades = pri,
        ranking = tab,
        top = top
      )
    )
  }

  eval_run_auto <- reactive({
    req(!isTRUE(input$eval_buscar_lags), !isTRUE(input$eval_buscar_clima), !isTRUE(input$eval_buscar_sazonal))
    x <- tryCatch(
      eval_core(),
      eval_cancelled = function(e) NULL
    )
    processar_eval_run(x)
  })

  eval_run_manual <- eventReactive(input$eval_run_button, {
    req(isTRUE(input$eval_buscar_lags) || isTRUE(input$eval_buscar_clima) || isTRUE(input$eval_buscar_sazonal))
    x <- tryCatch(
      eval_core(),
      eval_cancelled = function(e) NULL
    )
    processar_eval_run(x)
  }, ignoreInit = TRUE)

  eval_run <- reactive({
    if (isTRUE(input$eval_buscar_lags) || isTRUE(input$eval_buscar_clima) || isTRUE(input$eval_buscar_sazonal)) {
      x <- eval_run_manual()
      if (is.null(x)) {
        prev <- isolate(eval_view_data())
        if (!is.null(prev)) return(prev)
        return(eval_result_vazio())
      }
      return(x)
    }
    eval_run_auto()
  })

  observeEvent(eval_run(), {
    if (!isTRUE(eval_calc_running())) {
      eval_view_data(isolate(eval_run()))
    }
  }, ignoreInit = FALSE)

  for (i in 1:3) {
    local({
      idx <- i
      output[[paste0("eval_metrics_", idx)]] <- renderDT({
        x <- eval_view_data()
        if (is.null(x)) {
          tb <- tibble(Item = c("Status"), Valor = c("Carregando..."))
          return(DT::datatable(tb, options = list(dom = "t", ordering = FALSE), rownames = FALSE))
        }
        if (!is.list(x)) {
          tb <- tibble(Item = c("Status"), Valor = c("Carregando..."))
          return(DT::datatable(tb, options = list(dom = "t", ordering = FALSE), rownames = FALSE))
        }

        top_tbl <- x$top
        if (!is.data.frame(top_tbl)) top_tbl <- tibble()
        pri <- x$prioridades %||% c(eval_metric_default1, eval_metric_none, eval_metric_none)
        pri <- as.character(pri)
        if (length(pri) < 3) pri <- c(pri, rep(eval_metric_none, 3 - length(pri)))
        pri_show <- pri[pri %in% eval_metric_codes]

        if (nrow(top_tbl) < idx) {
          if (isTRUE(x$unavailable)) {
            tb <- tibble(
              Item = c("Status", "Mensagem"),
              Valor = c("indisponível", x$unavailable_msg %||% "Modelagem indisponível para esta base.")
            )
            return(DT::datatable(tb, options = list(dom = "t", ordering = FALSE), rownames = FALSE))
          }
          if (nzchar(as.character(x$aviso %||% ""))) {
            tb <- tibble(
              Item = c("Status", "Mensagem"),
              Valor = c("sem candidato", as.character(x$aviso))
            )
            return(DT::datatable(tb, options = list(dom = "t", ordering = FALSE), rownames = FALSE))
          }
          tb <- tibble(
            Item = c("Período analisado", "Posição no ranking", "Modelo", "d", "Fator de desconto (fd)", "Lags", "Sazonalidade", "Período sazonal", "Ordem harmônica", "Fator de desconto sazonal", "Covariável climática", "Percentil", "ℓ", "τ (RR, IC 95%)", eval_metric_labels[pri_show]),
            Valor = rep("", 14 + length(pri_show))
          )
          return(DT::datatable(tb, options = list(dom = "t", ordering = FALSE), rownames = FALSE))
        }
        r <- top_tbl[idx, , drop = FALSE]
        vals <- if (length(pri_show) > 0) {
          vapply(pri_show, function(mk) {
            vv <- suppressWarnings(as.numeric(r[[mk]][[1]] %||% NA_real_))
            if (!is.finite(vv)) NA_real_ else vv
          }, numeric(1))
        } else numeric(0)
        fmt <- vapply(vals, function(v) {
          if (!is.finite(v)) return("NA")
          formatC(v, format = "f", digits = 4)
        }, character(1))
        usar_sazonal_rot <- isTRUE(r$usar_sazonal[[1]] %||% FALSE)
        modelo_rot <- dplyr::case_when(
          identical(as.character(r$modelo[[1]] %||% "pdldglm"), "clima") && isTRUE(usar_sazonal_rot) ~ "PDLDGLM + clima + sazonal",
          identical(as.character(r$modelo[[1]] %||% "pdldglm"), "clima") ~ "PDLDGLM + clima",
          isTRUE(usar_sazonal_rot) ~ "PDLDGLM + sazonal",
          TRUE ~ "PDLDGLM"
        )
        sazonal_rot <- if (isTRUE(usar_sazonal_rot)) "Sim" else "Não"
        periodo_sazonal_rot <- if (isTRUE(usar_sazonal_rot) && is.finite(r$periodo_sazonal[[1]] %||% NA_real_)) as.integer(r$periodo_sazonal[[1]]) else "-"
        ordem_sazonal_rot <- if (isTRUE(usar_sazonal_rot) && is.finite(r$ordem_sazonal[[1]] %||% NA_real_)) as.integer(r$ordem_sazonal[[1]]) else "-"
        fd_sazonal_rot <- if (isTRUE(usar_sazonal_rot) && is.finite(r$fd_sazonal[[1]] %||% NA_real_)) {
          formatC(suppressWarnings(as.numeric(r$fd_sazonal[[1]])), format = "f", digits = 2)
        } else {
          "-"
        }
        covar_rot <- if (nzchar(as.character(r$covar[[1]] %||% ""))) rotulo_var_desc(as.character(r$covar[[1]])) else "-"
        perc_rot <- if (is.finite(r$perc[[1]] %||% NA_real_)) {
          paste0(ifelse(identical(r$lado[[1]] %||% "", "abaixo"), "< p", "> p"), sprintf("%02d", as.integer(round(100 * as.numeric(r$perc[[1]])))))
        } else {
          "-"
        }
        ell_rot <- if (is.finite(r$lag_covar[[1]] %||% NA_real_)) as.integer(r$lag_covar[[1]]) else "-"
        tau_rot <- "-"
        if (identical(as.character(r$modelo[[1]] %||% ""), "clima")) {
          fit_key <- r$fit_key[[1]] %||% ""
          if (nzchar(fit_key) && exists(fit_key, envir = eval_cache$fit, inherits = FALSE)) {
            fit_tau <- get(fit_key, envir = eval_cache$fit, inherits = FALSE)
            tau_m <- suppressWarnings(as.numeric(fit_tau$tau_media %||% NA_real_))
            tau_l <- suppressWarnings(as.numeric(fit_tau$tau_ic_inf %||% NA_real_))
            tau_u <- suppressWarnings(as.numeric(fit_tau$tau_ic_sup %||% NA_real_))
            if (is.finite(tau_m) && is.finite(tau_l) && is.finite(tau_u)) {
              tau_rot <- sprintf("%.2f [%.2f ; %.2f]", tau_m, tau_l, tau_u)
            }
          }
        }
        tb <- tibble(
          Item = c("Período analisado", "Posição no ranking", "Modelo", "d", "Fator de desconto (fd)", "Lags", "Sazonalidade", "Período sazonal", "Ordem harmônica", "Fator de desconto sazonal", "Covariável climática", "Percentil", "ℓ", "τ (RR, IC 95%)", eval_metric_labels[pri_show]),
          Valor = as.character(c(
            formatar_periodo_modelagem(x$data_ini, x$data_fim),
            idx,
            modelo_rot,
            suppressWarnings(as.integer(r$d[[1]] %||% NA_integer_)),
            formatC(suppressWarnings(as.numeric(r$fd[[1]] %||% NA_real_)), format = "f", digits = 2),
            suppressWarnings(as.integer(r$lags[[1]] %||% NA_integer_)),
            sazonal_rot,
            periodo_sazonal_rot,
            ordem_sazonal_rot,
            fd_sazonal_rot,
            covar_rot,
            perc_rot,
            ell_rot,
            tau_rot,
            fmt
          ))
        )
        DT::datatable(tb, options = list(dom = "t", ordering = FALSE), rownames = FALSE)
      }, server = FALSE)

      output[[paste0("eval_mu_", idx)]] <- renderPlotly({
        x <- eval_view_data()
        req(!is.null(x))
        if (nrow(x$top) < idx) {
          msg_plot <- if (isTRUE(x$unavailable)) {
            x$unavailable_msg %||% "Modelagem indisponível para esta base."
          } else if (nzchar(as.character(x$aviso %||% ""))) {
            as.character(x$aviso)
          } else if (isFALSE(x$buscar_lags %||% FALSE)) {
            "A informação disponível não foi suficiente para sustentar uma interpretação robusta dos efeitos defasados do poluente.<br>Tente buscar por outras janelas de defasagem."
          } else {
            "A informação disponível não foi suficiente para sustentar uma interpretação robusta dos efeitos defasados do poluente.<br>Você pode tentar inserir uma covariável climática."
          }
          return(
            plot_ly(type = "scatter", mode = "lines") %>%
              layout(
                xaxis = list(visible = FALSE),
                yaxis = list(visible = FALSE),
                annotations = if (nzchar(msg_plot)) list(
                  list(
                    x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                    text = msg_plot, showarrow = FALSE, align = "center",
                    font = list(size = 14, color = ic2025_theme_value("dashboard.muted"))
                  )
                ) else list(),
                paper_bgcolor = ic2025_theme_value("dashboard.surface"),
                plot_bgcolor = ic2025_theme_value("dashboard.surface")
              )
          )
        }
        fit_key <- x$top$fit_key[[idx]]
        fit <- get(fit_key, envir = eval_cache$fit, inherits = FALSE)
        modo <- input$eval_modo %||% "suavizado"

        mu_alt <- NULL
        if (!identical(modo, "suavizado") && !is.null(fit$ajuste1)) {
          lag_sel <- lag_modo_modelagem(modo)
          co <- try(stats::coef(fit$ajuste1, lag = lag_sel, eval.pred = TRUE, eval.metric = TRUE, pred.cred = 0.95), silent = TRUE)
          if (!inherits(co, "try-error")) {
            mu_alt <- extrair_mu_ic_kdglm(co)
          }
        }

        if (!is.null(mu_alt)) {
          y <- tail(x$df$Casos_Resp, nrow(mu_alt))
          dt <- tail(x$df$Data, nrow(mu_alt))
          dfm <- tibble(Data = dt, Y = y, Mu = mu_alt$mu, Lo = mu_alt$lo, Hi = mu_alt$hi)
        } else {
          y <- tail(x$df$Casos_Resp, length(fit$mu_media))
          dt <- tail(x$df$Data, length(fit$mu_media))
          dfm <- tibble(Data = dt, Y = y, Mu = as.numeric(fit$mu_media), Lo = as.numeric(fit$mu_ic_inf), Hi = as.numeric(fit$mu_ic_sup))
        }
        plot_mu_padrao_app(dfm, periodo_ref = x$periodo)
      })

      output[[paste0("eval_beta_", idx)]] <- renderPlotly({
        x <- eval_view_data()
        req(!is.null(x))
        if (nrow(x$top) < idx) {
          msg_plot <- if (isTRUE(x$unavailable)) {
            x$unavailable_msg %||% "Modelagem indisponível para esta base."
          } else if (nzchar(as.character(x$aviso %||% ""))) {
            as.character(x$aviso)
          } else if (isFALSE(x$buscar_lags %||% FALSE)) {
            "A informação disponível não foi suficiente para sustentar uma interpretação robusta dos efeitos defasados do poluente.<br>Tente buscar por outras janelas de defasagem."
          } else {
            "A informação disponível não foi suficiente para sustentar uma interpretação robusta dos efeitos defasados do poluente.<br>Você pode tentar inserir uma covariável climática."
          }
          return(
            plot_ly(type = "scatter", mode = "lines") %>%
              layout(
                xaxis = list(visible = FALSE),
                yaxis = list(visible = FALSE),
                annotations = if (nzchar(msg_plot)) list(
                  list(
                    x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                    text = msg_plot, showarrow = FALSE, align = "center",
                    font = list(size = 14, color = ic2025_theme_value("dashboard.muted"))
                  )
                ) else list(),
                paper_bgcolor = ic2025_theme_value("dashboard.surface"),
                plot_bgcolor = ic2025_theme_value("dashboard.surface")
              )
          )
        }
        fit_key <- x$top$fit_key[[idx]]
        lag_cur <- as.integer(x$top$lags[[idx]])
        lag_covar_cur <- as.integer(x$top$lag_covar[[idx]] %||% 0L)
        lags_covar_cur <- if ("lags_covar" %in% names(x$top)) {
          as.integer(x$top$lags_covar[[idx]] %||% lag_cur)
        } else {
          lag_cur
        }
        modelo_cur <- as.character(x$top$modelo[[idx]] %||% "pdldglm")
        fit <- get(fit_key, envir = eval_cache$fit, inherits = FALSE)
        plot_beta_padrao_app(
          fit,
          media_efetiva = media_efetiva_poluente_app(
            df = x$df,
            modelo = modelo_cur,
            lags = lag_cur,
            lag_covar = lag_covar_cur,
            lags_covar = lags_covar_cur
          ),
          sd_efetivo = sd_efetivo_poluente_app(
            df = x$df,
            modelo = modelo_cur,
            lags = lag_cur,
            lag_covar = lag_covar_cur,
            lags_covar = lags_covar_cur
          )
        )
      })
    })
  }

  
  # output$app_tau <- renderValueBox({
  #   x <- app_run()
  #   if (is.null(x) || is.null(x$fit$tau_media)) {
  #     valueBox("NA", "Tau (clima)", icon = icon("temperature-high"), color = "aqua")
  #   } else {
  #     valueBox(formatar_numero(x$fit$tau_media, 3), "Tau (clima)", icon = icon("temperature-high"), color = "aqua")
  #   }
  # })
  #
  # output$app_obs <- renderValueBox({
  #   x <- app_run()
  #   n <- if (is.null(x)) 0 else nrow(x$df)
  #   valueBox(n, "Obs usadas", icon = icon("database"), color = "teal")
  # })
}

shinyApp(ui, server)
