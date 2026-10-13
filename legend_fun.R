# Legend helpers for point data with values below the limit of detection (LOD)
# and missing values. Reusable across datasets (Lough Neagh, EA GCMS, NI, ...).
#
# Requires: leaflet, leaflegend, RColorBrewer, htmltools
# Source this before (or inside) map_fun.R.

# Shared colours, so markers and legends always match.
# In map_fun_LoughNeagh() use: lod_color = LOD_COLOR, na_color = NA_COLOR
LOD_COLOR <- "#FFFFFF"
NA_COLOR  <- "#9E9E9E"


# ---- 1. Classify values ------------------------------------------------------
# "0.5" -> detected, "<0.5" (or Q containing "<") -> below_lod, NA -> missing
classify_lod_values <- function(value, q = NULL) {
  value_chr <- trimws(as.character(value))
  below_lod <- !is.na(value_chr) & grepl("^<", value_chr)
  if (!is.null(q)) below_lod <- below_lod | (!is.na(q) & grepl("<", q))
  
  num <- suppressWarnings(as.numeric(gsub("[<> ]", "", value_chr)))
  
  status <- ifelse(below_lod, "below_lod",
                   ifelse(is.na(num), "missing", "detected"))
  
  data.frame(num = num, status = status, stringsAsFactors = FALSE)
}


# ---- 2. Palette from detected values only -----------------------------------
# Returns list(pal, values, use_log), or NULL if no value can be coloured.
make_detected_palette <- function(value, q = NULL,
                                  palette_name = "Blues",
                                  log_scale    = FALSE,
                                  na_color     = NA_COLOR) {
  cls <- classify_lod_values(value, q)
  det <- cls$status == "detected"
  
  use_log <- log_scale && any(det) && all(cls$num[det] > 0)
  vals    <- if (use_log) log10(cls$num[det]) else cls$num[det]
  vals    <- vals[is.finite(vals)]
  if (length(vals) == 0) return(NULL)
  
  rng <- range(vals)
  if (diff(rng) == 0) rng <- rng + c(-1, 1) * max(abs(rng), 1) * 0.05  # pad single value
  
  list(pal     = colorNumeric(RColorBrewer::brewer.pal(9, palette_name),
                              domain = rng, na.color = na_color),
       values  = rng,
       use_log = use_log)
}


# ---- 3. Numeric legend for detected values ----------------------------------
# pal_info: output of make_detected_palette(). Does nothing if NULL.
add_detected_legend <- function(map, pal_info,
                                title    = "",
                                subtitle = "",          # e.g. "Concentration µg/l"
                                position = "bottomright",
                                group    = NULL,
                                digits   = 3,
                                width    = 200,
                                height   = 10) {
  if (is.null(pal_info)) return(map)
  
  fmt <- if (pal_info$use_log) {
    function(x) format(signif(10^x, digits), trim = TRUE, scientific = FALSE)
  } else {
    function(x) format(signif(x, digits), trim = TRUE, scientific = FALSE)
  }
  
  full_title <- if (nzchar(subtitle)) paste0(title, "<br>", subtitle) else title
  
  leaflegend::addLegendNumeric(
    map,
    position     = position,
    pal          = pal_info$pal,
    values       = pal_info$values,
    title        = htmltools::HTML(full_title),
    shape        = "rect",
    orientation  = "horizontal",
    width        = width,
    height       = height,
    numberFormat = fmt,
    group        = group
  )
}


# ---- 4. Legend for below-LOD and missing classes ----------------------------
# status: the status column from classify_lod_values().
# Only classes present in the data are shown, unless show_always = TRUE.
add_lod_na_legend <- function(map, status = NULL, data = NULL,
                              value_col = "Value", q_col = "Q",
                              title       = NULL,
                              position    = "bottomright",
                              group       = NULL,
                              lod_label   = "Below LOD",
                              na_label    = "No data",
                              lod_color   = LOD_COLOR,
                              na_color    = NA_COLOR,
                              shape       = c("circle", "square"),
                              show_always = FALSE) {
  
  if (is.null(status)) {
    if (is.null(data)) stop("add_lod_na_legend(): supply either `status` or `data`")
    q      <- if (q_col %in% names(data)) data[[q_col]] else NULL
    status <- classify_lod_values(data[[value_col]], q)$status
  }
  
  shape <- match.arg(shape)
  
  present <- c(any(status == "below_lod", na.rm = TRUE),
               any(status == "missing",   na.rm = TRUE))
  if (show_always) present[] <- TRUE
  if (!any(present)) return(map)
  
  # leaflet inserts the colour into the swatch's CSS, so extra styles can be appended
  radius <- if (shape == "circle") " border-radius: 50%;" else ""
  swatch <- function(col) paste0(col, "; border: 1px solid #333;", radius)
  
  leaflet::addLegend(
    map,
    position = position,
    colors   = c(swatch(lod_color), swatch(na_color))[present],
    labels   = c(lod_label, na_label)[present],
    title    = title,
    opacity  = 1,
    group    = group
  )
}