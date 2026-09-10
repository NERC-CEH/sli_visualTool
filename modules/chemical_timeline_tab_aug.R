# =====================================================================
# SLI "Chemical History Timeline" tab  — MODULE, wired to real data
# ---------------------------------------------------------------------
# Data:  timeline_data.RData  -> sales_long (year, Product, Value)
#                                events_long (Chemical, Year, Event, Category, ...)
# Chart: timeline_plotly(product_choice, events_column, product_choice2)
#        from timeline_fun.R  (unchanged, sourced by the host app)
#
# Per your spec:
#   * three chemicals = Fipronil, Imidacloprid, Fluralaner
#   * ONE "Select product" dropdown per chemical (filtered to that chemical)
#   * event-label selector dropped; events_column is hardcoded to "Event"
#     and the dropdown only drives the sales line (single product; the
#     2nd product/axis of timeline_plotly is left empty).
#
# Host app must have run:  source("timeline_fun.R");  load("timeline_data.RData")
# =====================================================================

library(shiny)
library(bslib)
library(bsicons)
library(plotly)

# source("modules/timeline_fun_Aug.R")
# load("modules/timeline_data_Aug26.RData", envir = .GlobalEnv)


# ---- config: which chemicals, in order, with an icon + subtitle ------
CHEMS <- list(
  Fipronil     = list(icon = "bug",     subtitle = "Ectoparasiticide (phenylpyrazole)", icon_bg = "#8e5aa8"),
  Imidacloprid = list(icon = "bug",     subtitle = "Ectoparasiticide (neonicotinoid)",  icon_bg = "#3a6fb0"),
  Fluralaner   = list(icon = "capsule", subtitle = "Ectoparasiticide (isoxazoline)",    icon_bg = "#2a9d8f")
)

chem_list <- unique(as.character(df_chem_data$Compound_Name))

# ---- map event Category -> milestone dot colour ----------------------
cat_colors <- c(
  "Discovery"       = "#2c6fbb",
  "Market entry"    = "#2c6fbb",
  "Approval"        = "#7cb342",
  "Expansion"       = "#7cb342",
  "Regulation"      = "#ef8a3c",
  "Restriction"     = "#ef8a3c",
  "Risk assessment" = "#ef8a3c",
  "Ban"             = "#8e5aa8",
  "Policy end"      = "#8e5aa8"
)
cat_color <- function(x) ifelse(x %in% names(cat_colors), cat_colors[x], "#9aa7b4")

# products belonging to one chemical (prefix before "_", case-insensitive)
products_for <- function(chem) {
  p <- unique(as.character(combined_sales_prescription_data$product))
  p[startsWith(tolower(p), tolower(paste0(chem, "_")))]
}

# helper to extract chemical from product name
chem_from_product <- function(product) {
  sub("_.*$", "", product)   # everything before the first underscore
}

# ---- CSS -------------------------------------------------------------
cht_css <- "
.cht-wrap { font-family: inherit; padding: 4px 6px; }
.cht-header { display:flex; justify-content:space-between; align-items:flex-start;
  gap:24px; margin-bottom:6px; flex-wrap:wrap; }
.cht-title { font-size:1.55rem; font-weight:700; color:#1f3a5f; margin:0; }
.cht-subtitle { color:#5b7fa6; margin:2px 0 0; font-size:1rem; }

.cht-legend { display:flex; gap:26px; border:1px solid #d5dde6; border-radius:8px;
  padding:10px 16px; background:#fff; }
.leg-group h6 { font-weight:700; font-size:.8rem; margin:0 0 6px; color:#1f3a5f; }
.leg-item { display:flex; align-items:center; gap:7px; font-size:.8rem; margin-bottom:3px; color:#334; }
.leg-dot  { width:12px; height:12px; border-radius:50%; flex:none; }

.col-heads, .chem-row { display:grid; grid-template-columns:210px 1fr 1fr; gap:12px; }
.col-heads { margin:10px 0 4px; }
.col-badge { background:#12405f; color:#fff; font-weight:700; font-size:.85rem;
  padding:6px 14px; border-radius:6px; display:inline-block; }
.col-badge .sub { font-weight:400; opacity:.85; }

.chem-row { align-items:center; border:1px solid #e2e8f0; border-radius:10px;
  padding:14px 16px; margin-bottom:12px; background:#fbfcfe; }
.chem-row > * { min-width:0; }               /* let chart + timeline shrink, not overflow */
.chem-chart .js-plotly-plot, .chem-chart .plotly { width:100% !important; }

.chem-label { text-align:center; padding-right:10px; border-right:1px solid #e2e8f0; }
.chem-icon  { width:52px; height:52px; border-radius:50%; display:flex; align-items:center;
  justify-content:center; color:#fff; font-size:1.5rem; margin:0 auto 8px; }
.chem-name  { font-weight:700; font-size:1.05rem; color:#1f3a5f; }
.chem-sub   { color:#6b7a89; font-size:.78rem; margin:2px 0 8px; }
.chem-label .form-group, .chem-label .shiny-input-container { margin-bottom:0; text-align:left; }

.tl-heading { text-align:left; font-weight:700; color:#1f3a5f; font-size:.95rem; margin-bottom:6px; }
.tl-track { position:relative; padding:0 12px; }
.tl-line  { position:absolute; left:12px; right:12px; top:33px; height:3px; background:#9aa7b4; }
.tl-line::after { content:''; position:absolute; right:-2px; top:-4px;
  border-left:11px solid #9aa7b4; border-top:6px solid transparent; border-bottom:6px solid transparent; }
.tl-items { display:flex; justify-content:space-between; position:relative; }
.ms { flex:1; text-align:center; padding:0 6px; }
.ms-year { font-weight:700; height:26px; color:#1f3a5f; font-size:.9rem; }
.ms-dot  { width:15px; height:15px; border-radius:50%; border:3px solid;
  margin:0 auto; position:relative; z-index:1; background:#fff; }
.ms-desc { font-size:.74rem; color:#445; margin-top:10px; line-height:1.2; }

.cht-notes { margin-top:8px; color:#6b7a89; font-size:.82rem; }
.cht-notes p { margin:0 0 2px; }

@media (max-width: 900px) {
  .col-heads { display:none; }
  .chem-row { grid-template-columns:1fr; }
  .chem-label { border-right:none; border-bottom:1px solid #e2e8f0; padding:0 0 12px; margin-bottom:12px; }
  .tl-track { padding:6px 0 6px 4px; }
  .tl-line  { top:6px; bottom:6px; left:12px; right:auto; width:3px; height:auto; }
  .tl-line::after { left:8px; right:auto; top:auto; bottom:-4px;
    border-top:11px solid #9aa7b4; border-left:6px solid transparent;
    border-right:6px solid transparent; border-bottom:none; }
  .tl-items { flex-direction:column; align-items:stretch; gap:18px; }
  .ms { display:grid; grid-template-columns:26px 1fr; column-gap:12px; text-align:left; padding:0; }
  .ms-dot  { grid-column:1; grid-row:1; margin:3px 0 0; }
  .ms-year { grid-column:2; grid-row:1; height:auto; }
  .ms-desc { grid-column:2; grid-row:2; margin-top:2px; }
}
"

# ---- legend (built from the category->colour map) --------------------
legend_ui <- function() {
  seen <- c("Discovery / market entry" = "#2c6fbb",
            "Approval / expansion"      = "#7cb342",
            "Regulation / restriction"  = "#ef8a3c",
            "Ban / policy end"          = "#8e5aa8")
  item <- function(label, col) div(class = "leg-item",
                                   span(class = "leg-dot", style = paste0("background:", col)), label)
  div(class = "cht-legend",
      div(class = "leg-group", tags$h6("Regulatory milestone type"),
          Map(item, names(seen), seen)))
}

# ---- one milestone + the horizontal timeline -------------------------
milestone <- function(year, desc, col) {
  div(class = "ms",
      div(class = "ms-year", year),
      div(class = "ms-dot", style = paste0("border-color:", col, "; background:", col)),
      div(class = "ms-desc", desc))
}
timeline_for <- function(chem) {
  e <- events_long[events_long$Chemical == chem, ]
  e <- e[order(e$Year), ]
  div(class = "tl-track", div(class = "tl-line"),
      div(class = "tl-items",
          lapply(seq_len(nrow(e)), function(i)
            milestone(e$Year[i], e$Event[i], cat_color(e$Category[i])))))
}

# ---- one chemical row (ns for namespacing inside the module) ---------
chem_row <- function(ns, chem, meta) {
  prods <- products_for(chem)
  sel   <- if (any(grepl("_Total$", prods))) grep("_Total$", prods, value = TRUE)[1] else prods[1]
  div(class = "chem-row",
      div(class = "chem-label",
          div(class = "chem-icon", style = paste0("background:", meta$icon_bg), bs_icon(meta$icon)),
          div(class = "chem-name", chem),
          div(class = "chem-sub", meta$subtitle),
          selectInput(ns(paste0("prod_", chem)), "Select product:",
                      choices = prods, selected = sel, width = "100%"),
          selectInput(ns(paste0("chem_", chem)), "Select chemical:",
                      choices = chem_list, selected = chem, width = "100%")),
      div(class = "chem-timeline", timeline_for(chem)),
      div(class = "chem-chart", plotlyOutput(ns(paste0("plot_", chem)), height = "260px")))
}


flex_row_ui <- function(ns) {
  all_products <- sort(unique(as.character(combined_sales_prescription_data$product)))
  div(class = "chem-row",
      div(class = "chem-label",
          div(class = "chem-icon", style = "background:#6b7a89", bs_icon("graph-up")),
          div(class = "chem-name", "Custom"),
          div(class = "chem-sub", "Any product"),
          selectInput(ns("flex_product"), "Select product:",
                      choices = all_products, selected = all_products[1], width = "100%"),
          # selectInput(ns("flex_chem"), "Select chemical:",
          #             choices = chem_list, selected = chem_list[1], width = "100%")
          ),
      div(class = "chem-timeline", uiOutput(ns("flex_timeline"))),
      div(class = "chem-chart", plotlyOutput(ns("plot_flex"), height = "260px")))
}


# =====================================================================
# MODULE UI
# =====================================================================
chem_timeline_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(cht_css)),
    div(class = "cht-wrap",
        div(class = "cht-header",
            div(h2(class = "cht-title", "SLI Timeline: Regulatory History and Use of Selected Chemicals"),
                p(class = "cht-subtitle", "Companion-animal ectoparasiticides \u2014 regulatory milestones and use trends")),
            legend_ui()),
        div(class = "col-heads",
            div(),
            div(span(class = "col-badge", "HISTORY TIMELINE ",
                     span(class = "sub", "(Regulatory milestones)"))),
            div(span(class = "col-badge", "USE OVER TIME ",
                     span(class = "sub", "(Annual sales, kg)")))),
        #lapply(names(CHEMS), function(chem) chem_row(ns, chem, CHEMS[[chem]])),
        lapply(setdiff(names(CHEMS), "Fluralaner"),
               function(chem) chem_row(ns, chem, CHEMS[[chem]])),
        flex_row_ui(ns),
        div(class = "cht-notes",
            p(em("History timeline: key regulatory events (source: events dataset).")),
            p(em("Use over time: annual sales (kg) by product form; shaded bands mark regulatory events for the selected chemical."))))
  )
}

# =====================================================================
# MODULE SERVER  — one renderPlotly per chemical, using timeline_plotly()
# =====================================================================
# chem_timeline_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     lapply(names(CHEMS), function(chem) {
#       output[[paste0("plot_", chem)]] <- renderPlotly({
#         prod <- input[[paste0("prod_", chem)]]
#         chem_pick <- input[[paste0("chem_", chem)]]
#         req(prod, chem_pick)
#         # single product -> events_column hardcoded to "Event"; 2nd product empty
#         timeline_plotly(product_choice = prod, chem_choice = chem_pick)
#       })
#     })
#   })
# }


chem_timeline_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    lapply(setdiff(names(CHEMS), "Fluralaner"), function(chem) {
      output[[paste0("plot_", chem)]] <- renderPlotly({
        prod      <- input[[paste0("prod_", chem)]]
        chem_pick <- input[[paste0("chem_", chem)]]
        req(prod, chem_pick)
        timeline_plotly(product_choice = prod, chem_choice = chem_pick)
      })
    })
    
    output$plot_flex <- renderPlotly({
      req(input$flex_product, input$flex_chem)
      timeline_plotly(product_choice = input$flex_product, chem_choice = "")
    })
    
    
    output$flex_timeline <- renderUI({
      req(input$flex_product)
      chem_guess <- chem_from_product(input$flex_product)
      
      if (!chem_guess %in% unique(events_long$Chemical)) {
        return(div(class = "tl-track",
                   p(style = "color:#6b7a89; font-style:italic; padding:20px 0;",
                     "No events timeline available for this product")))
      }
      
      tagList(
        div(class = "tl-heading", style = "text-align:left; font-weight:700;", chem_guess),
        timeline_for(chem_guess)
      )
      
    })
    
  })
}

# =====================================================================
# Standalone demo harness (delete when integrating).
# =====================================================================
if (interactive()) {
#  source("timeline_fun.R")
#  load("timeline_data.RData", envir = .GlobalEnv)
  
  ui <- page_fillable(
    navset_underline(
      nav_panel("Chemical History Timeline", chem_timeline_ui("chemtl"))
    )
  )
  server <- function(input, output, session) chem_timeline_server("chemtl")
  shinyApp(ui, server)
}