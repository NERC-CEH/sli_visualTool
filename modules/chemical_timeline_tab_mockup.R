# =====================================================================
# SLI "Chemical History Timeline" — packaged as a Shiny MODULE
# ---------------------------------------------------------------------
# Use in your app:
#   UI:      chem_timeline_ui("chemtl")        # inside a nav_panel or page
#   server:  chem_timeline_server("chemtl")    # inside your server()
# Or run this file directly to see the standalone demo (bottom of file).
#
# Changes vs. previous version:
#   1. Timeline column and chart column are now 1:1 in width (both 1fr).
#   2. Responsive: on narrow screens each row collapses to a single
#      stacked column via a CSS media query.
#   3. Wrapped as a module so output ids are namespaced (no collisions).
# =====================================================================

library(shiny)
library(bslib)
library(bsicons)
library(plotly)

# ---- colour keys -----------------------------------------------------
ms_colors <- c(
  "Introduction / Approval"  = "#2c6fbb",
  "Change / Update"          = "#7cb342",
  "Restriction / Phase-down" = "#ef8a3c",
  "Ban / Withdrawal"         = "#8e5aa8"
)

use_colors <- c(
  vet   = "#2a9d8f",  # Veterinary medicines (companion animals)
  eng   = "#3a6fb0",  # Prescribed medicines (England)
  wales = "#7b3fa0"   # Prescribed medicines (Wales)
)

# ---- CSS -------------------------------------------------------------
# NOTE: timeline + chart columns are both 1fr  ->  1:1 width at full screen.
#       @media query collapses each row to one stacked column when narrow.
cht_css <- "
.cht-wrap { font-family: inherit; padding: 4px 6px; }

.cht-header { display:flex; justify-content:space-between;
  align-items:flex-start; gap:24px; margin-bottom:6px; flex-wrap:wrap; }
.cht-title { font-size:1.55rem; font-weight:700; color:#1f3a5f; margin:0; }
.cht-subtitle { color:#5b7fa6; margin:2px 0 0; font-size:1rem; }

.cht-legend { display:flex; gap:26px; border:1px solid #d5dde6;
  border-radius:8px; padding:10px 16px; background:#fff; }
.leg-group h6 { font-weight:700; font-size:.8rem; margin:0 0 6px; color:#1f3a5f; }
.leg-item { display:flex; align-items:center; gap:7px; font-size:.8rem; margin-bottom:3px; color:#334; }
.leg-dot  { width:12px; height:12px; border-radius:50%; flex:none; }
.leg-line { width:22px; height:3px; border-radius:2px; flex:none; }

/* shared 3-column grid: label | timeline | chart  (timeline:chart = 1:1) */
.col-heads, .chem-row { display:grid; grid-template-columns:170px 1fr 1fr; gap:12px; }

.col-heads { margin:10px 0 4px; }
.col-badge { background:#12405f; color:#fff; font-weight:700; font-size:.85rem;
  padding:6px 14px; border-radius:6px; display:inline-block; }
.col-badge .sub { font-weight:400; opacity:.85; }

.chem-row { align-items:center; border:1px solid #e2e8f0; border-radius:10px;
  padding:14px 16px; margin-bottom:12px; background:#fbfcfe; }
/* grid children default to min-width:auto, which lets the chart + timeline
   overflow instead of shrinking. min-width:0 lets them resize to fit. */
.chem-row > * { min-width:0; }
.chem-chart .js-plotly-plot, .chem-chart .plotly { width:100% !important; }

.chem-label { text-align:center; padding-right:10px; border-right:1px solid #e2e8f0; }
.chem-icon  { width:58px; height:58px; border-radius:50%; display:flex;
  align-items:center; justify-content:center; color:#fff; font-size:1.7rem; margin:0 auto 8px; }
.chem-name  { font-weight:700; font-size:1.1rem; color:#1f3a5f; }
.chem-sub   { color:#6b7a89; font-size:.8rem; margin-top:2px; }

.tl-track { position:relative; padding:0 12px; }
.tl-line  { position:absolute; left:12px; right:12px; top:33px; height:3px; background:#9aa7b4; }
.tl-line::after { content:''; position:absolute; right:-2px; top:-4px;
  border-left:11px solid #9aa7b4; border-top:6px solid transparent; border-bottom:6px solid transparent; }
.tl-items { display:flex; justify-content:space-between; position:relative; }
.ms { flex:1; text-align:center; padding:0 8px; }
.ms-year { font-weight:700; height:26px; color:#1f3a5f; }
.ms-dot  { width:16px; height:16px; border-radius:50%; border:3px solid;
  margin:0 auto; position:relative; z-index:1; background:#fff; }
.ms-desc { font-size:.78rem; color:#445; margin-top:12px; line-height:1.25; }

.cht-notes { margin-top:8px; color:#6b7a89; font-size:.82rem; }
.cht-notes p { margin:0 0 2px; }

/* ---------- responsive: collapse rows on narrow screens ---------- */
@media (max-width: 900px) {
  .col-heads { display:none; }                 /* badges make no sense stacked */
  .chem-row { grid-template-columns:1fr; }     /* one column, stacked */
  .chem-label { border-right:none; border-bottom:1px solid #e2e8f0;
    padding:0 0 10px; margin-bottom:10px; display:flex; align-items:center;
    gap:12px; text-align:left; }
  .chem-icon { margin:0; }
  .cht-header { flex-direction:column; }

  /* turn the horizontal timeline into a VERTICAL one so the 4 milestones
     stack down the page instead of overflowing sideways */
  .tl-track { padding:6px 0 6px 4px; }
  .tl-line  { top:6px; bottom:6px; left:12px; right:auto; width:3px; height:auto; }
  .tl-line::after {                              /* arrow now points down */
    left:8px; right:auto; top:auto; bottom:-4px;
    border-top:11px solid #9aa7b4; border-left:6px solid transparent;
    border-right:6px solid transparent; border-bottom:none; }
  .tl-items { flex-direction:column; align-items:stretch; gap:18px; }
  .ms { display:grid; grid-template-columns:26px 1fr; column-gap:12px;
        text-align:left; padding:0; }
  .ms-dot  { grid-column:1; grid-row:1; margin:3px 0 0; }
  .ms-year { grid-column:2; grid-row:1; height:auto; }
  .ms-desc { grid-column:2; grid-row:2; margin-top:2px; }
}
"

# ---- UI helpers (namespaced) -----------------------------------------
legend_ui <- function() {
  ms_item <- function(label) {
    div(class = "leg-item",
        span(class = "leg-dot", style = paste0("background:", ms_colors[[label]])), label)
  }
  use_item <- function(color, label) {
    div(class = "leg-item",
        span(class = "leg-line", style = paste0("background:", color)), label)
  }
  div(class = "cht-legend",
      div(class = "leg-group", tags$h6("Regulatory milestone type"),
          lapply(names(ms_colors), ms_item)),
      div(class = "leg-group", tags$h6("Use data categories"),
          use_item(use_colors[["vet"]],   "Veterinary medicines (companion animals)"),
          use_item(use_colors[["eng"]],   "Prescribed medicines (England)"),
          use_item(use_colors[["wales"]], "Prescribed medicines (Wales)"))
  )
}

milestone <- function(m) {
  col <- ms_colors[[m$type]]
  div(class = "ms",
      div(class = "ms-year", m$year),
      div(class = "ms-dot", style = paste0("border-color:", col, "; background:", col)),
      div(class = "ms-desc", m$desc))
}

timeline <- function(milestones) {
  div(class = "tl-track", div(class = "tl-line"),
      div(class = "tl-items", lapply(milestones, milestone)))
}

# `ns` needed so the plotlyOutput id is namespaced inside the module
chem_row <- function(ns, icon, name, subtitle, icon_bg, milestones, plot_id) {
  div(class = "chem-row",
      div(class = "chem-label",
          div(class = "chem-icon", style = paste0("background:", icon_bg), bs_icon(icon)),
          div(class = "chem-name", name),
          div(class = "chem-sub", subtitle)),
      div(class = "chem-timeline", timeline(milestones)),
      div(class = "chem-chart", plotlyOutput(ns(plot_id), height = "230px")))
}

# ---- milestone data --------------------------------------------------
permethrin_ms <- list(
  list(year = "1979", type = "Introduction / Approval",  desc = "First authorised for use in the UK (AVM)"),
  list(year = "1996", type = "Change / Update",          desc = "Biocidal Products Regulation introduced"),
  list(year = "2008", type = "Restriction / Phase-down", desc = "Biocidal action plan: review of pyrethroids"),
  list(year = "2016", type = "Ban / Withdrawal",         desc = "Outdoors use restricted under BPR (EU)")
)
amoxicillin_ms <- list(
  list(year = "1972", type = "Introduction / Approval",  desc = "First authorised for human use in the UK"),
  list(year = "1992", type = "Change / Update",          desc = "Included in the WHO Essential Medicines List"),
  list(year = "2005", type = "Restriction / Phase-down", desc = "AMR awareness and stewardship initiatives expand"),
  list(year = "2019", type = "Ban / Withdrawal",         desc = "UK AMR 20-year vision launched")
)
fipronil_ms <- list(
  list(year = "1996", type = "Introduction / Approval",  desc = "First authorised veterinary use (AVM)"),
  list(year = "2004", type = "Change / Update",          desc = "Additional formulations authorised"),
  list(year = "2013", type = "Restriction / Phase-down", desc = "EMA review on environmental risks"),
  list(year = "2017", type = "Ban / Withdrawal",         desc = "Outdoor uses further restricted (EU BPR)")
)

# ---- plotly helper ---------------------------------------------------
use_plot <- function(years, series, ytitle = "kg") {
  p <- plot_ly()
  for (s in series) {
    p <- add_trace(p, x = years, y = s$y, type = "scatter", mode = "lines+markers",
                   line = list(color = s$color, width = 2.5),
                   marker = list(color = s$color, size = 6), name = s$name)
  }
  p %>%
    layout(showlegend = FALSE,
           yaxis = list(type = "log", title = ytitle, tickformat = ",d", gridcolor = "#eef2f6"),
           xaxis = list(title = "", dtick = 1, gridcolor = "#eef2f6"),
           margin = list(l = 60, r = 12, t = 8, b = 28),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)") %>%
    config(displayModeBar = FALSE)
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
                p(class = "cht-subtitle", "Regulatory milestones and use trends in the UK")),
            legend_ui()),
        HTML('<p align="center" style="font-weight: bold;color:orange">For illustration only.</p>'),
        div(class = "col-heads",
            div(),
            div(span(class = "col-badge", "HISTORY TIMELINE ",
                     span(class = "sub", "(Regulatory milestones)"))),
            div(span(class = "col-badge", "USE OVER TIME ",
                     span(class = "sub", "(Annual sales / prescriptions)")))),
        chem_row(ns, "bug",     "Permethrin",  "Pyrethroid insecticide",         "#3a6fb0", permethrin_ms,  "plot_permethrin"),
        chem_row(ns, "capsule", "Amoxicillin", "Antibiotic (Penicillin)",        "#2a9d8f", amoxicillin_ms, "plot_amoxicillin"),
        chem_row(ns, "bug",     "Fipronil",    "Ectoparasiticide (insecticide)", "#8e5aa8", fipronil_ms,    "plot_fipronil"),
        div(class = "cht-notes",
            p(em("Notes: Milestones refer to key regulatory events in the UK (or EU where applicable).")),
            p(em("Use data: annual quantities (kg) from veterinary medicines sales (companion animals) and prescribed medicines in England and Wales."))))
  )
}

# =====================================================================
# MODULE SERVER
# =====================================================================
chem_timeline_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    yrs <- 2014:2023
    
    output$plot_permethrin <- renderPlotly({
      use_plot(yrs, list(
        list(y = c(8000,11000,18000,32000,52000,35000,28000,15000,11000,7000), color = use_colors[["vet"]],   name = "Veterinary"),
        list(y = c(1700,2100,2500,2900,3000,2400,2000,1500,1200,900),          color = use_colors[["eng"]],   name = "England"),
        list(y = c(300,320,360,400,420,410,380,330,280,230),                   color = use_colors[["wales"]], name = "Wales")
      ), ytitle = "kg")
    })
    
    output$plot_amoxicillin <- renderPlotly({
      use_plot(yrs, list(
        list(y = c(55000,70000,82000,95000,105000,98000,78000,55000,45000,42000), color = use_colors[["eng"]],   name = "England"),
        list(y = c(16000,20000,25000,30000,31000,25000,18000,15000,13000,12000),  color = use_colors[["wales"]], name = "Wales")
      ), ytitle = "kg (prescriptions)")
    })
    
    output$plot_fipronil <- renderPlotly({
      use_plot(yrs, list(
        list(y = c(200,210,480,720,1000,900,600,420,280,130), color = use_colors[["vet"]], name = "Veterinary")
      ), ytitle = "kg")
    })
  })
}

# =====================================================================
# Standalone demo harness (delete when integrating).
# In your real app instead do, inside navset_underline(...):
#     nav_panel("Chemical History Timeline", chem_timeline_ui("chemtl"))
# and inside server():
#     chem_timeline_server("chemtl")
# =====================================================================
if (interactive()) {
  ui <- page_fillable(
    navset_underline(
      nav_panel("Chemical History Timeline", chem_timeline_ui("chemtl"))
    )
  )
  server <- function(input, output, session) {
    chem_timeline_server("chemtl")
  }
  shinyApp(ui, server)
}