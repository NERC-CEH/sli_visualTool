library(shiny)
library(plotly)
library(lubridate)
source("timeline_fun_Aug.R")
load("timeline_data_Aug26.RData", envir = .GlobalEnv)

# # new stuff to save in .Rdata - how to add it to it
# source("old/events_data_tidying_290726.R")
# source("old/sales_data_tidying_290726.R")
# source("old/prescription_data_tidying_290726.R")


ui <- fluidPage(

  selectInput(
    "product_choice",
    "Select Product:",
    choices = unique(combined_sales_prescription_data$product),
    selected = "Fipronil_Pipettes_sprays_sales_UK"
  ),

  selectInput(
    "chem_choice",
    "Select Chemical:",
    choices = unique(df_chem_data$Compound_Name), # need to change this to chemical data
    selected = unique(df_chem_data$Compound_Name)[1]
  ),

  # selectInput(
  #   "chemical",
  #   "Select events relevant to this product:",
  #   choices = unique(events_long$Chemical)
  # ),



  plotlyOutput("timeline_plot")

)

server <- function(input, output, session) {

  output$timeline_plot = renderPlotly(timeline_plotly(input$product_choice, chem_choice = input$chem_choice))

}

options(shiny.fullstacktrace = TRUE)

shinyApp(ui, server)



