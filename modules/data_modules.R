# required functions for these modules, call them in main app
source('modules/slider_UI.R') 
# source('data_fun.R')


#' Dataset selection for plot user interface
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' 

dat_choices_pt <- c("EA water quality GCMS data", "EA pollution inventory 2021", "Predatory Bird Monitoring Scheme")

dat_choices_TS <- c('Predatory Bird Monitoring Scheme')

datselect_mod_ui <- function(id, dataset_i, dat_choices = dat_choices_pt) {
  
  ns <- NS(id)
  
  # define choices for dataset selection
  
  
  accordion_panel( paste0('Dataset ',dataset_i ,':'),
                   selectInput(ns("data_choice"), paste0("Choose dataset ",dataset_i, ":"), 
                               choices = dat_choices),
                   uiOutput(ns("ui_placeholder")),
                   uiOutput(ns("dynamic_select")),
                   open=TRUE
                   
  )
}

datselect_mod_server_OLD <-  function(id) {
  moduleServer(id, function(input, output, session) {
    return_value <- reactive({
      input$data_choice
    })
    ns <- session$ns
    
    # conditional UI
    output$ui_placeholder <- renderUI({
      type <- req(input$data_choice)
      
      if (type == "EA pollution inventory 2021") {
        ea_pollution_sliders(id)
      } else if (type == "EA water quality GCMS data") {
        ea_gcms_sliders(id)
      } else if (type =="Predatory Bird Monitoring Scheme")
        pbms_sliders(id)
    })
    result <- data_process_EA_pollution(IndustrySector = input$IndustrySector)
    filtered_data <- result[[1]]
    
    ## if we later want to do some more sophisticated logic
    ## we can add reactives to this list
    list(return_value = return_value, filtered_data = filtered_data)
  })
}




datselect_mod_server <-  function(id) {
  moduleServer(id, function(input, output, session) {
    return_value <- reactive({
      input$data_choice
    })
    ns <- session$ns
    
#    filtered_data <- reactiveVal()
    
    # conditional UI
    output$ui_placeholder <- renderUI({
      type <- req(input$data_choice)
      print(type)
      if (type == "EA pollution inventory 2021") {
        ea_pollution_sliders(id)
      } else if (type == "EA water quality GCMS data") {
        ea_gcms_sliders(id)
      } else if (type =="Predatory Bird Monitoring Scheme")
        pbms_sliders(id)
    })
    
    # alternative: 
    filtered_data <- reactive({
      type <- req(input$data_choice)
      if (length(type) == 0){
        data_process_EA_pollution(IndustrySector = 'Water Industry')[[1]] %>% head() # Hack to return some valid data while it waits for user input
      } else {
          if(type=='EA pollution inventory 2021') {
            data_process_EA_pollution(IndustrySector = input$IndustrySector)[[1]] 
          } else if (type =="Predatory Bird Monitoring Scheme") {
            data_process_pbms(var_biota = input$var_biota, var_sgar_map_sgl=input$var_sgar_map_sgl, var_metals_map_sgl = input$var_metals_map_sgl)[[1]] %>% 
              filter(year >= input$year_slider[1], year <= input$year_slider[2])
          } else {
            data_process_EA_WQ_gcms(CompoundName = input$gcms_compound) %>% 
              filter(year >= input$year_slider[1], year <= input$year_slider[2])
          }
      }
    })
    print(filtered_data)
    
      ## if we later want to do some more sophisticated logic
      ## we can add reactives to this list
      list(return_value = return_value, filtered_data = filtered_data)
    })
  }

DT_mod_server <-  function(id, tbl_data) {
  moduleServer(
    id,
    function(input, output, session) {
      #  ns <- session$ns
      # Render the reactive df `tbl_data` as a table
      output$outputTable  <- renderDT({
        datatable(tbl_data(), options = list(scrollX = TRUE))
      })
    })
}

DT_mod_ui <- function(id, dataset_i) {
  
  ns <- NS(id)
  accordion_panel( paste0('Table for dataset ',dataset_i ,':'),
                   h3("Selected data"),
               DTOutput(ns("outputTable"))
  )
}
# 
# new_id = 'dat_1'
# library(shiny)
# ui <- fluidPage(
#   "Hello, world!",
#   datselect_mod_ui(new_id,'test 1')
# )
# server <- function(input, output, session) {
#   new_handler <- datselect_mod_server(new_id)
# }
# shinyApp(ui, server)

  