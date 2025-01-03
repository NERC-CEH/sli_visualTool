# required functions for these modules, call them in main app
source('modules/slider_UI.R') 
# source('data_fun.R')

library(scales)


#' Dataset selection for plot user interface
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' 

dat_choices_pt <- c("EA water quality GCMS/LCMS data", "EA pollution inventory 2021", 
                    "Predatory Bird Monitoring Scheme", "PFAS", "HadUK-Grid Annual Rainfall", "APIENS",#
                    "EU Soil metals", "UK modelled air pollution emissions", "NAEI air pollution",
                    "UK cats and dogs density")

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
      } else if (type == "EA water quality GCMS/LCMS data") {
        ea_gcms_sliders(id)
      } else if (type =="Predatory Bird Monitoring Scheme") {
        pbms_sliders(id)
      } else if (type == "PFAS") {
        pfas_sliders(id)
      } else if (type == "HadUK-Grid Annual Rainfall") {
        rain_sliders(id)
      }
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
      } else if (type == "EA water quality GCMS/LCMS data") {
        ea_gcms_sliders(id)
      } else if (type == "Predatory Bird Monitoring Scheme") {
        pbms_sliders(id)
      } else if (type == "PFAS") {
        pfas_sliders(id)
      } else if (type == "HadUK-Grid Annual Rainfall") {
        rain_sliders(id)
      } else if (type == "APIENS") {
        apiens_sliders(id)
      } else {
        p('The selected dataset will be added soon.')
      }
    })
    
    
    # # update pbms_sliders selectinput for otters and sparrowhawks
    observeEvent(input$var_biota, {
        if (input$var_biota == 'Otter'){
          updateSelectInput(session, "var_map_sgl", choices = list(`metals` = metals_choices))
        } else if (input$var_biota == 'Sparrowhawk') {
          updateSelectInput(session, "var_map_sgl", choices = list( `SGARs` = SGARs_choices))
        }    
    })
    
    # alternative: 
    filtered_data <- reactive({
      type <- req(input$data_choice)
      print(type)
      if (length(type) == 0){
        data_process_EA_pollution(IndustrySector = 'Water Industry')[[1]] %>% head() # Hack to return some valid data while it waits for user input
      } else {
          if(type=='EA pollution inventory 2021') {
            data_process_EA_pollution(IndustrySector = input$IndustrySector)[[1]] 
          } else if (type =="Predatory Bird Monitoring Scheme") {
            data_process_pbms(var_biota = input$var_biota, 
                              var_map_sgl = input$var_map_sgl)[[1]] %>% 
              filter(year >= input$year_slider[1], year <= input$year_slider[2])
          } else if (type == "PFAS") {
            data_process_pfas(selected_matrix = input$matrix,
                               selected_substance = input$substance,
                               start_year = input$year_slider[1],
                               end_year = input$year_slider[2],
                               transform_method = input$transform)[[1]]
              
          } else if (type == "HadUK-Grid Annual Rainfall") {
            data_process_haduk_rain(year_slider = input$year_slider)
            
            
          } else if (type == "APIENS") {
            data_process_apiens(start_year = input$year_slider[1],
                                end_year = input$year_slider[2],
                                var_choices = input$variable_choices,
                                necd_choices = input$necd_choices)[[1]]
          } else {
            data_process_EA_WQ_gcms(CompoundName = input$gcms_compound) %>% 
               filter(year >= input$year_slider[1], year <= input$year_slider[2])
          }
      }
    })
    print(filtered_data())
    
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

CB_color_cycle = rep(c("darkorange","purple","cyan4",'#377eb8', '#4daf4a',
                       '#f781bf', '#a65628', '#984ea3',
                       '#999999', '#e41a1c', '#dede00'),10)


plot_mod_server <-  function(id, tbl_data) {
  moduleServer(
    id,
    function(input, output, session) {
      suppressWarnings({
        
      
      ns <- session$ns
      output$plot_placeholder <- renderUI({
        tagList(
          bslib::layout_column_wrap(
          selectInput(ns("plot_xvar"), 'Choose variable to plot (x):', 
                      choices = tbl_data() %>% select_if(is.numeric) %>% colnames()),
          selectInput(ns("plot_yvar"), 'Choose variable to plot (y)', 
                      choices = tbl_data() %>% select_if(is.numeric) %>% colnames(),
                      selected = {tbl_data() %>% select_if(is.numeric) %>% colnames()}[2]),
          selectInput(ns("plot_colorvar"), 'Choose variable to plot (colour):', 
                      choices = tbl_data() %>% colnames())
                      #choices = c('REGION','SEX'))
        )
        )
      })
        output$my_plotXY <- renderPlotly({
          validate(
            need(nrow(tbl_data()) > 0, message = FALSE)
          )
          
              data1 = tbl_data()
    

              #DO this: https://stackoverflow.com/questions/63565683/hide-error-message-with-custom-message-or-reactive-button-in-shiny-app
    
              ## plot the common features for all graphs
              p1<-ggplot(data1, aes(x=.data[[input$plot_xvar]], .data[[input$plot_yvar]], color=.data[[input$plot_colorvar]])) + 
                geom_point(size = 0.8) +
                theme_bw() +
                # labs(x=vars_Y[match(input$set_variable_Y, vars_Y)] %>% names(),
                #      y=vars_Y[match(input$set_variable_Y2,vars_Y)] %>% names()) + 
                ggtitle("Scatter plot:") #+
                #scale_y_continuous( breaks=pretty_breaks())
              
              if ( tolower(input$plot_xvar) == 'year') {
                p1 = p1 + scale_x_continuous(labels = label_number(big.mark = ''))
              }
                
              if ( tolower(input$plot_yvar) == 'year') {
                p1 = p1 + scale_y_continuous(labels = label_number(big.mark = ''))
              }
              
              # if( any(class(data1[!!input$plot_colorvar]) %in% c("factor", "character", "logical") )){
              # if( is.numeric(data1[input$plot_colorvar]))  {
              #   print(is.numeric(data1[input$plot_colorvar]))
              #   print(input$plot_colorvar)
              #   p1 <- p1+scale_color_manual(values = CB_color_cycle)
              # } else{
              #   p1 <- p1
              # }
              p1 + theme(aspect.ratio=1) # + coord_fixed(ratio = 1)
            })
          
        })
  })
}

plot_mod_ui <- function(id, dataset_i) {
  ns <- NS(id)
  tagList(
    accordion_panel(
      paste0('Plot for dataset ',dataset_i ,':'),
      uiOutput(ns("plot_placeholder")),
      plotlyOutput(ns("my_plotXY"))
  )
  )
}
# 
# ui <- fluidPage(
#   "Hello, world!",
#   plot_mod_ui('new_id1','test 1')
# )
# server <- function(input, output, session) {
#   #tbl_data <- reactiveVal(list())
#   new_handler <- plot_mod_server('new_id1', sparrowhawk_SGARs )
# }
# shinyApp(ui, server)

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

  