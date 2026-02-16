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

# note: if changing any of the dat choices, make sure you change them elsewhere too (app.R map switch, and output$ui_placeholder below)
dat_choices_pt <- c("EA water quality GCMS/LCMS data", "EA pollution inventory 2021", 
                    "Predatory Bird Monitoring Scheme", "PFAS", "HadUK-Grid Annual Rainfall", "APIENS",#
                    "EU Soil metals", "UK modelled air pollution emissions", "NAEI air pollution",
                    "UK cats and dogs density", "AgZero+ Input to Yield Ratio (IYR)", "Custom file upload (.csv)")

dat_choices_TS <- c('Predatory Bird Monitoring Scheme')

csv_upload_mod_server <- function(id) {
  # module for CSV file upload
  moduleServer(id, function(input, output, session) {
    
    data <- reactive({
      req(input$csv_filepath)  # Wait until file is uploaded
      read.csv(input$csv_filepath$datapath)
    })
    
    print(data())
    
    output$table <- renderTable({
      req(data())  # Only render if data is available
      data()
    })
    
    # # update csv_upload_sliders to display column choices
    
    csv_colnames <- data %>% colnames() 
    
    observeEvent(input$csv_filepath, {
      updateSelectInput(session, "lat_col", choices = csv_colnames)
      updateSelectInput(session, "long_col", choices = csv_colnames)
    })
    
    # Optionally return the reactive data for use elsewhere
    return(list(data=data, long_col=input$long_col, lat_col=input$lat_col))
  })
}

datselect_mod_ui <- function(id, dataset_i, dat_choices = dat_choices_pt) {
  
  ns <- NS(id)
  
  # define choices for dataset selection
  
  
  accordion_panel( paste0('Dataset ',dataset_i ,':'),
                   icon = bsicons::bs_icon('clipboard-data'),
                   selectInput(ns("data_choice"), 
                               label = tooltip(
                                 trigger = list(
                                   paste0("Choose dataset ",dataset_i, ":"),
                                   bs_icon("info-circle")
                                 ),
                                 "Choose a dataset. Your options below will change accordingly."
                               ),
                               choices = dat_choices),
                   uiOutput(ns("ui_placeholder")),
                   uiOutput(ns("dynamic_select")) 
                   
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
    print(input$data_choice)
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
      } else if (type == 'EU Soil metals') {
        euso_sliders(id)
      } else if (type == 'UK cats and dogs density') {
        cats_dogs_sliders(id)
      } else if (type == 'AgZero+ Input to Yield Ratio (IYR)') {
        IYR_sliders(id)
      } else if (type == 'Custom file upload (.csv)') {
        csv_upload_sliders(id)
      } else {
        p('The selected dataset will be added soon.')
      }
    })
    
    
    #### add reactive electives to individual datasets in the data module.
    
    # # update pbms_sliders selectinput for otters and sparrowhawks
    observeEvent(input$var_biota, {
      if (input$var_biota == 'Otter'){
        updateSelectInput(session, "var_map_sgl", choices = list(`metals` = metals_choices))
      } else if (input$var_biota == 'Sparrowhawk') {
        updateSelectInput(session, "var_map_sgl", choices = list( `SGARs` = SGARs_choices))
      } else if (input$var_biota == 'Buzzard') {
        updateSelectInput(session, "var_map_sgl", choices = list(`metals` = metals_choices, `SGARs` = SGARs_choices))
      }    
    })
    
    
    # # update ea_gcms_sliders to display chemical info
    
    ref_gcms<- data_process_chemref() # put in the main app to only load once?
    
    observeEvent(input$gcms_compound, {
      req(input$gcms_compound %in% ref_gcms$Compound_Name)
      output$chem_info <- renderTable(
        
        ref_gcms %>% filter(Compound_Name == input$gcms_compound) %>% 
          dplyr::select(method, USE, LOD,`Lowest PNEC Freshwater [µg//l]`) %>% t()
        ,
        rownames = TRUE,
        colnames = FALSE,
      )
    })

    # legend_data <- reactive({
    #   type <- req(input$data_choice)
    #   if (type == "EA pollution inventory 2021") {
    #      paste(input$IndustrySector)
    #     
    #   # } else if (type == "EA water quality GCMS/LCMS data") {
    #     # legend_choices <- "Dummy Legend gcms" #paste(input$gcms_compound)
    #     
    #     # } else if (type == "Predatory Bird Monitoring Scheme") {
    #     # } else if (type == "PFAS") {
    #     # } else if (type == "HadUK-Grid Annual Rainfall") {
    #     # } else if (type == "APIENS") {
    #     # } else if (type == 'EU Soil metals') {
    #     # } else if (type == 'UK cats and dogs density') {
    #     # } else if (type == 'AgZero+ Input to Yield Ratio (IYR)') {
    #   } else {
    #     'dummy'
    #   }
    # })
    
    # alternative: 
    filtered_data <- reactive({
      type <- req(input$data_choice)
      print(type)
      legend_choices <- NULL
      
      if (length(type) == 0){
        #data_process_EA_pollution(IndustrySector = 'Water Industry')[[1]] %>% head() # not needed,  slow<< Hack to return some valid data while it waits for user input
              
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
        } else if (type == "EU Soil metals") {
          data_process_EUSO(euso_var_choices = input$euso_var_choices)  
        } else if (type == "UK cats and dogs density") {
          data_process_catsdogs(var_choice = input$cats_or_dogs)  
        } else if (type == "AgZero+ Input to Yield Ratio (IYR)") {
          data_process_IYR(IYR_choice = input$IYR_choice)  
        } else if (type == " Custom file upload (.csv)") {
          csv_upload_mod_server(id)         
         
        } else {
          data_process_EA_WQ_gcms(CompoundName = input$gcms_compound, 
                                  start_year = input$year_slider[1],
                                  end_year = input$year_slider[2])  
        }
      }
    })
    print("filtered_data: ")
    print(filtered_data())
    
    ## if we later want to do some more sophisticated logic
    ## we can add reactives to this list
    list(return_value = return_value, filtered_data = filtered_data )#, legend_data = legend_data)
  })
}

DT_mod_server <-  function(id, tbl_data) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns # this was commented out
      
      
      
        # Render the reactive df `tbl_data` as a table
        output$outputTable  <- renderDT({
          
          tryCatch({
            datatable(tbl_data(), options = list(scrollX = TRUE))
          
          }, error = function(e) {
            
            # print(e$message)
            output$error_message <- renderUI({
              div("Gridded data cannot be shown as a table")
            })
            NULL
          
        })
        

        
      })
    })
}

DT_mod_ui <- function(id, dataset_i) {
  
  ns <- NS(id)
  accordion_panel( paste0('Table for dataset ',dataset_i ,':'),
                   h3("Selected data"),
              
                  uiOutput(ns("error_message")),  # Placeholder for error messages    
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
                      choices = tbl_data() %>% colnames()), 
                      #choices = c('REGION','SEX'))
        )
        )
      })
        output$my_plotXY <- renderPlotly({
          # validate(
          #   need(nrow(tbl_data()) > 0, message = FALSE)
          # )
          #
          tryCatch({
            
            suppressWarnings({
              data1 = tbl_data()
              
              #DO this: https://stackoverflow.com/questions/63565683/hide-error-message-with-custom-message-or-reactive-button-in-shiny-app
              
              ## plot the common features for all graphs
              p1<-ggplot(data1, aes(x=.data[[input$plot_xvar]], .data[[input$plot_yvar]], color=.data[[input$plot_colorvar]])) + 
                geom_point(size = 4) +
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
            
            
          }, error = function(e) {
            if (grepl("argument is of length zero", e$message)) {
              return(NULL)  # Ignore this specific error and continue
            }
            
            print(e$message)
            output$plot_placeholder <- renderUI({
              div("Gridded data cannot be plotted.")
            })
            NULL

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
      plotlyOutput(ns("my_plotXY"))  %>% withSpinner(type=5,color = "#A9A9A9")
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

  