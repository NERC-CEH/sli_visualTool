# help with passing reactive dataframes to and from modules
# https://stackoverflow.com/questions/68584478/how-to-update-shiny-module-with-reactive-dataframe-from-another-module

#' Dataset selection for plot user interface
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
datselect_mod_ui <- function(id, dataset_i) {
  
  ns <- NS(id)
  
  # define choices for dataset selection
  
  dat_choices <- c("EA water quality GCMS data", "EA pollution inventory 2021")
  
  wellPanel(
    selectInput(ns("data_choice"), paste0("Choose dataset ",dataset_i, ":"), 
                choices = dat_choices),
    uiOutput(ns("ui_placeholder")),
    uiOutput(ns("dynamic_select")) #,
    # selectInput("gcms_compound", "Choose compound:",
    #             c("Phenanthrene", "Benzothiazole", "Cocaine", "Ibuprofen", "2,4,7,9-Tetramethyl-5-decyne-4,7-diol")
    #             ),
    # selectInput("substance", "Choose pollutant:", 
    #             c("Ammonia", "Arsenic", "Mercury", "Particulate matter - total", "Phenols - total as C", "Pentachlorophenol (PCP)", "Perfluoro octanyl sulphate (PFOS)", "Polychlorinated biphenyls (PCBs)" )
    #            ),
    
  )
}

unique_industry_sector <- c('Agriculture','Water Industry')

# https://shiny.posit.co/r/articles/improve/modules/#:~:text=Using%20renderUI%20within%20modules

## great example:
# https://stackoverflow.com/questions/63126149/render-conditional-ui-components-within-shiny-module-based-on-input-within-the-s



#' dataset selection module server-side processing
#'
#' @param input,output,session standard \code{shiny} boilerplate
#'
#' @return list with following components
#' \describe{
#'   \item{xvar}{reactive character indicating x variable selection} << change
#'   \item{yvar}{reactive character indicating y variable selection}
#'   \item{facetvar}{reactive character indicating variable used for defining plot facets}
#' }
#' 



# for passing legend_choices

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
    
    # alternative: 
    filtered_data <- reactive({
      type <- req(input$data_choice)
      print(type)
      legend_choices <- NULL
      
      if (length(type) == 0){
        data_out <- data_process_EA_pollution(IndustrySector = 'Water Industry')[[1]] %>% head() # Hack to return some valid data while it waits for user input
        legend_choices <- "Dummy Legend"
      } else {
        if(type=='EA pollution inventory 2021') {
          data_out <- data_process_EA_pollution(IndustrySector = input$IndustrySector)[[1]] 
          legend_choices <- "Dummy legend EA pollution" # paste(input$IndustrySector)
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
        } else {
          data_out <- data_process_EA_WQ_gcms(CompoundName = input$gcms_compound) %>% 
            filter(year >= input$year_slider[1], year <= input$year_slider[2])
          legend_choices <- "Dummy Legend gcms" #paste(input$gcms_compound)
        }
      }
      list(data = data_out, legend_choices = legend_choices)
    })
    print("filtered_data: ")
    print(filtered_data())
    
    ## if we later want to do some more sophisticated logic
    ## we can add reactives to this list
    list(return_value = return_value, filtered_data = filtered_data)
  })
}






#' ## module approach 2:full example
ea_pollution_sliders <- function(id) {               
  tagList(
    selectInput(NS(id,"IndustrySector"), "Choose Industry Sector:",
                #c("Ammonia", "Arsenic", "Mercury", "Particulate matter - total", "Phenols - total as C", "Pentachlorophenol (PCP)", "Perfluoro octanyl sulphate (PFOS)", "Polychlorinated biphenyls (PCBs)")
                unique_industry_sector
    )
  )
}

ea_gcms_sliders <- function(id) {               
  tagList(
    selectInput((NS(id,"gcms_compound")), "Choose compound:",
                c("Phenanthrene", "Benzothiazole", "Cocaine", "Ibuprofen", "2,4,7,9-Tetramethyl-5-decyne-4,7-diol")
    ),
    sliderInput((NS(id,"year_slider")), "Select Year Range:",
                min = min(2013), max = max(2021),
                value = c("2020", "2021")
    )
  )
}

datselect_mod_server <-  function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      return_value <- reactive({input$data_choice})
      ns <- session$ns
      output$ui_placeholder <- renderUI({
        type <- req(input$data_choice)
        if(type == "EA pollution inventory 2021") {
          ea_pollution_sliders(id)
        } else if (type == "EA water quality GCMS data") {
          ea_gcms_sliders(id)
        }
      })
      
      ## if we later want to do some more sophisticated logic
      ## we can add reactives to this list
      list(return_value = return_value) 
    }
  )
}

ui <- fluidPage(
  
  titlePanel("Multiple datasets"),
  
  fluidRow(
    column(
      width = 6,
      wellPanel(
        datselect_mod_ui("dat1_ctrl1",'1')
      )
    ),
    column(
      width = 6,
      wellPanel(
        datselect_mod_ui("dat1_ctrl2",'2')
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      wellPanel(
        datselect_mod_ui("dat1_ctrl3",'3')
      )
    ),
    column(
      width = 6,
      wellPanel(
        datselect_mod_ui("dat1_ctrl4",'4')
      )
    )
  )
)

server <- function(input, output, session) {
  datselect_mod_server('dat1_ctrl1')
  datselect_mod_server('dat1_ctrl2')
  datselect_mod_server('dat1_ctrl3')
  datselect_mod_server('dat1_ctrl4')
}

shinyApp(ui = ui, server = server)



?moduleServer

##################### dyanmically adding UI ##########

#source: https://github.com/iSEE/iSEE/blob/devel/R/collapseBox.R
collapseBox <- function(id, title, ..., open = FALSE, style = NULL) {
  if(is.null(style)) {
    style <- "default"
  }
  
  sub.id <- paste0("collapse_", id)
  bsTag <- tags$div(class = paste0("isee-collapse-box panel panel-", style),
                    id=id,
                    value = title,
                    tags$div(class = "panel-heading",
                             role = "tab",
                             id = paste0("heading_", sub.id),
                             tags$h4(class = "panel-title",
                                     tags$a("data-toggle" = "collapse",
                                            href = paste0("#", sub.id),
                                            title
                                     )
                             )
                    ),
                    tags$div(
                      id = sub.id,
                      class = sprintf("panel-collapse %s", ifelse(open, "collapse in", "collapse")),
                      role = "tabpanel",
                      tags$div(class = "panel-body", list(...))
                    )
  )
  
  tagList(singleton(tags$head(tags$script(src = "iSEE/collapseBox.js"))), bsTag)
}

ui <- fluidPage(  
  sidebarLayout(
    sidebarPanel(
      div(id="placeholder"),
      actionButton("insertBtn", "Add Line",width = '100%'),p(),
      actionButton("removeBtn", "Remove dataset",width = '100%')
      
    ),
    mainPanel(verbatimTextOutput("out"),
              collapseBox("SomePanelType1_ParamBoxOpen",
                          title="Custom parameters",
                          open=FALSE,
                          selectInput("SomePanelType1_Thing",
                                      label="What thing?",
                                      choices=LETTERS, selected="A"
                          )
              )
    )
  )
)

shinyApp(ui = ui, server = server)

######## change to page sidebar


link_shiny <- tags$a(shiny::icon("github"), "Shiny", href = "https://github.com/rstudio/shiny", target = "_blank")
link_posit <- tags$a(shiny::icon("r-project"), "Posit", href = "https://posit.co", target = "_blank")
#source('theme_elements.R')

datselect_mod_ui <- function(id, dataset_i) {
  
  ns <- NS(id)
  
  # define choices for dataset selection
  
  dat_choices <- c("EA water quality GCMS data", "EA pollution inventory 2021")
  
  accordion_panel( paste0('Dataset ',dataset_i ,':'),
                   selectInput(ns("data_choice"), paste0("Choose dataset ",dataset_i, ":"), 
                               choices = dat_choices),
                   uiOutput(ns("ui_placeholder")),
                   uiOutput(ns("dynamic_select")),
                   open=TRUE
                   
  )
}

ui <- page_fillable(title = 'Systems Level Indicator Visual Tool',
                    theme = bs_theme(version = 5),
                    h1('Chemcial Pollution and the Environment'),
                    # or use page_navbar
                    navset_underline(
                      nav_panel(title = "Spatial Trends", 
                                card(
                                  #card_header("Card with sidebar"),
                                  layout_sidebar(
                                    sidebar = sidebar(width = 400,
                                                      accordion(
                                                        div(id="placeholder"),
                                                        multiple = TRUE
                                                      ),
                                                      actionButton("insertBtn", "Add Line",width = '100%'),p(),
                                                      actionButton("removeBtn", "Remove dataset",width = '100%')
                                    ),
                                    navset_card_underline(
                                      # title = "Visualizations",
                                      nav_spacer(),
                                      nav_panel("Map",
                                                verbatimTextOutput("out"),
                                                
                                                accordion(
                                                  accordion_panel(
                                                    "Other controls",
                                                    "Other controls go here"
                                                  ),
                                                  open=FALSE
                                                )
                                      ),
                                      nav_panel("Table", tableOutput("table")) 
                                    )
                                  )
                                )),
                      nav_panel(title = "Indicator", p("Second tab content.")),
                      nav_panel(title = "Time series", p("Third tab content")),
                      nav_panel(title = "Data Sources", tags$iframe(src='../../rstudio-visualtool/data_source.html')),
                      nav_spacer(),
                      nav_menu(
                        title = "Links",
                        nav_item(link_shiny),
                        nav_item(link_posit)
                      )
                    )
)


server <- function(input, output, session) {
  ui_handler <- reactiveVal(list())
  inserted_ids <- c()
  
  # observer to insert UI for another dataset
  observeEvent(input$insertBtn, {
    if (length(inserted_ids) <5) {
      #new_id <- paste("dat1_ctrl", input$insertBtn, sep = "_")  # based on counter hit
      new_id_ii <- length(inserted_ids)+1
      new_id <- paste("dat1_ctrl", new_id_ii , sep = "_")
      insertUI(
        selector = "#placeholder",
        where = "beforeBegin",
        ## wrap element in a div with id for ease of removal
        ui = tags$div(
          datselect_mod_ui(new_id,as.character(new_id_ii)), 
          id = new_id
        )
      )
      handler_list <- isolate(ui_handler())
      new_handler <- datselect_mod_server(new_id)
      
      handler_list[[new_id]] <- function() {
        list(
          return_value = new_handler$return_value(),
          legend_choices = new_handler$filtered_data()$legend_choices
        )
      }
      
      # handler_list <- c(handler_list, new_handler)
      # names(handler_list)[length(handler_list)] <- new_id
      ui_handler(handler_list$return_value)
      inserted_ids <<- c(inserted_ids, new_id)
      print("Handler_list:")
      print(handler_list)
    } else {
      shiny::showNotification('Maximum number of datasets allowed is 5.',type = 'warning')
    }
  })
  
  # observer to remove UI
  observeEvent(input$removeBtn, {
    print(inserted_ids)
    removeUI(
      ## pass in appropriate div id
      selector = paste0('#', inserted_ids[length(inserted_ids)])
    )
    inserted_ids <<- inserted_ids[-length(inserted_ids)]
  })
  
  
  output$out <- renderPrint({
    lapply(ui_handler(), function(handle) {
      handle()
    })
    
    #print('blah blah blah')
  })
}
shinyApp(ui, server)


