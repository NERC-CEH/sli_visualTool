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
#' ## module approach 1: observe event
datselect_mod_server <-  function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$dynamic_select <- renderUI({
              ns <- session$ns
              #req(ns(input$data_choice))
              
              # selectInput(ns("IndustrySector"), "Choose Industry Sector:",
              #             #c("Ammonia", "Arsenic", "Mercury", "Particulate matter - total", "Phenols - total as C", "Pentachlorophenol (PCP)", "Perfluoro octanyl sulphate (PFOS)", "Polychlorinated biphenyls (PCBs)")
              #             unique_industry_sector
              # )
              if(ns(input$data_choice) == "EA pollution inventory 2021") {
                  selectInput(ns("IndustrySector"), "Choose Industry Sector:",
                              #c("Ammonia", "Arsenic", "Mercury", "Particulate matter - total", "Phenols - total as C", "Pentachlorophenol (PCP)", "Perfluoro octanyl sulphate (PFOS)", "Polychlorinated biphenyls (PCBs)")
                              unique_industry_sector
                  )
              } else if(ns(input$data_choice) == "EA water quality GCMS data") {
                  tagList(
                    selectInput(ns("gcms_compound"), "Choose compound:",
                                c("Phenanthrene", "Benzothiazole", "Cocaine", "Ibuprofen", "2,4,7,9-Tetramethyl-5-decyne-4,7-diol")
                    ),
                    sliderInput(ns("year_slider"), "Select Year Range:",
                                min = min(2013), max = max(2021),
                                value = c("2020", "2021")
                    )


                  )
        }
                              })
    # choice for which data to plot
    observeEvent(input$data_choice, {
      req(input$data_choice)
      if(input$data_choice == "EA pollution inventory 2021") {
        output$dynamic_select <- renderUI({
          ns <- session$ns
          selectInput(ns("IndustrySector"), "Choose Industry Sector:",
                      #c("Ammonia", "Arsenic", "Mercury", "Particulate matter - total", "Phenols - total as C", "Pentachlorophenol (PCP)", "Perfluoro octanyl sulphate (PFOS)", "Polychlorinated biphenyls (PCBs)")
                      unique_industry_sector
          )
        })
      } else if(input$data_choice == "EA water quality GCMS data") {
        output$dynamic_select <- renderUI({
          ns <- session$ns
          tagList(
            selectInput(ns("gcms_compound"), "Choose compound:",
                        c("Phenanthrene", "Benzothiazole", "Cocaine", "Ibuprofen", "2,4,7,9-Tetramethyl-5-decyne-4,7-diol")
            ),
            sliderInput(ns("year_slider"), "Select Year Range:",
                        min = min(2013), max = max(2021),
                        value = c("2020", "2021")
            )


          )
        })
      }
    })
  
    }
  )
}

#' ## module approach 2:min example

datselect_mod_server <-  function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      return_value <- reactive({input$data_choice})
      ns <- session$ns
      output$ui_placeholder <- renderUI({
        type <- req(input$data_choice)
        if(type == "EA pollution inventory 2021") {
          textInput(ns("inner_element"), "Text:")
        } else if (type == "EA water quality GCMS data") {
          numericInput(ns("inner_element"), "Value:", 0)
        }
      })
      
      ## if we later want to do some more sophisticated logic
      ## we can add reactives to this list
      list(return_value = return_value) 
    }
  )
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
    handler_list <- c(handler_list, new_handler)
    names(handler_list)[length(handler_list)] <- new_id
    ui_handler(handler_list)
    inserted_ids <<- c(inserted_ids, new_id)
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


