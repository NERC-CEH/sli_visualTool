## Chemical Pollution visual tool generic version

# TODO: validate input: https://stackoverflow.com/questions/63565683/hide-error-message-with-custom-message-or-reactive-button-in-shiny-app
# add coutnry flags in shiny: https://stackoverflow.com/questions/70855108/load-countries-flags-to-be-displayed-in-multi-js-for-many-countries-in-a-shiny-a
library(shiny)
library(bslib)
library(bsicons)
library(leaflet)
library(DT)
library(raster)
library(dplyr)
library(leaflet.extras)
library(leafem)
library(shinyWidgets)
library(readxl)
library(sp)
library(sf)
library(plotly)
library(rnrfa) # for osg_parse
library(readr)
library(stringr)
library(shinycssloaders)
library(RColorBrewer)
library(tidyr)
library(esquisse) # for  palettePicker

source('data_fun.R')
source('map_fun.R')
source('modules/data_modules.R')
source('theme_elements.R')
# data(quakes)


rr <- htmltools::HTML('<a href="https://ceh.ac.uk/" target="_blank"> <img border="0" alt="ImageTitle" src="https://www.ceh.ac.uk/sites/default/files/images/theme/ukceh_logo_long_720x170_rgb.png" width="auto" height="40"> </a>')


link_shiny <- tags$a(shiny::icon("github"), "Code", href = "https://github.com/NERC-CEH/sli_visualTool", target = "_blank")
link_posit <- tags$a(shiny::icon("book"), "Documentation", href = "https://github.com/NERC-CEH/sli_visualTool", target = "_blank")
# link_guide <- tags$a(shiny::icon("book"), "User guide", href =  "https://cehacuk.sharepoint.com/:w:/r/sites/UKCEHJNCCsystemlevelindicators/Shared%20Documents/General/Visual%20tool%20manual.docx?d=wa4d3314bc019426bb921f088dd4a23ef&csf=1&web=1&e=yhHsic", target = "_blank")
link_guide <- tags$a(shiny::icon("book"), "User guide", href =  "Visual tool manual.pdf", target = "_blank")


# import LCM
gb_lcm_1km_dom_tar <- raster("./datasets/LCM/gb2021lcm1km_dominant_target.tif")
gb_lcm_1km_dom_tar[gb_lcm_1km_dom_tar == 0] <- NA
gb_lcm_1km_dom_tar$gb2021lcm1km_dominant_target = gb_lcm_1km_dom_tar$gb2021lcm1km_dominant_target %>% as.factor()
factpal <- colorFactor(color_data$RGB, values(gb_lcm_1km_dom_tar), na.color = 'transparent')

# NUTS processing is slow
CompoundName <-  "Phenanthrene"
NUTS_region <- get_NUTS_regions(NUTS_lvl_code = 1)
NUTS_region_with_gcms_data <- data_process_EA_WQ_gcms_with_NUTS(fp_gcms_withNUTS = './datasets/EA_water_quality_GCMS_LCMS/gcms_data_with_NUTS.csv', NUTS_region = NUTS_region, CompoundName = "Phenanthrene")

watermarkcss <- "
#watermark
{
 position:fixed;
 bottom:40%;
 left:33%;
 font-size: 100px;
 opacity:0.25;
 z-index:99;
 color: gray;
 text-align: center;
 transform: rotate(-30deg);
 pointer-events: none;
}

"

ui <- page_fillable(      
  
  # tags$head(
  #   tags$style(watermarkcss),
  #   HTML('<div id="watermark">SLI project. For demonstration only.</div>')
  # ),
  tags$head(tags$style(".rightAlign{float:right;}")),
  
  title = 'Systems Level Indicator Visual Tool',
  #theme = bs_theme(version = 5),
  theme = UKCEH_theme,  # << add this line
  #Application title
  UKCEH_titlePanel("Chemical Pollution and the Environment"),
  #h1('Chemcial Pollution and the Environment'),
  ## or use page_navbar
  # HTML('<p align="center" style="font-weight: bold;color:orange">For Demonstration purposes only. Under development.</p>'),
  tags$html(lang = "en"), # modification for accessibility
  tags$head(tags$title("Chemical Pollution and the Environment")), # modification for accessibility
  navset_underline(
    nav_panel(title = "Spatial Trends", 
              card(
                #card_header("Card with sidebar"),
                layout_sidebar(
                  sidebar = sidebar(width = 400,
                                    tags$div(
                                      tags$span(
                                        "Quick Tip: Start by clicking 'Add Dataset', and then expand the controls. See user guide for more details.",
                                        style = "font-weight: bold; font-size: 12px;"
                                      )),
                                    accordion(
                                      div(id="placeholder"),
                                      multiple = TRUE, open=TRUE
                                    ),
                                    actionButton("insertBtn", "Add dataset",width = '100%', class = "btn-primary"),p(),
                                    actionButton("removeBtn", "Remove dataset", class = "btn-warning",width = '100%'),p(),
                                    actionButton("updateBtn", "Update map", class = "btn-success",width = '100%')
                  ),
                  navset_card_underline(
                    # title = "Visualizations",
                    nav_spacer(),
                    
                    nav_panel("Map",
                              # https://rstudio.github.io/bslib/articles/tooltips-popovers/index.html
                              popover(
                                bs_icon("gear"),
                                p('Coming soon!'),
                                title = "Map controls", class = 'rightAlign',
                                palettePicker(
                                  inputId = "pal2", 
                                  label = "With a list of palette:", 
                                  choices = list(
                                    "Viridis" = list(
                                      "viridis" = viridis_pal(option = "viridis")(10),
                                      "magma" = viridis_pal(option = "magma")(10),
                                      "inferno" = viridis_pal(option = "inferno")(10),
                                      "plasma" = viridis_pal(option = "plasma")(10),
                                      "cividis" = viridis_pal(option = "cividis")(10)
                                    ),
                                    "Brewer" = list(
                                      "Blues" = brewer_pal(palette = "Blues")(8),
                                      "Reds" = brewer_pal(palette = "Reds")(8),
                                      "Paired" = brewer_pal(palette = "Paired")(8),
                                      "Set1" = brewer_pal(palette = "Set1")(8)
                                    )
                                  ), 
                                  textColor = c(
                                    rep("white", 5), rep("black", 4) 
                                  )
                                )
                              ),
                              leafletOutput('myMap',height = 650) %>% withSpinner(type=5,color = "#A9A9A9") #,
                              # accordion(
                              #   accordion_panel(
                              #     "Map controls",
                              #     "Coming soon"#,
                              #     # input_switch("marker_use_colourmap", "Colour markers by value.", TRUE)
                              #   ),
                              #   open=FALSE
                              # )
                    ),
                    nav_panel("Plot",
                              accordion(
                                div(id="placeholder-plots"),
                                multiple = TRUE, open=TRUE
                              )
                    ),
                    nav_panel("Table", 
                              # verbatimTextOutput("out"),   # uncomment for debugging
                              # verbatimTextOutput("out2"),
                              accordion(
                                div(id="placeholder-table"),
                                multiple = TRUE, open=TRUE
                              )
                    )
                    , full_screen = TRUE
                  )
                )
              )),
    nav_panel(title = "Indicator", 
              card(
                layout_sidebar(
                  sidebar = sidebar(width = 400,
                                    selectInput("RegtionOption", "Choose indicator to display:",
                                                c('Chemical Pollution Indicator', 
                                                  # 'Mean Pharceuticals', 
                                                  # 'Mean metals', 
                                                  'Mean Phenanthrene by region'  )),
                                    open = FALSE),
                  # nested right sidebar
                  layout_sidebar(
                    sidebar = sidebar(#"Right sidebar", 
                                      HTML('<p align="center" style="font-weight: bold;color:orange">For illustration only.</p>'),
                                      selectInput('countryInd', 'Choose Country:', c('England','Wales','Scotland','Northern Ireland'),),
                                      selectInput('compartmentInd', 'Choose Compartment:', c('Terrestrial','Freshwater','Marine','Air')),
                                      plotlyOutput("barplot_indicator"),
                                      position = "right", width = 500, open = TRUE),
                 
                  # value_box(
                  #   title = "",
                  #   value = "62%",
                  #   showcase = bs_icon("pie-chart"),
                  #   p('of land has 1 or more unhealthy'),
                  #   p('pollution indicators.'),
                  #   theme = "info"
                  # ),
                  leafletOutput('regionMap',height = 650) %>% withSpinner(type=5,color = "#A9A9A9")
                  )
                )
                , full_screen = TRUE)
    ),
    # nav_panel(title = "Time series",
    #           card(
    #             #card_header("Card with sidebar"),
    #             layout_sidebar(
    #               sidebar = sidebar(width = 400,
    #                                 accordion(
    #                                   div(id="placeholder-ts"),
    #                                   multiple = TRUE
    #                                 ),
    #                                 actionButton("insertBtn_TS", "Add dataset",width = '100%', class = "btn-primary"),p(),
    #                                 actionButton("removeBtn_TS", "Remove dataset", class = "btn-warning",width = '100%'),p() #,
    #                                 # actionButton("updateBtn_TS", "Update map", class = "btn-success",width = '100%')
    #               ),
    #               navset_card_underline(
    #                 # title = "Visualizations",
    #                 nav_spacer(),
    #                 nav_panel("Table",
    #                           p('The time series tab currently work similarly as the spatial trends tab. It will be used in the future to plot time series of regional or national data or metrics.'),
    # 
    #                           verbatimTextOutput("out_ts"),
    #                           # verbatimTextOutput("out2_ts"),
    #                           accordion(
    #                             div(id="placeholder-table-ts"),
    #                             multiple = TRUE
    #                           )
    #                 ),
    #                 nav_panel("Plot",
    #                           # add plot
    #                           p('Plot coming soon'),
    #                           accordion(
    #                             accordion_panel(
    #                               "Plot controls",
    #                               "Coming soon"
    #                             ),
    #                             open=FALSE
    #                           )
    #                 )
    # 
    #               )
    #             )
    #           )),
    nav_panel(title = "Data Sources", tags$iframe(src='data_source.html', width='100%',height=900), p(),p(),p(),hr()),
    nav_panel(title = "Data Catalogue", DTOutput('catalogueDT'),p(),p(),p(),hr()), 
    #nav_panel(title = "Chemical History Timeline", p('Coming soon.')),
    nav_panel(title = "Accessibility Statement", tags$iframe(src='accessibility_statement.html', width='100%', height=1500), p(),p(),p(),hr()),
    nav_panel(title = "About", 
              h2('About this visual tool'),
              p('This visual tool allows users to overlay several datasets to visualize their links in order to build an integrated understanding of chemical pollution. It also provides a demonstration to display a regional overall state of the environment indicator.'),
              br(),
              h2('Contact us'),
              p('For any issues, comments, or queries, please contact the lead developer at mtso _at_ ceh.ac.uk'),
              br(),
              h2('Compliance'),
              p('We have strived to comply with the following standards'),
              tags$div(
                tags$ul(
                  tags$li("Compliant with the NCSC - National Cyber Security Centre - 14 Cloud Security Principles."),
                  tags$li("GDPR compliant: This tools does not collect data and it does not use cookies."),
                  tags$li("An accessbility assessment was performed, and the accessbility of this app is improved based on the suggestions listed here: https://www.jumpingrivers.com/blog/accessible-shiny-standards-wcag/. See UKCEH accessibility statement here: https://www.ceh.ac.uk/accessibility-statement"),
                  tags$li("UKCEH has been independently assessed as meeting the Cyber Essentials standard in recognition that we have implemented the required cyber security controls."),
                  
                )
              ),
              br(),
              h2('Case study causal loop diagrams'),
              p('The follow causal loop diagrams were developed as part of the case study reports for the tool.'),
              navset_card_underline(
                nav_panel("England PFAS",
                    HTML('<iframe width="1000" height="880" frameborder="0" src="https://ncase.me/loopy/v1.1/?embed=1&data=[[[7,586,176,0.33,%22FIREFOAMS%22,5],[10,761,659,0.83,%22DISTRIBUTION%22,4],[12,676,482,0.5,%22APPLICATION%22,1],[13,757,816,1,%22PFAS%2520PRODUCTION%22,0],[14,861,519,0.66,%22MANUFACTURING%22,2],[15,1009,416,0,%22CLOTHING%22,2],[16,964,650,0.33,%22DOMESTIC%2520PRODUCTS%22,2],[17,1119,549,0.5,%22VEHICLES%22,2],[18,853,338,0,%22INDUSTRY%22,2],[19,461,46,0.33,%22INDUSTRIAL%2520SITES%22,5],[20,805,57,0.33,%22AIRPORTS%22,5],[21,373,213,0.33,%22WILDFIRES%22,5],[22,13,657,0,%22WWTP%22,1],[23,463,452,0,%22SLUDGE%2520TO%2520LAND%22,0],[24,1420,447,0.33,%22PRODUCT%2520USE%22,0],[25,1342,107,0.66,%22RUN%2520OFF%22,0],[26,265,366,0,%22PESTICIDES%22,0],[27,378,792,0.5,%22TO%2520WASTE%2520WATER%22,0],[28,100,483,0,%22TO%2520SOIL%22,0],[29,183,234,0,%22TO%2520GROUND%2520WATER%22,0],[30,253,75,0,%22TO%2520AIR%22,0],[31,1310,817,0.5,%22TO%2520LANDFILL%22,0],[33,49,305,0.33,%22soil%2520fauna%22,3],[34,-13,12,0.33,%22birds%22,3],[35,8,105,0.33,%22terrestrial%2520wildlife%22,3],[36,185,731,0.33,%22aquatic%2520wildlife%22,3],[37,870,172,0.33,%22terrestrial%2520animals%22,3],[38,313,636,0.33,%22vegetation%22,3],[39,1009,100,0.33,%22man%22,3],[40,62,856,0.33,%22freshwaters%22,3],[41,255,854,0.33,%22marine%22,3]],[[13,10,-31,1,0],[10,14,31,1,0],[10,12,-46,1,0],[14,18,20,1,0],[14,15,-15,1,0],[14,16,10,1,0],[14,17,9,1,0],[7,19,16,1,0],[7,21,-37,1,0],[7,20,-23,1,0],[12,7,24,1,0],[12,26,-37,1,0],[12,23,-46,1,0],[22,23,-36,1,0],[18,25,-24,1,0],[16,24,6,1,0],[17,25,19,1,0],[24,25,76,1,0],[14,25,-31,1,0],[18,25,42,1,0],[15,25,-41,1,0],[25,27,298,1,0],[18,27,-131,1,0],[27,22,161,1,0],[23,27,-43,1,0],[16,27,37,1,0],[17,27,210,1,0],[14,27,-33,1,0],[24,27,68,1,0],[12,27,65,1,0],[21,30,-34,1,0],[19,30,-29,1,0],[20,30,-88,1,0],[26,29,41,1,0],[26,28,-39,1,0],[23,28,-15,1,0],[23,29,-50,1,0],[21,28,137,1,0],[20,28,59,1,0],[19,28,148,1,0],[15,30,-69,1,0],[17,30,-74,1,0],[16,31,-48,1,0],[17,31,32,1,0],[15,31,186,1,0],[18,31,227,1,0],[31,29,126,1,0],[31,27,120,-1,0],[28,33,12,-1,0],[33,35,14,-1,0],[33,34,-9,-1,0],[33,35,-29,-1,0],[34,35,23,-1,0],[35,34,6,-1,0],[36,34,58,-1,0],[38,35,14,-1,0],[38,34,65,-1,0],[38,37,-64,-1,0],[38,39,142,-1,0],[37,39,-29,-1,0],[30,39,102,-1,0],[30,37,21,-1,0],[30,34,-22,-1,0],[30,35,24,-1,0],[22,40,55,-1,0],[40,41,10,-1,0],[40,36,20,-1,0],[41,36,-6,-1,0]],[],41%5D"></iframe>'),
                    p('')
                ),
                nav_panel("PBMS SGARs",
                    HTML('<iframe width="1000" height="880" frameborder="0" src="https://ncase.me/loopy/v1.1/?embed=1&data=[[[4,-12,170,1,%22SGAR%2520source%22,0],[5,480,233,0.33,%22mice%22,3],[6,892,265,0.5,%22rats%22,1],[7,1318,717,0.5,%22terrestrial%2520predator%22,0],[8,1350,122,0.5,%22scavenging%2520raptor%22,0],[10,522,587,0.33,%22passerine%2520birds%22,3],[11,851,537,0.33,%22corvids%22,1],[12,1419,428,0.5,%22prey%2520eating%2520raptor%22,0],[13,187,456,0.33,%22insects%22,3],[15,34,362,0.33,%22bacteria%22,3]],[[4,6,-53,1,0],[4,5,-21,1,0],[4,7,-36,1,0],[5,8,-72,1,0],[5,7,-49,1,0],[6,7,107,1,0],[8,7,-135,1,0],[10,8,261,1,0],[10,7,55,1,0],[5,11,35,1,0],[6,11,28,1,0],[11,7,-75,1,0],[11,8,-213,1,0],[5,12,-90,1,0],[4,13,12,1,0],[13,10,27,1,0],[13,5,59,1,0],[6,12,185,1,0],[10,12,-67,1,0],[4,15,-33,1,0],[15,13,-54,1,0],[12,7,74,1,0],[6,7,444,1,0],[6,8,-300,1,0],[6,11,-26,1,0],[13,6,-68,1,0],[13,11,-251,1,0]],[[549,804,%22Possible%2520uptake%2520route%2520of%2520Second%2520Generation%2520Anticoagulant%2520Rodenticides%2520(SGARs)%2520by%2520predators%22]],15%5D"></iframe>'),
                    p()
                ),
                nav_panel("PBMS metals",
                  HTML('<iframe width="1000" height="880" frameborder="0" src="https://ncase.me/loopy/v1.1/?embed=1&data=[[[4,741,329,0.5,%22Rain%22,4],[5,1067,153,0.5,%22soil%22,3],[6,505,116,0.5,%22Air%22,4],[7,933,768,0.5,%22Rivers%22,4],[8,1126,751,0.5,%22Lakes%22,4],[9,1339,726,0.5,%22Sea%22,4],[10,1403,191,0,%22terrestrial%22,1],[12,395,768,0,%22Marine%22,1],[13,483,369,0,%22Avian%22,1],[14,17,436,0,%22Freshwater%22,1],[15,1604,319,0,%22Humans%22,1],[16,788,-63,1,%22Mining%22,0],[17,1416,-75,0.5,%22Spills%252Fleaks%22,0],[18,-17,-4,0.83,%22Combustion%22,0],[19,501,-109,0.66,%22Industry%22,0],[20,1190,-114,0.83,%22Waste%2520disposal%22,0],[21,200,9,0.66,%22Transport%22,0]],[[6,5,56,1,0],[4,5,25,1,0],[6,4,18,1,0],[4,7,-13,1,0],[7,8,-3,1,0],[8,9,58,1,0],[5,7,18,1,0],[5,8,18,1,0],[5,9,46,1,0],[5,10,78,1,0],[6,13,-47,1,0],[4,13,36,1,0],[5,13,841,1,0],[9,12,43,1,0],[7,14,21,1,0],[8,14,36,1,0],[12,13,147,1,0],[14,13,33,1,0],[13,15,-507,1,0],[14,15,-184,1,0],[12,15,-25,1,0],[10,15,78,1,0],[16,5,-63,1,0],[17,5,25,1,0],[18,6,-41,1,0],[21,6,14,1,0],[19,6,23,1,0],[19,5,-46,1,0],[16,6,-71,1,0],[20,5,-21,1,0],[20,6,-75,1,0],[18,5,158,1,0],[21,5,242,1,0],[10,13,122,1,0],[18,4,-87,1,0],[21,7,-215,1,0],[17,7,41,1,0],[16,7,34,1,0]],[[-147,733,%22Metal%2520bioaccumulation%22]],21%5D"></iframe>'),
                  p()
                ), 
                nav_panel("Scotland Glyposate",
                          p('Note there is currenlty no data in the tool that directly contributes to this case study.')
                          
                          ),
                nav_panel("NI Lough Neagh",
                          p('Note there is currenlty no data in the tool that directly contributes to this case study.')
                          ),
                nav_panel("Fipronil", 
                          HTML('<iframe width="1000" height="880" frameborder="0" src="https://ncase.me/loopy/v1.1/?embed=1&data=[[[1,905,379,0.5,%22Rivers%22,4],[2,409,370,1,%22cats%2520and%2520dogs%22,0],[3,385,880,0.16,%22Farming%22,0],[4,1277,648,1,%22Bees%22,5],[5,904,628,0.33,%22crops%22,4],[6,1292,394,0.66,%22Aquatic%2520life%22,5],[7,396,600,0.5,%22handwashing%22,0],[9,616,573,0.5,%22wwTW%22,4]],[[5,4,46,1,0],[2,1,49,1,0],[3,7,-14,1,0],[2,7,17,1,0],[1,6,15,1,0],[3,5,-26,1,0],[3,1,-57,1,0],[7,9,-50,1,0],[9,1,-37,1,0]],[[382,978,%22Banned%22],[1131,758,%22https%253A%252F%252Fec.europa.eu%252Fcommission%252Fpresscorner%252Fdetail%252Fen%252Fip_13_708%22],[358,465,%22For%2520spot-on%2520flea%2520treatments%22],[872,305,%22Fipronil%2520was%2520detected%2520in%252098%2525%2520of%2520freshwater%2520samples%2520(2016-2018).%22],[711,464,%22https%253A%252F%252Fdoi.org%252F10.1016%252Fj.scitotenv.2020.143560%22],[709,493,%22https%253A%252F%252Fdoi.org%252F10.1016%252Fj.scitotenv.2024.170175%22]],9%5D"></iframe>')),
              )
              ),
    nav_spacer(),
    nav_item(link_guide),
    nav_menu(
      title = "Links",
      nav_item(link_shiny),
      nav_item(link_posit)
    ),
    nav_item(
      input_dark_mode(id = "dark_mode", mode = "light")
    )
  )
)


server <- function(input, output, session) {
  
  # TODO: make it csv, may read faster
  
  data_catalogue <- read_excel('www/Visual tool data catalogue.xlsx',skip=1) %>%
    mutate(`Dataset name` = ifelse(str_detect(`Link to dataset`,'https'),
                                   paste0('<a href="',`Link to dataset` ,  '" target="_blank">',`Dataset name` ,'</a>'),
                                   `Dataset name`)) %>%
    select(-`Link to dataset`) %>%
    rename_with(~str_c("Case study:", .), all_of(colnames(.)[9:13]))


  output$catalogueDT = renderDT({
    datatable(data_catalogue, escape = FALSE , class = 'cell-border stripe', rownames = F,
              caption = 'Table 1: List of datasets included in the visual tool.')
  })
  
  ############## Observers handling Point Data tab ####################
  ui_handler <- reactiveVal(list()) #stores the reactive UI 
  df_handler <- reactiveVal(list()) #store the reactive dataframes as list
  inserted_ids <- c()  #list of dynamic UI ids
  
  # storage of point datasets
  reactive_df <- reactiveValues(data = NULL)
  
  # observer to insert UI for another dataset (point data on map)
  observeEvent(input$insertBtn, {
    if (length(inserted_ids) <5) {
      #new_id <- paste("dat1_ctrl", input$insertBtn, sep = "_")  # based on counter hit
      new_id_ii <- length(inserted_ids)+1
      new_id <- paste("dat1_ctrl", new_id_ii , sep = "_")
      
      ## insert accordion UI
      insertUI(
        selector = "#placeholder",
        where = "beforeBegin",
        ## wrap element in a div with id for ease of removal
        ui = tags$div(
          datselect_mod_ui(new_id,as.character(new_id_ii)), 
          id = new_id
        )
      )
      
      ## insert table UI
      insertUI(
        selector = "#placeholder-table",
        where = "beforeBegin",
        ## wrap element in a div with id for ease of removal
        ui = tags$div(
          DT_mod_ui(paste0(new_id,'_table'),as.character(new_id_ii)), 
          id = paste0(new_id,'_table')
        )
      )
      
      ## insert plots UI
      insertUI(
        selector = "#placeholder-plots",
        where = "beforeBegin",
        ## wrap element in a div with id for ease of removal
        ui = tags$div(
          
          # paste0('Plot for dataset ',new_id_ii ,':'),
          plot_mod_ui(paste0(new_id,'_plots'),as.character(new_id_ii)), 
          id = paste0(new_id,'_plots')
          
        )
      )
      
      ## storing the added UI and data
      
      handler_list <- isolate(ui_handler())
      new_handler <- datselect_mod_server(new_id)
      handler_list <- c(handler_list, new_handler['return_value'])
      names(handler_list)[length(handler_list)] <- new_id
      ui_handler(handler_list)  # important: update the reactive list
      inserted_ids <<- c(inserted_ids, new_id)
      print(handler_list)
      
      ## Append data
      # reactive_df[[paste0('data_', new_id_ii)]] <- new_handler[['filtered_data']]
      # print(isolate(reactive_df[[paste0('data_', new_id_ii)]]))
      df_list <- isolate(df_handler())
      df_list <- c(df_list, new_handler['filtered_data'])
      names(df_list)[length(df_list)] <- new_id
      df_handler(df_list)
      
      #print(df_handler())
      
      DT_mod_server(paste0(new_id,'_table'), df_handler()[[new_id]])  # table module
      #DT_mod_server(paste0(new_id,'_table'), mtcars)
      plot_mod_server(paste0(new_id,'_plots'), df_handler()[[new_id]])
    } else {
      shiny::showNotification('Maximum number of datasets allowed is 5.',type = 'warning')
    }
  })
  
  # observer to remove UI
  observeEvent(input$removeBtn, {
    if (length(inserted_ids) >0) {
    print(inserted_ids)
    removeUI(
      ## pass in appropriate div id, for data selector
      selector = paste0('#', inserted_ids[length(inserted_ids)])
    )
    removeUI( 
      ## pass in appropriate div id, for data table
      selector = paste0('#',inserted_ids[length(inserted_ids)], '_table')
    )
    removeUI( 
      ## pass in appropriate div id, for data plots
      selector = paste0('#',inserted_ids[length(inserted_ids)], '_plots')
    )
    
    # remove associated datasets
    reactive_df[[inserted_ids[length(inserted_ids)]]] <- NULL
    
    # remove the id from list
    inserted_ids <<- inserted_ids[-length(inserted_ids)]
    }  else {
      shiny::showNotification('No more datasets to delete.',type = 'warning')
    }
    
  })
  
  
  output$out <- renderPrint({
    lapply(ui_handler(), function(handle) {
      handle()
    })
    #print('blah blah blah')
  })
  output$out2 <- renderPrint({
    lapply(df_handler(), function(handle) {
      handle()
    })
  })
  
  ############## Observers handling time series Data tab ####################
  
  ui_handler_TS <- reactiveVal(list()) #stores the reactive UI 
  df_handler_TS <- reactiveVal(list()) #store the reactive dataframes as list
  inserted_ids_TS <- c()  #list of dynamic UI ids
  
  # storage of point datasets
  reactive_df_TS <- reactiveValues(data = NULL)
  
  # observer to insert UI for another dataset (point data on map)
  observeEvent(input$insertBtn_TS, {
    if (length(inserted_ids_TS) <5) {
      #new_id <- paste("datTS_ctrl", input$insertBtn, sep = "_")  # based on counter hit
      new_id_ii <- length(inserted_ids_TS)+1
      new_id <- paste("datTS_ctrl", new_id_ii , sep = "_")
      
      ## insert accordion UI
      insertUI(
        selector = "#placeholder-ts",
        where = "beforeBegin",
        ## wrap element in a div with id for ease of removal
        ui = tags$div(
          datselect_mod_ui(new_id,as.character(new_id_ii)), 
          id = new_id
        )
      )
      
      ## insert table UI
      insertUI(
        selector = "#placeholder-table-ts",
        where = "beforeBegin",
        ## wrap element in a div with id for ease of removal
        ui = tags$div(
          DT_mod_ui(paste0(new_id,'_table'),as.character(new_id_ii)), 
          id = paste0(new_id,'_table')
        )
      )
      
      ## storing the added UI and data
      
      handler_list <- isolate(ui_handler_TS())
      new_handler <- datselect_mod_server(new_id)
      handler_list <- c(handler_list, new_handler['return_value'])
      names(handler_list)[length(handler_list)] <- new_id
      ui_handler_TS(handler_list)  # important: update the reactive list
      inserted_ids_TS <<- c(inserted_ids_TS, new_id)
      print(handler_list)
      
      ## Append data
      # reactive_df[[paste0('data_', new_id_ii)]] <- new_handler[['filtered_data']]
      # print(isolate(reactive_df[[paste0('data_', new_id_ii)]]))
      df_list <- isolate(df_handler_TS())
      df_list <- c(df_list, new_handler['filtered_data'])
      names(df_list)[length(df_list)] <- new_id
      df_handler_TS(df_list)
      
      print(df_handler_TS())
      
      DT_mod_server(paste0(new_id,'_table-ts'), df_handler_TS()[[new_id]])
      
    } else {
      shiny::showNotification('Maximum number of datasets allowed is 5.',type = 'warning')
    }
  })
  
  # observer to remove UI
  observeEvent(input$removeBtn_TS, {
    print(inserted_ids_TS)
    removeUI(
      ## pass in appropriate div id, for data selector
      selector = paste0('#', inserted_ids_TS[length(inserted_ids_TS)])
    )
    removeUI( 
      ## pass in appropriate div id, for data table
      selector = paste0('#',inserted_ids_TS[length(inserted_ids_TS)], '_table')
    )
    # remove associated datasets
    reactive_df_TS[[inserted_ids_TS[length(inserted_ids_TS)]]] <- NULL
    
    # remove the id from list
    inserted_ids_TS <<- inserted_ids_TS[-length(inserted_ids_TS)]
    
    
  })
  
  
  output$out_ts <- renderPrint({
    lapply(ui_handler_TS(), function(handle) {
      handle()
    })
    #print('blah blah blah')
  })
  output$out2_ts <- renderPrint({
    lapply(df_handler_TS(), function(handle) {
      handle()
    })
  })
  
  ####################### end ########################
  
  ### leaflet map for point data #####
  map = leaflet() %>% 
    addTiles(group = "OpenStreetMap") %>% 
    addProviderTiles(providers$Esri.WorldImagery,                  # try Esri. and see what other options are available.
                     group = "ESRI World Imagery",
                     options = providerTileOptions(noWrap = TRUE) # (noWrap = TRUE) avoids having multiple world maps
    ) %>%
    addProviderTiles(providers$CartoDB.Positron,
                     group = "CartoDB.Positron",
                     options = providerTileOptions(noWrap = TRUE) # (noWrap = TRUE) avoids having multiple world maps
    ) %>%
    addProviderTiles(providers$Esri.NatGeoWorldMap,
                     group = "Esri.NatGeoWorldMap",
                     options = providerTileOptions(noWrap = TRUE) # (noWrap = TRUE) avoids having multiple world maps
    ) %>%
    addProviderTiles(providers$OpenTopoMap,
                     group = "OpenTopoMap",
                     options = providerTileOptions(noWrap = TRUE) # (noWrap = TRUE) avoids having multiple world maps
    ) %>%
    addProviderTiles(providers$Stadia.StamenToner,
                     group = "Stadia.StamenToner",
                     options = providerTileOptions(noWrap = TRUE) # (noWrap = TRUE) avoids having multiple world maps
    ) %>%
    addProviderTiles("NASAGIBS.ViirsEarthAtNight2012",
                     group = "NASA Earth at Night 2012",
                     options = providerTileOptions(noWrap = TRUE)) %>% 
    addRasterImage(gb_lcm_1km_dom_tar, opacity = 0.5, color = factpal,
                   group = "LCM 2021 1km dominant target"
    ) %>%
    addWMSTiles('https://catalogue.ceh.ac.uk/maps/cca47088-8cdd-4d7a-86b4-90f0a1766364?request=getCapabilities&service=WMS&cache=false&',
                layers='HY.PhysicalWaters.Catchments.IHU_AreasWithCoastline',
                options = WMSTileOptions(crs=27700,opacity=0.5),
                group = 'IHU') %>%
    addLayersControl(baseGroups = c("OpenStreetMap", "ESRI World Imagery", 
                                    "CartoDB.Positron","Esri.NatGeoWorldMap",
                                    "OpenTopoMap","Stadia.StamenToner"
                                    #,"GeoportailFrance.orthos"
    ), 
    overlayGroups = c("IHU","LCM 2021 1km dominant target", "NASA Earth at Night 2012"),
    position = 'topleft') %>% 
    addLegend(
      position = "bottomright",
      colors = rgb(t(col2rgb(color_data$RGB)) / 255),
      labels = color_data$Class, opacity = 1,
      title = "LCM classes",
      group = "LCM 2021 1km dominant target"
    ) %>% 
    hideGroup(c("LCM 2021 1km dominant target", "IHU", "NASA Earth at Night 2012")) %>%
    setView(-3.0, 55.5, zoom = 6) 
  
  output$myMap = renderLeaflet(map)
  
  observeEvent(input$updateBtn, {
    print('press update button')
    showNotification("Map updating...", type = "warning",duration = 1.5)
    
    m = leafletProxy("myMap") %>% 
      removeLayersControl() %>% 
      clearShapes() %>% 
      clearControls() %>% 
      clearMarkers() %>%
      clearHeatmap() %>%
      clearImages() #%>% 
      
      # leafem::addGeotiff(file = 'datasets/empty_raster.tif',
      #                    opacity = 0.0) # empty raseter to clear addGeotiff, doesn't work
    
    #addMarkers(data = quakes[1:20,],~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))
    
    # unpack the reactive list
    # outstanding issues: seems to not shrink in legnth after removing datasets--use with care
    df_list <- lapply(df_handler(), function(handle) {
      handle()
    })
    handler_list <- lapply(ui_handler(), function(handle) {
      handle()
    })
    
    #colour palettes
    
    
    if (length(inserted_ids) > 0) {
      withProgress(message = 'Making plot', value = 0, {  # initialize counter
        
        for (new_id_ii in 1:length(inserted_ids)){
          
          new_id = paste("dat1_ctrl", new_id_ii , sep = "_")
          legend_title= paste0(as.character(new_id_ii) ,". ", handler_list[[new_id]] )
          
          single_color_sequential_palettes <- c("Reds", "Blues", "Greens", "Purples", "Oranges", "Greys")
          
          ## New function
          m = switch_map(m = m, 
               map_data = df_list[[new_id]], 
               input_choice = handler_list[[new_id]],
               legend_title=legend_title,
               showHeatmap = input$heatmap, 
               palette_name =  single_color_sequential_palettes[new_id_ii])
          
          #### start add dataset: it works but deprecating, replaced by switch_map
          
          # labFormat_transform = labelFormat(transform = function(x) round(exp(x) - 1, 1))         
          
          
          # if (handler_list[[new_id]] == 'EA water quality GCMS/LCMS data') {
          #   
          #   # Check if the dataset is empty or has no valid data
          #   if (nrow(df_list[[new_id]]) == 0 || all(is.na(df_list[[new_id]]$log_Concentration))) {
          #     dummy_color <- colorNumeric(palette = "Greys", domain = c(0, 1))
          #     # Add a message in the legend indicating no data
          #     m = m %>% addLegend(
          #       position = "bottomright",
          #       pal = dummy_color,
          #       values = c(0, 1),
          #       title = paste0(legend_title, "</br>", "No data available for this selection"),
          #       opacity = 1
          #     )
          #   } else {
          #     
          #     fillColor = colorNumeric(palette = brewer.pal(9, single_color_sequential_palettes[new_id_ii]), domain = df_list[[new_id]]$log_Concentration)
          #     
          #     m = m %>% map_fun_EA_WQ_gcms(df_list[[new_id]],
          #                                  fillColor =  ~fillColor(log_Concentration),
          #                                  legend_title= legend_title) %>%
          #       addLegend(data = df_list[[new_id]],
          #                 position = "bottomright",
          #                 pal = fillColor,
          #                 values = ~df_list[[new_id]]$log_Concentration,
          #                 title = paste0(legend_title ,"</br>","Concentration ug/l"),
          #                 opacity = 1,
          #                 group = legend_title,
          #                 labFormat = labFormat_transform)
          #   }
          #   
          # } else if (handler_list[[new_id]] == 'EA pollution inventory 2021') {
          #   #m = m %>% map_fun_EA_pollution(df_list[[new_id]],fillColor = color_data$RGB[new_id_ii])
          #   
          #   fillColor = colorNumeric(palette = brewer.pal(9, single_color_sequential_palettes[new_id_ii]), domain = df_list[[new_id]]$log_quantity_released_tons)
          #   
          #   m = m %>% map_fun_EA_pollution(df_list[[new_id]],
          #                                  fillColor =  ~fillColor(log_quantity_released_tons),
          #                                  legend_title= legend_title) %>%
          #     addLegend(data = df_list[[new_id]],
          #               position = "bottomright",
          #               pal = fillColor,
          #               values = df_list[[new_id]]$log_quantity_released_tons,
          #               title = paste0(legend_title ,"</br>","tonnes"),
          #               opacity = 1,
          #               group = legend_title,
          #               labFormat = labFormat_transform)
          #   
          #   
          # } else if (handler_list[[new_id]] == 'Predatory Bird Monitoring Scheme') {
          #   
          #   # added a trycatch for when there is no data for the selection
          #   m <- tryCatch({m %>% map_fun_pbms(df_list[[new_id]],
          #                                     colorPalette = single_color_sequential_palettes[new_id_ii],
          #                                     var_biota = df_list[[new_id]]$biota[1],
          #                                     legend_title= legend_title)
          #   }, error = function(e){
          #     dummy_palette <- colorNumeric(palette = "Greys", domain = c(0, 0)) # Dummy palette
          #     # add notification
          #     showNotification("No data available for this selection.", type = "error", duration = 5)
          #     
          #     # Add dummy legend with message
          #     m %>% addLegend("bottomright", pal = dummy_palette, values = c(0, 0),
          #                     title = paste(legend_title, "</br>No data available for this selection"),
          #                     opacity = 1)
          #   })
          #   
          # } else if (handler_list[[new_id]] == 'PFAS') {
          #   
          #   # Check if the dataset is empty or has no valid data
          #   if (nrow(df_list[[new_id]]) == 0 || all(is.na(df_list[[new_id]]$transform_value))) {
          #     
          #     dummy_palette <- colorNumeric(palette = "Greys", domain = c(0, 0)) # Dummy palette
          #     
          #     showNotification("No data available for this selection.", type = "error", duration = 5)
          #     
          #     # Add a message in the legend indicating no data
          #     m = m %>% addLegend(
          #       position = "bottomright",
          #       pal = dummy_palette,
          #       values = c(0, 0),
          #       title = paste0(legend_title, "</br>", "No data available for this selection"),
          #       opacity = 1
          #     )
          #   } else {
          #     #m = m %>% map_fun_pfas(df_list[[new_id]],fillColor = color_data$RGB[new_id_ii])
          #     # labFormat_transform = labelFormat(transform = function(x) round(exp(x) - 1, 1))
          #     
          #     selected_palette <- single_color_sequential_palettes[new_id_ii]
          #     
          #     fillColor = colorNumeric(palette = brewer.pal(9, selected_palette), domain = df_list[[new_id]]$transform_value)
          #     
          #     gradient_colors <- colorNumeric(
          #       palette = brewer.pal(9, selected_palette),
          #       domain = df_list[[new_id]]$transform_value
          #     )(seq(min(df_list[[new_id]]$transform_value, na.rm = TRUE),
          #           max(df_list[[new_id]]$transform_value, na.rm = TRUE),
          #           length.out = 256))
          #     
          #     m = m %>% map_fun_pfas(df_list[[new_id]],
          #                            fillColor = if (input$heatmap) gradient_colors else fillColor,  # Use gradient_colors in heatmap mode
          #                            legend_title= legend_title,
          #                            showHeatmap  = input$heatmap
          #     ) %>%
          #       addLegend(data = df_list[[new_id]],
          #                 position = "bottomright",
          #                 pal = fillColor,
          #                 values = df_list[[new_id]]$transform_value,
          #                 title = paste0(legend_title ,"</br>","ng/l"),
          #                 opacity = 1,
          #                 group = legend_title,
          #                 labFormat = labFormat_transform)
          #     
          #   }
          # } else if (handler_list[[new_id]] == "HadUK-Grid Annual Rainfall") {
          #   
          #   rain_values <- values(df_list[[new_id]])
          #   rain_values <- rain_values[!is.na(rain_values)]
          #   
          #   fillColor <- colorNumeric(
          #     palette = brewer.pal(9, single_color_sequential_palettes[new_id_ii]),
          #     domain=range(rain_values),
          #     na.color = "transparent"
          #   )
          #   
          #   m = m %>% map_fun_rain(df_list[[new_id]],
          #                          colors =  fillColor,
          #                          legend_title = legend_title) %>%
          #     addLegend(data = df_list[[new_id]],
          #               position = "bottomright",
          #               pal = fillColor,
          #               values = values(df_list[[new_id]]),
          #               title = paste0(legend_title ,"</br>","mm"),
          #               group = legend_title,
          #               na.label = NULL) 
          #   
          #   
          #   
          # } else if (handler_list[[new_id]] == 'APIENS') {
          #   #m = m %>% map_fun_EA_pollution(df_list[[new_id]],fillColor = color_data$RGB[new_id_ii])
          #   
          #   fillColor = colorNumeric(palette = brewer.pal(9, single_color_sequential_palettes[new_id_ii]), domain = df_list[[new_id]]$Value)
          #   
          #   m = m %>% map_fun_apiens(df_list[[new_id]],
          #                            fillColor =  ~fillColor(Value),
          #                            legend_title= legend_title) %>%
          #     addLegend(data = df_list[[new_id]],
          #               position = "bottomright",
          #               pal = fillColor,
          #               values = ~df_list[[new_id]]$Value,
          #               title = paste0(legend_title ,"</br>",unique(df_list[[new_id]]$Unit)),
          #               opacity = 1,
          #               group = legend_title)
          #   
          #   
          # }
            
          ## end add dataset
          
          
          # m = m %>%
          #   addMarkers(data = quakes[1:20,],~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))
          
          incProgress(1/length(inserted_ids), detail = paste("Adding map from dataset", new_id_ii)) # add counter
          
          print(head(df_list[[new_id]]))
          print(paste0(1:length(handler_list), '. ',handler_list))
        }
      })
    }
      
    # Add basemaps
      m %>%  
        addProviderTiles("NASAGIBS.ViirsEarthAtNight2012",
                                 group = "NASA Earth at Night 2012",
                                 options = providerTileOptions(noWrap = TRUE)) %>% 
        addRasterImage(gb_lcm_1km_dom_tar, opacity = 0.5, color = factpal,
                       group = "LCM 2021 1km dominant target"
        ) %>%
        addWMSTiles('https://catalogue.ceh.ac.uk/maps/cca47088-8cdd-4d7a-86b4-90f0a1766364?request=getCapabilities&service=WMS&cache=false&',
                    layers='HY.PhysicalWaters.Catchments.IHU_AreasWithCoastline',
                    options = WMSTileOptions(crs=27700,opacity=0.5),
                    group = 'IHU') %>%
        addLayersControl(baseGroups = c("OpenStreetMap", "ESRI World Imagery", 
                                        "CartoDB.Positron","Esri.NatGeoWorldMap",
                                        "OpenTopoMap","Stadia.StamenToner"#,
                                        #"GeoportailFrance.orthos"
                                        ), 
                             overlayGroups = c(paste0(1:length(inserted_ids), '. ',handler_list[1:length(inserted_ids)]),
                                               "IHU","LCM 2021 1km dominant target", "NASA Earth at Night 2012"),
                             position = 'topleft') 
     
  })  
  
  ###### indicator map #######
  ## regional averages ##
  regionMap = leaflet() %>% addTiles() %>% setView(-3.0, 55.5, zoom = 6)  %>% 
    addPolygons(
      data = NUTS_region_with_gcms_data,
      fillColor = ~colorQuantile("viridis", mean_concentration)(mean_concentration),
      fillOpacity = 0.6,
      weight = 1,
      color = "white",
      popup = ~paste("Compound: ", CompoundName, "<br>",
                     "Mean Concentration: ", round(mean_concentration, 2), "<br>",
                     "NUTS ID: ", NUTS_ID),
      group = "NUTS Level 1"
    ) %>%
    
    addRasterImage(gb_lcm_1km_dom_tar, opacity = 0.5, color = factpal,
                   group = "LCM 2021 1km dominant target"
    ) %>%
    addProviderTiles(providers$Stadia.StamenTonerLite,
                     options = providerTileOptions(noWrap = TRUE)
    ) %>%
    # addWMSTiles('https://map.sepa.org.uk/server/services/Open/Hydrography/MapServer/WMSServer?request=GetCapabilities&service=WMS',
    #             layers='12',
    #             options = WMSTileOptions(crs=27700,opacity=0.5),
    #             group = 'Scottish coastal areas') %>%  # have some trouble displaying, naming issues.
    # addWMSLegend('https://map.sepa.org.uk/server/services/Open/Hydrography/MapServer/WMSServer?request=GetLegendGraphic%26version=1.3.0%26format=image/png%26layer=12') %>%
    addWMSTiles('https://catalogue.ceh.ac.uk/maps/cca47088-8cdd-4d7a-86b4-90f0a1766364?request=getCapabilities&service=WMS&cache=false&',
                layers='HY.PhysicalWaters.Catchments.IHU_AreasWithCoastline',
                options = WMSTileOptions(crs=27700,opacity=0.5),
                group = 'IHU') %>%
    # addWMSTiles('https://catalogue.ceh.ac.uk/maps/032da3fa-10ba-42cc-b719-b19b6dfd11f5?request=getCapabilities&service=WMS&cache=false&',
    #             layers=c('LC.25m.GB', 'LC.25m.NI'),
    #             options = WMSTileOptions(crs=27700,opacity=0.5),
    #             group = 'Land cover map 2018 25m') %>%
    #  leaflet.extras::addWMSLegend('https://catalogue.ceh.ac.uk/maps/032da3fa-10ba-42cc-b719-b19b6dfd11f5?language=eng&version=1.3.0&service=WMS&request=GetLegendGraphic&sld_version=1.1.0&layer=WMS&format=image/png&STYLE=inspire_common:DEFAULT') %>%
    addLegend(
      position = "bottomright",
      colors = rgb(t(col2rgb(color_data$RGB)) / 255),
      labels = color_data$Class, opacity = 1,
      title = "LCM classes",
      group = "LCM 2021 1km dominant target"
    ) %>%
    addLayersControl(
      overlayGroups = c("NUTS Level 1", "LCM 2021 1km dominant target", "IHU"),
      #overlayGroups = c("base" ),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    hideGroup(c("LCM 2021 1km dominant target", "IHU","Land cover map 2018 25m")) %>%
    addControl(rr, position = "bottomleft")
  
  
  
  ######################### indicator map by nation##  SLOW #########
  uk_country <- read_sf("datasets/infuse_ctry_2011")
  uk_country <- st_transform(uk_country , 4326)
  uk_country <- rmapshaper::ms_simplify(uk_country) # maybe save this to be faster

  uk_country$colour <- c('#d7a12f','#9EC979','red','red')
  uk_country$`Terrestrial` <- c(0.62,0.5,0.73,0.7)
  uk_country$`Freshwater` <- c(0.8,0.5,0.83,0.77)
  uk_country$`Marine` <- c(0.22,0.2,0.72,0.5)
  uk_country$`Air` <- c(0.42,0.36,0.6,0.6)
  uk_country$`Cu_mean` <- c(20.8,13.2,16.9,18.0)
  uk_country$`Cd_mean` <- c(0.42,0.36,0.6,0.6)
  uk_country$`Zn_mean` <- c(0.42,0.36,0.6,0.6)
  uk_country$`spears` <- c(NA,NA,0.55,NA)
  uk_country$`Paracetamol` <- c(NA,107.3,75.79,93.97)
  uk_country$`Trimethoprim` <- c(NA,0.98,2.866,0.9)
  
  
  
  indicatorNationMap = leaflet(uk_country) %>% addTiles() %>% setView(-3.0, 55.5, zoom = 6)  %>%
    addPolygons(color = '#A9A9A9', weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                fillColor = ~colour,
                label = ~paste0(as.character(geo_label)),
                popup = ~paste0("<b><h2>",as.character(geo_label),"</h2></b>",
                                hr(style = "border: 1px solid black;"),
                                "<br/>","Terrestrial: ","<b text-align='right'>", sprintf("%.1f%%", Terrestrial * 100),"</b>",
                                "<br/>","Freshwater: ","<b >",sprintf("%.1f%%", Freshwater * 100),"</b>",
                                "<br/>","Marine: ","<b>", sprintf("%.1f%%", Marine * 100),"</b>",
                                "<br/>","Air: ","<b>", sprintf("%.1f%%", Air * 100),"</b>",
                                p(),
                                "<br/>","Mean Copper (mg kg<sup>-1</sup>): ","<b>", sprintf("%.1f", Cu_mean),"</b>",
                                "<br/>","Mean Paracetamol in estuaries (ng l<sup>-1</sup>): ","<b>", sprintf("%.1f", Paracetamol),"</b>",
                                "<br/>","Mean Trimethoprim in estuaries (ng l<sup>-1</sup>): ","<b>", sprintf("%.1f", Trimethoprim),"</b>",
                                
                                "<br/>","SPEAR<sub>pesticide</sub> for Summer 2019: ","<b>", sprintf("%.1f", spears),"</b>",
                                p(),
                                "<br/>","SPEAR<sub>pesticide</sub> value is derived from  <a href='https://doi.org/10.1016/j.scitotenv.2023.166519' target='_blank'>Poyntz-Wright et al. 2023 </a>",
                                "<br/>","(corrected) pharmaceutical values are derived from <a href='https://doi.org/10.1016/j.scitotenv.2019.04.182' target='_blank'>Lestingers et al. 2019</a>",
                                
                                "<p><font color='#c2c5cc'> &copy;" ,format(Sys.Date(), "%Y"),
                                " UK Centre for Ecology and Hydrology </font></p>"
                ),
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE))



  #output$regionMap = renderLeaflet(indicatorNationMap)
  # output$regionMap = renderLeaflet(regionMap)
  
  observeEvent(input$RegtionOption, {
    if(input$RegtionOption == "Mean Phenanthrene by region"){
      output$regionMap = renderLeaflet(regionMap)
    } else{
      output$regionMap = renderLeaflet(indicatorNationMap)
    }
  })
  
  
  output$barplot_indicator <- renderPlotly({
    pressures = data.frame(
      fieldname = c('Pesticides','Pharmaceuticals','Vet. medicine','Heavey metals', 'Predatory birds','Invertebrates','Land Use', 'Flooding'),
      value = c(0.35,0.77,0.1,0.79,0.12,0.6,0.38,0.1)
    )
    pressures$value_rev = 1.0-pressures$value
    
    {ggplot(pressures %>% pivot_longer(cols = value:value_rev)%>% arrange(fieldname)) +
      geom_bar(aes(x=fieldname, y=value,fill=name ),stat = "identity" ) +
      theme_minimal() + theme(legend.position="none", axis.title=element_blank())+
      scale_fill_manual(values=c('#9EC979','#B91E22')) + 
      ggtitle(paste0(input$countryInd,': ',  input$compartmentInd, ' (illustrative)'))+
      coord_flip()} %>% ggplotly()
  })
  
  
  
}

# options(shiny.sanitize.errors = FALSE)
options(shiny.reactlog=TRUE) #ctrl+F3 to bring up
shinyApp(ui, server)


