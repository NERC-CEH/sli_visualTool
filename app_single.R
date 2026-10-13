## minimal version of SLI tool
## to demonstrate loading a single slider, data_fun, and map_fun to handle one dataset.
## change the 4 places listed below to adpat to a new dataset

library(shiny)
library(bslib)
library(bsicons)
library(leaflet)
library(DT)
library(raster) # to retire?
library(terra)
library(dplyr)
library(leaflet.extras)
library(leafem)
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
library(shinymanager) # https://stackoverflow.com/questions/28987622/starting-shiny-app-after-password-input
library(leaflegend)
library(nanoparquet)
library(lubridate)
library(forcats) # factor re-ordering
library(rnaturalearth)
library(bsicons)



source('data_fun.R')
source('map_fun.R')
source('modules/data_modules.R')
source('modules/chemical_timeline_tab_aug.R')
source('modules/timeline_fun_Aug.R')
source('theme_elements.R')
# data(quakes)



rr <- htmltools::HTML('<a href="https://ceh.ac.uk/" target="_blank"> <img border="0" alt="ImageTitle" src="https://www.ceh.ac.uk/sites/default/files/images/theme/ukceh_logo_long_720x170_rgb.png" width="auto" height="40"> </a>')


link_shiny <- tags$a(shiny::icon("github"), "Code", href = "https://github.com/NERC-CEH/sli_visualTool", target = "_blank")
link_posit <- tags$a(shiny::icon("book"), "Documentation", href = "https://github.com/NERC-CEH/sli_visualTool", target = "_blank")
# link_guide <- tags$a(shiny::icon("book"), "User guide", href =  "https://cehacuk.sharepoint.com/:w:/r/sites/UKCEHJNCCsystemlevelindicators/Shared%20Documents/General/Visual%20tool%20manual.docx?d=wa4d3314bc019426bb921f088dd4a23ef&csf=1&web=1&e=yhHsic", target = "_blank")
link_guide <- tags$a(shiny::icon("book"), "User guide", href =  "Visual tool manual.pdf", target = "_blank")


##import LCM
gb_lcm_1km_dom_tar <- raster("./datasets/LCM/gb2021lcm1km_dominant_target.tif")
gb_lcm_1km_dom_tar[gb_lcm_1km_dom_tar == 0] <- NA
gb_lcm_1km_dom_tar$gb2021lcm1km_dominant_target = gb_lcm_1km_dom_tar$gb2021lcm1km_dominant_target %>% as.factor()
factpal <- colorFactor(color_data$RGB, values(gb_lcm_1km_dom_tar), na.color = 'transparent')



ui <- page_fillable(      

  tags$head(tags$style(".rightAlign{float:right;}")),
  tags$head(tags$style(HTML("
  .bslib-full-screen-enter {
    --bslib-color-fg: #ffffff;   /* icon color */
    --bslib-color-bg: #0483A4;   /* button background */
  }
  "))),
  title = 'Systems Level Indicator Visual Tool',
  #theme = bs_theme(version = 5),
  theme = UKCEH_theme,  # << add this line
  #Application title
  UKCEH_titlePanel("Chemical Pollution and the Environment", p("beta")),
  tags$html(lang = "en"), # modification for accessibility
  tags$head(tags$title("Chemical Pollution and the Environment")), # modification for accessibility
  navset_underline(
    nav_panel(title = "Spatial Trends",
              card(
                layout_sidebar(
                  sidebar = sidebar(width = 400,
                        Hunter_NI_sliders(NULL)               # 1. change the slider
                  ),
                  navset_card_underline(
                    nav_spacer(),
                    
                    nav_panel("Map",
                              leafletOutput('myMap',height = 800) %>% withSpinner(type=5,color = "#A9A9A9") #,
                    ), full_screen = TRUE
                  )
                )
              )),
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
  # 2. change the data_fun
  filtered_data <- reactive({
    data_process_Hunter_NI(NameDeterminandName= input$Hunter_NI_choice, 
                                                   RQ =input$toggle_RQ)
  })
  
 
  
  
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
  
  output$myMap = renderLeaflet({
    
    # this is what handled in switch_map in the main app
    
    # 3. change the map_fun
    
    legend_title = "Dataset"
    
    data = filtered_data()
    
    validate(need(is.numeric(data$Value) && any(!is.na(data$Value)),
                  "Selected variable has no numeric values to map."))
    
    fillColor = colorNumeric(palette = brewer.pal(9, 'Blues'), domain = data$Value)
    map = map %>% map_fun_Hunter_NI(data = data, 
                                    fillColor=fillColor,
                                    cluster_points = input$toggle_clustering,
                                    legend_title = legend_title
                                    )
    
    # 4. Add legend, change according to the map
    map = map %>% addLegendNumeric(
      data = data, ### <<< be careful with this line
      position = "bottomright",
      pal = fillColor,
      values = ~data$Value, ### <<< be careful with this line
      title = htmltools::HTML(paste0(
        legend_title, "<br>", "Concentration ng/l"
      )),
      shape = "rect",
      orientation = "horizontal",
      width = 200,
      height = 10 #,
      # numberFormat = function(x)
      #   format(round(exp(x) - 1, 3), trim = TRUE)
    )
    
    map
    })
  


  

  
}

shinyApp(ui, server)


