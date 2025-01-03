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

source('data_fun.R')
source('map_fun.R')
source('modules/data_modules.R')
source('theme_elements.R')
# data(quakes)


rr <- htmltools::HTML('<a href="https://ceh.ac.uk/" target="_blank"> <img border="0" alt="ImageTitle" src="https://brandroom.ceh.ac.uk/sites/default/files/images/theme/UKCEH-Logo_Long_Pos_RGB_720x170.png" width="auto" height="40"> </a>')


link_shiny <- tags$a(shiny::icon("github"), "Code", href = "https://github.com/NERC-CEH/sli_visualTool", target = "_blank")
link_posit <- tags$a(shiny::icon("book"), "Documentation", href = "https://github.com/NERC-CEH/sli_visualTool", target = "_blank")
link_guide <- tags$a(shiny::icon("book"), "User guide", href = "SLI_state of the environment approach.pdf", target = "_blank")


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
  title = 'Systems Level Indicator Visual Tool',
  #theme = bs_theme(version = 5),
  theme = UKCEH_theme,  # << add this line
  #Application title
  UKCEH_titlePanel("Chemical Pollution and the Environment"),
  #h1('Chemcial Pollution and the Environment'),
  ## or use page_navbar
  # HTML('<p align="center" style="font-weight: bold;color:orange">For Demonstration purposes only. Under development.</p>'),
  navset_underline(
    nav_panel(title = "Spatial Trends", 
              card(
                #card_header("Card with sidebar"),
                layout_sidebar(
                  sidebar = sidebar(width = 400,
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
                              
                              leafletOutput('myMap',height = 650) %>% withSpinner(type=5,color = "#A9A9A9"),
                              accordion(
                                accordion_panel(
                                  "Map controls",
                                  "Coming soon"#,
                                  # input_switch("marker_use_colourmap", "Colour markers by value.", TRUE)
                                ),
                                open=FALSE
                              )
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
                                    selectInput("RegtionOption", "Choose indicator:",
                                                'Chemical Pollution Indicator'
                                    )
                  ),
                  # value_box(
                  #   title = "",
                  #   value = "62%",
                  #   showcase = bs_icon("pie-chart"),
                  #   p('of land has 1 or more unhealthy'),
                  #   p('pollution indicators.'),
                  #   theme = "info"
                  # ),
                  leafletOutput('regionMap',height = 650) 
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
    nav_panel(title = "Chemical History Timeline", p('Coming soon.')),
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
      ## pass in appropriate div id, for data table
      selector = paste0('#',inserted_ids[length(inserted_ids)], '_plots')
    )
    
    # remove associated datasets
    reactive_df[[inserted_ids[length(inserted_ids)]]] <- NULL
    
    # remove the id from list
    inserted_ids <<- inserted_ids[-length(inserted_ids)]
    
    
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
    addProviderTiles(providers$GeoportailFrance.orthos,
                     group = "GeoportailFrance.orthos",
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
                                    "GeoportailFrance.orthos"),
                     overlayGroups = c("IHU","LCM 2021 1km dominant target", "NASA Earth at Night 2012"),
                     position = 'topleft') %>%
    hideGroup(c("LCM 2021 1km dominant target", "IHU", "NASA Earth at Night 2012")) %>%
    setView(-3.0, 55.5, zoom = 6) 
  output$myMap = renderLeaflet(map)
  
  observeEvent(input$updateBtn, {
    print('press update button')
    m = leafletProxy("myMap") %>% 
      removeLayersControl() %>% 
      clearShapes() %>% 
      clearControls() %>% 
      clearMarkers() %>%
      clearHeatmap() %>%
      clearImages()
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
    single_color_sequential_palettes <- c("Reds", "Blues", "Greens", "Purples", "Oranges", "Greys")
    labFormat_transform = labelFormat(transform = function(x) round(exp(x) - 1, 1))
    
    # withProgress(message = 'Making plot', value = 0, {
    if (length(inserted_ids) > 0) {  
      for (new_id_ii in 1:length(inserted_ids)){
        
        new_id = paste("dat1_ctrl", new_id_ii , sep = "_")
        legend_title= paste0(as.character(new_id_ii) ,". ", handler_list[[new_id]] )
        
        m = m %>%
        addMarkers(data = quakes[1:20,],~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))
        
        print(head(df_list[[new_id]]))
        
        # incProgress(1/length(inserted_ids), detail = paste("Adding map from dataset", new_id_ii))
        
        
        if (handler_list[[new_id]] == 'EA water quality GCMS/LCMS data') {
          
          # Check if the dataset is empty or has no valid data
          if (nrow(df_list[[new_id]]) == 0 || all(is.na(df_list[[new_id]]$log_Concentration))) {
            dummy_color <- colorNumeric(palette = "Greys", domain = c(0, 1))
            # Add a message in the legend indicating no data
            m = m %>% addLegend(
              position = "bottomright",
              pal = dummy_color, 
              values = c(0, 1), 
              title = paste0(legend_title, "</br>", "No data available for this selection"),
              opacity = 1
            )
          } else {
            
            fillColor = colorNumeric(palette = brewer.pal(9, single_color_sequential_palettes[new_id_ii]), domain = df_list[[new_id]]$log_Concentration)
            
            m = m %>% map_fun_EA_WQ_gcms(df_list[[new_id]], 
                                         fillColor =  ~fillColor(log_Concentration),
                                         legend_title= legend_title) %>% 
              addLegend(data = df_list[[new_id]], 
                        position = "bottomright", 
                        pal = fillColor, 
                        values = ~df_list[[new_id]]$log_Concentration, 
                        title = paste0(legend_title ,"</br>","Concentration ug/l"), 
                        opacity = 1,
                        group = legend_title,
                        labFormat = labFormat_transform)
          }
          
        } else if (handler_list[[new_id]] == 'EA pollution inventory 2021') {
          #m = m %>% map_fun_EA_pollution(df_list[[new_id]],fillColor = color_data$RGB[new_id_ii])
          
          fillColor = colorNumeric(palette = brewer.pal(9, single_color_sequential_palettes[new_id_ii]), domain = df_list[[new_id]]$log_quantity_released_tons)
          
          m = m %>% map_fun_EA_pollution(df_list[[new_id]],
                                         fillColor =  ~fillColor(log_quantity_released_tons),
                                         legend_title= legend_title) %>% 
            addLegend(data = df_list[[new_id]], 
                      position = "bottomright", 
                      pal = fillColor, 
                      values = df_list[[new_id]]$log_quantity_released_tons, 
                      title = paste0(legend_title ,"</br>","tonnes"), 
                      opacity = 1,
                      group = legend_title,
                      labFormat = labFormat_transform)
          
          
        } else if (handler_list[[new_id]] == 'Predatory Bird Monitoring Scheme') {
          
          # added a trycatch for when there is no data for the selection
          m <- tryCatch({m %>% map_fun_pbms(df_list[[new_id]], 
                                            colorPalette = single_color_sequential_palettes[new_id_ii], 
                                            var_biota = df_list[[new_id]]$biota[1],
                                            legend_title= legend_title)
          }, error = function(e){
            dummy_palette <- colorNumeric(palette = "Greys", domain = c(0, 0)) # Dummy palette
            # add notification
            showNotification("No data available for this selection.", type = "error", duration = 5)
            
            # Add dummy legend with message
            m %>% addLegend("bottomright", pal = dummy_palette, values = c(0, 0),
                            title = paste(legend_title, "</br>No data available for this selection"),
                            opacity = 1)
          })
          
        } else if (handler_list[[new_id]] == 'PFAS') {
          
          # Check if the dataset is empty or has no valid data
          if (nrow(df_list[[new_id]]) == 0 || all(is.na(df_list[[new_id]]$transform_value))) {
            
            dummy_palette <- colorNumeric(palette = "Greys", domain = c(0, 0)) # Dummy palette
            
            showNotification("No data available for this selection.", type = "error", duration = 5)
            
            # Add a message in the legend indicating no data
            m = m %>% addLegend(
              position = "bottomright",
              pal = dummy_palette, 
              values = c(0, 0), 
              title = paste0(legend_title, "</br>", "No data available for this selection"),
              opacity = 1
            )
          } else {
            #m = m %>% map_fun_pfas(df_list[[new_id]],fillColor = color_data$RGB[new_id_ii])
            # labFormat_transform = labelFormat(transform = function(x) round(exp(x) - 1, 1))
            
            selected_palette <- single_color_sequential_palettes[new_id_ii]
            
            fillColor = colorNumeric(palette = brewer.pal(9, selected_palette), domain = df_list[[new_id]]$transform_value)
            
            gradient_colors <- colorNumeric(
              palette = brewer.pal(9, selected_palette),
              domain = df_list[[new_id]]$transform_value
            )(seq(min(df_list[[new_id]]$transform_value, na.rm = TRUE), 
                  max(df_list[[new_id]]$transform_value, na.rm = TRUE), 
                  length.out = 256))
            
            m = m %>% map_fun_pfas(df_list[[new_id]], 
                                   fillColor = if (input$heatmap) gradient_colors else fillColor,  # Use gradient_colors in heatmap mode
                                   legend_title= legend_title,
                                   showHeatmap  = input$heatmap
                                   ) %>% 
              addLegend(data = df_list[[new_id]], 
                        position = "bottomright", 
                        pal = fillColor, 
                        values = df_list[[new_id]]$transform_value, 
                        title = paste0(legend_title ,"</br>","ng/l"), 
                        opacity = 1,
                        group = legend_title,
                        labFormat = labFormat_transform)
            
          }
        } else if (handler_list[[new_id]] == "HadUK-Grid Annual Rainfall") {
          
          rain_values <- values(df_list[[new_id]])
          rain_values <- rain_values[!is.na(rain_values)]
          
          fillColor <- colorNumeric(
            palette = brewer.pal(9, single_color_sequential_palettes[new_id_ii]),
            domain=range(rain_values),
            na.color = "transparent" 
          )
          
          m = m %>% map_fun_rain(df_list[[new_id]],
                                 colors =  fillColor)
        } else if (handler_list[[new_id]] == 'APIENS') {
          #m = m %>% map_fun_EA_pollution(df_list[[new_id]],fillColor = color_data$RGB[new_id_ii])
          
          fillColor = colorNumeric(palette = brewer.pal(9, single_color_sequential_palettes[new_id_ii]), domain = df_list[[new_id]]$Value)
          
          m = m %>% map_fun_apiens(df_list[[new_id]],
                                         fillColor =  ~fillColor(Value),
                                         legend_title= legend_title) %>% 
            addLegend(data = df_list[[new_id]],
                      position = "bottomright",
                      pal = fillColor,
                      values = ~df_list[[new_id]]$Value,
                      title = paste0(legend_title ,"</br>",unique(df_list[[new_id]]$Unit)),
                      opacity = 1,
                      group = legend_title)
          
          
        }
      } 
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
        addLayersControl(baseGroups = c("OpenStreetMap", "ESRI World Imagery", "GeoportailFrance.orthos"), 
                             overlayGroups = c(paste0(1:length(inserted_ids), '. ',handler_list[1:length(inserted_ids)]),
                                               "IHU","LCM 2021 1km dominant target", "NASA Earth at Night 2012"),
                             position = 'topleft') 
      
      print(paste0(1:length(handler_list), '. ',handler_list))
      
    }
    # })
  }
  )
  
  
  ###### indicator map #######
  
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
  output$regionMap = renderLeaflet(regionMap)
  
}

# options(shiny.sanitize.errors = FALSE)
options(shiny.reactlog=TRUE) #ctrl+F3 to bring up
shinyApp(ui, server)


