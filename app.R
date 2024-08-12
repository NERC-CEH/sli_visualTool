#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# 
# Here are the links to WMS tile layers that we can add
# 
# SEPA Marine etc. -- not OK, some issues with naming
# 
# https://spatialdata.gov.scot/geonetwork/srv/eng/catalog.search#/metadata/61a675be-a5a7-4e8f-8068-b0cb8f5e8cbb
# 
# LCM 2018--OK
# https://catalogue.ceh.ac.uk/documents/032da3fa-10ba-42cc-b719-b19b6dfd11f5

library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(shinyWidgets)
library(raster)
library(readxl)
library(dplyr)
library(sp)
library(sf)
library(DT)
library(plotly)
#library(sortable)

# use renv::install('<pakcage name>') to install more packages

source('theme_elements.R')
source("data_fun.R")


rr <- htmltools::HTML('<a href="https://ceh.ac.uk/" target="_blank"> <img border="0" alt="ImageTitle" src="https://brandroom.ceh.ac.uk/sites/default/files/images/theme/UKCEH-Logo_Long_Pos_RGB_720x170.png" width="auto" height="40"> </a>')


# Define UI for application that draws a histogram
ui <- fluidPage(
    #theme
    theme = UKCEH_theme,  # << add this line
    # Application title
    UKCEH_titlePanel("Chemical Pollution and the Environment"),
    
    tabsetPanel(
      tabPanel("Introduction",
               p(),
               p('Placeholder for text/image/video to explain the case study and the app')
               #includeHTML('data_source.html')
      ),
      tabPanel("Overview",
        hr(),
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
            sidebarPanel(
              h4(""), hr(),
                # sliderInput("bins",
                #             "Number of bins:",
                #             min = 1,
                #             max = 50,
                #             value = 30),
           
            p("Menu:"),
            prettySwitch(inputId = "switch1", label = "Map layer view"),
            prettySwitch(inputId = "switch2", label = "Land use view"),
            p("Search by location:"),
            selectInput("country_choice", "Choose country:",
                        c("England" = "ENG",
                          "Northern Ireland"="NI",
                          "Scotland" = "SCOT",
                          "Wales" = "WAL")),
            # selectInput("region_choice", "Choose a state:",
            #             list(`East Coast` = list("NY", "NJ", "CT"),
            #                  `West Coast` = list("WA", "OR", "CA"),
            #                  `Midwest` = list("MN", "WI", "IA"))
            # ),
            searchInput(
              inputId = "gridref_choice", 
              label = "Enter your grid reference :", 
              placeholder = "SD821672", 
              btnSearch = icon("search"), 
              btnReset = icon("remove"), 
              width = "100%"
            )
          ),
            
            # Show a plot of the generated distribution
            mainPanel(
              leafletOutput("mymap", height = '150%'),
              p(),
               # plotOutput("distPlot")
            )
        )
      ),
      tabPanel("Point data",
               h3("Pollutant point data"),
               sidebarLayout(
                 sidebarPanel(
                   actionButton('add_dataset_btn','Add a dataset',class='btn-primary', icon("paper-plane")),
                   actionButton('add_dataset_btn','Remove a dataset',class='btn-warning', icon("xmark")),
                   wellPanel(
                     selectInput("data_choice", "Choose dataset:", 
                                 choices = c("EA water quality GCMS data", "EA pollution inventory 2021")),
                     uiOutput("dynamic_select") #,
                     # selectInput("gcms_compound", "Choose compound:",
                     #             c("Phenanthrene", "Benzothiazole", "Cocaine", "Ibuprofen", "2,4,7,9-Tetramethyl-5-decyne-4,7-diol")
                     #             ),
                     # selectInput("substance", "Choose pollutant:", 
                     #             c("Ammonia", "Arsenic", "Mercury", "Particulate matter - total", "Phenols - total as C", "Pentachlorophenol (PCP)", "Perfluoro octanyl sulphate (PFOS)", "Polychlorinated biphenyls (PCBs)" )
                     #            ),
                   
                   )
                   ),
                 mainPanel(
                   verticalLayout(
                     leafletOutput("pollutant_map"),
                     plotlyOutput("plot")
                   ),
                 )
               )
      ),
      tabPanel("Data table",
               h3("Selected data"),
               DTOutput("outputTable"),
               style = "width: 1200px;"
        
      ),
      tabPanel("Data sources",
         #includeHTML('data_source.html')
      ),
    )
)  

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    # import LCM
    gb_lcm_1km_dom_tar <- raster("./datasets/LCM/gb2021lcm1km_dominant_target.tif")
    gb_lcm_1km_dom_tar[gb_lcm_1km_dom_tar == 0] <- NA
    gb_lcm_1km_dom_tar$gb2021lcm1km_dominant_target = gb_lcm_1km_dom_tar$gb2021lcm1km_dominant_target %>% as.factor() # important for colour mapping to work!
    
    CompoundName <-  "Phenanthrene"
    NUTS_region <- get_NUTS_regions(NUTS_lvl_code = 1)
    NUTS_region_with_gcms_data <- data_process_EA_WQ_gcms_with_NUTS(fp_gcms_withNUTS = './datasets/EA_water_quality_GCMS_LCMS/gcms_data_with_NUTS.csv', NUTS_region = NUTS_region, CompoundName = "Phenanthrene")
    
    # LCM colour paletted
    factpal <- colorFactor(color_data$RGB, values(gb_lcm_1km_dom_tar$gb2021lcm1km_dominant_target), na.color = "transparent")
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
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
        
        addRasterImage(gb_lcm_1km_dom_tar, opacity = 0.65, color = factpal,
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
         ###LCM 25m is too slow to load
          # addWMSTiles('https://catalogue.ceh.ac.uk/maps/032da3fa-10ba-42cc-b719-b19b6dfd11f5?request=getCapabilities&service=WMS&cache=false&',
          #             layers=c('LC.25m.GB', 'LC.25m.NI'),
          #             options = WMSTileOptions(crs=27700,opacity=0.5),
          #             group = 'Land cover map 2018 25m') %>% 
        #  leaflet.extras::addWMSLegend('https://catalogue.ceh.ac.uk/maps/032da3fa-10ba-42cc-b719-b19b6dfd11f5?language=eng&version=1.3.0&service=WMS&request=GetLegendGraphic&sld_version=1.1.0&layer=WMS&format=image/png&STYLE=inspire_common:DEFAULT') %>% 
        addLegend(
          position = "bottomright",
          colors = rgb(t(col2rgb(color_data$RGB)) / 255),
          labels = color_data$Class, opacity = 1,
          title = "LCM classes",group = 'LCM 2021 1km dominant target'
        ) %>%   
        setView(-2.7, 54.7, zoom = 4.5) %>%
          # addLegend(pal = pal, values = ~log10(bird_chem_carcass_data_noLocationNA$`BDE 47`), 
          #           title = ~'Bird BDE conc ng/g\n wet weight',
          #           #label = c('3.16','10.0','31.6','100.0','316.2'),
          #           labFormat = labelFormat(prefix = "10^"),
          #           opacity = 1,   na.label = "Non detected") %>% 
        
          addLayersControl(
            overlayGroups = c("NUTS Level 1", "LCM 2021 1km dominant target", "IHU","Land cover map 2018 25m"),
            #overlayGroups = c("base" ),
            options = layersControlOptions(collapsed = FALSE)
          ) %>%
          hideGroup(c("LCM 2021 1km dominant target", "IHU","Land cover map 2018 25m")) %>% 
          addControl(rr, position = "bottomleft") 
    })
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    
    
    # tab 2
    
    reactive_df <- reactiveValues(data = NULL)
    result <- data_process_EA_pollution()
    unique_industry_sector <- result[[2]]
    
    
    # Function to run when Tab 2 is selected
    observeEvent(input$tabsetPanel, {
      req(input$tabsetPanel)
      if(input$tabsetPanel == "Point data") {
        result <- data_process_EA_pollution()
        print(result)
        filtered_data <- result[[1]]
        unique_industry_sector <- result[[2]]
        
        # Update the reactive dataframe
        reactive_df$data <- filtered_data
        
        # update input choices
        updateSelectInput(session, "substance", choices = unique_industry_sector)
        }
    })
    
    # choice for which data to plot
    observeEvent(input$data_choice, {
      if(input$data_choice == "EA pollution inventory 2021") {
        output$dynamic_select <- renderUI({
          selectInput("IndustrySector", "Choose Industry Sector:",
                      #c("Ammonia", "Arsenic", "Mercury", "Particulate matter - total", "Phenols - total as C", "Pentachlorophenol (PCP)", "Perfluoro octanyl sulphate (PFOS)", "Polychlorinated biphenyls (PCBs)")
                      unique_industry_sector
          )        
        })
      } else if(input$data_choice == "EA water quality GCMS data") {
        output$dynamic_select <- renderUI({
          tagList(
            selectInput("gcms_compound", "Choose compound:",
                        c("Phenanthrene", "Benzothiazole", "Cocaine", "Ibuprofen", "2,4,7,9-Tetramethyl-5-decyne-4,7-diol")
                        ),
            sliderInput("year_slider", "Select Year Range:",
                        min = min(2013), max = max(2021), 
                        value = c("2020", "2021")
            )
          
          )
        })
      }
    })
    
    
    observeEvent(input$IndustrySector, {
      # Generate the filtered data based on the selected substance
      result <- data_process_EA_pollution(IndustrySector = input$IndustrySector)
      filtered_data <- result[[1]]
      
      # Update the reactive dataframe
      reactive_df$data <- filtered_data
      
      # Generate Leaflet map based on the filtered data
      output$pollutant_map <- renderLeaflet({
        leaflet(filtered_data) %>%
          addTiles() %>%
          setView(-2.7, 54.7, zoom = 4.5) %>%
          addControl(html = paste("<h5>", input$IndustrySector, "</h5>", sep = ""), position = "topright") %>%
          addCircleMarkers(
            lng = ~Longitude, lat = ~Latitude, radius = ~radius,
            popup = ~paste(
              "<b>Substance Name: </b>", substance_name, "<br/>",
              "<b>Quantity Released: </b>", quantity_released_kg, "kg<br/>",
              "<b>Sector: </b>", Regulated_Industry_Sector, "<br/>",
              "<b>Route: </b>", `ROUTE NAME`, "<br/>",
              sep = ""
            ),
            color = "black",   # Outline color
            fillColor = "blue", # Fill color
            fillOpacity = 0.5, # Opacity of the fill color
            weight = 0.5 
          )
      })
    })
    
    observeEvent(c(input$year_slider, input$gcms_compound),{
      filtered_data_gcms <- data_process_EA_WQ_gcms(CompoundName = input$gcms_compound)
      
      filtered_data_gcms <- filtered_data_gcms %>% filter(year >= input$year_slider[1], year <= input$year_slider[2])
      
      # Update the reactive dataframe
      reactive_df$data <- filtered_data_gcms
      
      # Generate Leaflet map based on the filtered data
      output$pollutant_map <- renderLeaflet({
        leaflet(filtered_data_gcms) %>%
          addTiles() %>%
          addRasterImage(gb_lcm_1km_dom_tar, opacity = 0.5,
                         colors = color_data$RGB, 
                         group = "LCM 2021 1km dominant target") %>% 
          setView(-2.7, 54.7, zoom = 4.5) %>%
          addControl(html = paste("<h5>", input$gcms_compound, " ", input$year_slider[1], " - ",  input$year_slider[2],  "</h5>", sep = ""), position = "topright") %>%
          addCircleMarkers(
            lng = ~Longitude, lat = ~Latitude, radius = ~radius,
            popup = ~paste(
              "<b>Compound Name: </b>", Compound_Name, "<br/>",
              "<b>Concentration: </b>", Concentration, "<br/>",
              "<b>Unit: </b>", unit, "<br/>",
              "<b>Sample description: </b>", SMC_DESC, "<br/>",
              "<b>Date: </b>", Sample_datetime, "<br/>",
              sep = ""
            ),
            color = "black",   # Outline color
            fillColor = "blue", # Fill color
            fillOpacity = 0.5, # Opacity of the fill color
            weight = 0.5 
          ) %>% 
        addLayersControl(
          overlayGroups = c("LCM 2021 1km dominant target"),
          #overlayGroups = c("base" ),
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
          hideGroup(c("LCM 2021 1km dominant target"))
      })
        # addLegend(pal = paste("'", legend_colors, "'", sep = ""), values = legend_labels,
        #           position = "bottomleft", title = "Land Cover Legend")
      
    })
    
    
    observeEvent(input$map_marker_click, {
      if(input$data_choice == "EA water quality GCMS data") {
        event <- input$map_marker_click
        if (!is.null(event)) {
          clicked_longitude <- event$lng
          clicked_latitude <- event$lat
          
          selected_data <- filtered_data_gcms[filtered_data_gcms$Longitude == clicked_longitude & filtered_data_gcms$Latitude == clicked_latitude, ]
          
          selected_data <- selected_data %>%  filter(n() > 1)
          
          # Convert datetime string to POSIXct
          selected_data$Sample_datetime <- as.POSIXct(selected_data$Sample_datetime, format = "%Y-%m-%d %H:%M:%S")
          
          selected_data <- selected_data[order(selected_data$Sample_datetime), ]
          
          print(selected_data)
          
          output$plot <- renderPlotly({
            plot_ly(selected_data, x = ~Sample_datetime, y = ~Concentration, type = "scatter", mode = "lines+markers") %>%
              layout(title = "Time Series Plot", xaxis = list(title = "Datetime"), yaxis = list(title = "Concentration"))
          })
        }
      }
    })
    
    # Render the dataframe in the reactive table
    output$outputTable  <- renderDT({
      datatable(reactive_df$data, options = list(scrollX = TRUE))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
