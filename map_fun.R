# functions for the leaflet map


# Create a data frame for colour-blind friendly LCM colour palette with the RGB values and class names
color_data_CB <- data.frame(
  Class = c("Broadleaved woodland", "Coniferous woodland", "Arable", "Improved grassland", "Neutral grassland",
            "Calcareous grassland", "Acid grassland", "Fen, Marsh and Swamp", "Heather and shrub",
            "Heather grassland", "Bog", "Inland rock", "Saltwater", "Freshwater", "Supralittoral rock",
            "Supralittoral sediment", "Littoral rock", "Littoral sediment", "Saltmarsh", "Urban", "Suburban"),
  RGB = c("#3E3D32", "#004433", "#ECF2BD", "#FEAA73", "#DFE6EC", "#99C9DD", "#E9D6EC", "#76835C",
          "#806480", "#B29278", "#9BB291", "#B3B3B3", "#000066", "#0000FF", "#B3B300", "#FFFFB3",
          "#FFFF00", "#FFFF00", "#0080FF", "#000000", "#808080")
)

# rgb_vector <- as.character(color_data$RGB)
# class_vector <- as.character(color_data$Class)
# 
# # Define legend colors and labels
# legend_colors <- color_data$RGB
# legend_labels <- color_data$Class

color_data <- data.frame(
  Class = c("Broadleaved woodland", "Coniferous woodland", "Arable", "Improved grassland", "Neutral grassland",
            "Calcareous grassland", "Acid grassland", "Fen, marsh and swamp", "Heather",
            "Heather grassland", "Bog", "Inland rock", "Saltwater", "Freshwater", "Supralittoral rock",
            "Supralittoral sediment", "Littoral rock", "Littoral sediment", "Saltmarsh", "Urban", "Suburban"),
  RGB = c("#E10000", "#006600", "#732600", "#00ff00", "#7fe57f", "#70a800", "#998100", "#ffff00",
          "#801a80", "#e68ca6", "#008073", "#d2d2ff", "#000080", "#0000FF", "#CCAA00", "#CCB300",
          "#FFFF80", "#FFFF80", "#8080ff", "#000000", "#808080")
)

map_fun_EA_pollution <- function(map, data, label_IndustrySector = 'Industry sector',
                                 fillColor = "blue",legend_title = 'EA Pollution Inventory 2021'){
  map %>% 
    #addControl(data = data, html = paste("<h5>", Regulated_Industry_Sector, "</h5>", sep = ""), position = "topright") %>%
    addCircleMarkers(
      data = data,
      lng = ~Longitude, lat = ~Latitude, radius = 8, #~radius,
      popup = ~paste(
        "<b>Substance Name: </b>", substance_name, "<br/>",
        "<b>Quantity Released: </b>", quantity_released_tons, " tonnes<br/>",
        "<b>Sector: </b>", Regulated_Industry_Sector, "<br/>",
        "<b>Route: </b>", `ROUTE NAME`, "<br/>",
        sep = ""
      ),
      color = "black",   # Outline color
      fillColor = fillColor, # Fill color
      fillOpacity = 0.8, # Opacity of the fill color
      weight = 0.5,
      group = legend_title
    )
}

## TEST:data = data_process_EA_WQ_gcms()
## leaflet() %>% addTiles() %>% map_fun_EA_WQ_gcms(data=data)
map_fun_EA_WQ_gcms <- function(map, data, fillColor = "blue",
                               legend_title= 'EA water quality GCMS data'){
  map %>% 
  #addControl(html = paste("<h5>", input$gcms_compound, " ", input$year_slider[1], " - ",  input$year_slider[2],  "</h5>", sep = ""), position = "topright") %>%
    addCircleMarkers(
      data = data, 
      lng = ~Longitude, lat = ~Latitude, radius = 8,# ~radius,
      #clusterOptions = markerClusterOptions(maxClusterRadius=1),
      popup = ~paste(
        "<b>Sample Site ID: </b>", Sample_Site_ID, "<br/>",
        "<b>Compound Name: </b>", Compound_Name, "<br/>",
        "<b>Concentration: </b>", Concentration, "<br/>",
        "<b>Unit: </b>", unit, "<br/>",
        "<b>Sample description: </b>", SMC_DESC, "<br/>",
        "<b>Date: </b>", Sample_datetime, "<br/>",
        "<b>Method: </b>", method, "<br/>",
        sep = ""
      ),
      color = "black",   # Outline color
      fillColor = fillColor, # Fill color
      fillOpacity = 0.8, # Opacity of the fill color
      weight = 0.5,
      group = legend_title
    )
}


## TEST:data = data_process_pbms(var_biota = 'Otter')
## leaflet() %>% addTiles() %>% map_fun_pbms(data=data[[1]], var_biota = 'Otter')
## var_map_sgl currently handled by data_fun.R
map_fun_pbms <- function(map, data, 
                         var_biota = "Otter", 
                         # var_map_sgl = "Cd", 
                         fillColor = "blue", 
                         colorPalette = 'Oranges',
                         legend_title= NULL){
  
    #temporary fix
    identical_columns <- sapply(data, function(col) identical(data[['value']], col))# check which one is identical to the column `value`
    identical_columns <- identical_columns %>% which() %>% names()
    print(identical_columns)
    
    identical_columns <- identical_columns[which(identical_columns !='value')]
    
    var_map_sgl <- identical_columns[1]
    print(var_map_sgl)

    if(var_biota =='Otter'){
      
      # data <- data %>% mutate(value = !!var_map_sgl)
      qpal <- colorBin(colorPalette, data$value, bins = 5, , na.color = 'grey')
      
      map %>% 
        addCircleMarkers(~long, ~lat, data=data, fillColor = ~qpal(value), group = legend_title,
                         popup = ~paste(
                           "<b>UWCRef: </b>", UWCRef, "<br/>",
                           "<b>Sex: </b>", Sex, "<br/>",
                           "<b>AgeClass: </b>", AgeClass, "<br/>",
                           "<b>Year: </b>", year, "<br/>",
                           "<b>Variable: </b>", var_map_sgl , "<br/>",
                           "<b>Value: </b>", round(value,digits=2) , "<br/>",
                           sep = ""
                         ),
                         color = "black",   # Outline color
                         # fillColor = fillColor, # Fill color
                         fillOpacity = 0.8, # Opacity of the fill color
                         weight = 0.5,) %>%  
        addLegend("bottomright", data=data, pal = qpal, values = ~value,
                  title = paste(legend_title, "</br>Metals conc. [µg/g dry weight]"),
                  opacity = 1, group = legend_title
        )
      
    } else if(var_biota =='Buzzard'){
      
      # check for no data
      if (is.null(data) || nrow(data) == 0 || all(is.na(data$value))) {
        # Add a dummy legend
        dummy_palette <- colorNumeric(palette = "Greys", domain = c(0, 1)) # Dummy palette
        map <- map %>%
          addLegend("bottomright", pal = dummy_palette, values = c(0, 1),
                    title = paste(legend_title, "</br>No data available for this selection"),
                    opacity = 1)
      } else {
       # else there is data and continue
      
        # data <- data %>% mutate(value = !!var_map_sgl)
        qpal <- colorBin(colorPalette, data$value, bins = 5, , na.color = 'grey')
        
  
        map = map %>% 
          addCircleMarkers(~long, ~lat, data=data, fillColor = ~qpal(value), 
                           color = "black", weight = 1.0, group = legend_title,
                           fillOpacity = 0.8, # Opacity of the fill color
                           popup = ~paste(
                             "<b>Sex: </b>", `Sex (Male/Female/Unknown)`, "<br/>",
                             "<b>AgeClass: </b>", `Age (Adult/Juvenile/Unknown)`, "<br/>",
                             "<b>Year: </b>", year, "<br/>",
                             "<b>Variable: </b>", var_map_sgl , "<br/>",
                             "<b>Value: </b>", value , "<br/>",
                             sep = ""))  
        
            if (var_map_sgl %in% metals_choices){
              map = map %>%
                    addLegend("bottomright", data=data, pal = qpal, values = ~value, group = legend_title,
                                title = paste(legend_title, "</br>Metals conc. [µg/g dry weight]"),
                                opacity = 1)
            } else if (var_map_sgl %in% SGARs_choices){
              map = map %>%
                addLegend("bottomright", data=data, pal = qpal, values = ~value, group = legend_title,
                          title = paste(legend_title, "</br>SGARs conc. [ng/g wet weight]"),
                          opacity = 1)
            } else {
              map = map
            }
            map
      }
          
    } else if(var_biota =='Sparrowhawk'){
      # data <- data %>% mutate(value = !!var_map_sgl)
      qpal2 <- colorBin(colorPalette, data$value, bins = 5, , na.color = 'grey')
      
      map %>% 
        addCircleMarkers(~long, ~lat, data=data,fillColor = ~qpal2(value),
                         color = "black", weight = 1.0,
                         fillOpacity = 0.8, # Opacity of the fill color
                         group = legend_title,
                         popup = ~paste(
                           "<b>AgeClass: </b>", AGE, "<br/>",
                           "<b>Year: </b>", year, "<br/>",
                           "<b>Variable: </b>", var_map_sgl , "<br/>",
                           "<b>Value: </b>", value , "<br/>",
                           sep = "")) %>%  
        addLegend("bottomright", data=data, pal = qpal2, values = ~value,
                  title = paste(legend_title, "</br>SGARs conc. [ng/g wet weight]"),
                  opacity = 1, group = legend_title)
      
    } else {
      map
    }
}


## TEST:data = data_process_pfas(var_biota = 'Otter')
## fillColor = colorNumeric(palette = brewer.pal(9, "Reds"), domain = data[[1]]$transform_value)
## leaflet() %>% addTiles() %>% map_fun_pfas(data=data[[1]],fillColor = fillColor)

map_fun_pfas <- function(map, data, fillColor, legend_title = "PFAS", showHeatmap = FALSE){
  
  if (showHeatmap) {
    
    map %>%
      addHeatmap(
        data = data,
        lng = ~Longitude, lat = ~Latitude, intensity = ~transform_value,
        blur = 15, radius = 25,
        minOpacity = 0.5,
        gradient = fillColor, #comment out to use default
       group = legend_title
        )
  } else {

    map  %>%
      # addTiles() %>%
      # setView(lng = mean(filtered_data$Longitude), lat = mean(filtered_data$Latitude), zoom = 5) %>%
      #addControl(html = paste("<h5>", selected_matrix, " ", selected_substance, " ", start_year, " - ", end_year, "</h5>", sep = ""), position = "topright") %>%
      addCircleMarkers(
        data = data,
        lng = ~Longitude, lat = ~Latitude, radius = 8,
        popup = ~paste(
          "<b>Matrix: </b>", matrix, "<br/>",
          "<b>Substance: </b>", substance, "<br/>",
          "<b>Quantity Released: </b>", value, "<br/>",
          "<b>Unit: </b>", "ng/l", "<br/>",
          #"<b>Quantile Bin (1-3): </b>", substance_value_bin, "<br/>",
          "<b>Date: </b>", date, "<br/>",
          "<b>Year: </b>", year, "<br/>",
          sep = ""
        ),
        color = "black",   # Outline color
        fillColor = ~fillColor(transform_value), # ~pal(transform_value), 
        fillOpacity = 0.8, # Opacity of the fill color
        weight = 0.2,
        group = legend_title
      ) 
  }
  # %>%
  #   addLegend("bottomright", pal = pal, values = ~transform_value,
  #             title = paste(selected_substance,  " ng/l"),
  #             labFormat = labFormat_transform,
  #             opacity = 1)
  
  
}


map_fun_rain <- function(map, data, colors, legend_title = "HadUK-Grid Annual Rainfall") {
  
  #pal <- colorNumeric("viridis", domain = values(data), na.color = "transparent")
  map %>% 
    addRasterImage(data, 
                   colors = colors, 
                   opacity = 0.7,
                   group = legend_title) 
  # %>% 
  #   addControl( HTML(paste("<b>Year: </b>", input$year_slider)), position = "topright")
  
}

## TEST:data = data_process_apiens()[[1]]
## fillColor = colorNumeric(palette = brewer.pal(9, 'Blues'), domain = data$Value)
## leaflet() %>% addTiles() %>% map_fun_apiens(data = data, fillColor=fillColor)
map_fun_apiens <- function(map, data, fillColor= "blue", legend_title = "APIENS"){
  
 
    map  %>%
      addCircleMarkers(
        data = data,
        lng = ~Site_lon, lat = ~Site_lat, radius = 8,
        popup = ~paste(
          "<b>Site_habitat: </b>", Site_habitat, "<br/>",
          "<b>Network: </b>", Network, "<br/>",
          "<b>Site_habitat: </b>", Site_habitat, "<br/>",
          "<b>Plot_ID_or_Horizon_name: </b>", Plot_ID_or_Horizon_name, "<br/>",
          "<b>Year: </b>", Year, "<br/>",
          "<b>Variable: </b>", Variable, "<br/>",
          "<b>Unit: </b>", Unit, "<br/>",
          sep = ""
        ),
        color = "black",   # Outline color
        fillColor = fillColor, # ~pal(transform_value), 
        fillOpacity = 0.8, # Opacity of the fill color
        weight = 0.2,
        group = legend_title
      ) 
  
  
  
}

map_fun_catsdogs  <- function(map, map_data, palette_name = 'viridis', legend_title = "Density by postcode") {
  
  pal <- colorNumeric(palette_name, NULL)
  
  head(map_data)
  
  map %>% 
    addPolygons(data = map_data,
                stroke = FALSE, smoothFactor = 0.5, fillOpacity = 0.5, weight = 1, opacity = 1.0,
                fillColor =  ~pal(Value),
                   group = legend_title) 
  
}

map_fun_EUSO <- function(map, data, colors, legend_title = "Soil health") {
  print('map_process_EUSO')
  
  #pal <- colorNumeric("viridis", domain = values(data), na.color = "transparent")
  map %>% 
    # addRasterImage(data, # can't handle large raster
    #                colors = colors, 
    #                opacity = 0.7,
    #                group = legend_title) 
    
    leafem::addGeotiff(data, 
                     colorOptions =  colorOptions(
                       palette = hcl.colors(256, palette = "inferno")
                       , na.color = "transparent"
                     ),
                      opacity = 0.7,
                      group = legend_title) %>% 
    setView(-3.0, 55.5, zoom = 6) 
  
}

map_fun_IYR <- function(map, data, colors, legend_title = "Input to Yield Ratio") {
  
  #pal <- colorNumeric("viridis", domain = values(data), na.color = "transparent")
  map %>% 
    addRasterImage(data, 
                   colors = colors, 
                   opacity = 0.7,
                   group = legend_title) 
  
}

switch_map <- function(m, map_data, input_choice, legend_title='legend', palette_name = 'Reds', showHeatmap=FALSE){
  
  labFormat_transform <- labelFormat(transform = function(x) round(exp(x) - 1, 7)) 
  print(paste0('switch_map', input_choice))
  
  if (input_choice == 'EA water quality GCMS/LCMS data') {
    
    # Check if the dataset is empty or has no valid data
    if (nrow(map_data) == 0 || all(is.na(map_data$log_Concentration))) {
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
      
      fillColor = colorNumeric(palette = brewer.pal(9,palette_name), domain = map_data$log_Concentration)
      # break_values <- seq(min(map_data$log_Concentration), max(map_data$log_Concentration), length.out = 5)
      
      m = m %>% map_fun_EA_WQ_gcms(map_data,
                                   fillColor =  ~fillColor(log_Concentration),
                                   legend_title= legend_title) %>%
        # addLegend(data = map_data,
        #           position = "bottomright",
        #           pal = fillColor,
        #           values = ~map_data$log_Concentration,
        #           title = paste0(legend_title ,"</br>","Concentration ug/l"),
        #           opacity = 1,
        #           group = legend_title,
        #           labFormat = labFormat_transform,
        #           orientation = "horizontal")
        
        
        addLegendNumeric(data = map_data,
                         position = "bottomright",
                         pal = fillColor,
                         values = ~map_data$log_Concentration,
                         title = htmltools::HTML(paste0(legend_title, "<br>", "Concentration ug/l")),
                         shape = "rect",
                         orientation = "horizontal",
                         width = 200,
                         height = 10,  
                         numberFormat = function(x) format(round(exp(x) - 1, 3), trim = TRUE))
        
    }
    
  } else if (input_choice == 'EA pollution inventory 2021') {
    #m = m %>% map_fun_EA_pollution(map_data,fillColor = color_data$RGB[new_id_ii])
    
    fillColor = colorNumeric(palette = brewer.pal(9,palette_name), domain = map_data$log_quantity_released_tons)
    
    m = m %>% map_fun_EA_pollution(map_data,
                                   fillColor =  ~fillColor(log_quantity_released_tons),
                                   legend_title= legend_title) %>%
      # addLegend(data = map_data,
      #           position = "bottomright",
      #           pal = fillColor,
      #           values = map_data$log_quantity_released_tons,
      #           title = paste0(legend_title ,"</br>","tonnes"),
      #           opacity = 1,
      #           group = legend_title,
      #           labFormat = labFormat_transform)
    
      addLegendNumeric(data = map_data,
                     position = "bottomright",
                     pal = fillColor,
                     values = ~map_data$log_quantity_released_tons,
                     title = htmltools::HTML(paste0(legend_title, "<br>", "tonnes")),
                     shape = "rect", 
                     orientation = "horizontal", 
                     width = 200,
                     height = 10,
                     numberFormat = function(x) format(round(exp(x) - 1, 3), trim = TRUE))
    
    
  } else if (input_choice == 'Predatory Bird Monitoring Scheme') {
    
    # added a trycatch for when there is no data for the selection
    m <- tryCatch({m %>% map_fun_pbms(map_data,
                                      colorPalette =palette_name,
                                      var_biota = map_data$biota[1],
                                      legend_title= legend_title)
    }, error = function(e){
      dummy_palette <- colorNumeric(palette = "Greys", domain = c(0, 0)) # Dummy palette
      # add notification
      showNotification("No data available for this selection.", type = "error", duration = 5)
      
      # Add dummy legend with message
      # m %>% addLegend("bottomright", pal = dummy_palette, values = c(0, 0),
      #                 title = paste(legend_title, "</br>No data available for this selection"),
      #                 opacity = 1)
      
      m %>% addLegendNumeric(
        position = "bottomright", pal = dummy_palette, values = c(0, 0),
        title = htmltools::HTML(paste0(legend_title, "</br>No data available for this selection")),
        shape = "rect",
        orientation = "horizontal",
        width = 200,
        height = 10
        
      )
      
    })
    
  } else if (input_choice == 'PFAS') {
    
    # Check if the dataset is empty or has no valid data
    if (nrow(map_data) == 0 || all(is.na(map_data$transform_value))) {
      
      dummy_palette <- colorNumeric(palette = "Greys", domain = c(0, 0)) # Dummy palette
      
      showNotification("No data available for this selection.", type = "error", duration = 5)
      
      # Add a message in the legend indicating no data
      m = m %>% addLegendNumeric(
            position = "bottomright",
            pal = dummy_palette,
            values = c(0, 0),
            title = htmltools::HTML(paste0(legend_title, "<br>", "No data available for this selection")),
            shape = "rect",
            orientation = "horizontal",
            width = 200,
            height = 10
          )
    } else {
      #m = m %>% map_fun_pfas(map_data,fillColor = color_data$RGB[new_id_ii])
      # labFormat_transform = labelFormat(transform = function(x) round(exp(x) - 1, 1))
      
      selected_palette <-palette_name
      
      fillColor = colorNumeric(palette = brewer.pal(9, selected_palette), domain = map_data$transform_value)
      
      gradient_colors <- colorNumeric(
        palette = brewer.pal(9, selected_palette),
        domain = map_data$transform_value
      )(seq(min(map_data$transform_value, na.rm = TRUE),
            max(map_data$transform_value, na.rm = TRUE),
            length.out = 256))
      
      m = m %>% map_fun_pfas(map_data,
                             fillColor = if (showHeatmap) gradient_colors else fillColor,  # Use gradient_colors in heatmap mode
                             legend_title= legend_title,
                             showHeatmap  = showHeatmap
      ) %>%
        addLegendNumeric(data = map_data,
                  position = "bottomright",
                  pal = fillColor,
                  values = map_data$transform_value,
                  title = htmltools::HTML(paste0(legend_title ,"<br>","ng/l")),
                  group = legend_title,
                  #labFormat = labFormat_transform,
                  numberFormat = function(x) format(round(exp(x) - 1, 3), trim = TRUE),
                  shape = "rect",
                  orientation = "horizontal",
                  width = 200,
                  height = 10
                  )
      
    }
  } else if (input_choice == "HadUK-Grid Annual Rainfall") {
    
    rain_values <- values(map_data)
    rain_values <- rain_values[!is.na(rain_values)]
    
    fillColor <- colorNumeric(
      palette = brewer.pal(9,palette_name),
      domain=range(rain_values),
      na.color = "transparent"
    )
    
    m = m %>% map_fun_rain(map_data,
                           colors =  fillColor,
                           legend_title = legend_title) %>%
      addLegendNumeric(data = map_data,
                      position = "bottomright",
                      pal = fillColor,
                      values = values(map_data),
                      title = htmltools::HTML(paste0(legend_title ,"<br>","mm")),
                      group = legend_title,
                      # na.label = NULL,
                      shape = "rect",
                      orientation = "horizontal",
                      width = 200,
                      height = 10
                      ) 
    
    
  } else if (input_choice == 'APIENS') {
    #m = m %>% map_fun_EA_pollution(map_data,fillColor = color_data$RGB[new_id_ii])
    
    fillColor = colorNumeric(palette = brewer.pal(9,palette_name), domain = map_data$Value)
    
    m = m %>% map_fun_apiens(map_data,
                             fillColor =  ~fillColor(Value),
                             legend_title= legend_title) %>%
      addLegendNumeric(data = map_data,
                      position = "bottomright",
                      pal = fillColor,
                      values = ~map_data$Value,
                      title = htmltools::HTML(paste0(legend_title ,"</br>",unique(map_data$Unit))),
                      group = legend_title,
                      shape = "rect",
                      orientation = "horizontal",
                      width = 200,
                      height = 10
                      )
    
    
  } else if (input_choice == 'UK cats and dogs density') {
    
    m = m %>% map_fun_catsdogs(map_data,
                               palette_name =  palette_name,
                             legend_title= legend_title) %>%
      addLegendNumeric(data = map_data,
                      position = "bottomright",
                      pal = colorNumeric(palette_name, NULL),
                      values = ~Value,
                      title = htmltools::HTML(paste0(legend_title)),
                      group = legend_title,
                      shape = "rect",
                      orientation = "horizontal",
                      width = 200,
                      height = 10)
    
    
  } else if (input_choice == "EU Soil metals") {
    
    #EUSO_values <- values(map_data)
    # EUSO_values <- EUSO_values[!is.na(EUSO_values)]
    
    # fillColor <- colorNumeric(
    #   palette = brewer.pal(9,palette_name),
    #   domain=range(EUSO_values),
    #   na.color = "transparent"
    # )
    
    m = m %>% map_fun_EUSO(map_data,
                          colors =  fillColor,
                          legend_title = legend_title) #%>%
      # addLegend(data = map_data,
      #           position = "bottomright",
      #           pal = fillColor,
      #           values = values(map_data),
      #           title = paste0(legend_title ,"</br>","[mg Kg-1]"),
      #           group = legend_title,
      #           na.label = NULL) 
    
  } else if (input_choice == "AgZero+ Input to Yield Ratio (IYR)") {
    
    IYR_values <- values(map_data)
    IYR_values <- IYR_values[!is.na(IYR_values)]
    
    fillColor <- colorNumeric(
      palette = brewer.pal(9,palette_name),
      domain=range(IYR_values),
      na.color = "transparent"
    )
    
    m = m %>% map_fun_IYR(map_data,
                           colors =  fillColor,
                           legend_title = legend_title) %>%
      addLegendNumeric(data = map_data,
                  position = "bottomright",
                  pal = fillColor,
                  values = values(map_data),
                  title = htmltools::HTML(paste0(legend_title ,"<br>","[unitless]")),
                  group = legend_title,
                  # na.label = NULL,
                  shape = "rect",
                  orientation = "horizontal",
                  width = 200,
                  height = 10) 
    
  } 

  return(m)

}