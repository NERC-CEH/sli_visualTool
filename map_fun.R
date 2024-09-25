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


map_fun_EA_WQ_gcms <- function(map, data, fillColor = "blue",
                               legend_title= 'EA water quality GCMS data'){
  map %>% 
  #addControl(html = paste("<h5>", input$gcms_compound, " ", input$year_slider[1], " - ",  input$year_slider[2],  "</h5>", sep = ""), position = "topright") %>%
    addCircleMarkers(
      data = data, 
      lng = ~Longitude, lat = ~Latitude, radius = 8,# ~radius,
      #clusterOptions = markerClusterOptions(maxClusterRadius=1),
      popup = ~paste(
        "<b>Compound Name: </b>", Compound_Name, "<br/>",
        "<b>Concentration: </b>", Concentration, "<br/>",
        "<b>Unit: </b>", unit, "<br/>",
        "<b>Sample description: </b>", SMC_DESC, "<br/>",
        "<b>Date: </b>", Sample_datetime, "<br/>",
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
                           # "<b>Vraiable: </b>", var_map_sgl , "<br/>",
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
                           # "<b>Vraiable: </b>", var_map_sgl , "<br/>",
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
                           # "<b>Vraiable: </b>", var_map_sgl , "<br/>",
                           "<b>Value: </b>", value , "<br/>",
                           sep = "")) %>%  
        addLegend("bottomright", data=data, pal = qpal2, values = ~value,
                  title = paste(legend_title, "</br>SGARs conc. [ng/g wet weight]"),
                  opacity = 1, group = legend_title)
      
    } else {
      map
    }
}


map_fun_pfas <- function(map, data, fillColor = "blue", legend_title = "PFAS"){
  
  #fillColor = ~colorNumeric(palette = brewer.pal(9, single_color_sequential_palettes[new_id_ii]), domain = data$transform_value)(transform_value)
  
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
      fillColor = fillColor, # ~pal(transform_value), 
      fillOpacity = 0.8, # Opacity of the fill color
      weight = 0.2,
      group = legend_title
    ) 
  # %>%
  #   addLegend("bottomright", pal = pal, values = ~transform_value,
  #             title = paste(selected_substance,  " ng/l"),
  #             labFormat = labFormat_transform,
  #             opacity = 1)
  
  
}



switch_map <- function(map, data, input_choice){
  
  return(map)

}