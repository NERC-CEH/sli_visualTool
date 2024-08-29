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

map_fun_EA_pollution <- function(map, data, label_IndustrySector = 'Industry sector',fillColor = "blue"){
  map %>% 
    addControl(html = paste("<h5>", label_IndustrySector, "</h5>", sep = ""), position = "topright") %>%
    addCircleMarkers(
      data = data,
      lng = ~Longitude, lat = ~Latitude, radius = ~radius,
      popup = ~paste(
        "<b>Substance Name: </b>", substance_name, "<br/>",
        "<b>Quantity Released: </b>", quantity_released_kg, "kg<br/>",
        "<b>Sector: </b>", Regulated_Industry_Sector, "<br/>",
        "<b>Route: </b>", `ROUTE NAME`, "<br/>",
        sep = ""
      ),
      color = "black",   # Outline color
      fillColor = fillColor, # Fill color
      fillOpacity = 0.5, # Opacity of the fill color
      weight = 0.5 
    )
}


map_fun_EA_WQ_gcms <- function(map, data,fillColor = "blue"){
  map %>% 
  #addControl(html = paste("<h5>", input$gcms_compound, " ", input$year_slider[1], " - ",  input$year_slider[2],  "</h5>", sep = ""), position = "topright") %>%
    addCircleMarkers(
      data = data, lng = ~Longitude, lat = ~Latitude, radius = ~radius,
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
      fillOpacity = 0.5, # Opacity of the fill color
      weight = 0.5 
    )
}

map_fun_pbms <- function(map, data, 
                         var_biota = "Otter", 
                         var_metal_map_sgl = "Cd", 
                         var_sgar_map_sgl = "ΣSGARs"){

    if(var_biota =='Otter'){
      
      metals <- data %>% rename(value = !!var_metal_map_sgl)
      qpal <- colorBin("Oranges", metals$value, bins = 5, , na.color = 'grey')
      
      map %>% 
        addCircleMarkers(~long, ~lat, data=metals,color = ~qpal(value), group = 'metals') %>%  
        addLegend("bottomright", data=metals, pal = qpal, values = ~value,
                  title = "Metals conc. [µg/g dry weight]",
                  opacity = 1
        )
      
    } else if(var_biota =='Buzzard'){
      metals <- data %>% rename(value = !!var_metal_map_sgl)
      SGARs <- data %>% rename(value = !!var_sgar_map_sgl)
      qpal <- colorBin("Oranges", metals$value, bins = 5, , na.color = 'grey')
      qpal2 <- colorBin("Blues", SGARs$value, bins = 5, na.color = 'grey')
      
      map %>% 
        addCircleMarkers(~long, ~lat, data=metals, color = ~qpal(value), group = 'metals') %>%  
        addCircleMarkers(~long, ~lat, data=SGARs,color = ~qpal2(value), group = 'SGARs') %>%  
        addLayersControl(overlayGroups = c("metals","SGARs")) %>%
        addLegend("bottomright", data=metals, pal = qpal, values = ~value,
                  title = "Metals conc. [µg/g dry weight]",
                  opacity = 1
        ) %>% 
        addLegend("bottomright", data=SGARs, pal = qpal2, values = ~value,
                  title = "SGARs conc. [ng/g wet weight]",
                  opacity = 1)
      
    } else if(var_biota =='Sparrowhawk'){
      
      SGARs <- sparrowhawk_SGARs %>% rename(value = !!var_sgar_map_sgl)
      qpal2 <- colorBin("Blues", SGARs$value, bins = 5, , na.color = 'grey')
      
      map %>% 
        addCircleMarkers(~long, ~lat, data=SGARs,color = ~qpal2(value), group = 'SGARs') %>%  
        addLayersControl(overlayGroups = c("SGARs")) %>%
        addLegend("bottomright", data=SGARs, pal = qpal2, values = ~value,
                  title = "SGARs conc. [ng/g wet weight]",
                  opacity = 1)
      
    } else {
      map
    }
}

switch_map <- function(map, data, input_choice){
  
  return(map)

}