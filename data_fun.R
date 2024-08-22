# function to take EA_pollution_inventory/2021 Pollution Inventory Dataset.xlsx and chosen polluting substance 
# and return filtered dataframe with lat lon values and normalised data for plotting on map

# define variables, maybe move to global.R

metals_choices <- c('Cd','Pb','Hg')
SGARs_choices <- c('Bromadiolone', 'Difenacoum', 'Brodifacoum', 'ΣSGARs' )


osg_parse2 <- function(grid_refs) { 
  # error handling and randomize if only 10km is avail.
  out <- tryCatch(
    {
      # if (nchar == 4) {
      #   #randomize
      #   grid_refs = grid_refs
      # } else{
      #   grid_refs = grid_refs
      # }
      osg_parse(grid_refs)
    },
    error=function(cond) {
      # message("Here's the original error message:")
      # message(cond)
      # message("")
      # print(grid_refs)
      # Choose a return value in case of error
      return(list(easting = NA, northing = NA))
    },
    warning=function(cond) {
      # message(paste("URL caused a warning"))
      # print(grid_refs)
      return(list(easting = NA, northing = NA))
    },
    finally={
      #message("Some other message at the end")
    }
  )
  
  return(out)
}


data_process_EA_pollution <- function(file_path = 'datasets/EA_pollution_inventory/2021 Pollution Inventory Dataset.xlsx', IndustrySector = 'Agriculture', substance = "all") {
  
  # Read in the data
  fp <- file_path 
  data <- read_excel(fp, skip = 9, col_names = TRUE)
  
  # Filter out rows with NA in Easting or Northing columns
  data <- data[complete.cases(data$EASTING, data$NORTHING), ]
  
  # Define the UK National Grid projection
  uk_proj <- "+init=epsg:27700"  # EPSG code for UK National Grid
  
  # Create an sf object using the filtered Easting and Northing coordinates
  sf_data <- st_as_sf(data, coords = c("EASTING", "NORTHING"), crs = uk_proj)
  
  # Transform UK National Grid coordinates to latitude and longitude
  sf_data <- st_transform(sf_data, crs = 4326)  # EPSG code for WGS84 (latitude and longitude)
  
  # Extract latitude and longitude directly from sf_data
  data$Latitude <- st_coordinates(sf_data)[, 2]
  data$Longitude <- st_coordinates(sf_data)[, 1]
  
  # rename column
  names(data)[names(data) == "QUANTITY RELEASED (kg)"] <- "quantity_released_kg"
  names(data)[names(data) == "SUBSTANCE NAME"] <- "substance_name"
  names(data)[names(data) == "REGULATED INDUSTRY SECTOR"] <- "Regulated_Industry_Sector"
  
  
  
  # get unique substance and industry names
  unique_substance_names <- sort(unique(data$substance_name))
  unique_industry_sector <- sort(unique(data$Regulated_Industry_Sector))
  
  # filter for selected  industry section
  filtered_data <- subset(data, Regulated_Industry_Sector == IndustrySector)
  
  filtered_data <- subset(filtered_data, substance_name == substance | substance == "all")
  
  
  
  
  # Filter out rows with NA values in the 'quantity_released_kg' column
  filtered_data <- filtered_data[!is.na(filtered_data$quantity_released_kg), ]
  
  # if filtered_data has no rows, i.e. no data in or all NA values in quantity_released_kg column and filtered out in last column
  if (dim(filtered_data)[1] == 0) {
    filtered_data[, 'radius'] = NA
    
  }else{ # filtered_data does have data in quantity_released_kg column
    
    # Calculate the minimum and maximum values
    min_value <- min(filtered_data$quantity_released_kg)
    max_value <- max(filtered_data$quantity_released_kg)
    
    # Normalize 'quantity_released_kg' to range [0, 1]
    filtered_data <- filtered_data %>%
      mutate(quantity_released_kg_norm = (quantity_released_kg - min_value) / (max_value - min_value))
    
    ## log bins
    
    # Determine the number of bins
    num_bins <- 5
    
    # Create logarithmic bins for normalized quantity values
    filtered_data <- filtered_data %>%
      mutate(log_quantity_released_kg_norm = log(quantity_released_kg_norm + 1),  # Adding 1 to avoid taking log of zero
             bin = cut(log_quantity_released_kg_norm, breaks = seq(min(log_quantity_released_kg_norm), max(log_quantity_released_kg_norm), length.out = num_bins + 1), labels = FALSE))
    
    # Define marker sizes for each bin (logarithmic scale)
    max_size <- 50  # Maximum marker size
    min_size <- 5   # Minimum marker size
    bin_sizes <- seq(min(filtered_data$log_quantity_released_kg_norm), max(filtered_data$log_quantity_released_kg_norm), length.out = num_bins + 1)  # Create logarithmic bins
    bin_sizes <- exp(bin_sizes)  # Convert back to original scale
    bin_sizes <- pmax(bin_sizes, 1)  # Ensure minimum size is 1
    bin_sizes <- (bin_sizes - min(bin_sizes)) / (max(bin_sizes) - min(bin_sizes))  # Normalize to [0, 1]
    bin_sizes <- bin_sizes * (max_size - min_size) + min_size  # Scale to desired range
    
    # Assign marker sizes based on the bins
    filtered_data <- filtered_data %>%
      mutate(radius = bin_sizes[bin])
    
  }
  
  return(list(filtered_data=filtered_data,
              unique_industry_sector=unique_industry_sector, 
              unique_substance_names=unique_substance_names))
}


# function to import and process EA WQ data
data_process_EA_WQ_gcms <- function(fp_gcms = 'datasets/EA_water_quality_GCMS_LCMS/GCMS Target and Non-Targeted Screening.csv', CompoundName = "Phenanthrene") {
  
  data_gcms <- read.csv(fp_gcms)
  
  filtered_data_gcms <- subset(data_gcms, Compound_Name == CompoundName)
  
  # Filter out rows with NA values in the 'quantity_released_kg' column
  filtered_data_gcms <- filtered_data_gcms[!is.na(filtered_data_gcms$Concentration), ]
  
  # Calculate the minimum and maximum values
  min_value <- min(filtered_data_gcms$Concentration)
  max_value <- max(filtered_data_gcms$Concentration)
  
  # Normalize 'quantity_released_kg' to range [0, 1]
  filtered_data_gcms <- filtered_data_gcms %>%
    mutate(Concentration_norm = (Concentration - min_value) / (max_value - min_value))
  
  # Determine the number of bins
  num_bins <- 5
  
  # Create logarithmic bins for normalized quantity values
  filtered_data_gcms <- filtered_data_gcms %>%
    mutate(log_Concentration_norm = log(Concentration_norm + 1),  # Adding 1 to avoid taking log of zero
           bin = cut(log_Concentration_norm, breaks = seq(min(log_Concentration_norm,na.rm = TRUE), max(log_Concentration_norm,na.rm = TRUE), length.out = num_bins + 1), labels = FALSE))
  
  # Define marker sizes for each bin (logarithmic scale)
  max_size <- 30  # Maximum marker size
  min_size <- 5   # Minimum marker size
  bin_sizes <- seq(min(filtered_data_gcms$log_Concentration_norm), max(filtered_data_gcms$log_Concentration_norm), length.out = num_bins + 1)  # Create logarithmic bins
  bin_sizes <- exp(bin_sizes)  # Convert back to original scale
  bin_sizes <- pmax(bin_sizes, 1)  # Ensure minimum size is 1
  bin_sizes <- (bin_sizes - min(bin_sizes)) / (max(bin_sizes) - min(bin_sizes))  # Normalize to [0, 1]
  bin_sizes <- bin_sizes * (max_size - min_size) + min_size  # Scale to desired range
  
  # Assign marker sizes based on the bins
  filtered_data_gcms <- filtered_data_gcms %>%
    mutate(radius = bin_sizes[bin])
  
  return(filtered_data_gcms)
  
}

data_process_pbms <- function(var_biota = 'buzzard', 
                              var_sgar_map_sgl = 'Cd',
                              var_metals_map_sgl = 'ΣSGARs') {
  
  # Read in the data
  if(var_biota =='Otter'){
    otter_metals <- read_csv('assets/bio-xter-liver-metal-habitat-uk-otters-2006-2017-v1/data/Concentrations_of_inorganic_elements_in_UK_otter_livers_2006–2017.csv')
    otter_metals[,c('long','lat')] <-sf_project(from = st_crs(27700), to = st_crs(4326),  otter_metals[,c('X','Y')])
    otter_metals_long <- otter_metals %>% select(UWCRef,Year, long, lat, !!metals_choices) %>% tidyr::pivot_longer(!!metals_choices)
    otter_choices <- metals_choices
    filtered_data = otter_metals %>% rename(year = Year)
  } else if(var_biota =='Buzzard'){
    
    buzzards <- read_excel('datasets/PBMS/20240704_APEX_Buzzard_Data_forMockUp.xlsx',skip=1)
    metadata1 <-   read_excel('datasets/PBMS/20240704_APEX_Buzzard_Data_forMockUp.xlsx', range = cell_rows(1), .name_repair = 'minimal') %>% 
      colnames() %>% .[nzchar(.)] %>% print()
    buzz_XY <-  sapply((buzzards) %>% 
                         select(`Finest LocationGridRef\r\n(most 6-digit)`) %>% pull(),FUN = osg_parse2) %>% 
      t() %>% as.data.frame() %>% rename(X=easting, Y=northing) 
    buzzards <- buzzards %>% mutate(X = unlist(buzz_XY$X), Y = unlist(buzz_XY$Y))
    buzzards[,c('long','lat')] <-sf_project(from = st_crs(27700), to = st_crs(4326),  buzzards[,c('X','Y')])
    buzzards_long <- buzzards %>% 
      mutate(across(where(is.numeric), ~tidyr::replace_na(., 0))) %>% 
      select(`PBMS ID`,Species, `Collection year`, long, lat, !!c(metals_choices,SGARs_choices)) %>% 
      tidyr::pivot_longer(!!c(metals_choices,SGARs_choices)) %>% 
      mutate(group = if_else(name %in% metals_choices, 'metals', 'SGARs'))
    
    
    buzzard_choices <- list(`metals` = metals_choices, `SGARs` = SGARs_choices)
    filtered_data = buzzards %>% rename(year = `Collection year`)
    
  } else if(var_biota =='Sparrowhawk'){
    
    # ## Sparrowhawk SGARs, no long lat in EIDC data, ND replaced by NA
    # sparrowhawk_SGARs <- read_csv("datasets/PBMS/1af003b1-2f70-4e45-a31a-b07a5fe6e929/data/chempop_sparrowhawks_sgars.csv",  na = c("ND"))
    # ## replace with file from Elaine, with lat lon
    sparrowhawk_SGARs <- read_csv("datasets/PBMS/Chempop data for shinji_ JNCC_ 2024_SGARs in Eurasian sparrowhawk livers 1995-2015 for Great Britain.csv",  na = c("ND"))
    sparrowhawk_SGARs[,c('long','lat')] <-sf_project(from = st_crs(27700), to = st_crs(4326),  sparrowhawk_SGARs[,c('EAST','NORTH')])
    
    #sparrowhawk_SGARs %>% tidyr::pivot_longer(cols = -c(BIRD,YEAR,REGION,AGE,SEX,Units))
    sparrowhawk_SGARs_long <- sparrowhawk_SGARs %>% rename(ΣSGARs = Sum_SGAR) %>% tidyr::pivot_longer(cols = Difenacoum:ΣSGARs)
    sparrowhawk_choices <- SGARs_choices
    
    filtered_data = sparrowhawk_SGARs %>% rename(year = YEAR)
  } else {
  }
  
 
    
  
  return(list(filtered_data=filtered_data))
}

get_NUTS_regions <- function(NUTS_lvl_code = 1) {
  library(sf)
  url = "https://tubcloud.tu-berlin.de/s/RHZJrN8Dnfn26nr/download/NUTS_RG_10M_2021_4326.geojson"
  spdf <- st_read(url)
  
  spdf_UK_NUTS = spdf[(spdf$LEVL_CODE == NUTS_lvl_code & spdf$CNTR_CODE == 'UK'), ] 
  return(spdf_UK_NUTS)
}

data_process_EA_WQ_gcms_with_NUTS <- function(fp_gcms_withNUTS = './datasets/EA_water_quality_GCMS_LCMS/gcms_data_with_NUTS.csv', NUTS_region = NUTS_region, CompoundName = "Phenanthrene") {
  
  data_gcms_with_NUTS <- read.csv(fp_gcms_withNUTS)
  
  # subset data by chosen compound
  filtered_data <- subset(data_gcms_with_NUTS, Compound_Name == CompoundName)
  
  # find mean for each region
  mean_concentration <- filtered_data %>%
    group_by(NUTS_ID) %>%
    summarize(mean_concentration = mean(Concentration, na.rm = TRUE))
  
  threshold <- 50  # Set your threshold value here
  
  above_threshold <- filtered_data %>%
    group_by(NUTS_ID) %>%
    summarize(above_threshold = any(Concentration > threshold, na.rm = TRUE))
  
  # merge with nuts df
  NUTS_region_with_gcms_data <- merge(NUTS_region, mean_concentration, by = "NUTS_ID", all.x = TRUE)
  
  return(NUTS_region_with_gcms_data)
}

