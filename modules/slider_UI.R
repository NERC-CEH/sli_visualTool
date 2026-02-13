
# loaded at start up, be careful with performance, PFAS: 10ms, APIENS 2x10ms

unique_industry_sector <- c("Agriculture","Biowaste Treatment","Cement and Minerals",
                            "Chemicals","combustion","Combustion","EfW","Food & Drink",
                            "Hazardous Waste","Landfill","Metals","Metals Recycling",
                            "No Far Sector","Non-Hazardous & Inert","Nuclear",
                            "Oil and Gas","Paper and textiles",
                            "Refineries & Fuel","Water Industry") # data_process_EA_pollution()[['unique_industry_sector']]
#unique_industry_sector <- c('Agriculture','Water Industry')

pbms_biota_choices =  c('Buzzard','Sparrowhawk','Otter')
unique_pfas_names <- data_process_pfas()[['unique_pfas_names']]

unique_apiens_varnames <- data_process_apiens()[['unique_apiens_varnames']][-1] # drop first one
unique_apiens_NECD <- data_process_apiens()[['unique_apiens_NECD']]

ea_gcms_choices <-
  list(`Pharmaceuticals` = list( "Diphenyl ether","Ibuprofen", "Ketamine","Mirtazapine", "Phenanthrene"), #"Benzothiazole",
     `Fungicides` = list("Azoxystrobin", "Metalaxyl","Propiconazole", "Tebuconazole (Terbuconazole)", "Thiabendazole"),
     `Herbicides` = list("Atrazine","Diuron","Metolachlor","Simazine"),
     `Insecticides` = list("Diflufenican", "Fipronil", "Imidacloprid"),
     `Others` = list("Caffeine","Cocaine", "2,4,7,9-Tetramethyl-5-decyne-4,7-diol"))

ea_gcms_choices %>% unlist() %>% unname()

ea_pollution_sliders <- function(id) {   
  
  tagList(
    HTML('<p align="center" style="font-weight: bold;color:orange">Note: This data is not intended for comparing the pollution contributions of different sectors.</p>'),
    
    selectInput(NS(id,"IndustrySector"), "Choose Industry Sector:",
                unique_industry_sector
    )
  )
}


# data_gcms <- read.csv('datasets/EA_water_quality_GCMS_LCMS/GCMS Target and Non-Targeted Screening.csv')
# data_gcms %>% distinct(Compound_Name)
ea_gcms_sliders <- function(id) {               
  tagList(
    HTML('<p align="center" style="font-weight: bold;color:orange">Note: please read key info regarding the semi-quantitative screen data <a href="https://environment.data.gov.uk/dataset/e85a7a52-7a75-4856-a0b3-8c6e4e303858" target="_blank">here</a></p>'),
  
    selectInput((NS(id,"gcms_compound")), 
                label = tooltip(
                  trigger = list(
                    "Choose compound:",
                    bs_icon("info-circle")
                  ),
                  "We have only listed a small subset of the 1000+ chemicals available here."
                ),
                # change to groups: e.g. Vet med.
                choices = ea_gcms_choices,
                selected = "Phenanthrene"
                ),
    tableOutput(NS(id,"chem_info")),
    # code("code displays your text similar to computer code"),
    sliderInput((NS(id,"year_slider")), "Select Year Range:",
                min = min(2013), max = max(2024),
                sep = "",
                value = c("2020", "2021"), animate = FALSE
    )
                
  )
}


pbms_sliders <- function(id) {               
  tagList(
      HTML('<b><center>Spatial trend input - predator</center></b>'), p(),
      HTML('<p align="center" style="font-weight: bold;color:orange">Note: The location of carcases collected and analysed by the PBMS cannot be used alone to interpret cause of death or indicate criminal activity.</p>'),
      
      selectInput(NS(id,'var_biota'), 'Choose biota:', choices = pbms_biota_choices),
      sliderInput(NS(id,"year_slider"), "Select Year Range:",
                  min = min(2000), max = 2024, 
                  sep = "",
                  animate = FALSE,
                  value = c("2005", "2015")
      ),
      # selectInput(NS(id,'var_sgar_map_sgl'), 'Choose a SGARs species:', choices = SGARs_choices, selected = NULL, multiple = FALSE),
      # selectInput(NS(id,'var_metal_map_sgl'), 'Choose a metal species:', choices = metals_choices, multiple = FALSE)
      
      # change to only slider, conditional on biota choice, update choices
      selectInput(NS(id,'var_map_sgl'), 'Choose a metal or SGARs species:', choices = list(`metals` = metals_choices, `SGARs` = SGARs_choices), multiple = FALSE)
      
  )
}


pfas_sliders <- function(id) {
  tagList(
    sliderInput(NS(id,"year_slider"), "Select Year Range:",
                min = 2004, max = 2023, 
                value = c(2014, 2023),
                sep = "",
                animate = FALSE
    ),
    selectInput(NS(id,"matrix"), "Choose matrix:",
                choices = c("Surface water", "Sea water", "Groundwater", "Unknown", "Wastewater", "Biota", "Drinking water", "Sediment")
    ),
    selectInput(NS(id,"substance"), "Choose substance:",
                choices =c(unique_pfas_names), selected = 'PFOS'
    ) ,
   selectInput(NS(id,"transform"), "Choose transform method:",
               choices =c("Natural Log")#c("Natural Log", "Base 10 Log")
   ),
   checkboxInput("heatmap", "Show Heatmap", FALSE)
  )
}

rain_sliders <- function(id) {
  tagList(
    sliderInput(NS(id,"year_slider"), "Select Year:",
                min = 2000, max = 2023, 
                value = 2023, 
                sep = "",
                step = 1)
    )
}

apiens_sliders <- function(id) {
  tagList(
    sliderInput(NS(id,"year_slider"), "Select Year Range:",
                min = 1998, max = 2023, 
                value = c(2018, 2020),
                sep = "",
                animate = FALSE
    ),
    # selectInput(NS(id,"matrix"), "Choose matrix:",
    #             choices = c("Surface water", "Sea water", "Groundwater", "Unknown", "Wastewater", "Biota", "Drinking water", "Sediment")
    # ),
    # selectInput(NS(id,"substance"), "Choose substance:",
    #             choices =c(unique_pfas_names), selected = 'PFOS'
    # ) ,
    # selectInput(NS(id,"transform"), "Choose transform method:",
    #             choices =c("Natural Log")#c("Natural Log", "Base 10 Log")
    # )
    selectInput(NS(id,"variable_choices"), "Choose variable:",
                choices = unique_apiens_varnames[1:20], #c("NH4-N","NO3-N"),
                selected = c("NH4-N","NO3-N"), #unique_apiens_varnames[2], #c("NH4-N","NO3-N")
                multiple = TRUE
    ),
    selectInput(NS(id,"necd_choices"), "Choose NECD classes:",
                choices =unique_apiens_NECD,
                selected = unique_apiens_NECD,
                multiple = TRUE
    ),
    p()
  )
}


euso_sliders <- function(id) {
  tagList(
    selectInput(NS(id,"euso_var_choices"), "Choose variable:",
                choices = c('Cu','Cd','Zn') 
    ),
    p()
  )
}

cats_dogs_sliders <- function(id) {
  tagList(
    p('Overlaying this dataset on the map may take a few seconds.'),
    selectInput(NS(id,"cats_or_dogs"), "Choose density:",
                choices = c('Estimated Cat Population ','Estimated Dog Population','Usual Residents') 
    ),
    p()
  )
}


IYR_sliders <- function(id) {
  tagList(
    selectInput(NS(id,"IYR_choice"), "Choose input to yield ratio for wheat:",
                choices = c("Pesticide risks to earthworms" = "earthworms",
                            "Pesticide risks to Honeybees" = "honeybees",
                            "N fertilisers" = "nitrogen_fertilisers",
                            "P fertilisers" = "phosphorus_fertilisers")
    ),
    p()
  )
}


Biotoxins_sliders <- function(id) {
  tagList(
    selectInput(NS(id,"biotoxin_choice"), "Choose biotoxin variable:",
                choices = c("Amnesic Shellfish Poison (ASP, mg/kg) " = "ASP(mgPerkg)")
    ),
    p()
  )
}


csv_upload_sliders <- function(id) {   
  
  tagList(
    # HTML('<p align="center" style="font-weight: bold;color:orange">Note: This data is not intended for comparing the pollution contributions of different sectors.</p>'),
    
    fileInput(NS(id,"csv_filepath"), "Upload your own CSV File", accept = ".csv"),
    
    tags$style("
             .btn-file {  
             background-color:#0483A4; 
             border-color: #0483A4; 
             }

             .progress-bar {
             background-color: #99ddff;
             }

             "),
    tableOutput(NS(id,"table")),
    checkboxInput(NS(id,"if_filter"), "Filter by column values?", TRUE),
    checkboxInput(NS(id,"if_colour"), "Colour points on map by column values?", TRUE),
    selectInput(NS(id,"lat_col"), "Choose latitude column:", choices = NULL), 
    selectInput(NS(id,"long_col"), "Choose longitude column:", choices = NULL), 
    
  )
}