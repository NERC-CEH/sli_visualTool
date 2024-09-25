unique_industry_sector <- data_process_EA_pollution()[['unique_industry_sector']]
#unique_industry_sector <- c('Agriculture','Water Industry')
pbms_biota_choices =  c('Buzzard','Sparrowhawk','Otter')
unique_pfas_names <- data_process_pfas()[['unique_pfas_names']]


ea_pollution_sliders <- function(id) {               
  tagList(
    selectInput(NS(id,"IndustrySector"), "Choose Industry Sector:",
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
                sep = "",
                value = c("2020", "2021"), animate = FALSE
    )
  )
}


pbms_sliders <- function(id) {               
  tagList(
      HTML('<b><center>Spatial trend input - predator</center></b>'), p(),
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
   )
  )
}

