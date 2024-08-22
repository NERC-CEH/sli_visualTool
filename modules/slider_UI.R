unique_industry_sector <- data_process_EA_pollution()[['unique_industry_sector']]
#unique_industry_sector <- c('Agriculture','Water Industry')
pbms_biota_choices =  c('Buzzard','Sparrowhawk','Otter')

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
                value = c("2020", "2021"), animate = TRUE
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
                  animate = TRUE,
                  value = c("2005", "2015")
      ),
      selectInput(NS(id,'var_sgar_map_sgl'), 'Choose a SGARs speices:', choices = SGARs_choices, selected = NULL, multiple = FALSE),
      selectInput(NS(id,'var_metal_map_sgl'), 'Choose a metal spcies:', choices = metals_choices, multiple = FALSE)
  )
}