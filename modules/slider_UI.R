unique_industry_sector <- data_process_EA_pollution()[['unique_industry_sector']]
#unique_industry_sector <- c('Agriculture','Water Industry')

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