
library(stringr)
(cnames <-read_excel('www/Visual tool data catalogue.xlsx')%>% 
  names())

data_catalogue <- read_excel('www/Visual tool data catalogue.xlsx',skip=1) %>% 
  mutate(`Dataset name` = ifelse(str_detect(`Link to dataset`,'https'),
                                 paste0('<a href="',`Link to dataset` ,  '">',`Dataset name` ,'</a>'),
                                   `Dataset name`)) %>% 
  select(-`Link to dataset`) %>% 
  rename_with(~str_c("Case study:", .), all_of(colnames(.)[9:13]))



datatable(data_catalogue, escape = FALSE , class = 'cell-border stripe', rownames = F,
          caption = 'Table 1: List of datasets included in the visual tool.')
