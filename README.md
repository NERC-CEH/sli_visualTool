# Developer guide to adding new datasets to the JNCC visual tool

## Overview of app data handling

## Steps to add new datasets.
For each new dataset or dataset groups,
1. Add a new slider UI function `*_sliders` for that dataset in `modules/slider_UI.R`
2. Add a new data handling function `data_process_*` in `data_fun.R`
3. For map data, add `map_fun_*` in `map_fun.R`

Then in `data_modules.R`. Add call to these functiosn in the appropriate places.
1. Add the new dataset as an option in `dat_choices` in the module `datselect_mod_ui`
1. Add call to `*_sliders` in `output$ui_placeholder` in the module `datselect_mod_server`.
2. Add call to `data_process_` in `filtered_data` in the module `datselect_mod_server`.
3. Add call to `map_fun_*` at ... (work in progress)
