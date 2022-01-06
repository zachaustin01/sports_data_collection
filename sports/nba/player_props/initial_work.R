# rm(list=ls())

################################################################################

sport = "nba"
project = "player_props"
d_base = "C:/Users/zacha/OneDrive/Desktop"

d_code = file.path(d_base,"sports_data_collection")
d_sport_code = file.path(d_code,"sports",sport)
d_sport_project = file.path(d_sport_code,project)

d_data = file.path(d_base,"sports_data_storage")
d_sport_data = file.path(d_data,"sports",sport)
d_sport_data_project = file.path(d_sport_data,project)

d_tools = file.path(d_code,"tools")

source(file.path(d_tools,"package_load_funcs.r"))

p = c('reticulate','tidyverse')
load_all_packages(p)

################################################################################

source_python(file.path(d_sport_project,'bs_pull.py'))
