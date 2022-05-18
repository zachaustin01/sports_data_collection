################################################################################
#
# File to run collection of daily pregame MLB lines
# - Author: Zach Austin
#
################################################################################

rm(list=ls())

################################################################################

project_name = 'sports_data_collection/sports/mlb/pregame_lines'

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
repo_path = file.path(substr(getwd(),0,gregexpr(pattern = project_name ,
                                                getwd())[[1]][1]-2),
                      project_name)

source(file.path(repo_path,
                 'code',
                 'utilities',
                 'load_directories.r'),
       local = knitr::knit_global())

p <- c("tidyverse"
)

load_all_packages(p)

################################################################################

source(file.path(objects_path,'bet_scraping_dk.r'))

f5_lines = f5_odds() %>%
  mutate(GAME_DATE = Sys.Date(),
         CATEGORY = "F5")

nrfi_lines = nrfi_odds() %>%
  mutate(GAME_DATE = Sys.Date(),
         CATEGORY = "FI")

game_lines = full_game() %>%
  mutate(GAME_DATE = Sys.Date(),
         CATEGORY = "FG")

final = rbind(f5_lines,nrfi_lines,game_lines)

write.csv(final,file.path(data_out,paste0('game_lines_',Sys.Date(),'.csv')),row.names = F)



