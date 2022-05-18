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

p = c('reticulate','tidyverse','zoo')
load_all_packages(p)

################################################################################

g_date = Sys.Date()
source_python(file.path(d_sport_project,'bs_pull.py'))
# player_box_score_df = get_all_box_scores('2021-22')
player_box_score_df = read.csv(file.path(d_sport_data,paste0('player_box_scores_ao_',g_date,'.csv')))
team_box_score_df = read.csv(file.path(d_sport_data,paste0('team_box_scores_ao_',g_date,'.csv')))

merged_df = team_box_score_df %>%
  select(SEASON_ID,
         TEAM_ID,
         TEAM_ABBREVIATION,
         GAME_ID,
         GAME_DATE,
         MATCHUP) %>%
  arrange(SEASON_ID,TEAM_ABBREVIATION,GAME_DATE) %>%
  left_join(player_box_score_df,by=c("GAME_ID"="GAME_ID","TEAM_ID"="TEAM_ID")) %>%
  mutate(MIN = round(as.numeric(substr(MIN,1,str_locate(MIN,":")-1)) + as.numeric(substr(MIN,str_locate(MIN,":")+1,5))/60,2)) %>%
  rename("TEAM_ABBREVIATION" = "TEAM_ABBREVIATION.x") %>%
  select(-TEAM_ABBREVIATION.y)

tbl_df = merged_df %>%
  filter(!is.na(MIN))

# Create tidy DF
tidy_df = tbl_df %>%
  gather(key="STAT","VALUE",13:32) %>%
  filter(!is.na(VALUE))

merged_df = tidy_df %>%
  left_join(tbl_df,by=c("SEASON_ID", "TEAM_ID", "TEAM_ABBREVIATION", "GAME_ID",
                        "GAME_DATE", "MATCHUP", "TEAM_CITY", "PLAYER_ID",
                        "PLAYER_NAME", "NICKNAME", "START_POSITION", "COMMENT"))

rolling_df = merged_df %>%
  group_by(PLAYER_NAME,STAT) %>%
  arrange(PLAYER_NAME,STAT,GAME_DATE) %>%
  mutate(ROLLING_5_VALUE = rollmean(VALUE,5,fill = NA),
         ROLLING_10_VALUE = rollmean(VALUE,10,fill = NA),
         ROLLING_3_VALUE = rollmean(VALUE,3,fill = NA),
         AVG_VALUE = cummean(VALUE))

old_merged_df = read.csv(file.path(d_sport_data,"dashboard_data.csv"))

dashboard_df = rbind(old_merged_df,merged_df)

max(dashboard_df$GAME_DATE)

write.csv(dashboard_df,file.path(d_sport_data,"dashboard_data.csv"),row.names = F)
