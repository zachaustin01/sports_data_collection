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

merged_df[is.na(merged_df)] <- 0

info_df = merged_df %>%
  select(SEASON_ID,
         TEAM_ID,
         TEAM_ABBREVIATION,
         GAME_ID,
         GAME_DATE,
         MATCHUP,
         TEAM_CITY,
         PLAYER_ID,
         PLAYER_NAME,
         START_POSITION,
         COMMENT) %>%
  unique()

cumulative_stats = merged_df %>%
  select(GAME_DATE,
         PLAYER_ID,
         MIN,
         FGM,
         FGA,
         FG_PCT,
         FG3M,
         FG3A,
         FG3_PCT,
         FTM,
         FTA,
         FT_PCT,
         OREB,
         DREB,
         REB,
         AST,
         STL,
         BLK,
         TO,
         PF,
         PTS,
         PLUS_MINUS) %>%
  filter(MIN>0) %>%
  group_by(PLAYER_ID) %>%
  arrange(PLAYER_ID,GAME_DATE) %>%
  mutate(
    AVG_MIN = round(cummean(MIN),2),
    AVG_FGM = round(cummean(FGM),2),
    AVG_FGA = round(cummean(FGA),2),
    AVG_FG_PCT = round(cummean(FG_PCT),2),
    AVG_FG3M = round(cummean(FG3M),2),
    AVG_FG3A = round(cummean(FG3A),2),
    AVG_FG3_PCT = round(cummean(FG3_PCT),2),
    AVG_FTM = round(cummean(FTM),2),
    AVG_FTA = round(cummean(FTA),2),
    AVG_FT_PCT = round(cummean(FT_PCT),2),
    AVG_OREB = round(cummean(OREB),2),
    AVG_DREB = round(cummean(DREB),2),
    AVG_REB = round(cummean(REB),2),
    AVG_AST = round(cummean(AST),2),
    AVG_STL = round(cummean(STL),2),
    AVG_BLK = round(cummean(BLK),2),
    AVG_TO = round(cummean(TO),2),
    AVG_PF = round(cummean(PF),2),
    AVG_PTS = round(cummean(PTS),2),
    TOTAL_PLUS_MINUS = round(cumsum(PLUS_MINUS),2)
  )

final_df = info_df %>%
  left_join(cumulative_stats,by=c("GAME_DATE"="GAME_DATE","PLAYER_ID"="PLAYER_ID"))


write.csv(final_df,file.path(d_sport_data_project,paste0('visualization_data_ao_',g_date,'.csv')),row.names = F)

# Create tidy DF
tidy_df = final_df %>%
  gather(key="STAT","VALUE",12:51) %>%
  filter(!is.na(VALUE))

write.csv(tidy_df,file.path(d_sport_data_project,paste0('tidy_visualization_data_ao_',g_date,'.csv')),row.names = F)

