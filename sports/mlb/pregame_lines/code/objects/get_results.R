################################################################################
# Linescores

dates = seq(as.Date("2022-05-18"), Sys.Date()-1, by="days")
pks = c()

for(i in 1:length(dates)){

  print(dates[i])
  pk = mlb_game_pks(dates[i])$game_pk
  pks = c(pks,pk)
}

t = mlb_game_linescore(game_pk = pks[1])
for(i in 2:length(pks)){
  print(paste0(i,' out of ',length(pks)))
  res = mlb_game_linescore(game_pk = pks[i])
  t = rbind(t,res,fill = T)
}

# Update File
write.csv(t,file.path(data_out,'game_results','linescores.csv'),row.names = F)
################################################################################

# PKS
################################################################################

pks = mlb_game_pks(dates[1])
for(i in 2:length(dates)){

  print(dates[i])
  pk = mlb_game_pks(dates[i])
  pks = rbind(pks,pk,fill = T)
}

write.csv(pks,file.path(data_out,'game_results','pks.csv'),row.names = F)
################################################################################

# Merged

pks = read.csv(file.path(data_out,'game_results','pks.csv'))
linescores = read.csv(file.path(data_out,'game_results','linescores.csv'))

################################################################################

game_date_df = pks %>%
  select(game_pk,
         officialDate,
         teams.away.team.name,
         teams.home.team.name,
         gameNumber) %>%
  unique() %>%
  filter(officialDate < Sys.Date())

scores_df = linescores %>%
  select(game_pk,
         away_team_abbreviation,
         home_team_abbreviation,
         num,
         home_runs,
         away_runs) %>%
  rename("INN" = "num") %>%
  group_by(game_pk) %>%
  mutate(total_home_runs = cumsum(home_runs),
         total_away_runs = cumsum(away_runs)) %>%
  select(-c(home_runs,away_runs)) %>%
  fill(total_home_runs,total_away_runs)

fi_scores = scores_df %>%
  filter(INN==1) %>%
  select(-INN) %>%
  mutate(total_runs = total_home_runs + total_away_runs,
         home_lead = total_home_runs - total_away_runs,
         CATEGORY = "FI")

f5_scores = scores_df %>%
  filter(INN==5) %>%
  select(-INN) %>%
  mutate(total_runs = total_home_runs + total_away_runs,
         home_lead = total_home_runs - total_away_runs,
         CATEGORY = "F5")

fg_scores = scores_df %>%
  group_by(game_pk) %>%
  arrange(game_pk,INN) %>%
  mutate(max_INN = max(INN)) %>%
  ungroup() %>%
  filter(max_INN == INN) %>%
  select(-c(INN,max_INN)) %>%
  mutate(total_runs = total_home_runs + total_away_runs,
         home_lead = total_home_runs - total_away_runs,
         CATEGORY = "FG")

results = rbind(fi_scores,f5_scores,fg_scores) %>%
  arrange(game_pk)

final_results = game_date_df %>%
  left_join(results,by="game_pk")

names(final_results)

final_results_noloc =
  rbind(
  final_results %>%
  rename("TEAM_NAME" = "teams.away.team.name",
         "OPP_NAME" = "teams.home.team.name",
         "TEAM_NAME_A" = "away_team_abbreviation",
         "OPP_NAME_A" = "home_team_abbreviation",
         "TEAM_RUNS" = "total_away_runs",
         "OPP_RUNS" = "total_home_runs") %>%
  mutate(TEAM_LEAD = -home_lead,
         HOME = 0)
  ,
  final_results %>%
  rename("TEAM_NAME" = "teams.home.team.name",
         "OPP_NAME" = "teams.away.team.name",
         "TEAM_NAME_A" = "home_team_abbreviation",
         "OPP_NAME_A" = "away_team_abbreviation",
         "TEAM_RUNS" = "total_home_runs",
         "OPP_RUNS" = "total_away_runs") %>%
  mutate(TEAM_LEAD = home_lead,
         HOME = 1)
  ) %>%
  arrange(game_pk) %>%
  select(-home_lead)

write.csv(final_results_noloc,file.path(data_out,'merged','merged1.csv'),row.names = F)

names(final_results_noloc)

tidy_merged = final_results_noloc %>%
  gather(key="")
