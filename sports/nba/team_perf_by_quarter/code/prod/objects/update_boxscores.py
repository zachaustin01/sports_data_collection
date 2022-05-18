import requests
from nba_api.stats.endpoints import leaguegamefinder, boxscoretraditionalv2
from nba_api.stats.library.parameters import SeasonType, SeasonID, LeagueID
import time
import math as m
import pandas as pd
from datetime import date, timedelta

def pull_games(lake_file_path,season_yr,quarter):

  cur_date = (date.today()- timedelta(days=1)).strftime('%Y-%m-%d')
  
  old_file = pd.read_csv(lake_file_path)
  old_file_filtered = old_file[old_file['GAME_DATE']<cur_date]
  
  valid_ids = old_file_filtered['GAME_ID'].unique()
  
  gamefinder = leaguegamefinder.LeagueGameFinder(
    season_type_nullable=SeasonType.regular,
    season_nullable=season_yr,
    league_id_nullable=LeagueID.nba)
  games = gamefinder.get_data_frames()[0]
  
  games.to_csv(lake_file_path,index = False)
  
  all_games = games['GAME_ID'].unique()
  
  # Pull only game_ids that weren't in the old file
  game_ids = [x for x in all_games if int(x) not in valid_ids]

  k = 0
  boxscores = []
  for game in game_ids:
    k += 1
    try:
      boxscore_finder = boxscoretraditionalv2.BoxScoreTraditionalV2(
          game_id = game,
          range_type = 2,
          start_range = 0*min(1-quarter,0), 
          end_range = 21600*quarter)
      boxscore = boxscore_finder.get_data_frames()[0]
      boxscores.append(boxscore)
      time.sleep(0.5)
    except:
      print("Exception")
    print(f"{k} games read out of {len(game_ids)}")

  big_df = pd.concat(boxscores)
  return big_df
