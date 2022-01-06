# !pip install nba_api

from time import sleep
import sys
import math as m
import pandas as pd
import requests
from nba_api.stats.endpoints import leaguegamefinder, boxscoretraditionalv2
from nba_api.stats.library.parameters import SeasonType, SeasonID, LeagueID

def get_box_score_df(g_id):
    bs = boxscoretraditionalv2.BoxScoreTraditionalV2(game_id = g_id).get_data_frames()[0]
    return bs

def get_all_box_scores(season_yr):
    gamefinder = leaguegamefinder.LeagueGameFinder(season_type_nullable=SeasonType.regular,season_nullable=season_yr,
                                                   league_id_nullable=LeagueID.nba)
                                                   
    games = gamefinder.get_data_frames()[0]
    
    all_games = list(games['GAME_ID'].unique())
        
    all_results = []
    
    N = len(all_games)
    
    print(f"Reading {N} games from NBA_API")
    
    n = 0
    i = 0
    partition = int(N/50)
    for g_id in all_games:
        if (n % partition == 0) or (n==0):
            sys.stdout.write('\r')
            # the exact output you're looking for:
            sys.stdout.write("[%-50s] %d%%" % ('='*i, 2*i))
            sys.stdout.flush()
            i += 1
        all_results.append(get_box_score_df(g_id))
        sleep(0.25)
        n += 1
    
    return all_results
