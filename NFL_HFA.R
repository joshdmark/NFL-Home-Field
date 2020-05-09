library(tidyverse)
library(data.table)
library(sqldf)
library(gridExtra)
library(RCurl)
library(lubridate)

## data provided by Kaggle: "NFL scores and betting data" 
## https://www.kaggle.com/tobycrabtree/nfl-scores-and-betting-data 

## get game scores data
url_scores <- RCurl::getURL('https://raw.githubusercontent.com/joshdmark/NFL-Home-Field/master/spreadspoke_scores.csv')
score_data <- read.csv(text = url_scores, stringsAsFactors = FALSE) %>% data.frame()

## get teams data
url_teams <- RCurl::getURL('https://raw.githubusercontent.com/joshdmark/NFL-Home-Field/master/nfl_teams.csv')
teams <- read.csv(text = url_teams, stringsAsFactors = FALSE) %>% data.frame()

## clean data
## FILTERS:
# 1) has gambling odds
# 2) after 2000 season
# 3) regular season only
scores <- score_data %>% 
  filter(!is.na(spread_favorite) &    
           schedule_season >= 2000 &  
           schedule_playoff == FALSE) 

## add team ids
scores <- sqldf('select s.*, t2.team_id as home_team_id, t3.team_id as away_team_id
             from scores s 
             left join teams t2 on s.team_home = t2.team_name 
             left join teams t3 on s.team_away = t3.team_name') %>% distinct() %>% 
  mutate(schedule_date = mdy(schedule_date)
         ,schedule_playoff = as.numeric(schedule_playoff)
         ,stadium_neutral = as.numeric(stadium_neutral)
         ,total = score_home + score_away) %>% 
  ## select cols needed
  select(game_dt = schedule_date
         ,season = schedule_season 
         ,week = schedule_week 
         ,home_team_name = team_home 
         ,home_team_id
         ,score_home
         ,away_team_name = team_away 
         ,away_team_id
         ,score_away
         ,favorite = team_favorite_id 
         ,spread = spread_favorite
         ,over_under = over_under_line
         ,total
         ,stadium_neutral)

## home field advantage data
hfa <- scores %>% 
  filter(stadium_neutral == 0) %>% data.table() %>% 
  ## add column for favored team's score
  .[, favorite_score := score_away] %>% 
  .[favorite == home_team_id, favorite_score := score_home] %>% 
  data.frame() %>% 
  ## add column for underdog team's score 
  mutate(underdog_score = ifelse(favorite_score == score_home, 
                                 score_away, score_home), 
         diff = favorite_score - underdog_score)

## loop for each team's cover pct at home / on road 
team_ids <- unique(hfa$home_team_id)
## create data frames for loop outputs 
all_results <- data.frame()
season_results <- data.frame()
all_games <- data.frame() 
## begin loop (1 loop per team, 32x)
for (team in team_ids){
  print(team)
  team_data <- hfa %>% filter(home_team_id == team | away_team_id == team)
  team_data <- team_data %>% 
    mutate(team_id = team, 
           favored_ind = as.numeric(favorite == team_id), 
           location = ifelse(home_team_id == team_id, 'Home', 'Away'), 
           team_score = ifelse(team_id == home_team_id, 
                               score_home, score_away), 
           opp_score = ifelse(team_id == home_team_id, 
                              score_away, score_home), 
           team_diff = team_score - opp_score, 
           team_spread = ifelse(favored_ind == 1, abs(spread), spread), 
           cover_ind = ifelse(team_diff > team_spread, 1, 0))
  ## create copy of team_data for all teams 
  team_games <- team_data 
  
  ## get cover results by team
  team_results <- team_data %>% 
    group_by(team_id, location, cover_ind) %>% 
    summarise(games = n())
  ## get team's cover results by season
  team_results_season <- team_data %>% 
    group_by(team_id, season, location, cover_ind) %>% 
    summarise(games = n())
  
  all_results <- suppressWarnings(bind_rows(all_results, team_results))
  season_results <- suppressWarnings(bind_rows(season_results, team_results_season))
  all_games <- suppressWarnings(bind_rows(all_games, team_games))
}

## team cover pcts home vs away
cover_pcts_all <- all_results %>% 
  mutate(covers = ifelse(cover_ind == 1, games, 0)) %>% 
  group_by(team_id, location) %>% 
  summarise(games = sum(games), covers = sum(covers)) %>% 
  mutate(cover_pct = covers / games) %>% 
  select(team_id, location, cover_pct) %>% 
  spread(key = location, value = cover_pct) %>% 
  mutate(diff = Home - Away) %>% arrange((-diff))

## best TEAM home (SEA)
## 43% ATS away, 54% ATS at home
cover_pcts_all[which.max(cover_pcts_all$diff), ]

## worst season at home (NYG)
## 55% ATS away, 42% ATS at home
cover_pcts_all[which.min(cover_pcts_all$diff), ]

## team cover pcts home vs away (by)
cover_pcts_season <- season_results %>% 
  mutate(covers = ifelse(cover_ind == 1, games, 0)) %>% 
  group_by(team_id, season, location) %>% 
  summarise(games = sum(games), covers = sum(covers)) %>% 
  mutate(cover_pct = covers / games) %>% 
  select(team_id, season, location, cover_pct) %>% 
  spread(key = location, value = cover_pct) %>% 
  mutate(diff = Home - Away)

## best season at home (ARI 2003)
## 0-8 ATS away, 6-2 ATS at home
cover_pcts_season[which.max(cover_pcts_season$diff), ]

## worst season at home (NYG 2018)
## 7-1 ATS away, 1-7 ATS at home
cover_pcts_season[which.min(cover_pcts_season$diff), ]

## write files for Tableau viz
fwrite(cover_pcts_season, 'Desktop/SPORTS/NFL_HFA_outputs/cover_pcts_season.csv')
fwrite(cover_pcts_all, 'Desktop/SPORTS/NFL_HFA_outputs/cover_pcts_all.csv') 
fwrite(season_results, 'Desktop/SPORTS/NFL_HFA_outputs/season_results.csv')
fwrite(all_results, 'Desktop/SPORTS/NFL_HFA_outputs/all_results.csv')
fwrite(all_games, 'Desktop/SPORTS/NFL_HFA_outputs/all_games.csv')

## facts
# SEA has the most to LOSE from playing at neutral sites (biggest difference ATS home vs away).
# NYG has the most to GAIN from playing at neutral sites (13% more games covered away than at home).
# Season that mattered the MOST for home field? 2003 ARI 6-2 ATS at home, 0-8 ATS away. 
# Season that mattered the LEAST for home field? 2018 NYG 7-1 ATS, 1-7 ATS home. 
# NE has been good regardless of location. 58% ATS at home AND away. On to Cincinnati. 

## sample plots
cover_pcts %>% 
  ggplot() + 
  geom_col(aes(team_id, Home), size = 2, fill = 'dodgerblue') + 
  geom_col(aes(team_id, Away), size = 1, fill = 'grey', alpha = .5) + 
  coord_flip()

cover_pcts_all %>% 
  ggplot() + 
  geom_point(aes(Home, Away)) + 
  geom_label(aes(Home, Away, label = team_id, fill = team_id)) + 
  theme(legend.position = 'none')
