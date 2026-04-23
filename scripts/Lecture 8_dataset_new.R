# Week 8: Lecture 8: new dataset ----

## Initial Setup ----

install.packages("worldfootballR")
library(worldfootballR)

library(tidyverse)

## SportsDataVerse ----

# Inspect the function FBREF
?fb_league_urls
?player_dictionary_mapping
?fb_teams_urls
?load_match_results
?load_fb_big5_advanced_season_stats

# Pull data for men
prem <- load_match_results(country = c("ENG"), gender = "M", season_end_year = 2025,
                         tier = "1st")

# Pull data for WSL
prem_wsl <- load_match_results(country = c("ENG"), gender = "F", season_end_year = 2025,
                           tier = "1st")

# Inspect the data prem men
dim(prem)
names(prem)
glimpse(prem)
head(prem)

# Inspect the data prem WSL
dim(prem_wsl)
names(prem_wsl)
glimpse(prem_wsl)
head(prem_wsl)
unique(prem$Competition_Name)


### Prepare the data (data cleaning) ----
# make every observation into 1 row
home_teams <- prem %>%
  select(Competition_Name, Gender, Country, Season_End_Year, Wk, Day, Date, Time,
         Venue, Referee, Attendance,
         Team = Home, Opponent = Away,
         Goals = HomeGoals, xG = Home_xG,
         opp_Goals = AwayGoals, opp_xG = Away_xG) %>%
  mutate(Location = "Home")

away_teams <- prem %>%
  select(Competition_Name, Gender, Country, Season_End_Year, Wk, Day, Date, Time,
         Venue, Referee, Attendance,
         Team = Away, Opponent = Home,
         Goals = AwayGoals, xG = Away_xG,
         opp_Goals = HomeGoals, opp_xG = Home_xG) %>%
  mutate(Location = "Away")

prem_long <- bind_rows(home_teams, away_teams) %>%
  arrange(Date, Wk)

prem_long <- prem_long %>%
  mutate(Result = case_when(
    Goals > opp_Goals ~ "W",
    Goals < opp_Goals ~ "L",
    TRUE ~ "D"
  )) %>%
  mutate(poins = case_when(
    Goals > opp_Goals ~ 3,
    Goals < opp_Goals ~ 0,
    TRUE ~ 1
  )) %>%
  mutate(
    xG_diff  = Goals - xG,
    xG_perf  = case_when(
      xG_diff > 0.25 ~ "Overperformed",
      xG_diff < -0.25 ~ "Underperformed",
      TRUE            ~ "As Expected"
    ),
    xG_diff_against = opp_xG - opp_Goals
  )
  
# team stats
prem_team_stats <- prem_long %>%
  group_by(Team) %>%
  summarise(
    games = n(),
    wins = sum(Result == "W", na.rm = TRUE),
    draws = sum(Result == "D", na.rm = TRUE),
    losses = sum(Result == "L", na.rm = TRUE),
    points = sum(poins, na.rm = TRUE),
    goals_for = sum(Goals, na.rm = TRUE),
    goals_against = sum(opp_Goals, na.rm = TRUE),
    goal_diff = goals_for - goals_against,
    avg_xG = round(mean(xG, na.rm = TRUE), 2),
    avg_xG_against = round(mean(opp_xG, na.rm = TRUE), 2),
    avg_xG_diff = round(mean(xG_diff, na.rm = TRUE), 2),
    avg_xG_diff_against = round(mean(xG_diff_against, na.rm = TRUE), 2),
    games_overperformed = sum(xG_perf == "Overperformed", na.rm = TRUE),
    games_as_expected = sum(xG_perf == "As Expected", na.rm = TRUE),
    games_underperformed = sum(xG_perf == "Underperformed", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    win_pct         = round(wins / games * 100, 1),
    points_per_game = round(points / games, 2)
  ) %>%
  arrange(desc(points), desc(goal_diff))

# players


## save
saveRDS(nba_team_stats, "data/nba_2024_team_stats.rds")
write_csv(nba_team_stats, "data/nba_2024_team_stats.csv")

saveRDS(nba_player_stats, "data/nba_2024_player_stats.rds")
write_csv(nba_player_stats, "data/nba_2024_player_stats.csv")