# Week 8: Lecture 8 ----

## Initial Setup ----

library(tidyverse)
library(hoopR)

## SportsDataVerse ----

# Inspect the function

?load_nba_team_box

# Pull data
nba_team_box <- load_nba_team_box(seasons = 2024)


# Inspect the data
dim(nba_team_box)
names(nba_team_box)
glimpse(nba_team_box)
head(nba_team_box)


### Prepare the data (data cleaning) ----
nba_regular <- nba_team_box |>
  filter(season_type == 2)

nba_team_stats <- nba_regular |>
  group_by(team_short_display_name) |>
  summarise(
    games = n(),
    wins = sum(team_winner == TRUE, na.rm = TRUE), #just as a safety
    losses = sum(team_winner == FALSE, na.rm = TRUE),
    avg_pts = round(mean(team_score, na.rm = TRUE), 1),
    avg_fg_pct = round(mean(field_goals_made / field_goals_attempted * 100, na.rm = TRUE), 1),
    avg_3pt_made = round(mean(three_point_field_goals_made, na.rm = TRUE), 1),
    avg_rebounds = round(mean(total_rebounds, na.rm = TRUE), 1),
    avg_assists = round(mean(assists, na.rm = TRUE), 1),
    avg_turnovers = round(mean(turnovers, na.rm = TRUE), 1),
    avg_steals = round(mean(steals, na.rm = TRUE), 1),
    avg_blocks = round(mean(blocks, na.rm = TRUE), 1),
    .groups = "drop"
  ) |>
  mutate(
    win_pct = round(wins / games * 100, 1)
  ) |>
  arrange(desc(win_pct))

print(nba_team_stats, n = 30)
summary(nba_team_stats)
colSums(is.na(nba_team_stats))

# players
nba_player_box <- load_nba_player_box(seasons = 2024)

nba_player_stats <- nba_player_box |>
  filter(season_type == 2, minutes > 0) |>
  group_by(athlete_display_name, team_short_display_name) |> #certain players may moved teams
  summarise(
    games = n(),
    avg_pts = round(mean(points, na.rm = TRUE), 1),
    avg_rebounds = round(mean(rebounds, na.rm = TRUE), 1),
    avg_assists = round(mean(assists, na.rm = TRUE), 1),
    avg_fg_pct = round(mean(field_goals_made / field_goals_attempted * 100, na.rm = TRUE), 1),
    avg_3pt_made = round(mean(three_point_field_goals_made, na.rm = TRUE), 1),
    avg_minutes = round(mean(minutes, na.rm = TRUE), 1),
    .groups = "drop"
  ) |>
  filter(games >= 40) |>
  arrange(desc(avg_pts))

head(nba_player_stats, 20)

## save
saveRDS(nba_team_stats, "data/nba_2024_team_stats.rds")
write_csv(nba_team_stats, "data/nba_2024_team_stats.csv")

saveRDS(nba_player_stats, "data/nba_2024_player_stats.rds")
write_csv(nba_player_stats, "data/nba_2024_player_stats.csv")

list.files("data/raw")
list.files("data/cleaned")



