library(tidyverse)
library(nflverse)
library(dplyr)

# What I want: Obtain the QB data who have played in the years 2020-2022

# 2022 test
season_data <- nflreadr::load_player_stats(seasons = 2022)

starting_qbs <- season_data %>% 
  filter(position == 'QB', sum(attempts) >= 200) %>% 
  group_by(player_id) %>% 
  View()

qb_names <- starting_qbs %>% 
  pull(player_display_name)

regular_season_summary <- starting_qbs %>% 
  filter(!is.na(attempts), season_type == 'REG') %>%
  group_by(player_id) %>% 
  summarise(
    player_name = first(player_display_name),
    team = first(recent_team),
    completions = sum(completions),
    total_attempts = sum(attempts),
    pass_yards = sum(passing_yards),
    pass_tds = sum(passing_tds),
    interceptions = sum(interceptions),
    passing_air_yards = sum(passing_air_yards),
    passing_epa = sum(passing_epa),
    rushing_attempts = sum(carries),
    rushing_yards = sum(rushing_yards),
    rushing_tds = sum(rushing_tds),
    rushing_epa = sum(rushing_epa),
    fumbles = sum(sack_fumbles_lost, rushing_fumbles_lost, receiving_fumbles_lost)
  ) %>%
  filter(total_attempts >= 200) %>% 
  write.csv("Regular_Season_QB_Stats.csv", row.names=FALSE)

contracts <- nflreadr::load_contracts() %>% 
  filter(position == 'QB', is_active == TRUE, player %in% qb_names)
View(contracts)

