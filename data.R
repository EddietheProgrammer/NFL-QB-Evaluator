library(tidyverse)
library(nflverse)
library(dplyr)
library(rvest)
library(stringr)

# What I want: Obtain the QB data who have played in 2022

# 2022 test
season_data <- nflreadr::load_player_stats(seasons = 2022)
qbr <- nflreadr::load_espn_qbr(league = 'nfl', season = 2022, summary_type = 'weekly')
advanced_stats <- nflreadr::load_pfr_advstats(seasons = 2022) 
passing_stats <- nflreadr::load_pfr_advstats(seasons = 2022, stat_type = 'pass', summary_level = 'week')
snap_counts <- nflreadr::load_snap_counts(seasons = 2022) %>% filter(position == 'QB')

qb_names <- season_data %>% 
  filter(position == 'QB') %>% 
  select(player_display_name) %>% 
  distinct()

qbs_from_season_data <- season_data %>% 
  group_by(player_display_name) %>% 
  filter(position == 'QB') %>% 
  reframe(
    team = recent_team, 
    week = week,
    season_type = season_type,
    completions = completions,
    attempts = attempts,
    passing_yards = passing_yards,
    passing_tds = passing_tds,
    interceptions = interceptions,
    rushing_attempts = carries,
    rushing_yards = rushing_yards,
    rushing_tds = rushing_tds,
    sacks = sacks,
    fumbles = sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost
  ) %>% 
  rename(name = player_display_name) 

times_pressured <- advanced_stats %>% 
  group_by(pfr_player_name) %>% 
  summarize(
    bad_throws = sum(passing_bad_throws),
    times_pressured = sum(times_pressured)
  ) %>% 
  rename(name = pfr_player_name)

## The metrics I want to consider:
## Regular Season: 
## (3.7 * Completions + 
## (PassingYards / 5) + 
## (11.3 * PassingTDs) - 
## (14.1* Interceptions) - 
## (8 * Sacks / times_pressured) - 
## (1.1 * RushAttempts) + 
## (0.6 * RushingYards) + 
## (15.9 * RushingTDs)- 
## (2.2 * PassAttempts) - 
## (2 * TotalFumbles) - 
## (1.1 * BadThrows) + (0.9 * Wins) + GWD
## Postseason: (3.7 * Completions + (PassingYards / 5) + (11.3 * PassingTDs) - (14.1* Interceptions) - (8 * Sacks / times_pressured) - (1.1 * RushAttempts) + (0.6 * RushingYards) + (15.9 * RushingTDs)- (2.2 * PassAttempts) - (2 * TotalFumbles) - (1.1 * BadThrows) + (0.9 * Wins) + GWD + poseason_finsish

## Stats I need: wins, game winning drives, postseason finish

# Extract the game winning drives, qb record, and player link if they have a game winning drive:
get_players <- function() {
  url <- 'https://www.pro-football-reference.com/years/2022/passing.htm#passing::gwd'
  html <- read_html(url)
  
  table <- html_table(html, fill = TRUE)
  return(table)
}

players_data <- get_players()
players_df <- players_data[[1]]
players_df$Player <- str_replace_all(players_df$Player, "\\*|\\+", "")
players_df <- players_df[which(players_df$Player != 'Player'), ]

colnames(players_df) <- make.unique(colnames(players_df))

players_df <- players_df %>%
  mutate(
    name = Player,
    GS = as.numeric(GS),
    Wins = as.numeric(str_extract(QBrec, "\\d+")),
    GWD = as.numeric(GWD)
  ) %>%
  filter(Pos == 'QB') %>% 
  select(-Player, -QBrec) %>% 
  reframe(
    name = name,
    GS = GS,
    Wins = Wins,
    GWD = GWD
  )

merged_data <- times_pressured %>% 
  left_join(players_df, by = "name")

View(merged_data)

contracts <- nflreadr::load_contracts() %>% 
  filter(position == 'QB', is_active == TRUE, player %in% qb_names) %>% 
  summarize(
    name = player,
    team = team,
    year_signed = year_signed,
    length = years,
    total = value,
    per_year = apy,
    guaranteed = guaranteed,
    inflated_total_value = inflated_value,
    inflated_guaranteed = inflated_guaranteed
  ) %>% 
  write.csv("Current_QB_Contracts.csv", row.names = FALSE)