library(tidyverse)
library(nflverse)
library(dplyr)

# What I want: Obtain the QB data who have played in the years 2020-2022

# 2022 test
season_data <- nflreadr::load_player_stats(seasons = 2022)
advanced_stats <- nflreadr::load_pfr_advstats(seasons = 2022)

starting_qbs <- season_data %>% 
  group_by(player_id) %>% 
  filter(position == 'QB')

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
  filter(total_attempts >= 200) 

sacks <- advanced_stats %>% 
  group_by(pfr_player_name) %>% 
  summarize(
    sacks = sum(times_sacked)
  ) %>% 
  rename(player_name = pfr_player_name) %>% 
  filter(player_name %in% qb_names)


url <- 'https://www.pro-football-reference.com/years/2022/passing.htm'
page <- read_html(url)
table <- page %>% 
  html_element('#passing') %>% 
  html_table()

record <- table
record$Player <- str_replace_all(record$Player, "\\*|\\+", "")
record <- record[which(record$Player != 'Player'), ]
colnames(record) <- make.unique(colnames(record))
  
record <- record %>%
  mutate(
    GS = as.numeric(GS),
    Wins = as.numeric(str_extract(QBrec, "\\d+")),
    Losses = as.numeric(str_extract(QBrec, "(?<=-)\\d+"))
  ) %>%
  filter(Pos == 'QB')
  
record <- record %>%
  rename(player_name = Player) %>% 
  group_by(player_name) %>% 
  filter(player_name %in% qb_names) %>% 
  summarize(
    Wins = as.numeric(Wins),
    Losses = as.numeric(Losses)
  ) 


merged_data <- left_join(regular_season_summary, sacks, by = "player_name") %>% 
  left_join(record, by = 'player_name') %>% 
  filter(!is.na(Wins))

contracts <- nflreadr::load_contracts() %>% 
  filter(position == 'QB', is_active == TRUE, player %in% qb_names) %>% 
  reframe(
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
  write.csv("Current_QB_Contracts.csv")


regular_season_valuation <- merged_data %>% 
  mutate(
    QB_Valuation = ((3.7 * completions) + (pass_yards / 5) + (11.3 * pass_tds) - (14.1 * interceptions) - (8 * sacks) - (1.1 * rushing_attempts) + (0.6 * rushing_yards) + (15.9 * rushing_tds) - (2.2 * total_attempts) - (2 * fumbles) + (0.9 * Wins) - (0.1 * Losses))
  )

post_season_summary <- starting_qbs %>% 
  filter(!is.na(attempts), season_type == 'POST') %>%
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
  ) 

quarterbacks <- nflreadr::load_schedules(seasons = 2022) %>%
  filter(week >= 19) %>%
  select(week, away_qb = away_qb_name, home_qb = home_qb_name)

merged_qbs <- quarterbacks %>%
  select(week, player_name = away_qb) %>%
  bind_rows(quarterbacks %>% select(week, player_name = home_qb))

highest_week_qb <- merged_qbs %>%
  group_by(player_name) %>%
  summarize(max_week = max(week))

merge_post <- post_season_summary %>% 
  left_join(highest_week_qb, by = 'player_name') %>% 
  filter(!is.na(max_week))

post_season_valuation <- merge_post %>%
  mutate(
    week_adjustment = ifelse(max_week == 19, 1,
                             ifelse(max_week == 20, 3,
                                    ifelse(max_week == 21, 6,
                                           ifelse(max_week == 22, 10, 0))))
    ) %>%
      mutate_at(vars(completions, pass_yards, pass_tds, interceptions, rushing_attempts, rushing_yards, rushing_tds, total_attempts, fumbles),
                ~ as.numeric(coalesce(., 0))
      ) %>%
      transmute(
        player_name,
        QB_Valuation = ((3.7 * completions) + (pass_yards / 5) + (11.3 * pass_tds) - (14.1 * interceptions) - (1.1 * rushing_attempts) + (0.6 * rushing_yards) + (15.9 * rushing_tds) - (2.2 * total_attempts) - (2 * fumbles) + week_adjustment)
      ) 


total_valuation <- regular_season_valuation %>%
  left_join(post_season_valuation, by = "player_name") %>%
  mutate(QB_Valuation = coalesce(QB_Valuation.x, 0) + coalesce(QB_Valuation.y, 0)) %>%
  select(-QB_Valuation.x, -QB_Valuation.y)

simple_valuation <- total_valuation %>% 
  reframe(
    player_name = player_name,
    team = team,
    QB_Valuation = QB_Valuation
  ) %>% 
  write_csv("QBs_with_Elo_Unweighted.csv")
