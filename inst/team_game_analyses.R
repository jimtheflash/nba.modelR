library(tidyverse)
library(ggridges)

# totals ------------------------------------------------------------------

totals <- tidier_data %>%
  filter(season_type == 'Regular Season', season_year == '2020-21') %>%
  group_by(game_id, season_year) %>%
  mutate(total_pts = sum(pts)) %>%
  ungroup() %>%
  select(team_name, game_id, total_pts) %>%
  group_by(team_name, game_id, total_pts) %>%
  distinct() %>%
  ungroup() %>%
  group_by(team_name) %>%
  mutate(med_pts = median(total_pts)) %>%
  ungroup() %>%
  arrange(med_pts) %>%
  mutate(team_name = fct_inorder(team_name))

ggplot(totals, aes(y = team_name, x = total_pts, fill = team_name)) +
  geom_density_ridges() +
  theme(legend.position = 'none')

ggplot(totals, aes(y = team_name, x = total_pts, fill = team_name)) +
  geom_boxplot(alpha = .5) +
  scale_x_continuous(breaks = seq(180, 300, 20)) +
  theme_bw() +
  theme(legend.position = 'none')

# spreads -----------------------------------------------------------------

spreads <- tidier_data %>%
  filter(season_type == 'Regular Season', season_year == '2020-21') %>%
  mutate(home_away = if_else(grepl('@', matchup), 'away', 'home')) %>%
  group_by(game_id, team_name, home_away, season_year) %>%
  mutate(team_pts = sum(pts)) %>%
  ungroup() %>%
  select(game_id, team_name, home_away, team_pts) %>%
  group_by(game_id, team_name, home_away, team_pts) %>%
  distinct() %>%
  ungroup() %>%
  group_by(game_id) %>%
  mutate(total_pts = sum(team_pts),
         opp_pts = total_pts - team_pts,
         margin = team_pts - opp_pts) %>%
  ungroup() %>%
  group_by(team_name) %>%
  mutate(med_margin = median(margin)) %>%
  ungroup() %>%
  arrange(med_margin) %>%
  mutate(team_name = fct_inorder(team_name))

ggplot(spreads, aes(y = team_name, x = margin, fill = team_name)) +
  geom_density_ridges() +
  scale_x_continuous(breaks = seq(-40, 40, 10)) +
  theme_bw() +
  theme(legend.position = 'none')

ggplot(spreads, aes(y = team_name, x = margin, fill = team_name)) +
  geom_boxplot(alpha = .5) +
  scale_x_continuous(breaks = seq(-40, 40, 10)) +
  theme_bw() +
  theme(legend.position = 'none')
