setwd("/Users/Adam/Sports Analytics Hobby/NFL Analytics/nfl-2025-dashboard/")

nflreadr::.clear_cache()

library(flexdashboard)
library(nflreadr)
library(ggplot2)
library(nflfastR)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(gt)
library(ggimage)
library(gtExtras)
library(kableExtra)

## EXPLOSIVE PLAY % ################# 

pbp_25 <- load_pbp(2025)

teams_colors_logos <- load_teams()

#names(pbp_25)

pbp_run <- pbp_25 %>%
  filter(play_type == "run") %>%
  mutate(rushing_yards = ifelse(is.na(rushing_yards), 0, rushing_yards),
         explosive = ifelse(rushing_yards >= 10, 1, 0))

setwd("/Users/Adam/Sports Analytics Hobby/NFL Analytics/nfl-2025-dashboard/")
pbp_run_exp <- pbp_run %>%
  filter(!is.na(rusher_id)) %>%
  mutate(explosive = ifelse(rushing_yards >= 10, 1, 0))

players <- load_players()

pbp_run_player <- pbp_run_exp %>%
  group_by(rusher_id, rusher) %>%
  summarize(explosive_play_pct = mean(explosive) * 100,
            n = n(),
            total_rushing_yards = sum(rushing_yards),
            total_tds = sum(touchdown),
            team_abbr = last(posteam),
            avg_epa = mean(epa)) %>%
  filter(n > 50) %>%
  arrange(-avg_epa)

pbp_run_player_id <- pbp_run_player %>%
  inner_join(players, by = c("rusher_id" = "gsis_id"))

pbp_run_player_id <- pbp_run_player_id %>%
  inner_join(teams_colors_logos, by = c("latest_team" = "team_abbr"))

# EXPLOSIVE PLAY % VS AVG EPA PER PLAY
pbp_run_player_id %>%
  ggplot(aes(x = avg_epa, y = explosive_play_pct)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) + 
  geom_label_repel(aes(label = rusher),
                   box.padding = 0.5, 
                   point.padding = 0.5, 
                   segment.color = 'grey50') + 
  theme_bw() + 
  labs(x = "Average EPA per play",
       y = "Explosive play percentage",
       title = "Running Back Explosive Play and EPA Comparison",
       subtitle = "An explosive play is considered any rush 10 yards or greater (Min. 50 carries)") + 
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = 0.5,),
        panel.grid.major.y = element_line(size = 0.1),
        plot.subtitle = element_text(size = 13, hjust = 0.5),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 16, face = "bold"))

ggsave('exp_play_rbs.png', width = 14, height = 10, dpi = 'retina')



## RYOE and RYOE per play ################# 
pbp_r <- load_pbp(2015:2025)

pbp_run <- pbp_r %>%
  filter(play_type == "run" & !is.na(rusher_id) &
           !is.na(down) & !is.na(run_location)) %>%
  mutate(rushing_yards = ifelse(is.na(rushing_yards), 0, rushing_yards))

pbp_run <- pbp_run %>%
  mutate(down = as.character(down))

expected_yards_run <- 
  lm(rushing_yards ~ down * ydstogo + yardline_100 + 
       run_location + score_differential + shotgun,
     data = pbp_run)


pbp_run <- pbp_run %>%
  mutate(ryoe = resid(expected_yards_run))

ryoe_r <- pbp_run %>%
  group_by(season, rusher_id, rusher) %>%
  summarize(n = n(), 
            ryoe_total = sum(ryoe),
            ryoe_per = mean(ryoe),
            yards_per_carry = mean(rushing_yards),
            team_abbr = last(posteam)) %>%
  filter(n > 50) %>%
  arrange(-ryoe_total) %>%
  print(n = 10)

ryoe_2025 <- ryoe_r %>%
  filter(season == 2025)

ryoe_2025 <- ryoe_2025 %>%
  inner_join(teams_colors_logos, by = "team_abbr")

ryoe_2025 %>%
  ggplot(aes(x = n, y = ryoe_total)) + 
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) + 
  geom_label_repel(aes(label = rusher),
                                  box.padding = 0.5, 
                                  point.padding = 0.5, 
                                  segment.color = 'grey50') + 
  theme_bw() + 
  labs(x = "Total Carries (runs only)",
       y = "Total RYOE this season",
       title = "Running Back RYOE (rush yards over expected) Totals",
       subtitle = "Minimum 50 carries") + 
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = 0.5,),
        panel.grid.major.y = element_line(size = 0.1),
        plot.subtitle = element_text(size = 13, hjust = 0.5),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 16, face = "bold"))

ggsave('ryoe_rbs.png', width = 14, height = 10, dpi = 'retina')


### Fantasy football RB #############

weekly_player_stats <- nflreadr::load_player_stats(
  seasons = 2025,
  summary_level = "week", 
)
weekly_rushing_yards <- weekly_player_stats %>%
  # Keep only regular season data
  filter(season_type == "REG") %>%
  filter(position == "RB") %>%
  # Select the key columns for game-by-game analysis
  select(
    player_name,
    player_id,
    team,
    season,
    week,
    rushing_yards,# Also useful to see air yards per game
    fantasy_points_ppr,
    rushing_2pt_conversions,
    carries
  ) %>%
  # Filter out players who had zero receiving yards in a game
  filter(rushing_yards > 0) %>%
  # Sort by player, then by week
  arrange(player_name, week)

rbs_avg <- weekly_rushing_yards %>%
  group_by(player_id) %>%
  summarize(Name = last(player_name),
            avg_yds = mean(rushing_yards),
            avg_ppr_pts = mean(fantasy_points_ppr),
            min_ppr = min(fantasy_points_ppr),
            max_ppr = max(fantasy_points_ppr),
            sd_ppr = sd(fantasy_points_ppr),
            games = n(),
            carries = sum(carries),
            recent_team = last(team)) %>%
  filter(carries >= 50,
         avg_ppr_pts >= 9) %>%
  arrange(-sd_ppr)


rbs_avg <- rbs_avg %>%
  inner_join(teams_colors_logos, by = c("recent_team" = "team_abbr"))

rbs_avg %>%
  arrange(-avg_ppr_pts) %>%
  ggplot(aes(x = reorder(Name, avg_ppr_pts), y = avg_ppr_pts)) + 
  geom_boxplot(aes(
    lower = avg_ppr_pts - sd_ppr,
    upper = avg_ppr_pts + sd_ppr,
    middle = avg_ppr_pts,
    ymin = min_ppr,
    ymax = max_ppr,
    fill = team_color,
    color = team_color2
  ),
  stat = "identity",
  width = 0.5,
  ) + 
  coord_flip() +
  theme_bw() + 
  scale_fill_identity() + 
  scale_color_identity() + 
  labs(y = "Average PPR Points, Minimum, Maximum, and ± the Standard Deviation",
       x = "Player Name",
       title = "Fantasy RBs and their Volatility in 2025",
       subtitle = "Min. 50 carries & greater than 9 PPR PPG") + 
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = 0.5,),
        panel.grid.major.y = element_line(size = 0.1),
        plot.subtitle = element_text(size = 13, hjust = 0.5),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 16, face = "bold"))

ggsave('fantasy_vars_rbs.png', width = 14, height = 10, dpi = 'retina')


## RB Updated Stats Table Format ################# 

names(weekly_player_stats)

season_player_stats <- nflreadr::load_player_stats(
  seasons = 2025,
  summary_level = "reg", 
)

rb_season_stats <- season_player_stats %>%
  filter(position == "RB") %>%
  select(player_id,
         player_name,
         player_display_name,
         carries,
         rushing_yards,
         rushing_tds,
         rushing_fumbles,
         rushing_fumbles_lost,
         rushing_first_downs,
         rushing_epa,
         targets,
         receptions,
         receiving_yards,
         receiving_tds,
         receiving_epa,
         fantasy_points_ppr
  ) %>%
  mutate(total_epa = receiving_epa + rushing_epa) %>%
  filter(carries >= 50) %>%
  arrange(-rushing_yards)

