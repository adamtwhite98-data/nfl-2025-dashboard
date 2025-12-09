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

### AIR YARDS VS YAC CHART ####################
pbp <- load_pbp(2025)

pbp_p <- pbp %>%
  filter(pass_attempt == 1) %>%      # BEST OPTION
  filter(!is.na(receiving_yards))    # keep only targeted/completed plays


pbp_s_wr <- pbp_p %>%
  group_by(receiver_player_id, receiver_player_name) %>%
  summarize(
    team_abbr = last(posteam),
    catches = sum(complete_pass == 1, na.rm = TRUE),
    total_yards = sum(receiving_yards, na.rm = TRUE),
    air_yards = sum(air_yards, na.rm = TRUE),
    yac = sum(yards_after_catch, na.rm = TRUE)
  ) %>%
  arrange(desc(total_yards))

pbp_s_wr_yac <- pbp_s_wr %>%
  pivot_longer(
    cols = c(yac, air_yards),
    names_to = "yards type",
    values_to = "yards total"
  ) %>%
  arrange(-total_yards)


# top 20 receivers by air yards 
pbp_s_wr_yac[1:40,] %>%
  ggplot(aes(x = `yards total`, 
             y = fct_reorder(receiver_player_name, `yards total`),
             fill = `yards type`)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = `yards total`),
            position = position_stack(vjust = 0.5, reverse = TRUE),
            color = "white", size = 5, fontface = 'bold' ) +
  geom_text(data = pbp_s_wr_yac[1:40,], aes(x = total_yards, 
                                     y = fct_reorder(receiver_player_name, `yards total`), 
                                     label = total_yards),
            vjust = 0.5, hjust = 'outward',
            color = "black", size = 5, fontface = 'bold') +
  theme_bw() + 
  labs(x = "Total Receiving Yards",
       y = "Player Name",
       title = "Top 20 NFL Receiving Yards Leaders",
       caption = "Created By: Adam White",
       fill = "Yards Type") + 
  scale_fill_manual(values = c("#663831", "#376f32"),
                    labels = c("Air Yards", "YAC")) + 
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = 0.5),
        panel.grid.major.y = element_line(size = 0.1),
        legend.position = c(0.85,0.15),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 22),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = 'bold'),
        axis.title.y = element_text(size = 18, face = 'bold'))


ggsave('Air_YAC_yards_25.png', width = 14, height = 10, dpi = 'retina')


### EXPLOSIVE PLAYS % versus Catches per game chart #####################


pbp_25 <- load_pbp(2025)

teams_colors_logos <- load_teams()

#names(pbp_25)

pbp_pass <- pbp_25 %>%
  filter(play_type == "pass") %>%
  filter(!is.na(epa) & !is.na(receiver_id)) %>%
  mutate(receiving_yards = ifelse(is.na(receiving_yards), 0, receiving_yards),
         explosive = ifelse(air_yards >= 20, 1, 0))

pbp_pass_exp <- pbp_pass %>%
  filter(!is.na(receiver_id)) %>%
  mutate(explosive = ifelse(air_yards >= 20, 1, 0))

players <- load_players()

pbp_pass_player <- pbp_pass_exp %>%
  group_by(receiver_id, receiver) %>%
  summarize(explosive_play_pct = mean(explosive) * 100,
            n = n(),
            total_receiving_yards = sum(receiving_yards),
            total_tds = sum(touchdown),
            team_abbr = last(posteam),
            avg_epa = mean(epa)) %>%
  filter(n > 40) %>%
  arrange(-avg_epa)

pbp_pass_player_id <- pbp_pass_player %>%
  inner_join(players, by = c("receiver_id" = "gsis_id"))

pbp_pass_player_id <- pbp_pass_player_id %>%
  inner_join(teams_colors_logos, by = c("latest_team" = "team_abbr"))

# EXPLOSIVE PLAY % VS AVG EPA PER PLAY
pbp_pass_player_id %>%
  ggplot(aes(x = n, y = explosive_play_pct)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) + 
  geom_label_repel(aes(label = receiver),
                   box.padding = 0.5, 
                   point.padding = 0.5, 
                   segment.color = 'grey50') + 
  theme_bw() + 
  labs(x = "Total Targets",
       y = "Explosive play percentage",
       title = "Receiver Explosive Play Percentage and Total Targets",
       subtitle = "An explosive play is considered any catch where the ball traveled over 20 air yards (Min. 40 targets)") + 
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = 0.5,),
        panel.grid.major.y = element_line(size = 0.1),
        plot.subtitle = element_text(size = 13, hjust = 0.5),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 16, face = "bold"))

ggsave('exp_play_wrs.png', width = 14, height = 10, dpi = 'retina')



### FANTASY VOLATILITY CHART ############################

weekly_player_stats <- nflreadr::load_player_stats(
  seasons = 2025,
  summary_level = "week", 
)
weekly_rushing_yards <- weekly_player_stats %>%
  # Keep only regular season data
  filter(season_type == "REG") %>%
  filter(position == "WR") %>%
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
    receptions,
    receiving_yards
  ) %>%
  # Filter out players who had zero receiving yards in a game
  filter(receiving_yards > 0) %>%
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
            catches = sum(receptions),
            recent_team = last(team)) %>%
  filter(catches >= 30,
         avg_ppr_pts >= 10) %>%
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
       title = "Fantasy WRs and their Volatility in 2025",
       subtitle = "Min. 30 catches & greater than 10 PPR PPG") + 
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = 0.5,),
        panel.grid.major.y = element_line(size = 0.1),
        plot.subtitle = element_text(size = 13, hjust = 0.5),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 16, face = "bold"))

ggsave('fantasy_vars_wrs.png', width = 14, height = 10, dpi = 'retina')


### WR Stats so far

season_player_stats <- nflreadr::load_player_stats(
  seasons = 2025,
  summary_level = "reg", 
)

names(season_player_stats)

wr_season_stats <- season_player_stats %>%
  filter(position == "WR" | position == "TE" | position == "RB") %>%
  select(player_id,
         player_name,
         games,
         receptions,
         targets,
         target_share, 
         receiving_yards,
         receiving_tds,
         receiving_yards_after_catch,
         receiving_air_yards,
         receiving_first_downs,
         receiving_epa,
         target_share,
         wopr,
         racr,
         air_yards_share,
         fantasy_points_ppr) %>%
  filter(targets >= 10) %>%
  arrange(-receiving_yards)

