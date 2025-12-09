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
library(plotly)


qbr <- load_espn_qbr(2025)
pbp <- load_pbp(2025)


qbr_r <- qbr %>%
  select(name_short, 
         qbr_total,
         epa_total,
         qb_plays,
         headshot_href,
         pass, run,
         qb_plays) %>%
  mutate(epa_pass_perc = (100 * pass)/(run + pass),
         epa_run_perc = (100 * run)/(run + pass),
         epa_pp = epa_total / qb_plays)

a2 = c(30, 45, 60, 75, 90, 105, 120, 135, 150, 165)
b2 = c(rep(-1, length(a2)))

qbr_r$z <- ((qbr_r$qbr_total) + (qbr_r$epa_total))/2

qbr_r %>%
  filter(qb_plays >= 100) %>%
  ggplot(aes(x = epa_total, y = qbr_total)) +
  geom_tile(aes(fill = z)) +
  geom_point(shape = 21, size = 3, color = "black") + 
  geom_abline(intercept = a2, slope = b2, color = "gray", linetype = "longdash") + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") + 
  geom_image(aes(image = headshot_href), size = 0.08, asp = 16/9) +
  scale_fill_gradient2(
    low = "lightcoral",    # better "light red"
    mid = "white",
    high = "lightgreen",
    midpoint = mean(qbr_r$z, na.rm = TRUE),
    guide = "none"
  ) + 
  theme_bw() + 
  labs(
    title = "NFL QB's total QBR and EPA through Week 7",
    x = "Total EPA (Rushing and Passing)",
    y = "Total QBR",
    caption = "Created By: Adam White"
  ) +
  theme(
    plot.title = element_text(size = 24, face = 'bold', hjust = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18, face = 'bold'),
    axis.title.y = element_text(size = 18, face = 'bold')
  )


## PRO FOOTBALL REFERENCE DATA
pfr <- load_pfr_advstats(stat_type = "pass",summary_level = "season")


qbr <- load_espn_qbr(2025)


qbr_r <- qbr %>%
  select(name_short, 
         qbr_total,
         epa_total,
         qb_plays,
         headshot_href,
         pass, run) %>%
  mutate(epa_pass_perc = (100 * pass)/(run + pass),
         epa_run_perc = (100 * run)/(run + pass),
         epa_pp = epa_total / qb_plays)


on_tgt <- pfr %>%
  filter(season == 2025 & pass_attempts >= 100) %>%
  mutate(player_name = player %>% str_replace_all("^(\\w)\\w+ (?=\\w)", "\\1."),
         player_name = player_name %>% str_replace(" ", ""))

qbr_r <- qbr_r %>%
  mutate(player_name = name_short %>% str_replace_all("^(\\w)\\w+ (?=\\w)", "\\1."),
         player_name = player_name %>% str_replace(" ", ""))

qbr_r$player_name[which(qbr_r$player_name == "M.Penix Jr.")] <- "M.Penix"


on_tgt <- on_tgt %>%
  inner_join(qbr_r, by = "player_name")

names(on_tgt)

df_ontgt_qbr <- on_tgt %>%
  select(team,
         player_name,
         pass_attempts,
         drop_pct,
         bad_throw_pct,
         times_blitzed,
         times_pressured,
         pressure_pct,
         on_tgt_pct,
         qbr_total,
         headshot_href,
         epa_pp,
         pocket_time,
  )

names(df_ontgt_qbr)

## Lets look at on tgt pct and qbr_total for corr
df_ontgt_qbr %>%
  ggplot(aes(x = on_tgt_pct, y = qbr_total)) + 
  geom_point() + 
  geom_hline(yintercept = mean(df_ontgt_qbr$qbr_total), color = "gray", 
             linetype = "dashed") + 
  geom_vline(xintercept = mean(df_ontgt_qbr$on_tgt_pct), color = "gray", 
             linetype = "dashed") + 
  geom_image(aes(image = headshot_href), size = 0.08, asp = 16/9) + 
  geom_label_repel(aes(label = player_name),
                   box.padding = 0.5, 
                   point.padding = 0.5, 
                   segment.color = 'grey50') + 
  theme_bw() + 
  labs(x = "On Target Throw % (PFR)",
       y = "Total QBR This Season (ESPN)",
       title = "On target throw % and total QBR by QB",
       caption = "Created By: Adam White using nflreadR",
       subtitle = "Data from Pro Football Reference (PFR) & EPSN") + 
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = 0.5,),
        panel.grid.major.y = element_line(size = 0.1),
        panel.grid.major.x = element_line(size = 0.1),
        plot.subtitle = element_text(size = 13, hjust = 0.5),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 16, face = "bold"))

ggsave('qbr_ontgt.png', width = 14, height = 10, dpi = 'retina')

## lets look at pct pressured and bad throw pct - 
df_ontgt_qbr %>%
  ggplot(aes(x = pressure_pct, y = bad_throw_pct)) + 
  geom_point() + 
  stat_smooth(method = "lm", se = FALSE) + 
  geom_image(aes(image = headshot_href), size = 0.08, asp = 16/9) + 
  geom_label_repel(aes(label = player_name),
                   box.padding = 0.5, 
                   point.padding = 0.5, 
                   segment.color = 'grey50') + 
  theme_bw() + 
  labs(x = "Pressure %",
       y = "Bad Throw %",
       title = "Does percentage pressured affect bad throw percentage?",
       caption = "Created By: Adam White using nflreadR",
       subtitle = "Data from Pro Football Reference") + 
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = 0.5,),
        panel.grid.major.y = element_line(size = 0.1),
        plot.subtitle = element_text(size = 13, hjust = 0.5),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 16, face = "bold"))

ggsave('bad_throw_pressure.png', width = 14, height = 10, dpi = 'retina')

# on tgt pct and drop pct - POST THIS ONE
on_tgt_qbr <- df_ontgt_qbr %>%
  ggplot(aes(x = on_tgt_pct, y = drop_pct)) + 
  geom_point() + 
  stat_smooth(method = "lm", se = FALSE, linetype = "dashed") + 
  geom_image(aes(image = headshot_href), size = 0.08, asp = 16/9) + 
  geom_label_repel(aes(label = player_name),
                   box.padding = 0.5, 
                   point.padding = 0.5, 
                   segment.color = 'grey50') + 
  theme_bw() +
  labs(x = "On Target Percentage",
       y = "Drop Percentage",
       title = "NFL Qb's on target throws verus drop percentage",
       subtitle = "Data from Pro Football Reference",
       caption = "Created By: Adam White using nflreadR") + 
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = 0.5,),
        panel.grid.major.y = element_line(size = 0.1),
        plot.subtitle = element_text(size = 13, hjust = 0.5),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 16, face = "bold"))

ggsave('on_target_drops.png', width = 14, height = 10, dpi = 'retina')





pbp_q <- load_pbp(2025)


pbp_attempts <- pbp_q %>%
  filter(play_type == "pass" & !is.na(passer_player_id),
         !is.na(air_yards))


pbp_attempts <- pbp_attempts %>%
  group_by(passer_player_id, passer_player_name) %>%
  summarize(n = n(),
            sum(yards_gained)) 

pbp_q <- pbp_q %>%
  filter(play_type == "pass" & !is.na(passer_player_id),
         !is.na(air_yards))

pbp_comp_pct <- pbp_q %>%
  group_by(passer_player_id, passer_player_name, air_yards) %>%
  filter() %>%
  summarise(
         comp_pct = mean(complete_pass),
         yds = sum(passing_yards))


pbp_comp_pct %>%
  select(passer_player_name,
         air_yards,
         comp_pct,
         yds) %>%
  arrange(passer_player_name, -air_yards)

pbp_comp_pct <- pbp_comp_pct %>%
  left_join(pbp_attempts, by = c("passer_player_id", "passer_player_name"))

pbp_comp_pct <- pbp_comp_pct %>%
  filter(n >= 150)

league_avg <- pbp_comp_pct %>%
  group_by(air_yards) %>%
  summarise(
    comp_pct = mean(comp_pct, na.rm = TRUE)
  )

comp_pct_yds <- pbp_comp_pct %>%
  ggplot(aes(x = air_yards, y = comp_pct)) +
  geom_smooth(mapping = aes(x = air_yards, y = comp_pct), se = FALSE) + 
  geom_smooth(
    data = league_avg,
    aes(x = air_yards, y = comp_pct),
    color = "red",
    se = FALSE,
    size = 1.2
  ) +
  facet_wrap(~ passer_player_name, scales = "free_y") + 
  xlim(-10,50) +  ylim(0,1) +
  labs(x = "Air Yards",
       y = "Completion Percentage",
       title = "Modeled Completion Percentage for each QB (BLUE) Versus League Average (RED)") + 
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = 0.5,),
        panel.grid.major.y = element_line(size = 0.1),
        plot.subtitle = element_text(size = 13, hjust = 0.5),
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 16, face = "bold"))

ggsave("qb_comp_pct_air_yds.png", width = 14, height = 10, dpi = 'retina')  
