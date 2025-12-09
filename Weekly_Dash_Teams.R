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


pbp <- load_pbp(2025)


### Practice filtering, truncating, and graphing ---------

# load run or pass plays where EPA not null
pbp_rp <- pbp %>%
  filter(rush == 1 | pass == 1) %>%
  filter(!is.na(epa), !is.na(yards_gained))

# group by the team with possession 
offense_25 <- pbp_rp %>%
  group_by(posteam) %>%
  summarize(off_snaps = n(),
            epa_off_25 = mean(epa),
            yards_gained = mean(yards_gained))

defense_25 <- pbp_rp %>%
  filter(rush == 1 | pass == 1) %>%
  group_by(defteam) %>%
  summarize(def_snaps = n(),
            epa_def_25 = -mean(epa),
            yards_allowed = mean(yards_gained))

off_def_25 = offense_25 %>%
  left_join(defense_25, by = c("posteam" = "defteam"))

### CAREFUL TO RUN ONLY ONCE OR DUPE LOGOS, ETC.
off_def_25 <- off_def_25 %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

# Offensive and defensive EPA for 2025 (Update weekly)
off_def_25 %>%
  ggplot(aes(x = epa_off_25, y = epa_def_25)) +
  geom_hline(yintercept = mean(off_def_25$epa_def_25), linetype = 'dashed') + 
  geom_vline(xintercept = mean(off_def_25$epa_off_25), linetype = 'dashed') +
  geom_image((aes(image = team_logo_espn)), size = 0.05, asp = 16/9) +
  theme_bw() + 
  labs(x = "Offensive EPA",
       y = "Defensive EPA", 
       title = "Offensive and Defensive EPA by Team in 2025",
       caption = "Created By: Adam White") + 
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = 0.5,),
        plot.subtitle = element_text(size = 18, hjust = 0.5),
        panel.grid.major.y = element_line(size = 0.1),
        legend.position = c(0.85,0.15),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 22),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = 'bold'),
        axis.title.y = element_text(size = 18, face = 'bold'))

ggsave('off-def-EPA-25.png', width = 14, height = 10, dpi = 'retina')


## Offensive and Defensive YPP for 2025
off_def_25 %>%
  ggplot(aes(x = yards_gained, y = yards_allowed)) +
  geom_hline(yintercept = mean(off_def_25$yards_allowed), linetype = 'dashed') + 
  geom_vline(xintercept = mean(off_def_25$yards_gained), linetype = 'dashed') +
  geom_image((aes(image = team_logo_espn)), size = 0.05, asp = 16/9) +
  theme_bw() + 
  scale_y_reverse() + 
  labs(x = "Offensive YPP",
       y = "Defensive YPP (Allowed)", 
       title = "Offensive and Defensive YPP by Team in 2025",
       caption = "Created By: Adam White") + 
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = 0.5,),
        panel.grid.major.y = element_line(size = 0.1),
        legend.position = c(0.85,0.15),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 22),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18, face = 'bold'),
        axis.title.y = element_text(size = 18, face = 'bold'))

ggsave('off-def-YPP-25.png', width = 14, height = 10, dpi = 'retina')
