library(scales)
library(readr)
library(baseballr)
library(tidyverse)
library(stringi)
library(fuzzyjoin)
library(nflreadr)
library(zoo)
library(corrplot)

options(scipen = 99999999)

mlbcontracts_ids <- read_csv('mlbcontracts_ids.csv')

#scraping war totals
#MAKE ALL MILB DEALS HAVE 0 FOR AAV and 0 FOR YEARS
mlbcontracts_ids <- mlbcontracts_ids %>% 
  mutate(AAV = if_else(AAV == 1 | is.na(AAV), 0, AAV)) %>% 
  #remove johan santana due to missing data
  filter(Player != 'Santana, Johan')

#only hitters
hitters <- mlbcontracts_ids %>% 
  filter(Position != 'RP' | Position != 'CP', Position != 'SP') 

hitter_fg_ids <- as.vector(hitters$key_fangraphs)

#scrape stats

hitter_war <- fg_bat_leaders(2005, 2022, league = 'all', ind = 1, exc_p = TRUE, qual = 0)

hitter_war_small <- hitter_war %>% 
  select(playerid, Name, Season, Team, Age, PA, WAR, ISO, wRC_plus, `O-Swing_pct`, `Z-Swing_pct`, 
         SwStr_pct, BsR, Hard_pct, Soft_pct, K_pct, BB_pct) %>% 
  mutate(K_BB_diff = K_pct - BB_pct)

#remove dataset from environment
rm(hitter_war)

#only free agents now
hitter_war_small1 <- hitter_war_small %>% 
  filter(playerid %in% hitter_fg_ids)

#group by player and year
hitter_war_small <- hitter_war_small1 %>% 
  arrange(Name, Season) %>% 
  group_by(playerid) %>% 
  #edit the number of years into rolling war here
  mutate(WAR_rolling = rollsumr(WAR, k = 3, fill = NA),
         ISO_rolling = cummean(ISO * PA) / cummean(PA),
         WRC_rolling = cummean(wRC_plus * PA) / cummean(PA),
         O_Swing_rolling = cummean(`O-Swing_pct` * PA) / cummean(PA),
         Z_Swing_rolling = cummean(`Z-Swing_pct` * PA) / cummean(PA),
         SwStr_rolling = cummean(SwStr_pct * PA) / cummean(PA),
         BSR_rolling = rollsumr(BsR, k = 3, fill = NA),
         Hard_rolling = cummean(Hard_pct * PA) / cummean(PA),
         Soft_rolling = cummean(Soft_pct * PA) / cummean(PA),
         K_BB_diff_rolling = cummean(K_BB_diff * PA) / cummean(PA)) %>% 
  mutate(playerid = as.integer(playerid))

rm(hitter_war_small1)

#get war per pa 
hitter_war_pa <- hitter_war_small %>% 
  ungroup() %>% 
  group_by(playerid, Name, Season) %>% 
  mutate(war_per_pa = WAR / PA)

#rolling avg war per pa
hitter_war_pa <- hitter_war_pa %>% 
  ungroup() %>% 
  group_by(playerid) %>% 
  mutate(WAR_per_pa_rolling = cummean(war_per_pa * PA) / cummean(PA))

#join free agent years
hitter_fa_years <- hitters %>% 
  select(key_fangraphs, FAYear)

hitters_fa <- left_join(hitter_war_pa, hitter_fa_years, by = c('playerid' = 'key_fangraphs'))

rm(hitter_war_pa, hitter_war_small, hitter_fa_years)

#filter for FA year
hitters_fa <- hitters_fa %>% 
  filter(Season == FAYear)

#remove missing data
hitters_fa <- hitters_fa %>% 
  filter(!is.na(WAR_rolling), !is.na(SwStr_rolling), !is.na(O_Swing_rolling),
         !is.na(Hard_rolling), !is.na(Soft_rolling))


#join years and aav to above
fa <- mlbcontracts_ids %>% 
  select(key_fangraphs, FAYear, Years, AAV, Guarantee)

hitters <- left_join(hitters_fa, fa, by = c("playerid" = 'key_fangraphs', 'FAYear'))

rm(hitters_fa, fa)

#random testing
cor(hitters$WRC_rolling, hitters$AAV)^2

#select columns for correlation testing
cor_test <- hitters %>% 
  select(Age, WAR_rolling, ISO_rolling, WRC_rolling, O_Swing_rolling, Z_Swing_rolling, SwStr_rolling,
         BSR_rolling, K_BB_diff_rolling, Hard_rolling, Soft_rolling, WAR_per_pa_rolling, Years, AAV, Guarantee)

corr_hitters <- cor(cor_test)

rm(cor_test)

#correlation plot for hitter variables
corrplot::corrplot(corr_hitters, method = 'number')

#plot of rolling war to AAV
ggplot(hitters, aes(x = WAR_rolling, y = AAV)) +
  geom_point() +
  theme_minimal() +
  labs(
    y = 'Free Agent Contract AAV',
    x = '3 Year Cumulative fWAR',
    title = "WAR's Effect on Free Agent Value",
    caption = 'Data: @Fangraphs via baseballR | @ajaypatell8'
  )


#save csv
write_csv(hitters, 'hitters_contracts_stats.csv')

