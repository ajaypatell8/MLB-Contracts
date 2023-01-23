library(corrplot)
library(scales)
library(readr)
library(baseballr)
library(tidyverse)
library(stringi)
library(fuzzyjoin)
library(nflreadr)
library(zoo)

mlbcontracts_ids <- read_csv('mlbcontracts_ids.csv')

mlbcontracts_ids <- mlbcontracts_ids %>% 
  mutate(AAV = if_else(AAV == 1 | is.na(AAV), 0, AAV))

#only pitchers
pitchers <- mlbcontracts_ids %>% 
  filter(Position == 'RP' | Position == 'CP' | Position == 'SP') 

pitcher_fg_ids <- as.vector(pitchers$key_fangraphs)

#scrape stats

pitcher_war <- fg_pitch_leaders(2005, 2022, league = 'all', ind = 1, pitcher_type = 'pit', qual = 0)

pitcher_war_small <- pitcher_war %>% 
  select(playerid, Name, Season, Team, Age, IP, `ERA-`, SV, `K-BB_pct`, HR_9, HR_FB,
         BABIP, `FIP-`, GB_pct, LD_pct, FB_pct, WAR, `xFIP-`, `O-Swing_pct`, `Z-Swing_pct`,
         SwStr_pct, SIERA, Soft_pct, Hard_pct, kwERA)

#remove dataset from environment
rm(pitcher_war)


#only free agents now
pitcher_war_small1 <- pitcher_war_small %>% 
  filter(playerid %in% pitcher_fg_ids)

#group by player and year
pitcher_war_small <- pitcher_war_small1 %>% 
  arrange(Name, Season) %>% 
  group_by(playerid) %>% 
  #edit the number of years into rolling war here
  mutate(WAR_rolling = rollsumr(WAR, k = 3, fill = NA),
         IP_rolling = rollsumr(IP, k = 3, fill = NA),
         ERA_Minus_rolling = cummean(`ERA-` * IP) / cummean(IP),
         SV_rolling = rollsumr(SV, k =3 , fill = NA),
         K_BB_diff_rolling = cummean(`K-BB_pct` * IP) / cummean(IP),
         HR_9_rolling = cummean(HR_9 * IP) / cummean(IP),
         HR_FB_rolling = cummean(HR_FB * IP) / cummean(IP),
         BABIP_rolling = cummean(BABIP * IP) / cummean(IP),
         FIP_Minus_rolling = cummean(`FIP-` * IP) / cummean(IP),
         xFIP_Minus_rolling = cummean(`xFIP-` * IP) / cummean(IP),
         SIERA_rolling = cummean(SIERA * IP) / cummean(IP),
         kwERA_rolling = cummean(kwERA * IP) / cummean(IP),
         GB_pct_rolling = cummean(GB_pct * IP) / cummean(IP),
         LD_pct_rolling = cummean(LD_pct * IP) / cummean(IP),
         FB_pct_rolling = cummean(FB_pct * IP) / cummean(IP),
         O_Swing_rolling = cummean(`O-Swing_pct` * IP) / cummean(IP),
         Z_swing_rolling = cummean(`Z-Swing_pct` * IP) / cummean(IP),
         SwStr_rolling = cummean(SwStr_pct * IP) / cummean(IP),
         Soft_pct_rolling = cummean(Soft_pct * IP) / cummean(IP),
         Hard_pct_rolling = cummean(Hard_pct * IP) / cummean(IP)) %>% 
  mutate(playerid = as.integer(playerid))

rm(pitcher_war_small1)

#get war per ip
pitcher_war_pa <- pitcher_war_small %>% 
  ungroup() %>% 
  group_by(playerid, Name, Season) %>% 
  mutate(war_per_ip = WAR / IP)

#rolling avg war per ip
pitcher_war_pa <- pitcher_war_pa %>% 
  ungroup() %>% 
  group_by(playerid) %>% 
  mutate(WAR_per_ip_rolling = cummean(war_per_ip * IP) / cummean(IP))

#join free agent years
pitcher_fa_years <- pitchers %>% 
  select(key_fangraphs, FAYear)

pitchers_fa <- left_join(pitcher_war_pa, pitcher_fa_years, by = c('playerid' = 'key_fangraphs'))

rm(pitcher_war_pa, pitcher_war_small, pitcher_fa_years)

#filter for FA year
pitchers_fa <- pitchers_fa %>% 
  filter(Season == FAYear)

#remove missing data
pitchers_fa <- na.omit(pitchers_fa)

#join years and aav to above
fa <- mlbcontracts_ids %>% 
  select(key_fangraphs, FAYear, Years, AAV, Guarantee, Position)

pitchers <- left_join(pitchers_fa, fa, by = c("playerid" = 'key_fangraphs', 'FAYear'))

rm(pitchers_fa, fa)

#fix missing data, zach davies smh
pitchers <- pitchers %>% 
  mutate(Years = if_else(Name == 'Zach Davies' & Season == 2022, 1, Years))

#random testing
cor(pitchers$WAR_rolling, pitchers$AAV)^2

#select columns for correlation testing
cor_test <- pitchers %>% 
  select(Age, WAR_rolling, IP_rolling, ERA_Minus_rolling, SV_rolling, K_BB_diff_rolling, HR_9_rolling,
         HR_FB_rolling, BABIP_rolling, FIP_Minus_rolling, xFIP_Minus_rolling, SIERA_rolling, kwERA_rolling,
         GB_pct_rolling, LD_pct_rolling, FB_pct_rolling, O_Swing_rolling, Z_swing_rolling, SwStr_rolling,
         Soft_pct_rolling, Hard_pct_rolling, WAR_per_ip_rolling, 
         Years, AAV, Guarantee)

corr_pitchers <- cor(cor_test)

rm(cor_test)

#correlation plot for hitter variables
corrplot::corrplot(corr_pitchers, method = 'color')

#plot of rolling war to AAV
ggplot(pitchers, aes(x = WAR_rolling, y = AAV)) +
  geom_point() +
  theme_minimal() +
  labs(
    y = 'Free Agent Contract AAV',
    x = '3 Year Cumulative fWAR',
    title = "WAR's Relationship To Free Agent Value",
    caption = 'Data: @Fangraphs via baseballR | @ajaypatell8'
  )

#save csv
write_csv(pitchers, 'pitchers_contracts_stats.csv')
