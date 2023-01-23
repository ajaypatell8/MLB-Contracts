library(tidyverse)
library(readr)
library(stringi)

mlbcontracts <- read_csv("mlb_fa_contracts.csv")

mlbcontracts %>% 
  group_by(PlayerAgent) %>% 
  summarise(total_money_no_comma = sum(Guarantee), total_money = scales::comma(sum(Guarantee))) %>% 
  arrange(desc(total_money_no_comma)) %>% 
  View()

mlbcontracts %>% 
  filter(FAYear <= 2022) %>% 
  arrange(desc(AAV)) %>% 
  View()

#next step is to get ids for each player in
mlbplayerids <- baseballr::get_chadwick_lu() 

mlbplayerids <- mlbplayerids %>% 
  select(name_last, name_first, key_mlbam, key_fangraphs, key_bbref, mlb_played_first, mlb_played_last) %>% 
  filter(!is.na(key_mlbam), !is.na(key_fangraphs), !is.na(key_bbref), !is.na(mlb_played_first))

#clean strings
mlbplayerids$name_last <- stri_trans_general(mlbplayerids$name_last, id = 'Latin-ASCII')
mlbplayerids$name_first <- stri_trans_general(mlbplayerids$name_first, id = 'Latin-ASCII')

mlbplayerids$name_last <- nflreadr::clean_player_names(mlbplayerids$name_last)
mlbplayerids$name_first <- nflreadr::clean_player_names(mlbplayerids$name_first)

mlbplayerids$name_first <- str_remove(mlbplayerids$name_first, " ")

mlbcontracts$`First Name` <- nflreadr::clean_player_names(mlbcontracts$`First Name`)
mlbcontracts$`Last Name` <- nflreadr::clean_player_names(mlbcontracts$`Last Name`)

mlbcontracts$`First Name` <- stri_trans_general(mlbcontracts$`First Name`, id = 'Latin-ASCII')
mlbcontracts$`Last Name` <- stri_trans_general(mlbcontracts$`Last Name`, id = 'Latin-ASCII')

#we want current players only that are in the dataset
mlbplayerids <- mlbplayerids %>% 
  filter(mlb_played_last >= 2013)

#join to contract data
mlbcontracts_ids <- left_join(mlbcontracts, mlbplayerids, by = c("First Name" = 'name_first', 
                                                                 "Last Name" = 'name_last'))


write_csv(mlbcontracts_ids, 'mlbcontracts_ids.csv')
