# prepare tennis match data
rm(list=ls())
require(dplyr)
require(data.table)
setwd('/Users/vickimunn/Desktop/R Stuff/github/Data/')

# load match data
files <- list.files(pattern="atp_matches_[^_]*.csv")
Data <- do.call("rbind", lapply(files, function(x) read.csv(x, sep=",",stringsAsFactors = FALSE)))

# clean data
Data$tourney_date <- as.Date(as.character(Data$tourney_date),format='%Y%m%d', origin = "1900/01/01")
Data <- Data %>% mutate(winner_id = as.character(winner_id),
                        loser_id = as.character(loser_id))

# dictate what variables get assigned to what data
match.vars <- c(
  'tourney_id',
  'tourney_date',
  'match_num',
  'winner_id',
  'loser_id',
  'score',
  'round',
  'minutes'
)
tourney.vars <- c(
  'tourney_id',
  'tourney_name',
  'tourney_date',
  'surface',
  'draw_size'
)
seed.vars <- c(
  'tourney_id',
  'winner_id',
  'loser_id',
  'winner_seed',
  'loser_seed',
  'round'
)
player.vars <- c(
  'winner_id',
  'winner_name',
  'loser_id',
  'loser_name'
)

# prepare the match data, which is simply a subet (source Data is already one row per match)
match.data <- Data[,match.vars]

# prepare tournament data
tourney.data <- Data[,tourney.vars] %>%
  distinct()

# prepare seedings data
seedings.data <- Data[,seed.vars]
seedings.data2 <- rbind(
  (seedings.data[!is.na(seedings.data$winner_seed),c('tourney_id','winner_id','winner_seed')] %>% rename(player_id = winner_id, seed = winner_seed)),
  (seedings.data[!is.na(seedings.data$loser_seed),c('tourney_id','loser_id','loser_seed')] %>% rename(player_id = loser_id, seed = loser_seed))
) %>% distinct() %>% 
  arrange(tourney_id)
seedings.data <- seedings.data2
rm(seedings.data2,files,match.vars,seed.vars,tourney.vars)

# prepare player data
player.data <- Data[,player.vars]
player.data2 <- rbind(
  player.data[,c('winner_id','winner_name')] %>% rename(player_id = winner_id, player_name = winner_name),
  player.data[,c('loser_id','loser_name')] %>% rename(player_id = loser_id, player_name = loser_name) 
) %>% distinct()
player.data <- player.data2 %>% arrange(player_id)
rm(player.data2,player.vars)

# ensure no duplicates in the player data
qa.1 <- player.data %>% group_by(player_id) %>% summarise(rows = n()) %>% filter(rows > 1)
qa.2 <- player.data %>% group_by(player_name) %>% summarise(rows = n()) %>% filter(rows > 1)
rm(qa.1,qa.2)

