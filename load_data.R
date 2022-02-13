# prepare tennis match data
rm(list=ls())
require(dplyr)
require(data.table)
setwd('/Users/vickimunn/Desktop/R Stuff/github/Data/')

# load match data
files <- list.files(pattern="atp_matches_[^_]*.csv")
Data <- do.call("rbind", lapply(files, function(x) fread(x)))

# clean data
Data[,tourney_date := as.Date(as.character(tourney_date) ,format='%Y%m%d', origin = "1900/01/01")]
Data[,`:=`(winner_id = as.character(winner_id),loser_is = as.character(loser_id))]

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
match.data <- Data[,.SD,.SDcols = match.vars]

# prepare tournament data
tourney.data <- Data[,.SD,.SDcols = tourney.vars] %>% distinct()

# prepare seedings data
seedings.data <- Data[,.SD,.SDcols = seed.vars]
seedings.data2 <- rbind(
  seedings.data[!is.na(winner_seed),.(tourney_id,winner_id,winner_seed)][,.(tourney_id, player_id = winner_id, seed = winner_seed)],
  seedings.data[!is.na(loser_seed),.(tourney_id,loser_id,loser_seed)][,.(tourney_id, player_id = loser_id, seed = loser_seed)])[order(tourney_id)] %>% distinct()
seedings.data <- seedings.data2
rm(seedings.data2,files,match.vars,seed.vars,tourney.vars)

# prepare player data
player.data <- Data[,.SD,.SDcols = player.vars]
player.data2 <- rbind(
  player.data[,.(winner_id,winner_name)][,.(player_id = winner_id, player_name = winner_name)],
  player.data[,.(loser_id,loser_name)][,.(player_id = loser_id, player_name = loser_name)])[order(player_id)] %>% distinct()
player.data <- player.data2
rm(player.data2,player.vars)

# ensure no duplicates in the player data
qa1 <- player.data[,.(count = .N),by = player_id][count>1,]
rm(qa1)

