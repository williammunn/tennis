# store current directory
library(dplyr)
library(data.table)
library(lubridate)
dir <- getwd()
setwd('/Users/williammunn/Documents/Github/tennis/Data/')

# load match data
files <- list.files(pattern="atp_matches_[^_]*.csv")
Data <- do.call("rbind", lapply(files, function(x) fread(x)))

# clean data
Data[,tourney_date := as.Date(as.character(tourney_date) ,format='%Y%m%d', origin = "1900/01/01")]
Data[,`:=`(winner_id = as.character(winner_id),loser_id = as.character(loser_id))]

# dictate what variables get assigned to what data
match.vars <- c('tourney_id','tourney_date','winner_id','loser_id','score','round','minutes')
tourney.vars <- c('tourney_id','tourney_name','tourney_date','surface','draw_size')
seed.vars <- c('tourney_id','winner_id','loser_id','winner_seed','loser_seed','round')
player.vars <- c('winner_id','winner_name','loser_id','loser_name')

# prepare the match data, which is simply a subet (source Data is already one row per match)
match.data <- Data[!(round %in% c("BR","ER")),.SD,.SDcols = match.vars] ; setkey(match.data,round)
sortorderdf <- data.table(round=c("RR","R128","R64","R32","R16","QF","SF","F"),order=1:8) ; setkey(sortorderdf,round)
match.data <- sortorderdf[match.data][order(tourney_id,order)] ; rm(sortorderdf)
match.data[,match_num := seq_len(.N), by = .(tourney_id)]

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

# ensure that the no the loser of a match does not play in a match with a higher ordering
perplayer <- rbind(match.data[,.(tourney_id,player_id=winner_id,order,results='w')],match.data[,.(tourney_id,player_id=loser_id,order,results='l')])[order(tourney_id,player_id,order)]
qa1 <- perplayer[,.(max_round = max(order)), by = .(tourney_id,player_id,results)]
bestwin <- qa1[results == 'w',.(tourney_id,player_id,maxwin=max_round)] ; setkey(bestwin,tourney_id,player_id)
bestloss <- qa1[results == 'l',.(tourney_id,player_id,maxloss=max_round)] ; setkey(bestloss,tourney_id,player_id)
problem <- bestloss[bestwin][maxwin > maxloss & !is.na(maxloss)] ; setkey(problem,tourney_id)
tourney <- distinct(Data[,.(tourney_id,tourney_name)]) ; setkey(tourney,tourney_id)
tourney[problem]

rm(bestloss,bestwin,perplayer,problem,qa1,tourney)

# re-set working directory
setwd(dir)
