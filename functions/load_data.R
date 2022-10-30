require(data.table);require(lubridate);require(dplyr)
setwd('/Users/williammunn/Documents/Github/tennis/Data/')

# load match data
files <- list.files(pattern="atp_matches_[^_]*.csv")
Data <- do.call("rbind", lapply(files, function(x) fread(x)))

# clean data
Data[,tourney_date := as.Date(as.character(tourney_date) ,format='%Y%m%d', origin = "1900/01/01")]
Data[,`:=`(winner_id = as.character(winner_id),loser_id = as.character(loser_id))]

# dictate what variables get assigned to what data
match.vars <- c('tourney_id','tourney_date','winner_id','loser_id','best_of','score','round','minutes')
stat.vars <- c('tourney_id','winner_id','loser_id','round','w_svpt','w_SvGms','w_1stWon','w_2ndWon','w_bpSaved','w_bpFaced','l_svpt','l_SvGms','l_1stWon','l_2ndWon','l_bpSaved','l_bpFaced')
tourney.vars <- c('tourney_id','tourney_name','tourney_date','surface','draw_size')
seed.vars <- c('tourney_id','winner_id','loser_id','winner_seed','loser_seed','round')
player.vars <- c('winner_id','winner_name','loser_id','loser_name')

# matches
match.data <- Data[!(round %in% c("BR","ER")),.SD,.SDcols = match.vars][,match_id := seq_len(.N)] ; setkey(match.data,round)
sortorderdf <- data.table(round=c("RR","R128","R64","R32","R16","QF","SF","F"),order=1:8) ; setkey(sortorderdf,round)
match.data <- sortorderdf[match.data][order(tourney_id,order)] ; rm(sortorderdf)
match.data[,match_num := seq_len(.N), by = .(tourney_id)]
setkey(match.data,match_id)

# stats per player per match
stat.data <- Data[!(round %in% c("BR","ER")),.SD,.SDcols = stat.vars][,match_id := seq_len(.N)]
setkey(stat.data,match_id)
stat.data[match.data, on = 'match_id', match_num := i.match_num]
stat.data <- rbind(
  stat.data[,.(match_id,match_num,tourney_id,round,winner_id,loser_id,player_id=winner_id,svpts=w_svpt,svgms=w_SvGms,svpts_won=w_1stWon+w_2ndWon,bp_faced=w_bpFaced,bp_saved=w_bpSaved)],
  stat.data[,.(match_id,match_num,tourney_id,round,winner_id,loser_id,player_id=loser_id,svpts=l_svpt,svgms=l_SvGms,svpts_won=l_1stWon+l_2ndWon,bp_faced=l_bpFaced,bp_saved=l_bpSaved)]
)[,svgms_won := svgms - (bp_faced - bp_saved)]

# tournaments
tourney.data <- Data[,.SD,.SDcols = tourney.vars]
tourney.data <- tourney.data[!duplicated(tourney.data),]

# prepare seedings data
seedings.data <- Data[,.SD,.SDcols = seed.vars]
seedings.data2 <- rbind(
  seedings.data[!is.na(winner_seed),.(tourney_id,winner_id,winner_seed)][,.(tourney_id, player_id = winner_id, seed = winner_seed)],
  seedings.data[!is.na(loser_seed),.(tourney_id,loser_id,loser_seed)][,.(tourney_id, player_id = loser_id, seed = loser_seed)]
)
seedings.data2 <- seedings.data2[!duplicated(seedings.data2),]
seedings.data <- seedings.data2[order(tourney_id,seed)]
rm(seedings.data2,files,match.vars,stat.vars,seed.vars,tourney.vars)

# prepare player data
player.data <- Data[,.SD,.SDcols = player.vars]
player.data2 <- rbind(
  player.data[,.(winner_id,winner_name)][,.(player_id = winner_id, player_name = winner_name)],
  player.data[,.(loser_id,loser_name)][,.(player_id = loser_id, player_name = loser_name)])[order(player_id)] %>% distinct()
player.data <- player.data2
rm(player.data2,player.vars)

setwd("/Users/williammunn/Documents/Github/tennis/functions")
print('All data loaded')

