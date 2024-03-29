require(data.table);require(lubridate);require(dplyr);require(readr)

# take data from 2000 > present
years <- c(2000:2023)
for (year in years) {
  temp_data <- read_csv(url(paste0("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_",year,".csv")))
  assign(paste0('atp_matches_',year),temp_data)
}
  
# load match data
files <- ls(envir = .GlobalEnv,pattern="atp_matches_[^_]*")
Data <- do.call("rbind", lapply(files, function(x) {as.data.table(get(x))}))
rm(list = files)

# clean data
Data[,tourney_date := as.Date(as.character(tourney_date) ,format='%Y%m%d', origin = "1900/01/01")]
Data[,`:=`(winner_id = as.character(winner_id),loser_id = as.character(loser_id))]

# dictate what variables get assigned to what data
match_vars <- c('tourney_id','tourney_date','winner_id','loser_id','winner_name','loser_name','best_of','score','round','minutes')
stat_vars <- c('tourney_id','winner_id','loser_id','round','w_svpt','w_SvGms','w_1stWon','w_2ndWon','w_bpSaved','w_bpFaced','l_svpt','l_SvGms','l_1stWon','l_2ndWon','l_bpSaved','l_bpFaced')
tourney_vars <- c('tourney_id','tourney_name','tourney_date','surface','draw_size')
player_vars <- c('winner_id','winner_name','loser_id','loser_name')

# matches
match_data <- Data[!(round %in% c("BR","ER")),.SD,.SDcols = match_vars][,match_id := seq_len(.N)] ; setkey(match_data,round)
sortorderdf <- data.table(round=c("RR","R128","R64","R32","R16","QF","SF","F"),order=1:8) ; setkey(sortorderdf,round)
match_data <- sortorderdf[match_data][order(tourney_id,order)] ; rm(sortorderdf)
match_data[,match_num := seq_len(.N), by = .(tourney_id)]
setkey(match_data,match_id)

# stats per player per match
stat_data <- Data[!(round %in% c("BR","ER")),.SD,.SDcols = stat_vars][,match_id := seq_len(.N)]
setkey(stat_data,match_id)
stat_data[match_data, on = 'match_id', match_num := i.match_num]
stat_data <- rbind(
  stat_data[,.(match_id,match_num,tourney_id,round,winner_id,loser_id,player_id=winner_id,svpts=w_svpt,svgms=w_SvGms,svpts_won=w_1stWon+w_2ndWon,bp_faced=w_bpFaced,bp_saved=w_bpSaved)],
  stat_data[,.(match_id,match_num,tourney_id,round,winner_id,loser_id,player_id=loser_id,svpts=l_svpt,svgms=l_SvGms,svpts_won=l_1stWon+l_2ndWon,bp_faced=l_bpFaced,bp_saved=l_bpSaved)]
)[,svgms_won := svgms - (bp_faced - bp_saved)]

# tournaments
tourney_data <- Data[,.SD,.SDcols = tourney_vars]
tourney_data <- tourney_data[!duplicated(tourney_data),]

# prepare player data
player_data <- Data[,.SD,.SDcols = player_vars]
player_data2 <- rbind(
  player_data[,.(winner_id,winner_name)][,.(player_id = winner_id, player_name = winner_name)],
  player_data[,.(loser_id,loser_name)][,.(player_id = loser_id, player_name = loser_name)])[order(player_id)] %>% distinct()
player_data <- player_data2

# remove what we no longer need
rm(player_data2,player_vars,Data,files,match_vars,stat_vars,tourney_vars)

setwd("/Users/williammunn/Documents/Github/tennis/functions")
print('All data loaded')

