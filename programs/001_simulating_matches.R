rm(list=ls())
library(ggplot2)
library(data.table)
setwd('/Users/vickimunn/Desktop/R Stuff/github/functions/');source("simulate_match.R")

# probability of winning a game given different % service points won
ntimes <- 1000
winners <- sapply(1:ntimes,function(x) {play.game("P1",0.8)[[1]]})
sum(winners=="P1")/length(winners)

# record how the result changes with respect to number of simulatons
ntimes <- 2000
winners <- sapply(1:ntimes,function(x) {play.game("P1",0.6)[[1]]})
graphdata <- data.table(n = c(1:ntimes),winner = winners)[,pct:=cumsum(winner=="P1")/.I]
p <- ggplot(data, aes(x=n, y=pct)) + geom_line(colour = "cyan 3", size = 0.8)
p

# relationship between points won and games won
ntimes <- 500
srvpct <- rep(seq(0,1,0.025),ntimes)
results <- data.table(srv_pct = srvpct, winner = sapply(srvpct, function(x) {play.game("P1",x)[[1]]}))
results <- results[,.(win_pct = sum(winner == "P1")/.N),by = srv_pct]
p <- ggplot(results, aes(x=srv_pct, y=win_pct)) + geom_line(colour = "cyan 3", size = 0.8)
p

rm(list=ls())
setwd('/Users/vickimunn/Desktop/R Stuff/github/functions/');source("load_data.R")

# data for actual matches
rm(player.data,seedings.data,tourney.data)
stats.data <- Data[year(tourney_date)>=2010,.(tourney_name,tourney_date,winner_name,loser_name,score,
                      w_svpt,w_SvGms,w_1stWon,w_2ndWon,w_bpFaced,w_bpSaved,
                      l_svpt,l_SvGms,l_1stWon,l_2ndWon,l_bpFaced,l_bpSaved)]
stats.data <- rbind(
  stats.data[,.(tourney_name,tourney_date,score,winner_name,player_name=winner_name,svpt=w_svpt,svgames=w_SvGms,fwon=w_1stWon,swon=w_2ndWon,bpsaved=w_bpSaved,bpfaced=w_bpFaced)],
  stats.data[,.(tourney_name,tourney_date,score,winner_name,player_name=loser_name,svpt=l_svpt,svgames=l_SvGms,fwon=l_1stWon,swon=l_2ndWon,bpsaved=l_bpSaved,bpfaced=l_bpFaced)]
)[,`:=`(
  svpt_won = fwon + swon,
  svgames_won = svgames - (bpfaced - bpsaved)
)]

# check derived svgames_won by examining the score
setwd('/Users/vickimunn/Desktop/R Stuff/github/functions/');source("score_string.R")

convert.score("7-6 4-2")

