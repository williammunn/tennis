rm(list=ls())
library(ggplot2)
library(data.table)
library(scales)

setwd('/Users/williammunn/Documents/Github/tennis/functions/')
source("load_data.R")

# data for actual matches, convert to player-centric
rm(player.data,seedings.data,tourney.data,dir)
stats <- Data[,.(tourney_name,tourney_date,winner_name,loser_name,score,w_svpt,w_1stWon,w_2ndWon,l_svpt,l_1stWon,l_2ndWon)
              ][,`:=`(
                  w_sv = (w_1stWon + w_2ndWon)/w_svpt,
                  l_sv = (l_1stWon + l_2ndWon)/l_svpt)
                ][,`:=`(
                    w_rt = 1 - l_sv,
                    l_rt = 1 - w_sv)
                  ][,`:=`(
                    w_diff = w_sv - w_rt,
                    l_diff = l_sv - l_rt)
                    ][,.(tourney_name,tourney_date,winner_name,loser_name,score,w_sv,w_rt,w_diff,l_sv,l_rt,l_diff)]

# remove matches that ended in retirement or were walkovers, and matches without stats
stats <- stats[!(substr(score,nchar(score)-2,nchar(score))  %in% c('RET','W/O')),]
stats <- na.omit(stats)

# look at the extremes at both ends
bad_serving <- stats[year(tourney_date)>=2010,][order(w_diff)]
good_serving <- stats[year(tourney_date)>=2010,][order(-l_diff)]

# round percentages
bad_serving[,c("w_sv","w_rt","w_diff","l_sv","l_rt","l_diff")] <- lapply(bad_serving[,c("w_sv","w_rt","w_diff","l_sv","l_rt","l_diff")], function(x) percent(x,accuracy=2))
good_serving[,c("w_sv","w_rt","w_diff","l_sv","l_rt","l_diff")] <- lapply(good_serving[,c("w_sv","w_rt","w_diff","l_sv","l_rt","l_diff")], function(x) percent(x,accuracy=2))

# tour average
stats[year(tourney_date)>=2010,.(mean_sv = mean(w_sv),mean_rt = mean(w_rt))]

rm(list=ls())
source('/Users/williammunn/Documents/Github/tennis/functions/simulate_match.R')

# show that play.point is eventually right
outcomes <- sapply(1:1000, function(x) play.point("P1",0.8))
win_percent <- sum(outcomes == "P1")/length(outcomes)
percent(win_percent)

# probability of winning a game given different % service points won
#ntimes <- 1000
#winners <- sapply(1:ntimes,function(x) {play.game("P1",0.8)[[1]]})
#sum(winners=="P1")/length(winners)

# record how the result changes with respect to number of simulatons
#ntimes <- 2000
#winners <- sapply(1:ntimes,function(x) {play.game("P1",0.6)[[1]]})
#graphdata <- data.table(n = c(1:ntimes),winner = winners)[,pct:=cumsum(winner=="P1")/.I]
#p <- ggplot(data, aes(x=n, y=pct)) + geom_line(colour = "cyan 3", size = 0.8)
#p

# relationship between points won and games won
#ntimes <- 500
#srvpct <- rep(seq(0,1,0.025),ntimes)
#results <- data.table(srv_pct = srvpct, winner = sapply(srvpct, function(x) {play.game("P1",x)[[1]]}))
#results <- results[,.(win_pct = sum(winner == "P1")/.N),by = srv_pct]
#p <- ggplot(results, aes(x=srv_pct, y=win_pct)) + geom_line(colour = "cyan 3", size = 0.8)
#p

# relationship between points won and sets won
#simulations <- 100
#p1srange <- rep(seq(0.1,0.9,0.025),simulations)
#p2srange <- rep(seq(0.5,0.9,0.1),simulations)
#comb <- expand.grid(p1 = p1srange, p2 = p2srange)
#results <- apply(comb, 1, function(x) play.set("P1",x[1],x[2])[[1]])
#results2 <- data.table(p1 =comb$p1, p2=comb$p2, winner=results)[,.(win_pct = sum(winner=="P1")/.N),by=.(p1,p2)][order(p1,p2)]
#p <- ggplot(results2, aes(x=p1, y = win_pct, group = p2, color = p2)) + geom_line(size = 0.8) + theme(legend.position = "none")
#p
