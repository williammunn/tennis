rm(list=ls())
library(ggplot2)
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
