rm(list=ls())
library(lubridate,dplyr,data.table)
setwd("/Users/vickimunn/Desktop/R stuff/github")

# load tennis data, remove what we don't need
source("load_data.R")
lapply(list(match.data,player.data,seedings.data,tourney.data),setDT)

# 2019 Australian Open has tourney_id = 2019-580
matches <- match.data[tourney_id == "2019-580" & round == "R128"]
players <- unique(c(matches[,winner_id],matches[,loser_id]))

# matrix to store elo ratings (one row per player)
matrix.elo <- matrix(data = 1500, nrow = length(players), ncol = 1, dimnames = list(players,"elo"))

# winner of a match
match.winner <- function(arg.p1elo,arg.p2elo) {
  if(runif(1) < (1/(1 + 10^((arg.p2elo - arg.p1elo)/400)))) {
    return(arg.p1)
  } else {
    return(arg.p2)
  }
}

# play the tournament
play.tournament <- function(
  arg.playerlist, # a vector with a list of player id's arranged like the draw of the first round
  arg.elo # a data table having a player_id and an elo column
  ) {
  # create matrix objects to store results
  matrix.results <- matrix(data = 0, nrow = 128, ncol = 7, dimnames = list(players, c("R128","R64","R32","R16","QF","SF","F")))
  matrix.matchid <- matrix.results
  for (round in 1:ncol(matrix.matchid)) {
    matrix.matchid[,round] <- sort(rep(c(1:(nrow(matrix.matchid)/2^round)),2^round))
  }
  # main logic
  for (round in c(1:ncol(matrix.matchid))) {
    for (matchnum in c(1:max(matrix.matchid[,round]))) {
      if (round == 1) {
        # get players for each match number
        p1 <- players[matrix.matchid[, round] == matchnum][1]
        p2 <- players[matrix.matchid[, round] == matchnum][2]
      } else {
        # get players for each match number by searching for winners from last round
        p1 <- players[(matrix.matchid[, round] == matchnum) & (matrix.results[, round-1] == 1)][1]
        p2 <- players[(matrix.matchid[, round] == matchnum) & (matrix.results[, round-1] == 1)][2]
      }
      # play match using elo win probabilities
      winner <- match.winner(arg.elo[player_id==p1,elo], arg.elo[player_id==p2,elo])
      loser <- ifelse(winner == p1, p2, p1)
      # record win in results matrix
      matrix.results[winner, round] <- 1
      matrix.results[loser, round] <- -1
    }
  }
  return(list(winner,matrix.results))
}

winner <- play.tournament(players,matrix.elo)[[1]]
