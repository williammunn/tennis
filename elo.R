rm(list=ls())
setwd("/Users/vickimunn/Desktop/R stuff/github")

# load tennis data
source("load_data.R")

# a function that computes the elo points added/subtracted from the winner/loser following one match
elo.calculate.points <- function(arg.p1,arg.p2,arg.winner,arg.p1.matches,arg.p2.matches,arg.prevelo.p1,arg.prevelo.p2) {
  # calculate the pre-match win probabilities of each player on the day of the match
  e_p1 <- 1/(1 + 10^((arg.prevelo.p2 - arg.prevelo.p1)/400))
  e_p2 <- 1/(1 + 10^((arg.prevelo.p1 - arg.prevelo.p2)/400))
  # create the k factor for the winner and loser (depends on prior number of matches)
  k_p1 <- 250/((arg.p1.matches + 5)^0.4)
  k_p2 <- 250/((arg.p2.matches + 5)^0.4)
  # actual outcome for winner and loser
  if(arg.winner == arg.p1) {
    s_p1 <- 1
    s_p2 <- 0
  } else {
    s_p1 <- 0
    s_p2 <- 1
  }
  # update elo for players
  elo_p1 <- arg.prevelo.p1 + k_p1*(s_p1 - e_p1)
  elo_p2 <- arg.prevelo.p2 + k_p2*(s_p2 - e_p2)
  # return new elo points of player 1 and 2 respectively
  return(list(elo_p1,elo_p2))
}

