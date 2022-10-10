rm(list=ls())
library(lubridate,dplyr,data.table)
setwd("/Users/williammunn/Documents/Github/tennis/functions")

# load tennis data, remove what we don't need
source("load_data.R")
rm(list=setdiff(ls(),'match.data','player.data'))
lapply(list(Data,match.data,player.data),setDT)
match.data <- match.data[, .(tourney_id,tourney_date,match_num,winner_id,loser_id)]

# subset of data for matches played in 2019
elo.input.data <- match.data[year(tourney_date) %in% c(2010:2019)]

# a function that computes the elo points added/subtracted from the winner/loser following one match
elo.calculate.points <- function(arg.p1,
                                 arg.p2,
                                 arg.winner,
                                 arg.p1.matches,
                                 arg.p2.matches,
                                 arg.prevelo.p1,
                                 arg.prevelo.p2
                                 ) {
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
  elo_p1 <- round(arg.prevelo.p1 + k_p1*(s_p1 - e_p1),0)
  elo_p2 <- round(arg.prevelo.p2 + k_p2*(s_p2 - e_p2),0)
  # return new elo points of player 1 and 2 respectively
  return(list(elo_p1,elo_p2))
}

# sort data by tourney_date and match_num
temp <- elo.input.data[order(tourney_date,tourney_id,match_num)]#[c(1:10),]

# vectors for players and their current Elo ratings
players <- unique(c(elo.input.data[['winner_id']],elo.input.data[['loser_id']]))
elo <- rep(1500,length=length(players))
matches <- rep(0,length(players))

# go through match-by-match, updating Elos for each player
output <- apply(
  temp,
  MARGIN = 1,
  function(x) {
    # determine indices for winner and loser
    winner <- which(players==x[4])
    loser <- which(players==x[5])
    # calculate winner and loser Elo points
    points <- elo.calculate.points(
      arg.p1 = x[4],
      arg.p2 = x[5],
      arg.winner = x[4],
      arg.p1.matches = matches[winner],
      arg.p2.matches = matches[loser],
      arg.prevelo.p1 = elo[winner],
      arg.prevelo.p2 = elo[loser]
    )
    # update matches count
    matches[winner] <<- matches[winner] + 1
    matches[loser] <<- matches[loser] + 1
    # update Elos
    elo[winner] <<- points[[1]]
    elo[loser] <<- points[[2]]
    # output
    return(points)
  }
)

# extract values and put these into the data
temp2 <- temp[,`:=`(
  winner_elo = unlist(output)[c(TRUE,FALSE)],
  loser_elo = unlist(output)[c(FALSE,TRUE)]
)][order(tourney_date,tourney_id,match_num)]

# sadly we do not know the date of each actual match
# this means that the best we can do is do store the
# Elo ratings for each player for the final match of
# each tournament he/she played in
# so we will take this match now

# first we need to convert the data to one row per player/match
temp3 <- rbind(
  copy(temp2)[,.(tourney_id,tourney_date,match_num,player_id=winner_id,elo=winner_elo)],
  copy(temp2)[,.(tourney_id,tourney_date,match_num,player_id=loser_id,elo=loser_elo)]
)[order(player_id,tourney_date,match_num)]

# take last match they played for each tourney_date
final_match <- temp3[,.SD[.N],by=.(player_id,tourney_date)]

