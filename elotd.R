rm(list=ls())
library(lubridate)
library(dplyr)
library(data.table)
setwd("/Users/vickimunn/Desktop/R stuff/github")

# load tennis data, remove what we don't need
source("load_data.R")
rm(Data,seedings.data,tourney.data,files,match.vars,player.vars,tourney.vars,seed.vars)

# convert match.data to a data.table
match.data <- setDT(match.data)

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
  elo_p1 <- round(arg.prevelo.p1 + k_p1*(s_p1 - e_p1),0)
  elo_p2 <- round(arg.prevelo.p2 + k_p2*(s_p2 - e_p2),0)
  # return new elo points of player 1 and 2 respectively
  return(list(elo_p1,elo_p2))
}

# subset of data for matches played in 2019
elo.input.data <- match.data[year(tourney_date)==2019]

# the number of rows in the matrix corresponds to the number of unique players
dim1 <- length(unique(c(elo.input.data[,winner_id],elo.input.data[,loser_id])))
dim2 <- max(rbind(elo.input.data[,.(id = winner_id)],elo.input.data[,.(id = loser_id)])[,.(count=.N),by=.(id)][,count])+1

# create the elo history array
array.elo <- array(numeric(),c(dim1,dim2))
dimnames(array.elo)[[1]] <- unique(c(elo.input.data[,winner_id],elo.input.data[,loser_id]))

rm(dim1,dim2)

# initialise everyone's elo as 1500
array.elo[,1] <- 1500

# convert match data into one row per player/match
per.player <- rbind(elo.input.data[,.(tourney_date,tourney_id,match_num,id=winner_id)],
                    elo.input.data[,.(tourney_date,tourney_id,match_num,id=loser_id)])[order(id,tourney_date,match_num)][,player_matches := seq_len(.N)-1, by=.(id)]

# get column having prevous_match counts onto the main table
setkey(elo.input.data,tourney_id,tourney_date,match_num,winner_id)
setkey(per.player,tourney_id,tourney_date,match_num,id)
elo.input2 <- elo.input.data[per.player, winner_prev_matches := i.player_matches]
setkey(elo.input2,tourney_id,tourney_date,match_num,loser_id)
elo.input3 <- elo.input2[per.player, loser_prev_matches := i.player_matches]

# clean up
elo.input.data <- elo.input3[order(tourney_date,match_num)][,.(tourney_id,tourney_date,match_num,winner_id,loser_id,winner_prev_matches,loser_prev_matches)]
rm(elo.input2,elo.input3)

# data is now ready to be looped through
for (i in 1:nrow(elo.input.data)) {
  elo.points <- elo.calculate.points(
    arg.p1 = elo.input.data$winner_id[i],
    arg.p2 = elo.input.data$loser_id[i],
    arg.winner = elo.input.data$winner_id[i],
    arg.p1.matches = elo.input.data$winner_prev_matches[i],
    arg.p2.matches = elo.input.data$loser_prev_matches[i],
    arg.prevelo.p1 = array.elo[elo.input.data$winner_id[i],elo.input.data$winner_prev_matches[i]+1][[1]],
    arg.prevelo.p2 = array.elo[elo.input.data$loser_id[i],elo.input.data$loser_prev_matches[i]+1][[1]]
  )
  winner.elo.points <- elo.points[[1]]
  loser.elo.points <- elo.points[[2]]
  # update array
  array.elo[elo.input.data$winner_id[i],elo.input.data$winner_prev_matches[i]+2] <- winner.elo.points
  array.elo[elo.input.data$loser_id[i],elo.input.data$loser_prev_matches[i]+2] <- loser.elo.points
}

# check for highest value
max(array.elo[which(!is.na(array.elo))])

# array can now be converted into elo rankings
# we can ignore the NA entries as they don't correspond to a match played

# pre-allocate data frame having one row per non empty entry in array.elo
rows <- length(which(!is.na(array.elo)))
elo.results <- data.table(
  player_id = character(rows),
  match_number = numeric(rows),
  elo_points = numeric(rows)
)

k <- 1
for (i in 1:dim(array.elo)[1]) {
  for (j in 1:dim(array.elo)[2]) {
    if(!is.na(array.elo[i,j])) {
      elo.results[k,'player_id'] <- rownames(array.elo)[i]
      elo.results[k,'match_number'] <- j-1
      elo.results[k,'elo_points'] <- array.elo[i,j]
      k <- k + 1
    }
  }
}
rm(i,j,k)


# add in the match date (we're currently using tourney_date)
# take the last set of elo points for each tournament for each player
                 