rm(list=ls())
library(lubridate,dplyr,data.table)
setwd("/Users/vickimunn/Desktop/R stuff/github")

# load tennis data, remove what we don't need
source("load_data.R")
rm(list=setdiff(ls(),c('Data','match.data')))
lapply(list(Data,match.data),setDT)
match.data <- match.data[, .(tourney_id,tourney_date,match_num,winner_id,loser_id)]

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
rownames <- unique(c(elo.input.data[,winner_id],elo.input.data[,loser_id]))%>%sort()
numcols <- rbind(elo.input.data[,.(id = winner_id)],elo.input.data[,.(id = loser_id)])[,.(count=.N),by=.(id)][,count]%>%max()+1L

# create the elo history matrix
matrix.elo <- matrix(data = NA, nrow = length(rownames), ncol = numcols, dimnames = list(rownames,NULL))
rm(numcols,rownames)

# initialise everyone's elo as 1500
matrix.elo[,1] <- 1500

# data is now ready to be looped through

for (i in 1:nrow(elo.input.data)) {
  # get values
  p1 <- elo.input.data$winner_id[i]
  p2 <- elo.input.data$loser_id[i]
  winner <- elo.input.data$winner_id[i]
  p1.matches <- sum(!is.na(matrix.elo[p1,]))-1
  p2.matches <- sum(!is.na(matrix.elo[p2,]))-1
  p1.previous.elo <- matrix.elo[p1,p1.matches+1][[1]]
  p2.previous.elo <- matrix.elo[p2,p2.matches+1][[1]]
  # run elo function to get updated elos for p2 and p2
  elo.points <- elo.calculate.points(
    arg.p1 = p1,
    arg.p2 = p2,
    arg.winner = winner,
    arg.p1.matches = p1.matches,
    arg.p2.matches = p2.matches,
    arg.prevelo.p1 = p1.previous.elo,
    arg.prevelo.p2 = p2.previous.elo
  )
  winner.elo.points <- elo.points[[1]]
  loser.elo.points <- elo.points[[2]]
  # update array
  matrix.elo[p1,p1.matches+2] <- winner.elo.points
  matrix.elo[p2,p2.matches+2] <- loser.elo.points
}
rm(i,p1,p2,p1.matches,p2.matches,p1.previous.elo,p2.previous.elo,winner,winner.elo.points,loser.elo.points,elo.points)

# append all non-missing values from the matrix into a vector
elos <- na.omit(as.vector(t(matrix.elo[,2:ncol(matrix.elo)])))

# convert the matches data into one row per player so it is compatible with the elo output
results1 <- rbind(
  elo.input.data[,.(tourney_id,tourney_date,match_num,player_id=winner_id)],
  elo.input.data[,.(tourney_id,tourney_date,match_num,player_id=loser_id)]
)[order(player_id,tourney_date,match_num)][,elo := elos]
# take last match per tourney
results2 <- results1[,.SD[.N],by=.(player_id,tourney_date)]
# convert to longitudinal data
