rm(list=ls())
library(dplyr)
library(lubridate)
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
  elo_p1 <- round(arg.prevelo.p1 + k_p1*(s_p1 - e_p1),0)
  elo_p2 <- round(arg.prevelo.p2 + k_p2*(s_p2 - e_p2),0)
  # return new elo points of player 1 and 2 respectively
  return(list(elo_p1,elo_p2))
}

# subset of data for matches played in 2019
test.data <- match.data %>% filter(year(tourney_date) == 2019)
# how big must the array be to hold all players and matches ?
dim1 <- length(unique(c(test.data[,'winner_id'],test.data[,'loser_id'])))
dim2 <- data.frame(id = c(test.data[,'winner_id'],test.data[,'loser_id']))%>%group_by(id)%>%summarise(matches=n())%>%select(matches)%>%max()+1

# names of array elements
dimnames1 <- as.character(sort(unique(c(test.data[,'winner_id'],test.data[,'loser_id']))))

# create the elo history array
array.elo <- array(numeric(),c(dim1,dim2+1))
dimnames(array.elo)[[1]] <- dimnames1
rm(dim1,dim2)

# initialise everyone's elo as 1000
array.elo[,1] <- 1500

# want to get a column on the data indicating number of matches played by each player
test.data2 <- rbind(
  test.data[,c('tourney_date','tourney_id','match_num','winner_id')] %>% rename(id = winner_id),
  test.data[,c('tourney_date','tourney_id','match_num','loser_id')] %>% rename(id = loser_id)
) %>% arrange(id,tourney_date,tourney_id,match_num) %>% group_by(id) %>% mutate(player_matches = row_number()-1)

# join back to get the number of matches played
test.data3 <- left_join(x=test.data,y=(test.data2 %>% rename(winner_id = id)),by.x=c("winner_id","tourney_id","match_num")) %>% rename(winner_prev_matches = player_matches)
test.data4 <- left_join(x=test.data3,y=(test.data2 %>% rename(loser_id = id)),by.x=c("loser_id","tourney_id","match_num")) %>% rename(loser_prev_matches = player_matches)
test.data <- test.data4 %>% arrange(tourney_date,match_num)
rm(test.data3,test.data4)

# data is now ready to be looped through
for (i in 1:nrow(test.data)) {
  elo.points <- elo.calculate.points(
    arg.p1 = test.data$winner_id[i],
    arg.p2 = test.data$loser_id[i],
    arg.winner = test.data$winner_id[i],
    arg.p1.matches = test.data$winner_prev_matches[i],
    arg.p2.matches = test.data$loser_prev_matches[i],
    arg.prevelo.p1 = array.elo[test.data$winner_id[1],test.data$winner_prev_matches+1][[1]],
    arg.prevelo.p2 = array.elo[test.data$loser_id[1],test.data$loser_prev_matches+1][[1]]
  )
  winner.elo.points <- elo.points[[1]]
  loser.elo.points <- elo.points[[2]]
  # update array
  array.elo[test.data$winner_id[i],test.data$winner_prev_matches[i]+2] <- winner.elo.points
  array.elo[test.data$loser_id[i],test.data$loser_prev_matches[i]+2] <- loser.elo.points
}

# array can now be converted into elo rankings
# we can ignore the NA entries as they don't correspond to a match played

# pre-allocate data frame having one row per non empty entry in array.elo
rows <- length(which(!is.na(array.elo)))
elo.results <- data.frame(
  player_id = character(rows),
  match_number = numeric(rows),
  elo_points = numeric(rows),
  stringsAsFactors=FALSE
)

k <- 1
for (i in 1:dim(array.elo)[1]) {
  for (j in 1:dim(array.elo)[2]) {
    if(!is.na(array.elo[i,j])) {
      elo.results[k,'player_id'] <- dimnames1[i]
      elo.results[k,'match_number'] <- j-1
      elo.results[k,'elo_points'] <- array.elo[i,j]
      k <- k + 1
    }
  }
}
rm(i,j,k)

# add in the match date (we're currently using tourney_date)
# take the last set of elo points for each tournament for each player

# add fields to the test.data2 dataset
test.data.join <- test.data2 %>% ungroup() %>% mutate(player_id = id, match_number = player_matches+1) %>% select(player_id,match_number,tourney_date)
elo.results2 <- inner_join(x=elo.results,y=test.data.join,by=c("player_id","match_number")) %>%
  group_by(player_id,tourney_date) %>%
  filter(row_number() == n()) %>% select(-match_number) %>%
  rename(date = tourney_date)
elo.results <- elo.results2
rm(elo.results2)


                 