
library(dplyr);library(lubridate);library(data.table);library(sqldf)
setwd("/Users/williammunn/Documents/Github/tennis/functions")

# load tennis data, remove what we don't need
source("load_data.R")
lapply(list(match_data,player_data),setDT)
match_data <- match_data[, .(tourney_id,tourney_date,match_num,winner_id,loser_id)]

# subset of data for matches played in 2019
elo_input_data <- match_data[year(tourney_date) %in% c(2000:2022)]

# a function that computes the elo points added/subtracted from the winner/loser following one match
elo_calculate_points <- function(arg_winner_matches,
                                 arg_loser_matches,
                                 arg_winner_prevelo,
                                 arg_loser_prevelo
                                 ) {
  # calculate the pre-match win probabilities of each player on the day of the match
  e_winner <- 1/(1 + 10^((arg_loser_prevelo - arg_winner_prevelo)/400))
  e_loser <- 1/(1 + 10^((arg_winner_prevelo - arg_loser_prevelo)/400))
  # create the k factor for the winner and loser (depends on prior number of matches)
  k_winner <- 250/((arg_winner_matches + 5)^0.4)
  k_loser <- 250/((arg_loser_matches + 5)^0.4)
  # update elo for players
  elo_winner <- round(arg_winner_prevelo + k_winner*(1 - e_winner),0)
  elo_loser <- round(arg_loser_prevelo + k_loser*(0 - e_loser),0)
  # return new elo points of player 1 and 2 respectively
  return(list(elo_winner,elo_loser))
}

# sort data by tourney_date and match_num
temp <- elo_input_data[order(tourney_date,tourney_id,match_num)]

# vectors for players and their current Elo ratings
players <- unique(c(elo_input_data[['winner_id']],elo_input_data[['loser_id']]))
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
    points <- elo_calculate_points(
      arg_winner_matches = matches[winner],
      arg_loser_matches = matches[loser],
      arg_winner_prevelo = elo[winner],
      arg_loser_prevelo = elo[loser]
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
print("Elos calculated")


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
final_match <- temp3[,.SD[.N],by=.(player_id,tourney_date)][,match_num := NULL][order(player_id,-tourney_date)]
final_match[,tourney_date2 := as.Date(tourney_date,"%y-%m-%d")]

# from tourney_date to next row tourney_date - 1 day
# identify first and last records
elo_history <- final_match[, num_matches := .N, by = .(player_id)][
  , match_num := seq_len(.N), by = .(player_id)][
    , first := ifelse(match_num == 1, TRUE, FALSE)][
      , last := ifelse(match_num == num_matches, TRUE, FALSE)][
        , lag_tourney_date := lag(tourney_date)][
          , from_date := tourney_date][
            , to_date := as.Date(ifelse(first, ifelse(tourney_date < as.Date("2018-06-01"),tourney_date,as.Date("31dec9999","%d%b%Y")),lag_tourney_date-1),origin="1970-01-01")][
              order(player_id,from_date,to_date),.(player_id,from_date,to_date,elo)]

# remove working datasets
rm(elo_input_data,temp2,temp3,final_match,output,temp,elo,matches,players)
setwd("/Users/williammunn/Documents/Github/tennis/functions")
print("Done!")
