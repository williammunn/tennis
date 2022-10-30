rm(list=ls())
library(dplyr);library(lubridate);library(data.table)
setwd("/Users/williammunn/Documents/Github/tennis/functions")
source("elo.R")
source("load_data.R")
source("functions.R")

simulate_tournament <- function(
  tournament_date,
  player_list
) {
  # ensure there are no duplicates on the players list
  if(sum(duplicated(player_list))>0) {
    stop("Remove duplicates on your player_list")
  }
  # look up elo ratings from elo_history dataset
  players  <- as.character(snapshot(elo_history,tournament_date)$player_id)
  # ensure all players on player_list exist in the snapshot
  players_in_player_list <- players[players %in% player_list]
  if(length(player_list) != length(players_in_player_list)) {
    stop("Not all players in your list have Elo ratings")
  }
  # get elos in same order as players appear in player_list
  elos <- as.numeric(snapshot(elo_history,tournament_date)$elo)[players %in% player_list]
  players <- players_in_player_list
  # determine number of rounds
  # if there are too few players in the list (i.e. not a power of 2)
  # then the function will fill in the blanks with byes
  rounds <- log(length(players))/log(2)
  if (rounds%%1 != 0) {
    rounds <- ceiling(rounds)
  }
  # insert byes randomly into players and elos vector
  # dataset for results
  results_df <- data.frame(
    player1 <- as.character(2^rounds),
    player2 <- as.character(2^rounds),
    round <- as.character(2^rounds),
    elo1 <- as.integer(2^rounds),
    elo2 <- as.integer(2^rounds),
    winner <- as.character(2^rounds)
  )
  setDT(results_df)
  # generate sequences to keep track of who should play who
  for (x in 1:rounds) {
    assign(paste0("id_round",x),rep(1:((2^rounds)/(2^x)),each=(2^x)))
  }
  # need temp vectors for players and elos that can reduce in size
  players_tmp <- players
  elos_tmp <- elos
  # play matches for each round
  for (round in 1:rounds) {
    # make life easier
    id_round <- paste0("id_round",round)
    id_round_val <- get(id_round)
    # player id's
    ids1 <- players_tmp[!duplicated(id_round_val)]
    ids2 <- players_tmp[duplicated(id_round_val)]
    # elos
    elos1<- elos_tmp[!duplicated(id_round_val)]
    elos2 <- elos_tmp[duplicated(id_round_val)]
    # play each other
    outcomes <- mapply(elo_outcome,elos1,elos2)
    winners <- character(length(outcomes))
    for(i in 1:length(outcomes)) {
      if(outcomes[i] > runif(1)) {
        winners[i] <- ids1[i]
      } else {
        winners[i] <- ids2[i]
      }
    }
    # update rows in the results_df data frame
    for (i in 1:length(outcomes)) {
      results_df$player1[i] <- ids1[i]
      results_df$player2[i] <- ids2[i]
      results_df$round[i] <- round
      results_df$elo1[i] <- elos1[i]
      results_df$elo2[i] <- elos2[i]
      results_df$winner[i] <- winners[i]
    }
    
    # players_tmp and elos_tmp get reduced to reflect only winners
    elos_tmp <- elos_tmp[players_tmp %in% winners]
    players_tmp <- players_tmp[players_tmp %in% winners]
    # if not the final round
    # we need to remove the loser entries
    # from the next id_round vector
    if(round != rounds) {
      tmp <- get(paste0("id_round",round+1))
      tmp <- tmp[players %in% winners]
      # replace id_round(X+1) with tmp
      assign(paste0("id_round",round+1),tmp)
    }
    
  }
  
  return(reuslts_df)
}

# get some players
x <- snapshot(elo_history,"2019-06-01")
y <- as.character(x$player_id)
z <- sample(y,size=32,replace=F)

# function
simulate_tournament("2019-06-01",z)



rounds <- 5
s <- 2
assign(paste0("id_round",s),rep(1:((2^rounds)/(2^s)),each=(2^s)))
id_round2
tmp <- c(1,NA,1,NA,NA,NA,2,2,NA,3,3,NA,4,4,NA,NA,NA,NA,5,5,6,NA,NA,6,7,NA,NA,7,8,NA,8,NA)
duplicated(rm.na(id_round1))


x1 <- 5
x2 <- "x1"
get(x2)
