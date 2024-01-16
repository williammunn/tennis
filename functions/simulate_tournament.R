rm(list=ls())
library(dplyr);library(lubridate);library(data.table);library(sqldf)
setwd("/Users/williammunn/Documents/Github/tennis/functions")
source("functions.R")
source("elo.R")


simulate_tournament <- function(
  tournament_date,
  player_list,
  fill_elos = FALSE
) {
  # ensure there are no duplicates on the players list
  if(sum(duplicated(player_list))>0) {
    stop("Remove duplicates on your player_list")
  }
  # look up elo ratings from elo_history dataset
  snapshot_data <- snapshot(elo_history,tournament_date)[,.(player_id,elo)]
  players  <- snapshot_data$player_id
  # ensure all players on player_list exist in the snapshot
  players_in_player_list <- players[players %in% player_list]
  if(length(player_list) != length(players_in_player_list)) {
    if (!fill_elos) {
      stop("Not all players in your list have Elo ratings")
    } else {
      # we need to insert Elos for the players who are missing
      players_without_elos <- player_list[!(player_list %in% players)]
      snapshot_data <- rbind(
        snapshot_data,
        data.table(
          data.frame(
            player_id = players_without_elos,
            elo = rep(1500,length(players_without_elos))
          )
        )
      )
      # re generate the players object, as it now needs to be larger
      players  <- snapshot_data$player_id
    }
  }
  # get elos in same order as players appear in player_list
  elos <- snapshot_data$elo[players %in% player_list]
  # get players in the same order
  players <- snapshot_data$player_id[players %in% player_list]
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
    match_num = numeric(length=(2^rounds)-1),
    player1 = character(length=(2^rounds)-1),
    player2 = character(length=(2^rounds)-1),
    round = numeric(length=(2^rounds)-1),
    elo1 = numeric(length=(2^rounds)-1),
    elo2 = numeric(length=(2^rounds)-1),
    winner = character(length=(2^rounds)-1)
  )
  # generate sequences to keep track of who should play who
  for (x in 1:rounds) {
    assign(paste0("id_round",x),rep(1:((2^rounds)/(2^x)),each=(2^x)))
  }
  # need temp vectors for players and elos that can reduce in size
  players_tmp <- players
  elos_tmp <- elos
  matches_played <- 0
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
      results_df$match_num[matches_played+i] <- matches_played+i
      results_df$player1[matches_played+i] <- ids1[i]
      results_df$player2[matches_played+i] <- ids2[i]
      results_df$round[matches_played+i] <- round
      results_df$elo1[matches_played+i] <- elos1[i]
      results_df$elo2[matches_played+i] <- elos2[i]
      results_df$winner[matches_played+i] <- winners[i]
    }
    # how many matches have now been player in total?
    matches_played <- matches_played + length(outcomes)
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
  # output
  return(list(winners,results_df))
}
