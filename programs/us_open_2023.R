library(dplyr);library(lubridate);library(data.table);library(sqldf)
setwd("/Users/williammunn/Documents/Github/tennis/functions")

# load tennis data, remove what we don't need
source("load_data.R")
source("simulate_tournament.R")

# look up draw of 2023 US Open
players <- match_data[tourney_id == '2023-560' & round == 'R128',]
distinct_players <- rbind(
  players[,.(player_id = winner_id)],
  players[,.(player_id = loser_id)]
) %>% unique()
distinct_players <- distinct_players$player_id

# simulate winners
player_data[player_id == simulate_tournament("22aug2023",distinct_players,fill_elos = TRUE)[[1]],player_name]
