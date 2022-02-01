# calculate elo rankings
rm(list=ls())
require(dplyr)
setwd('/Users/vickimunn/Desktop/R Stuff/github/Data/')

# load match data
files <- list.files(pattern="atp_matches_[^_]*.csv")
#files <- 'atp_matches_2019.csv'
matches_raw <- do.call("rbind", lapply(files, function(x) read.csv(x, sep=",",stringsAsFactors = FALSE)))

# subset and clean
matches <- matches_raw[c("winner_name","loser_name","tourney_date")]
matches$tourney_date <- as.Date(as.character(matches$tourney_date),format='%Y%m%d', origin = "1900/01/01")
matches <- matches %>% arrange(tourney_date)
matches <- matches %>% mutate(order = row_number())

# a function that computes the elo points added/subtracted from the winner/loser
elo.calculate <- function(arg.p1,arg.p2,arg.winner,arg.match.date,arg.elo.data,arg.match.data) {
  # calculate the pre-match win probabilities of each player on the day of the match
  elo_p1 <- arg.elo.data$elo[arg.elo.data$name == arg.p1 & arg.elo.data$to_date == as.Date('9999-12-31')]
  elo_p2 <- arg.elo.data$elo[arg.elo.data$name == arg.p2 & arg.elo.data$to_date == as.Date('9999-12-31')]
  e_p1 <- 1/(1 + 10^((elo_p2 - elo_p1)/400))
  e_p2 <- 1/(1 + 10^((elo_p1 - elo_p2)/400))
  # number of matches played before this one by winner and loser
  p1_matches <- sum(arg.match.data$p1 == arg.p1 & arg.match.data$tourney_date < arg.match.date)
  p2_matches <- sum(arg.match.data$p2 == arg.p2 & arg.match.data$tourney_date < arg.match.date)
  # create the k factor for the winner and loser (depends on prior number of matches)
  k_p1 <- 250/((p1_matches + 5)^0.4)
  k_p2 <- 250/((p2_matches + 5)^0.4)
  # actual outcome for winner and loser
  if(arg.winner == arg.p1) {
    s_p1 <- 1
    s_p2 <- 0
  } else {
    s_p1 <- 0
    s_p2 <- 1
  }
  # update elo for players
  elo_p1 <- elo_p1 + k_p1*(s_p1 - e_p1)
  elo_p2 <- elo_p2 + k_p2*(s_p2 - e_p2)
  # return results
  return(list(elo_p1,elo_p2))
}

# a function that will take in match data and return the corresponding elo history
elo.history <- function(arg.matches,arg.initial.elo) {
  # pre-create an elo history dataset, because we know elo changes at each match played
  p1_matches <- unique(arg.matches[c('p1','tourney_date')]) %>% rename(name = p1)
  p2_matches <- unique(arg.matches[c('p2','tourney_date')]) %>% rename(name = p2)
  player_matches <- rbind(p1_matches,p2_matches) %>% distinct(name,tourney_date) %>% arrange(name,tourney_date)
  player_matches$to_date <- sapply(1:nrow(player_matches),function(x) 
    ifelse(player_matches$name[x] == player_matches$name[x+1],
           player_matches$tourney_date[x+1]-1,
           player_matches$to_date[x] <- as.Date('9999-12-31')
   )
  )
  player_matches <- player_matches %>% rename(from_date = tourney_date)
  return(player_matches)
}

testdata <- matches[1:100,] %>% rename(p1 = winner_name,p2 = loser_name)

output <- elo.history(arg.matches = testdata,arg.initial.elo = NULL) %>% arrange(name,from_date)

