rm(list=ls())
cat("\014")

source('/Users/williammunn/Documents/Github/tennis/functions/simulate_match.R')

# play a point
play.point(
  arg.point.server = 'P1',
  arg.point.serve.pct = 0.8
)

# play a match
match <- play.match(
  arg.best.of = 3,
  arg.match.first.server = 'P1',
  arg.match.p1.serve.pct = 0.6,
  arg.match.p2.serve.pct = 0.8,
  arg.match.inherit = FALSE,
  arg.match.inherited.score = NULL
)

match[[2]]

# play many points
games <- sapply(
  1:100,
  function(x) {
    temp <- play.game(
      arg.game.server = "P1",
      arg.game.serve.pct = 0.5,
      arg.game.inherit = FALSE,
      arg.game.inherited.score = NULL
    )
    return(temp[[1]])
  }
)

# who played the most matches in 2019?
data <- match.data[year(tourney_date) == 2019,]
id <- rbind(
  data[,.(id = winner_id)],
  data[,.(id = loser_id)]
) 
id %>% setDT
most_matches <- id[,.N,by=.(id)][order(-N)][1:20][,.(player_id = id,N)]
most_matches[player.data, on = 'player_id', name := i.player_name]

# tiebreak


