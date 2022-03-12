rm(list=ls())
library(ggplot2)
library(data.table)
setwd('/Users/williammunn/Documents/Github/tennis/functions/');source("simulate_match.R")

# probability of winning a game given different % service points won
ntimes <- 1000
winners <- sapply(1:ntimes,function(x) {play.game("P1",0.8)[[1]]})
sum(winners=="P1")/length(winners)

# record how the result changes with respect to number of simulatons
ntimes <- 2000
winners <- sapply(1:ntimes,function(x) {play.game("P1",0.6)[[1]]})
graphdata <- data.table(n = c(1:ntimes),winner = winners)[,pct:=cumsum(winner=="P1")/.I]
p <- ggplot(data, aes(x=n, y=pct)) + geom_line(colour = "cyan 3", size = 0.8)
p

# relationship between points won and games won
ntimes <- 500
srvpct <- rep(seq(0,1,0.025),ntimes)
results <- data.table(srv_pct = srvpct, winner = sapply(srvpct, function(x) {play.game("P1",x)[[1]]}))
results <- results[,.(win_pct = sum(winner == "P1")/.N),by = srv_pct]
p <- ggplot(results, aes(x=srv_pct, y=win_pct)) + geom_line(colour = "cyan 3", size = 0.8)
p

## relationship between points won and sets won

# modify play.set to speed it up
play.set <- function(arg.set.first.server,arg.set.p1.serve.pct,arg.set.p2.serve.pct) {
  # initial values
  set.over <- FALSE
  p1.games <- 0
  p2.games <- 0
  tiebreak <- FALSE
  current.server <- arg.set.first.server
  set.most.recent.server <- current.server
  # play games until the set is over
  while(set.over == FALSE) {
    if (!tiebreak) {
      # play a game
      game.object <- play.game(arg.game.server = current.server, arg.game.serve.pct = ifelse(current.server == 'P1', arg.set.p1.serve.pct, arg.set.p2.serve.pct))
      # immediately switch off the inherited switch, so that any future games start at 0-0
      inherit.game <- FALSE
      # retrieve the winner of the game
      game.winner <- game.object
      # update the most recent server variable
      set.most.recent.server <- current.server
      # award winner a game
      if (game.winner == 'P1') {
        p1.games <- p1.games + 1
      } else {
        p2.games <- p2.games + 1
      }
      # check if set is over
      if((p1.games == 6 & p2.games <= 4) | (p2.games == 6 & p1.games <= 4) | p1.games == 7 | p2.games == 7) {
        set.over <- TRUE
      }
      # swap server
      current.server = ifelse(current.server == 'P1','P2','P1')
      # check if tiebreak needed
      if (p1.games == 6 & p2.games == 6) {
        tiebreak <- TRUE
      }
    }
    if(tiebreak) {
      game.object <- play.tiebreak(arg.tiebreak.first.server = current.server,arg.tiebreak.p1.serve.pct = arg.set.p1.serve.pct,arg.tiebreak.p2.serve.pct = arg.set.p2.serve.pct)
      game.winner <- game.object
      set.over <- TRUE
      # award winner of tiebreak a game
      if (game.winner == 'P1') {
        p1.games <- p1.games + 1
      } else {
        p2.games <- p2.games + 1
      }
    }
  }
  if (p1.games > p2.games) {
    set.winner <- 'P1'
  } else {
    set.winner <- 'P2'
  }
  return(set.winner)
}



# define play.game
play.game <- function(arg.game.server,arg.game.serve.pct) {
  # initial values
  p1.points <- 0
  p2.points <- 0
  current.server <- arg.game.server
  game.over <- FALSE
  # play points until the game is over
  while(game.over == FALSE) {
    # play a point
    point.winner <- play.point(arg.point.server = current.server, arg.point.serve.pct = arg.game.serve.pct)
    # increment points of the winner of the point just played
    if (point.winner == 'P1') {
      p1.points <- p1.points + 1
    } else {
      p2.points <- p2.points + 1
    }
    if (p1.points == 4 & p2.points == 4) {
      p1.points <- 3
      p2.points <- 3
    }
    # check if the game is over
    if ((p1.points >= 4 | p2.points >= 4) &
        (abs(p1.points - p2.points) >= 2)) {
      game.over <- TRUE
    }
  }
  if (p1.points > p2.points) {
    game.winner <- 'P1'
  } else {
    game.winner <- 'P2'
  }
  return(game.winner)
}


play.tiebreak <- function(arg.tiebreak.first.server,arg.tiebreak.p1.serve.pct,arg.tiebreak.p2.serve.pct) {
  # initial values
  tiebreak.over <- FALSE
  tiebreak.current.server <- arg.tiebreak.first.server
  p1.points <- 0
  p2.points <- 0
  tiebreak.serves.remaining <- 1 # first person to serve only serves once
  # play the remainder of the tiebreak
  while(tiebreak.over == FALSE) {
    # play the current server's 2 points
    while (tiebreak.serves.remaining > 0) {
      # play the first point on this player's server
      point.winner <- play.point(arg.point.server = tiebreak.current.server, arg.point.serve.pct = ifelse(tiebreak.current.server == 'P1',arg.tiebreak.p1.serve.pct,arg.tiebreak.p2.serve.pct))
      # award point to the winner
      if (point.winner == 'P1') {
        p1.points <- p1.points + 1
      } else {
        p2.points <- p2.points + 1
      }
      # increment the points served by this current server
      tiebreak.serves.remaining <- tiebreak.serves.remaining - 1
      # check if tiebreak should be ended
      if ((p1.points >= 7 | p2.points >= 7) & (abs(p1.points - p2.points) > 1)) {
        tiebreak.over <- TRUE
        break
      }
    }
    # after the first point players get 2 serves each
    tiebreak.serves.remaining <- 2
    # swap server
    tiebreak.current.server <- ifelse(tiebreak.current.server == "P1","P2","P1")
  }
  # determine who wins the tiebreak
  if (p1.points > p2.points) {
    tiebreak.winner <- 'P1'
  } else {
    tiebreak.winner <- 'P2'
  }
  # return the winner of the tiebreak
  return(tiebreak.winner)
}

# this takes a while to run
simulations <- 100
p1srange <- rep(seq(0.1,0.9,0.025),simulations)
p2srange <- rep(seq(0.5,0.9,0.1),simulations)
comb <- expand.grid(p1 = p1srange, p2 = p2srange)
results <- apply(comb, 1, function(x) play.set("P1",x[1],x[2])[[1]])
results2 <- data.table(p1 =comb$p1, p2=comb$p2, winner=results)[,.(win_pct = sum(winner=="P1")/.N),by=.(p1,p2)][order(p1,p2)]
p <- ggplot(results2, aes(x=p1, y = win_pct, group = p2, color = p2)) + geom_line(size = 0.8) + theme(legend.position = "none")
p


rm(list=ls())
setwd('/Users/williammunn/Documents/Github/tennis/functions/');source("load_data.R")

# data for actual matches
rm(player.data,seedings.data,tourney.data,dir)
stats.data <- Data[year(tourney_date)>=2010,.(tourney_name,tourney_date,winner_name,loser_name,score,
                      w_svpt,w_SvGms,w_1stWon,w_2ndWon,w_bpFaced,w_bpSaved,
                      l_svpt,l_SvGms,l_1stWon,l_2ndWon,l_bpFaced,l_bpSaved)]
stats.data <- rbind(
  stats.data[,.(tourney_name,tourney_date,score,winner_name,player_name=winner_name,svpt=w_svpt,svgames=w_SvGms,fwon=w_1stWon,swon=w_2ndWon,bpsaved=w_bpSaved,bpfaced=w_bpFaced)],
  stats.data[,.(tourney_name,tourney_date,score,winner_name,player_name=loser_name,svpt=l_svpt,svgames=l_SvGms,fwon=l_1stWon,swon=l_2ndWon,bpsaved=l_bpSaved,bpfaced=l_bpFaced)]
)[,`:=`(
  svpt_won = fwon + swon,
  svgames_won = svgames - (bpfaced - bpsaved)
)][,`:=`(
  svpts_pct_won = svpt_won / svpt,
  svgames_pct_won = svgames_won / svgames
)][!(substr(score,nchar(score)-2,nchar(score))  %in% c('RET','W/O')),][!is.na(svpts_pct_won),]

# determine average % pts won vs % svgames held
results <- stats.data[,.(point = round(svpts_pct_won/0.01)*0.01,game = round(svgames_pct_won/0.01)*0.01)][point >= 0.4,.(avg_games = sum(game)/.N), by = point]
p <- ggplot(results, aes(x = point, y = avg_games)) + geom_line(size = 0.8, color = "cyan 3")
p
