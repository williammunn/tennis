# tennis code
rm(list=ls())

require(tidyverse)
require(lubridate)
require(ggplot2)

play.point <- function(serving, serve_pct) {
  # generate random number
  random.number <- runif(1)
  # determine winner of point
  if (serve_pct > random.number) { #server wins point
    if (serving == "P1") {
      point.winner <<- "P1"
      return("P1")
    } else if (serving == "P2") {
      point.winner <<- "P2"
      return("P2")
    }
  } else { # server loses point
    if (serving == "P1") {
      point.winner <<- "P2"
      return("P2")
    } else if (serving == "P2") {
      point.winner <<- "P1"
      return("P1")
    }
  }
}

# default starting value for variables
game.over <- 0
set.over <- 0
tiebreak.over <- 0
points.played <- 0
p1.serve.pct <- 0.8
p2.serve.pct <- 0.8
current.server <- 'P1'
current.server.pct <- ifelse(current.server == 'P1',p1.serve.pct,p2.serve.pct)
p1.points <- 0
p2.points <- 0
p1.games <- 0
p2.games <- 0
p1.sets <- 0
p2.sets <- 0
point.score <- ''
game.score <- ''
set.score <- ''
point.winner <- ''
game.winner <- ''
tiebreak <- 0
scoreboard <- data.frame(
  point = numeric(),
  server = character(),
  winner = character(),
  p1_points = numeric(),
  p2_points = numeric(),
  p1_games = numeric(),
  p2_games = numeric(),
  point_score = character(),
  game_score = character(),
  set_score = character()
)

play.game <- function(game_server,game_serve_pct) {
  while(game.over != 1) {
    play.point(serving = game_server , serve_pct = game_serve_pct)
    update.score()
    check.game.over()
  }
  if (game.over == 1) {
    # clear up the points for the next game to be played
    game.over <<- 0
    p1.points <<- 0
    p2.points <<- 0
    # update scoreboard to reflect game winner
    scoreboard[points.played,'p1_games'] <<- p1.games
    scoreboard[points.played,'p2_games'] <<- p2.games
    # swap the server over
    if (current.server == 'P1') {
      current.server <<- 'P2'
      current.server.pct <<- p2.serve.pct
    } else {
      current.server <<- 'P1'
      current.server.pct <<- p1.serve.pct
    }
    return(game.winner)
  }
}

play.tiebreak <- function() {
  # play the first point of the tiebreak
  play.point(serving = current.server , serve_pct = ifelse(current.server == 'P1',p1.serve.pct,p2.serve.pct))
  update.score()
  if (current.server == 'P1') {
    current.server <<- 'P2'
    current.server.pct <<- p2.serve.pct
  } else if (current.server == 'P2') {
    current.server <<- 'P1'
    current.server.pct <<- p1.serve.pct
  }
  # play the remaining points of the tiebreak
  while(tiebreak.over != 1) {
    # first point
    play.point(serving = current.server , serve_pct = ifelse(current.server == 'P1',p1.serve.pct,p2.serve.pct))
    update.score()
    check.tiebreak.over()
    # second point
    play.point(serving = current.server , serve_pct = ifelse(current.server == 'P1',p1.serve.pct,p2.serve.pct))
    update.score()
    check.tiebreak.over()
    # swap server
    if (current.server == 'P1') {
      current.server <<- 'P2'
      current.server.pct <<- p2.serve.pct
    } else if (current.server == 'P2') {
      current.server <<- 'P1'
      current.server.pct <<- p1.serve.pct
    }
  }
  if (tiebreak.over == 1) {
    # clear up the points for the next game to be played
    tiebreak.over <<- 0
    p1.points <<- 0
    p2.points <<- 0
    # update scoreboard to reflect game winner
    scoreboard[points.played,'p1_games'] <<- p1.games
    scoreboard[points.played,'p2_games'] <<- p2.games
    # swap the server over - need to check how this works in real life

    return(game.winner)
  }
}

check.tiebreak.over <- function() {
  if ((p1.points >= 7 | p2.points >= 7) &
      (abs(p1.points - p2.points) >= 2)) {
    if (p1.points > p2.points) {
      game.winner <<- "P1"
      p1.games <<- p1.games + 1
    } else{
      game.winner <<- "P2"
      p2.games <<- p2.games + 1
    }
    tiebreak.over <<- 1
  }
}

reset.values <- function() {
  scoreboard <<- scoreboard[0,]
  game.over <<- 0
  set.over <<- 0
  tiebreak.over <<- 0
  points.played <<- 0
  p1.points <<- 0
  p2.points <<- 0
  p1.games <<- 0
  p2.games <<- 0
  p1.sets <<- 0
  p2.sets <<- 0
  tiebreak <<- 0
  point.winner <<- ''
  game.winner <<- ''
  set.winner <<- ''
  point.score <<- ''
  game.score <<- ''
  set.score <<- ''
}

check.game.over <- function() {
  if ((p1.points >= 4 | p2.points >= 4) &
      (abs(p1.points - p2.points) >= 2)) {
    if (p1.points > p2.points) {
      game.winner <<- "P1"
      p1.games <<- p1.games + 1
    } else{
      game.winner <<- "P2"
      p2.games <<- p2.games + 1
    }
    game.over <<- 1
  }
}

create.score <- function() {
  if (p1.points == 0) {
    point.score <<- ifelse(current.server == 'P1', 
                           switch(p2.points + 1,'0-0','0-15','0-30','0-40','Game P2'),
                           switch(p2.points + 1,'0-0','15-0','30-0','40-0','Game P2')
    )
  } else if (p1.points == 1) {
    point.score <<- ifelse(current.server == 'P1',
                           switch(p2.points + 1,'15-0','15-15','15-30','15-40','Game P2'),
                           switch(p2.points + 1,'0-15','15-15','30-15','40-15','Game P2')
    )
  } else if (p1.points == 2) {
    point.score <<- ifelse(current.server == 'P1',
                           switch(p2.points + 1,'30-0','30-15','30-30','30-40','Game P2'),
                           switch(p2.points + 1, '0-30','15-30','30-30','40-30','Game P2')
    )
  } else if (p1.points == 3) {
    point.score <<- ifelse(current.server == 'P1',
                           switch(p2.points + 1, '40-0','40-15','40-30','40-40','40-Ad','Game P2'),
                           switch(p2.points + 1, '0-40','15-40','30-40','40-40','Ad-40','Game P2')
    )
  } else if (p1.points == 4) {
    point.score <<- ifelse(current.server == 'P1',
                           switch(p2.points + 1, 'Game P1','Game P1','Game P1','Ad-40','40-40','40-Ad','Game P2'),
                           switch(p2.points + 1, 'Game P1','Game P1','Game P1','40-Ad','40-40','Ad-40','Game P2')
    )
  } else if (p1.points == 5) {
    point.score <<- ifelse(current.server == 'P1',
                           switch(p2.points + 1,'error','error','error','Game P1','Ad-40'),
                           switch(p2.points + 1,'error','error','error','Game P1','40-Ad')
    ) 
  } else if (p1.points == 6) {
    point.score <<- ifelse(current.server == 'P1',
                           switch(p2.points + 1,'error','error','error','error','Game P1'),
                           switch(p2.points + 1,'error','error','error','error','Game P1')
    )
  }
}

update.score <- function() {
  points.played <<- points.played + 1
  p1.points <<- ifelse(point.winner == "P1", p1.points + 1, p1.points)
  p2.points <<- ifelse(point.winner == "P2", p2.points + 1, p2.points)
  # after the score gets to 5-5, we reset it back to 4-4
  if (tiebreak == 0) {
    if (p1.points == 5 & p2.points == 5) {
      p1.points <<- 3
      p2.points <<- 3
    }
    create.score()
  } else {
    point.score <<- ifelse(current.server == 'P1',
                           paste0(p1.points,'-',p2.points),
                           paste0(p2.points,'-',p1.points)
    )
  }
  # update the scoreboard
  scoreboard <<- rbind(
    scoreboard,
    list(
      point = points.played,
      server = current.server,
      winner = point.winner,
      p1_points = p1.points,
      p2_points = p2.points,
      point_score = point.score,
      p1_games = p1.games,
      p2_games = p2.games,
      game_score = game.score,
      p1_sets = p1.sets,
      p2_sets = p2.sets,
      set_score = set.score
    )
  )
}

# play a game
#reset.values()
#play.game(game_server = "P1" , game_serve_pct = 0.7)

play.set <- function(set_first_server) {
  # assign the first server
  current.server <<- set_first_server
  current.server.pct <<- ifelse(current.server == 'P1',p1.serve.pct,p2.serve.pct)
  while(set.over != 1) {
    if (tiebreak == 0) {
      play.game(game_server = current.server , game_serve_pct = current.server.pct)
    } else {
      play.tiebreak()
    }
    check.set.over()
  }
  if (set.over == 1) {
    return(set.winner)
  }
}

check.set.over <- function() {
  # check if set is over
  if ((p1.games == 6 & p2.games < 5) | (p1.games == 7)) {
    set.winner <<- 'P1'
    p1.sets <<- p1.sets + 1
    set.over <<- 1
    scoreboard[points.played,'p1_sets'] <<- p1.sets
  } else if ((p2.games == 6 & p1.games < 5) | (p2.games == 7)) {
    set.winner <<- 'P2'
    p2.sets <<- p2.sets + 1
    set.over <<- 1
    scoreboard[points.played,'p2_sets'] <<- p2.sets
  }
  # check if tiebreak is needed
  if (p1.games == 6 & p2.games == 6) {
    tiebreak <<- 1
  }
}

# play a single set
reset.values()
play.set(set_first_server = 'P1')
