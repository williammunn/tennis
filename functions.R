# tennis code
rm(list=ls())

# define play.match
play.match <- function(arg.best.of,arg.match.first.server,arg.match.p1.serve.pct,arg.match.p2.serve.pct,arg.match.print.score=FALSE) {
  # set initial values
  match.over <- FALSE
  current.server <- arg.match.first.server
  current.server.pct <- ifelse(arg.match.first.server == 'P1', arg.match.p1.serve.pct, arg.match.p2.serve.pct)
  p1.sets <- 0
  p2.sets <- 0
  set.winner <- ''
  sets.played <- 0
  set.winners <- character()
  # play sets until the match is over
  while(match.over == FALSE) {
    set.winner <- play.set(arg.set.first.server = current.server, arg.set.p1.serve.pct = arg.match.p1.serve.pct, arg.set.p2.serve.pct = arg.match.p2.serve.pct,arg.set.print.score = arg.match.print.score)
    # swap server
    current.server <- ifelse(current.server == 'P1','P2','P1') # this bit of code is not working, because it needs to be able to look at who was last serving, defined within play.set
    # increment number of sets played
    sets.played <- sets.played + 1
    # record the set winner
    set.winners[sets.played] <- set.winner
    # increment the set winner's count
    if (set.winner == 'P1') {
      p1.sets <- p1.sets + 1
    } else {
      p2.sets <- p2.sets + 1
    }
    # check if match is over
    if (p1.sets == ceiling(arg.best.of/2) | p2.sets == ceiling(arg.best.of/2)) {
      match.over <- TRUE
    }
  }
  # determine winner
  if (p1.sets > p2.sets) {
    match.winner <- 'P1'
  } else {
    match.winner <- 'P2'
  }
  if(arg.match.print.score){print(paste0(match.winner,' won the match ',p1.sets,'-',p2.sets))}
  return(match.winner)
}

# define play.set
play.set <- function(arg.set.first.server,arg.set.p1.serve.pct,arg.set.p2.serve.pct,arg.set.print.score=FALSE) {
  # initial values
  set.over <- FALSE
  p1.games <- 0
  p2.games <- 0
  tiebreak <- FALSE
  current.server <- arg.set.first.server
  server.before.tiebreak <- current.server
  # play games until the set is over
  while(set.over == FALSE) {
    # play a game
    game.winner <- play.game(arg.game.server = current.server, arg.game.serve.pct = ifelse(current.server == 'P1', arg.set.p1.serve.pct, arg.set.p2.serve.pct), arg.game.print.score = arg.set.print.score)
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
    if(tiebreak==TRUE) {
      game.winner <- play.tiebreak(arg.tiebreak.first.server = current.server, arg.tiebreak.p1.serve.pct = arg.set.p1.serve.pct, arg.tiebreak.p2.serve.pct = arg.set.p2.serve.pct, arg.tiebreak.print.score = arg.set.print.score)
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
  # print outcome of set
  if(arg.set.print.score){print(paste0(set.winner,' won the set, ',p1.games,'-',p2.games))}
  return(set.winner)
}

# define play.game
play.game <- function(arg.game.server, arg.game.serve.pct, arg.game.print.score=FALSE) {
  # initial values
  p1.points <- 0
  p2.points <- 0
  game.over <- FALSE
  # play points until the game is over
  while(game.over == FALSE) {
    # play a point
    point.winner <- play.point(arg.point.server = arg.game.server, arg.point.serve.pct = arg.game.serve.pct)
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
  if(arg.game.print.score){print(paste0(game.winner,' won the game, ',p1.points,'-',p2.points,' ',arg.game.server,' was serving'))}
  return(game.winner)
}

# define play.point
play.point <- function(arg.point.server, arg.point.serve.pct) {
  # generate random number
  random.number <- runif(1)
  # determine winner of point
  if (arg.point.serve.pct > random.number) {
    if (arg.point.server == 'P1') {
      return('P1')
    } else if (arg.point.server == 'P2') {
      return('P2')
    }
  } else {
    if (arg.point.server == 'P1') {
      return('P2')
    } else if (arg.point.server == 'P2') {
      return('P1')
    }
  }
}

play.tiebreak <- function(arg.tiebreak.first.server,arg.tiebreak.p1.serve.pct,arg.tiebreak.p2.serve.pct,arg.tiebreak.print.score=FALSE) {
  # initial values
  tiebreak.over <- FALSE
  tiebreak.current.server <- arg.tiebreak.first.server
  p1.points <- 0
  p2.points <- 0
  # play the first point of the tiebreak
  point.winner <- play.point(arg.point.server = tiebreak.current.server, arg.point.serve.pct = ifelse(tiebreak.current.server == 'P1',arg.tiebreak.p1.serve.pct,arg.tiebreak.p2.serve.pct))
  # award point to the winner
  if (point.winner == 'P1') {
    p1.points <- 1
  } else {
    p2.points <- 1
  }
  # play the remainder of the tiebreak
  while(tiebreak.over == FALSE) {
    # swap server
    tiebreak.current.server <- ifelse(tiebreak.current.server == 'P1','P2','P1')
    # play the first point on this player's server
    point.winner <- play.point(arg.point.server = tiebreak.current.server, arg.point.serve.pct = ifelse(tiebreak.current.server == 'P1',arg.tiebreak.p1.serve.pct,arg.tiebreak.p2.serve.pct))
    # award point to the winner
    if (point.winner == 'P1') {
      p1.points <- p1.points + 1
    } else {
      p2.points <- p2.points + 1
    }
    # check if tiebreak should be ended
    if ((p1.points >= 7 | p2.points >= 7) & (abs(p1.points - p2.points) > 1)) {
      tiebreak.over <- TRUE
      break
    }
    # play the second point on this player's server
    point.winner <- play.point(arg.point.server = tiebreak.current.server, arg.point.serve.pct = ifelse(tiebreak.current.server == 'P1',arg.tiebreak.p1.serve.pct,arg.tiebreak.p2.serve.pct))
    # award point to the winner
    if (point.winner == 'P1') {
      p1.points <- p1.points + 1
    } else {
      p2.points <- p2.points + 1
    }
    # check if tiebreak should be ended
    if ((p1.points >= 7 | p2.points >= 7) & (abs(p1.points - p2.points) > 1)) {
      tiebreak.over <- TRUE
      break
    }
  }
  # determine who wins the tiebreak
  if (p1.points > p2.points) {
    tiebreak.winner <- 'P1'
  } else {
    tiebreak.winner <- 'P2'
  }
  # return the winner of the tiebreak
  if(arg.tiebreak.print.score){print(paste0(tiebreak.winner,' won the tiebreak ',p1.points,'-',p2.points))}
  return(tiebreak.winner)
}

winner <- play.match(arg.best.of = 5, arg.match.first.server = 'P1', arg.match.p1.serve.pct = 0.9, arg.match.p2.serve.pct = 0.9, arg.match.print.score = TRUE)


