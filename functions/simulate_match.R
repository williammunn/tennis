# define play.match
play.match <- function(arg.best.of,arg.match.first.server,arg.match.p1.serve.pct,arg.match.p2.serve.pct,arg.match.inherit = FALSE,arg.match.inherited.score = NULL) {
  # initial values
  match.over <- FALSE
  current.server <- arg.match.first.server
  p1.sets <- 0
  p2.sets <- 0
  sets.played <- sum(p1.sets,p2.sets)
  current.server <- arg.match.first.server
  current.server.pct <- ifelse(arg.match.first.server == 'P1', arg.match.p1.serve.pct, arg.match.p2.serve.pct)
  set.winner <- ''
  inherit.set <- FALSE
  # inherited
  if (arg.match.inherit) {
    inherited.score <- convert.score(arg.match.inherited.score)
    p1.sets <- inherited.score[["p1.sets"]]
    p2.sets <- inherited.score[["p2.sets"]]
    inherit.set <- TRUE
  }
  # create dataframe to hold the score
  points.df <- data.frame(
    point = numeric(),
    server = character(),
    winner = character()
  )
  # this function additionally outputs a data frame with the results of each point
  match.df <- data.frame(
    set_number <- numeric(),
    game_number <- numeric(),
    point_number <- numeric(),
    server <- character(),
    winner <- character(),
    tiebreak_ind <- logical()
  )
  # play sets until the match is over
  while(match.over == FALSE) {
    set.object <- play.set(arg.set.first.server = current.server, arg.set.p1.serve.pct = arg.match.p1.serve.pct, arg.set.p2.serve.pct = arg.match.p2.serve.pct, arg.set.inherit = inherit.set, arg.set.inherited.score = arg.match.inherited.score)
    # immediately switch off the inherited switch, so that any future sets start at 0-0
    inherit.set <- FALSE
    set.winner <- set.object[[1]]
    set.df <- set.object[[2]]
    set.last.server <- set.object[[3]]
    # swap server, based on who served in the final non-tiebreak game of the previous set
    current.server <- ifelse(set.last.server == 'P1','P2','P1')
    # increment number of sets played
    sets.played <- sets.played + 1
    # increment the set winner's count
    if (set.winner == 'P1') {
      p1.sets <- p1.sets + 1
    } else {
      p2.sets <- p2.sets + 1
    }
    # update the match.df object
    set.df['set_number'] <- p1.sets + p2.sets
    match.df <- rbind(
      match.df,
      set.df
    )
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
  return(list(match.winner,match.df))
}

# define play.set
play.set <- function(arg.set.first.server,arg.set.p1.serve.pct,arg.set.p2.serve.pct,arg.set.inherit = FALSE,arg.set.inherited.score = NULL) {
  # initial values
  set.over <- FALSE
  p1.games <- 0
  p2.games <- 0
  tiebreak <- FALSE
  current.server <- arg.set.first.server
  set.most.recent.server <- current.server
  inherit.game <- FALSE
  # inherit
  if (arg.set.inherit) {
    inherited.score <- convert.score(arg.set.inherited.score)
    p1.games <- inherited.score[["p1.games"]]
    p2.games <- inherited.score[["p2.games"]]
    tiebreak <- inherited.score[["tiebreak.ind"]]
    current.server <- arg.set.inherited.server
    inherit.game <- TRUE
  }
  # this function additionally outputs a data frame with the results of each point
  set.df <- data.frame(
    game_number <- numeric(),
    point_number <- numeric(),
    server <- character(),
    winner <- character(),
    tiebreak_ind <- logical()
  )
  # play games until the set is over
  while(set.over == FALSE) {
    if (!tiebreak) {
      # play a game
      game.object <- play.game(arg.game.server = current.server, arg.game.serve.pct = ifelse(current.server == 'P1', arg.set.p1.serve.pct, arg.set.p2.serve.pct), arg.game.inherit = inherit.game, arg.game.inherited.score = arg.set.inherited.score)
      # immediately switch off the inherited switch, so that any future games start at 0-0
      inherit.game <- FALSE
      # retrieve the winner of the game
      game.winner <- game.object[[1]]
      # retrieve the game.df object that shows the points played in the game just played
      game.df <- game.object[[2]]
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
      # update the set.df data frame
      game.df['game_number'] <- p1.games + p2.games
      game.df['tiebreak_ind'] <- tiebreak
      set.df <- rbind(
        set.df,
        game.df
      )
      # swap server
      current.server = ifelse(current.server == 'P1','P2','P1')
      # check if tiebreak needed
      if (p1.games == 6 & p2.games == 6) {
        tiebreak <- TRUE
      }
    }
    if(tiebreak) {
      game.object <- play.tiebreak(arg.tiebreak.first.server = current.server,arg.tiebreak.p1.serve.pct = arg.set.p1.serve.pct,arg.tiebreak.p2.serve.pct = arg.set.p2.serve.pct,arg.tiebreak.inherit = inherit.game,arg.tiebreak.inherited.score = arg.set.inherited.score)
      # immediately switch off the inherited switch
      inherit.game <- FALSE
      game.winner <- game.object[[1]]
      game.df <- game.object[[2]]
      set.over <- TRUE
      # award winner of tiebreak a game
      if (game.winner == 'P1') {
        p1.games <- p1.games + 1
      } else {
        p2.games <- p2.games + 1
      }
      # update set.df data frame
      game.df['game_number'] <- p1.games + p2.games
      game.df['tiebreak_ind'] <- tiebreak
      set.df <- rbind(
        set.df,
        game.df
      )
    }
  }
  if (p1.games > p2.games) {
    set.winner <- 'P1'
  } else {
    set.winner <- 'P2'
  }
  return(list(set.winner,set.df,set.most.recent.server))
}

# define play.game
play.game <- function(arg.game.server,arg.game.serve.pct,arg.game.inherit = FALSE,arg.game.inherited.score = NULL) {
  # initial values
  p1.points <- 0
  p2.points <- 0
  current.server <- arg.game.server
  game.over <- FALSE
  # inherit
  if (arg.game.inherit) {
    inherited.score <- convert.score(arg.game.inherited.score)
    p1.points <- inherited.score[["p1.points"]]
    p2.points <- inherited.score[["p2.points"]]
  }
  # this function additionally outputs a data frame with a record of points and their outcomes
  game.df <- data.frame(
    point_number = numeric(),
    server = character(),
    winner = character()
  )
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
    # add a row to the game.df data frame
    game.df <- rbind(
      game.df,
      data.frame(point_number = p1.points + p2.points, server = arg.game.server, winner = point.winner)
    )
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
  return(list(game.winner,game.df))
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

play.tiebreak <- function(arg.tiebreak.first.server,arg.tiebreak.p1.serve.pct,arg.tiebreak.p2.serve.pct,arg.tiebreak.inherit=FALSE,arg.tiebreak.inherited.score=NULL) {
  # initial values
  tiebreak.over <- FALSE
  tiebreak.current.server <- arg.tiebreak.first.server
  p1.points <- 0
  p2.points <- 0
  tiebreak.serves.remaining <- 1 # first person to serve only serves once
  # inherit
  if (arg.tiebreak.inherit) {
    inherited.score <- convert.score(arg.tiebreak.inherited.score)
    p1.points <- inherited.score[["p1.points"]]
    p2.points <- inherited.score[["p2.points"]]
    tiebreak.server.points.served <- (sum(p1.points,p2.points)%%2) + 1
  }
  # this function additionally outputs a data frame with the outcomes of individual points
  tiebreak.df <- data.frame(
    point_number = numeric(),
    server = character(),
    winner = character()
  )
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
      # update the tiebreak.df object
      tiebreak.df <- rbind(
        tiebreak.df,
        data.frame(point_number = p1.points + p2.points, server = tiebreak.current.server, winner = point.winner)
      )
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
  return(list(tiebreak.winner,tiebreak.df))
}

# example of a match
play.match(arg.best.of = 3, arg.match.first.server = 'P1', arg.match.p1.serve.pct = 0.9, arg.match.p2.serve.pct = 0.9)[[2]]
