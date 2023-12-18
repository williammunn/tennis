# define play_match
play_match <- function(arg_best_of,
                       arg_match_first_server,
                       arg_match_p1_serve_pct,
                       arg_match_p2_serve_pct,
                       arg_match_inherit = FALSE,
                       arg_match_inherited_score = NULL) {
  # initial values
  match_over <- FALSE
  current_server <- arg_match_first_server
  p1_sets <- 0
  p2_sets <- 0
  sets_played <- sum(p1_sets,p2_sets)
  current_server <- arg_match_first_server
  current_server_pct <- ifelse(arg_match_first_server == 'P1', arg_match_p1_serve_pct, arg_match_p2_serve_pct)
  set_winner <- ''
  inherit_set <- FALSE
  # inherited
  if (arg_match_inherit) {
    inherited_score <- convert_score(arg_match_inherited_score)
    p1_sets <- inherited_score[["p1_sets"]]
    p2_sets <- inherited_score[["p2_sets"]]
    inherit_set <- TRUE
  }
  # create dataframe to hold the score
  points_df <- data.frame(
    point = numeric(),
    server = character(),
    winner = character()
  )
  # this function additionally outputs a data frame with the results of each point
  match_df <- data.frame(
    set_number <- numeric(),
    game_number <- numeric(),
    point_number <- numeric(),
    server <- character(),
    winner <- character(),
    tiebreak_ind <- logical()
  )
  # play sets until the match is over
  while(match_over == FALSE) {
    set_object <- play_set(arg_set_first_server = current_server, 
                           arg_set_p1_serve_pct = arg_match_p1_serve_pct, 
                           arg_set_p2_serve_pct = arg_match_p2_serve_pct, 
                           arg_set_inherit = inherit_set, 
                           arg_set_inherited_score = arg_match_inherited_score)
    # immediately switch off the inherited switch, so that any future sets start at 0-0
    inherit_set <- FALSE
    set_winner <- set_object[[1]]
    set_df <- set_object[[2]]
    set_last_server <- set_object[[3]]
    # swap server, based on who served in the final non-tiebreak game of the previous set
    current_server <- ifelse(set_last_server == 'P1','P2','P1')
    # increment number of sets played
    sets_played <- sets_played + 1
    # increment the set winner's count
    if (set_winner == 'P1') {
      p1_sets <- p1_sets + 1
    } else {
      p2_sets <- p2_sets + 1
    }
    # update the match_df object
    set_df['set_number'] <- p1_sets + p2_sets
    match_df <- rbind(
      match_df,
      set_df
    )
    # check if match is over
    if (p1_sets == ceiling(arg_best_of/2) | p2_sets == ceiling(arg_best_of/2)) {
      match_over <- TRUE
    }
  }
  # determine winner
  if (p1_sets > p2_sets) {
    match_winner <- 'P1'
  } else {
    match_winner <- 'P2'
  }
  return(list(match_winner,match_df))
}

# define play_set
play_set <- function(arg_set_first_server,
                     arg_set_p1_serve_pct,
                     arg_set_p2_serve_pct,
                     arg_set_inherit = FALSE,
                     arg_set_inherited_score = NULL) {
  # initial values
  set_over <- FALSE
  p1_games <- 0
  p2_games <- 0
  tiebreak <- FALSE
  current_server <- arg_set_first_server
  set_most_recent_server <- current_server
  inherit_game <- FALSE
  # inherit
  if (arg_set_inherit) {
    inherited_score <- convert_score(arg_set_inherited_score)
    p1_games <- inherited_score[["p1_games"]]
    p2_games <- inherited_score[["p2_games"]]
    tiebreak <- inherited_score[["tiebreak_ind"]]
    current_server <- arg_set_inherited_server
    inherit_game <- TRUE
  }
  # this function additionally outputs a data frame with the results of each point
  set_df <- data.frame(
    game_number <- numeric(),
    point_number <- numeric(),
    server <- character(),
    winner <- character(),
    tiebreak_ind <- logical()
  )
  # play games until the set is over
  while(set_over == FALSE) {
    if (!tiebreak) {
      # play a game
      game_object <- play_game(arg_game_server = current_server, 
                               arg_game_serve_pct = ifelse(current_server == 'P1', arg_set_p1_serve_pct, arg_set_p2_serve_pct), 
                               arg_game_inherit = inherit_game, 
                               arg_game_inherited_score = arg_set_inherited_score)
      # immediately switch off the inherited switch, so that any future games start at 0-0
      inherit_game <- FALSE
      # retrieve the winner of the game
      game_winner <- game_object[[1]]
      # retrieve the game_df object that shows the points played in the game just played
      game_df <- game_object[[2]]
      # update the most recent server variable
      set_most_recent_server <- current_server
      # award winner a game
      if (game_winner == 'P1') {
        p1_games <- p1_games + 1
      } else {
        p2_games <- p2_games + 1
      }
      # check if set is over
      if((p1_games == 6 & p2_games <= 4) | (p2_games == 6 & p1_games <= 4) | p1_games == 7 | p2_games == 7) {
        set_over <- TRUE
      }
      # update the set_df data frame
      game_df['game_number'] <- p1_games + p2_games
      game_df['tiebreak_ind'] <- tiebreak
      set_df <- rbind(
        set_df,
        game_df
      )
      # swap server
      current_server = ifelse(current_server == 'P1','P2','P1')
      # check if tiebreak needed
      if (p1_games == 6 & p2_games == 6) {
        tiebreak <- TRUE
      }
    }
    if(tiebreak) {
      game_object <- play_tiebreak(arg_tiebreak_first_server = current_server,
                                   arg_tiebreak_p1_serve_pct = arg_set_p1_serve_pct,
                                   arg_tiebreak_p2_serve_pct = arg_set_p2_serve_pct,
                                   arg_tiebreak_inherit = inherit_game,
                                   arg_tiebreak_inherited_score = arg_set_inherited_score)
      # immediately switch off the inherited switch
      inherit_game <- FALSE
      game_winner <- game_object[[1]]
      game_df <- game_object[[2]]
      set_over <- TRUE
      # award winner of tiebreak a game
      if (game_winner == 'P1') {
        p1_games <- p1_games + 1
      } else {
        p2_games <- p2_games + 1
      }
      # update set_df data frame
      game_df['game_number'] <- p1_games + p2_games
      game_df['tiebreak_ind'] <- tiebreak
      set_df <- rbind(
        set_df,
        game_df
      )
    }
  }
  if (p1_games > p2_games) {
    set_winner <- 'P1'
  } else {
    set_winner <- 'P2'
  }
  return(list(set_winner,set_df,set_most_recent_server))
}

# define play_game
play_game <- function(arg_game_server,
                      arg_game_serve_pct,
                      arg_game_inherit = FALSE,
                      arg_game_inherited_score = NULL) {
  # initial values
  p1_points <- 0
  p2_points <- 0
  current_server <- arg_game_server
  game_over <- FALSE
  # inherit
  if (arg_game_inherit) {
    inherited_score <- convert_score(arg_game_inherited_score)
    p1_points <- inherited_score[["p1_points"]]
    p2_points <- inherited_score[["p2_points"]]
  }
  # this function additionally outputs a data frame with a record of points and their outcomes
  game_df <- data.frame(
    point_number = numeric(),
    server = character(),
    winner = character()
  )
  # play points until the game is over
  while(game_over == FALSE) {
    # play a point
    point_winner <- play_point(arg_point_server = current_server, 
                               arg_point_serve_pct = arg_game_serve_pct)
    # increment points of the winner of the point just played
    if (point_winner == 'P1') {
      p1_points <- p1_points + 1
    } else {
      p2_points <- p2_points + 1
    }
    if (p1_points == 4 & p2_points == 4) {
      p1_points <- 3
      p2_points <- 3
    }
    # add a row to the game_df data frame
    game_df <- rbind(
      game_df,
      data.frame(point_number = p1_points + p2_points, server = arg_game_server, winner = point_winner)
    )
    # check if the game is over
    if ((p1_points >= 4 | p2_points >= 4) &
        (abs(p1_points - p2_points) >= 2)) {
      game_over <- TRUE
    }
  }
  if (p1_points > p2_points) {
    game_winner <- 'P1'
  } else {
    game_winner <- 'P2'
  }
  return(list(game_winner,game_df))
}

# define play_point
play_point <- function(arg_point_server,
                       arg_point_serve_pct) {
  # generate random number
  random_number <- runif(1)
  # determine winner of point
  if (arg_point_serve_pct > random_number) {
    if (arg_point_server == 'P1') {
      return('P1')
    } else if (arg_point_server == 'P2') {
      return('P2')
    }
  } else {
    if (arg_point_server == 'P1') {
      return('P2')
    } else if (arg_point_server == 'P2') {
      return('P1')
    }
  }
}

play_tiebreak <- function(arg_tiebreak_first_server,
                          arg_tiebreak_p1_serve_pct,
                          arg_tiebreak_p2_serve_pct,
                          arg_tiebreak_inherit=FALSE,
                          arg_tiebreak_inherited_score=NULL) {
  # initial values
  tiebreak_over <- FALSE
  tiebreak_current_server <- arg_tiebreak_first_server
  p1_points <- 0
  p2_points <- 0
  tiebreak_serves_remaining <- 1 # first person to serve only serves once
  # inherit
  if (arg_tiebreak_inherit) {
    inherited_score <- convert_score(arg_tiebreak_inherited_score)
    p1_points <- inherited_score[["p1_points"]]
    p2_points <- inherited_score[["p2_points"]]
    tiebreak_server_points_served <- (sum(p1_points,p2_points)%%2) + 1
  }
  # this function additionally outputs a data frame with the outcomes of individual points
  tiebreak_df <- data.frame(
    point_number = numeric(),
    server = character(),
    winner = character()
  )
  # play the remainder of the tiebreak
  while(tiebreak_over == FALSE) {
    # play the current server's 2 points
    while (tiebreak_serves_remaining > 0) {
      # play the first point on this player's server
      point_winner <- play_point(arg_point_server = tiebreak_current_server,
                                 arg_point_serve_pct = ifelse(tiebreak_current_server == 'P1',arg_tiebreak_p1_serve_pct,arg_tiebreak_p2_serve_pct))
      # award point to the winner
      if (point_winner == 'P1') {
        p1_points <- p1_points + 1
      } else {
        p2_points <- p2_points + 1
      }
      # increment the points served by this current server
      tiebreak_serves_remaining <- tiebreak_serves_remaining - 1
      # update the tiebreak_df object
      tiebreak_df <- rbind(
        tiebreak_df,
        data.frame(point_number = p1_points + p2_points, server = tiebreak_current_server, winner = point_winner)
      )
      # check if tiebreak should be ended
      if ((p1_points >= 7 | p2_points >= 7) & (abs(p1_points - p2_points) > 1)) {
        tiebreak_over <- TRUE
        break
      }
    }
    # after the first point players get 2 serves each
    tiebreak_serves_remaining <- 2
    # swap server
    tiebreak_current_server <- ifelse(tiebreak_current_server == "P1","P2","P1")
  }
  # determine who wins the tiebreak
  if (p1_points > p2_points) {
    tiebreak_winner <- 'P1'
  } else {
    tiebreak_winner <- 'P2'
  }
  # return the winner of the tiebreak
  return(list(tiebreak_winner,tiebreak_df))
}

# example of a match
play_match(arg_best_of = 3, arg_match_first_server = 'P1', arg_match_p1_serve_pct = 0.9, arg_match_p2_serve_pct = 0.9)[[2]]
