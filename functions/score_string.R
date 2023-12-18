# convert a string representing the score into numeric variables
convert_score <- function(arg_score_string,arg_completed) {
  
  # initial values
  p1_games <- 0
  p2_games <- 0
  p1_total_games <- 0
  p2_total_games <- 0
  p1_total_tiebreaks <- 0
  p2_total_tiebreaks <- 0
  p1_points <- 0
  p2_points <- 0
  
  score_indexes <- c(0,sapply(gregexpr(pattern = " ", arg_score_string)[[1]], FUN = function(x) {x+1}))
  scores <- character(length(score_indexes)-1)
  for (i in 1:length(score_indexes)) {
    scores[i] <- substr(arg_score_string,score_indexes[i],ifelse(is.na(score_indexes[i+1]-2),nchar(arg_score_string),score_indexes[i+1]-2))
  } ; rm(i)
  
  # if the match is not completed, the last element of 'scores' won't a set
  if (!arg_completed) {
    num_sets <- length(score_indexes)-1
    num_completed_sets <- num_sets-1
  } else{
    num_sets <- length(score_indexes)
    num_completed_sets <- num_sets
  }
  
  # function to get the winner of each completed set
  get_set_winner <- function(arg_set_string) {
    p1_games <- as_integer(substr(arg_set_string,1,1))
    p2_games <- as_integer(substr(arg_set_string,3,3))
    if ((p1_games == 6 & p2_games < 5) | (p1_games == 7)) {
      return(list("P1",p1_games,p2_games))
    } else if ((p2_games == 6 & p1_games < 5) | (p2_games == 7)) {
      return(list("P2",p1_games,p2_games))
    } else {
      return("error")
    }
  }
  
  # retrieve winners of completed sets
  set_winners <- character(num_completed_sets)
  if (num_completed_sets > 0) {
    for (set_num in 1:num_completed_sets) {
      set_winners[set_num] <- get_set_winner(scores[set_num])[[1]]
      p1_total_games <- p1_total_games + min(6,get_set_winner(scores[set_num])[[2]])
      p2_total_games <- p2_total_games + min(6,get_set_winner(scores[set_num])[[3]])
      p1_total_tiebreaks <- p1_total_tiebreaks + ifelse(get_set_winner(scores[set_num])[[2]]==7,1,0)
      p2_total_tiebreaks <- p2_total_tiebreaks + ifelse(get_set_winner(scores[set_num])[[3]]==7,1,0)
    }
  }
  
  # extract number of completed sets won
  p1_sets <- sum(set_winners == "P1")
  p2_sets <- sum(set_winners == "P2")
  rm(set_num,set_winners)
  
  # get the number of games in the current set, if there is an uncompleted set
  if (!arg_completed) {
    p1_games <- as_integer(substr(scores[num_sets],1,1))
    p2_games <- as_integer(substr(scores[num_sets],3,3))
    p1_total_games <- p1_total_games + p1_games
    p2_total_games <- p2_total_games + p2_games
  }
  
  # determine if a tiebreak is currently happening
  if(p1_games == 6 & p2_games == 6) {
    tiebreak_ind <- TRUE
  } else {
    tiebreak_ind <- FALSE
  }
  
  # index of "-" for points
  if (!arg_completed) {
    dash_index <- gregexpr(pattern = "-", scores[length(scores)])[[1]]
  
    # get the number of points in the current game, depends
    p1_points <- substr(scores[length(scores)],1,dash_index-1)
    p2_points <- substr(scores[length(scores)],dash_index+1,nchar(scores[length(scores)]))
  
    # if not a tiebreak then need to convert scores like 40 and 30 to integer number of points
    if(!(tiebreak_ind)) {
      p1_points <- switch(p1_points,
                          "0" = 0,
                          "15" = 1,
                          "30" = 2,
                          "40" = 3,
                          "Ad" = 4)
      p2_points <- switch(p2_points,
                          "0" = 0,
                          "15" = 1,
                          "30" = 2,
                          "40" = 3,
                          "Ad" = 4)
    }
    p1_points <- as_integer(p1_points)
    p2_points <- as_integer(p2_points)
    
  }
  
  return(list("p1_sets" = p1_sets,
              "p2_sets" = p2_sets,
              "p1_games" = p1_games,
              "p2_games" = p2_games,
              "p1_points" = p1_points,
              "p2_points" = p2_points,
              "tiebreak_ind" = tiebreak_ind,
              "p1_total_games" = p1_total_games,
              "p2_total_games" = p2_total_games,
              "p1_total_tiebreaks" = p1_total_tiebreaks,
              "p2_total_tiebreaks" = p2_total_tiebreaks))
  
}
