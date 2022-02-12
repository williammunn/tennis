# convert a string representing the score into numeric variables
convert.score <- function(arg.score.string,arg.current.server) {
  
  object.indexes <- sapply(gregexpr(pattern = " ", arg.score.string)[[1]], FUN = function(x) {x+1})
  num.sets <- length(object.indexes)
  num.completed.sets <- num.sets-1
  
  # function to get the winner of each completed set
  get.set.winner <- function(arg.set.string) {
    p1.games <- substr(arg.set.string,1,1)
    p2.games <- substr(arg.set.string,3,3)
    if ((p1.games == 6 & p2.games < 5) | (p1.games == 7)) {
      return("P1")
    } else if ((p2.games == 6 & p1.games < 5) | (p2.games == 7)) {
      return("P2")
    } else {
      return("error")
    }
  }
  
  # retrieve winners of completed sets
  set.winners <- character()
  if (num.completed.sets > 0) {
    # first set
    set.winners[1] <- get.set.winner(substr(arg.score.string,1,3))
    if (num.completed.sets > 1) {
      for (set.num in 2:num.completed.sets) {
        set.winners[set.num] <- get.set.winner(substr(arg.score.string,object.indexes[set.num-1],object.indexes[set.num-1]+2))
      }
    }
  }
  
  # extract number of completed sets won
  p1.sets <- sum(set.winners == "P1")
  p2.sets <- sum(set.winners == "P2")
  
  # get the number of games in the current set
  if (num.completed.sets > 0) {
    p1.games <- as.integer(substr(arg.score.string,object.indexes[num.sets-1],object.indexes[num.sets-1]))
    p2.games <- as.integer(substr(arg.score.string,object.indexes[num.sets-1]+2,object.indexes[num.sets-1]+2))
  } else {
    p1.games <- as.integer(substr(arg.score.string,1,1))
    p2.games <- as.integer(substr(arg.score.string,3,3))
  }
  
  # determine if a tiebreak is currently happening
  if(p1.games == 6 & p2.games == 6) {
    tiebreak.ind <- TRUE
  } else {
    tiebreak.ind <- FALSE
  }
  
  # index of "-" for points
  dash.index <- tail(gregexpr(pattern = "-", arg.score.string)[[1]],n=1)
  
  # get the number of points in the current game, depends
  p1.points <- substr(arg.score.string,object.indexes[num.sets],dash.index-1)
  p2.points <- substr(arg.score.string,dash.index+1,nchar(arg.score.string))
  
  # if not a tiebreak then need to convert scores like 40 and 30 to integer number of points
  if(!(tiebreak.ind)) {
    p1.points <- switch(p1.points,
                        "0" = 0,
                        "15" = 1,
                        "30" = 2,
                        "40" = 3,
                        "Ad" = 4)
    p2.points <- switch(p2.points,
                        "0" = 0,
                        "15" = 1,
                        "30" = 2,
                        "40" = 3,
                        "Ad" = 4)
  }
  p1.points <- as.integer(p1.points)
  p2.points <- as.integer(p2.points)
  
  return(list("p1.sets" = p1.sets,
              "p2.sets" = p2.sets,
              "p1.games" = p1.games,
              "p2.games" = p2.games,
              "p1.points" = p1.points,
              "p2.points" = p2.points,
              "tiebreak.ind" = tiebreak.ind,
              "current.server" = arg.current.server))
  
}