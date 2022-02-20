# convert a string representing the score into numeric variables
convert.score <- function(arg.score.string,arg.completed) {
  
  # initial values
  p1.games <- 0
  p2.games <- 0
  p1.total.games <- 0
  p2.total.games <- 0
  p1.total.tiebreaks <- 0
  p2.total.tiebreaks <- 0
  p1.points <- 0
  p2.points <- 0
  
  score.indexes <- c(0,sapply(gregexpr(pattern = " ", arg.score.string)[[1]], FUN = function(x) {x+1}))
  scores <- character(length(score.indexes)-1)
  for (i in 1:length(score.indexes)) {
    scores[i] <- substr(arg.score.string,score.indexes[i],ifelse(is.na(score.indexes[i+1]-2),nchar(arg.score.string),score.indexes[i+1]-2))
  } ; rm(i)
  
  # if the match is not completed, the last element of 'scores' won't a set
  if (!arg.completed) {
    num.sets <- length(score.indexes)-1
    num.completed.sets <- num.sets-1
  } else{
    num.sets <- length(score.indexes)
    num.completed.sets <- num.sets
  }
  
  # function to get the winner of each completed set
  get.set.winner <- function(arg.set.string) {
    p1.games <- as.integer(substr(arg.set.string,1,1))
    p2.games <- as.integer(substr(arg.set.string,3,3))
    if ((p1.games == 6 & p2.games < 5) | (p1.games == 7)) {
      return(list("P1",p1.games,p2.games))
    } else if ((p2.games == 6 & p1.games < 5) | (p2.games == 7)) {
      return(list("P2",p1.games,p2.games))
    } else {
      return("error")
    }
  }
  
  # retrieve winners of completed sets
  set.winners <- character(num.completed.sets)
  if (num.completed.sets > 0) {
    for (set.num in 1:num.completed.sets) {
      set.winners[set.num] <- get.set.winner(scores[set.num])[[1]]
      p1.total.games <- p1.total.games + min(6,get.set.winner(scores[set.num])[[2]])
      p2.total.games <- p2.total.games + min(6,get.set.winner(scores[set.num])[[3]])
      p1.total.tiebreaks <- p1.total.tiebreaks + ifelse(get.set.winner(scores[set.num])[[2]]==7,1,0)
      p2.total.tiebreaks <- p2.total.tiebreaks + ifelse(get.set.winner(scores[set.num])[[3]]==7,1,0)
    }
  }
  
  # extract number of completed sets won
  p1.sets <- sum(set.winners == "P1")
  p2.sets <- sum(set.winners == "P2")
  rm(set.num,set.winners)
  
  # get the number of games in the current set, if there is an uncompleted set
  if (!arg.completed) {
    p1.games <- as.integer(substr(scores[num.sets],1,1))
    p2.games <- as.integer(substr(scores[num.sets],3,3))
    p1.total.games <- p1.total.games + p1.games
    p2.total.games <- p2.total.games + p2.games
  }
  
  # determine if a tiebreak is currently happening
  if(p1.games == 6 & p2.games == 6) {
    tiebreak.ind <- TRUE
  } else {
    tiebreak.ind <- FALSE
  }
  
  # index of "-" for points
  if (!arg.completed) {
    dash.index <- gregexpr(pattern = "-", scores[length(scores)])[[1]]
  
    # get the number of points in the current game, depends
    p1.points <- substr(scores[length(scores)],1,dash.index-1)
    p2.points <- substr(scores[length(scores)],dash.index+1,nchar(scores[length(scores)]))
  
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
    
  }
  
  return(list("p1.sets" = p1.sets,
              "p2.sets" = p2.sets,
              "p1.games" = p1.games,
              "p2.games" = p2.games,
              "p1.points" = p1.points,
              "p2.points" = p2.points,
              "tiebreak.ind" = tiebreak.ind,
              "p1.total.games" = p1.total.games,
              "p2.total.games" = p2.total.games,
              "p1.total.tiebreaks" = p1.total.tiebreaks,
              "p2.total.tiebreaks" = p2.total.tiebreaks))
  
}
