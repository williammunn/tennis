snapshot <- function(arg.data,arg.date) {
  # check if arg.data is already a data table > convert if not
  x <- arg.data
  setDT(x)
  # filter tmp
  x <- x[from_date < as.Date(arg.date) & to_date > as.Date(arg.date),]
  return(x)
}

elo_outcome <- function(arg.elo1, arg.elo2) {
  x1 = arg.elo2 - arg.elo1
  x2 = x1/400
  x3 = 10^x2
  x4 = 1/(1+x3)
  return(x4)
}