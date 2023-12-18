snapshot <- function(arg_data,arg_date) {
  # check if arg.data is already a data table > convert if not
  x <- arg_data
  setDT(x)
  # filter tmp
  x <- x[from_date < as.Date(arg_date) & to_date > as.Date(arg_date),]
  return(x)
}

elo_outcome <- function(arg_elo1, arg_elo2) {
  x1 = arg_elo2 - arg_elo1
  x2 = x1/400
  x3 = 10^x2
  x4 = 1/(1+x3)
  return(x4)
}
