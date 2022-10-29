snapshot <- function(arg.data,arg.date) {
  # check if arg.data is already a data table > convert if not
  x <- arg.data
  setDT(x)
  # filter tmp
  x <- x[from_date < as.Date(arg.date) & to_date > as.Date(arg.date),]
  return(x)
}