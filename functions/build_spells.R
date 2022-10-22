rm(list=ls())
library(data.table)
# function to convert point-in-time data
# to spell-based data
# to be used for creating rankings
build_spells <- function(
    arg.data, # input data to be converted
    arg.keyvar, # id variable
    arg.datevar # the name of the date variable
) {
  # convert to data table
  temp <- setDT(arg.data)
  # sort by id and date
  temp[order(substitute(arg.datevar))]
  # output
  return(temp)
}

testdata <- data.frame(
  id = c(1,1,1),
  id2 = c("x","x","x"),
  elo_date = c(as.Date("2021-01-01"),as.Date("2019-02-01"),as.Date("2019-03-01")),
  val = c(1500,1800,2100)
)

build_spells(arg.data = testdata,arg.keyvar = id,arg.datevar = 'elo_date')