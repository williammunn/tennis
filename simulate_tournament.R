rm(list=ls())
library(lubridate,dplyr,data.table)
setwd("/Users/vickimunn/Desktop/R stuff/github")

# load tennis data, remove what we don't need
source("load_data.R")
lapply(list(match.data,player.data,seedings.data,tourney.data),setDT)

# 2019 Australian Open has tourney_id = 2019-580
setkey(tourney.data,tourney_id)
setkey(seedings.data,tourney_id,player_id)
setkey(player.data,player_id)

x <- tourney.data[seedings.data][tourney_id=="2019-580",.(player_id,seed)]
setkey(x,player_id)
y <- player.data[x][order(seed)]