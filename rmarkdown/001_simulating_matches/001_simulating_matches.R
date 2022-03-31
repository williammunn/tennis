require(ggplot2);require(scales);require(data.table)

source('/Users/williammunn/Documents/Github/tennis/functions/load_data.R')
source('/Users/williammunn/Documents/Github/tennis/functions/simulate_match.R')

# data for actual matches
rm(player.data,seedings.data,tourney.data,dir)
stats <- Data[,.(tourney_name,tourney_date,best_of,winner_name,loser_name,score,w_SvGms,w_bpSaved,w_bpFaced,l_SvGms,l_bpSaved,l_bpFaced,w_svpt,w_1stWon,w_2ndWon,l_svpt,l_1stWon,l_2ndWon)
              ][,`:=`(
                  w_sv = (w_1stWon + w_2ndWon)/w_svpt,
                  l_sv = (l_1stWon + l_2ndWon)/l_svpt)
                ][,`:=`(
                    w_rt = 1 - l_sv,
                    l_rt = 1 - w_sv)
                  ][,`:=`(
                    w_diff = w_sv - w_rt,
                    l_diff = l_sv - l_rt)
                    ][,.(tourney_name,tourney_date,best_of,winner_name,loser_name,score,w_SvGms,w_bpSaved,w_bpFaced,l_SvGms,l_bpSaved,l_bpFaced,w_sv,w_rt,w_diff,l_sv,l_rt,l_diff)]

# remove matches that ended in retirement or were walkovers, and matches without stats
stats <- stats[!(substr(score,nchar(score)-2,nchar(score))  %in% c('RET','W/O')),]
stats <- na.omit(stats)

# look at the extremes at both ends
bad_serving <- stats[year(tourney_date)>=2010,][order(w_diff)]
good_serving <- stats[year(tourney_date)>=2010,][order(-l_diff)]

# round percentages
bad_serving[,c("w_sv","w_rt","w_diff","l_sv","l_rt","l_diff")] <- lapply(bad_serving[,c("w_sv","w_rt","w_diff","l_sv","l_rt","l_diff")], function(x) percent(x,accuracy=2))
good_serving[,c("w_sv","w_rt","w_diff","l_sv","l_rt","l_diff")] <- lapply(good_serving[,c("w_sv","w_rt","w_diff","l_sv","l_rt","l_diff")], function(x) percent(x,accuracy=2))

# tour average
stats[year(tourney_date)>=2010,.(mean_sv = mean(w_sv),mean_rt = mean(w_rt))]

# show that play.point is eventually right
outcomes <- sapply(1:1000, function(x) play.game("P1",0.75)[[1]])
sum(outcomes == "P1")/length(outcomes)

# average number of points in a match since 2000
matchlength <- Data[!(substr(score,nchar(score)-2,nchar(score))  %in% c('RET','W/O')),][best_of == '3',]
median(na.omit(matchlength$w_svpt + matchlength$l_svpt))
mean(na.omit(matchlength$w_svpt + matchlength$l_svpt))

# count breaks of serve
matchdata <- play.match(3,"P1",0.8,0.8)[[2]]

# count number of games that went to deuce at least once
setDT(matchdata)[,`:=`(
  p1.points = cumsum(winner == 'P1'),
  p2.points = cumsum(winner == 'P2')),
  by = .(set_number,game_number)
  ][p1.points == 3 & p2.points == 3,.N, by = .(set_number,game_number)
    ][,.N]

# many simulations for two players that serve well
deuces <- sapply(1:1000, function(x) {
  result <- play.match(3,"P1",0.8,0.8)[[2]]
  setDT(result)
  return(
    result[,`:=`(
      p1.points = cumsum(winner == 'P1'),
      p2.points = cumsum(winner == 'P2')),
      by = .(set_number,game_number)
    ][p1.points == 3 & p2.points == 3,.N, by = .(set_number,game_number)
    ][,.N]
  ) }
)

mean(deuces)
# 3.21

# same thing for a game of two mediocre servers
deuces <- sapply(1:1000, function(x) {
  result <- play.match(3,"P1",0.5,0.5)[[2]]
  setDT(result)
  return(
    result[,`:=`(
      p1.points = cumsum(winner == 'P1'),
      p2.points = cumsum(winner == 'P2')),
      by = .(set_number,game_number)
    ][p1.points == 3 & p2.points == 3,.N, by = .(set_number,game_number)
    ][,.N]
  ) }
)

mean(deuces)
# 7.6

# john isner's % of points won on serve in 2019
stats[(winner_name == 'John Isner' | loser_name == 'John Isner'),
][,sv := ifelse(winner_name == 'John Isner',w_sv,l_sv)][,.(mean(sv),.N), by = year(tourney_date)]

# john isner's % of points won on return in 2019
stats[(winner_name == 'John Isner' | loser_name == 'John Isner'),
][,sv := ifelse(winner_name == 'John Isner',w_rt,l_rt)][,.(mean(sv),.N), by = year(tourney_date)]

# Isner's actual % of sv games won
stats[(winner_name == 'John Isner' | loser_name == 'John Isner') & year(tourney_date) == 2019,
][,`:=`(
  w_svgames_won = w_SvGms - w_bpFaced + w_bpSaved,
  l_svgames_won = l_SvGms - l_bpFaced + l_bpSaved)
  ][, `:=`(
  svgames_won = ifelse(winner_name == 'John Isner',w_svgames_won,l_svgames_won),
  svgames = ifelse(winner_name == 'John Isner', w_SvGms, l_SvGms))
  ][,lapply(.SD,sum),.SDcols=c("svgames","svgames_won")]
586/623

# impact on Isner's serve holding if he serves a little better, or a little worse
serve_pct <- rep(c(0.70,0.75,0.80),10000)
outcomes <- sapply(serve_pct,function(x) play.game("P1",x)[[1]])
data.table(points_pct = serve_pct, winner = outcomes)[,.(games_pct = sum(winner=="P1")/.N),by=points_pct]

# Isner's returning
serve_pct <- rep(c(0.65,0.70,0.75),1000)
outcomes <- sapply(serve_pct,function(x) play.game("P1",x)[[1]])
data.table(points_pct = serve_pct, winner = outcomes)[,.(games_pct = sum(winner=="P1")/.N),by=points_pct]

# theoretical % of matches won with 75% / 30% attributes
winners <- sapply(1:1000,function(x) {play.match(3,"P1",0.75,0.70)[[1]]})
sum(winners=="P1")/length(winners)
# 71.9%

# adjust strategy
winners <- sapply(1:1000,function(x) {play.match(3,"P1",0.70,0.65)[[1]]})
sum(winners=="P1")/length(winners)
# 73.3%

# Isner match win % in 2019
stats[winner_name == 'John Isner' | loser_name == 'John Isner'][, won := ifelse(winner_name == 'John Isner',1,0)
][year(tourney_date) == 2019,.(.N,pct = sum(won)/.N), by = .(best_of)]

# look at serve, return, and win stats for each player and season
rm(allplayers)
allplayers <- rbind(
  copy(stats)[,`:=`(
  serve = w_sv,
  return = w_rt,
  diff = w_diff,
  player_name = winner_name)]
  ,
  copy(stats)[,`:=`(
  serve = l_sv,
  return = l_rt,
  diff = l_diff,
  player_name = loser_name)])[,.(tourney_name,tourney_date,winner_name,loser_name,player_name,serve,return,diff)
                              ][,win := ifelse(player_name == winner_name,1,0)
                                ][,.(serve = mean(serve), return = mean(return), win_pct = sum(win)/.N,matches = .N), by = .(player_name,year(tourney_date))
                                ][,diff := serve - return][order(-win_pct)][matches >= 40,]

john <- allplayers[player_name == 'John Isner',]
