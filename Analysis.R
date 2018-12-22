require(xlsx)
require(MCMCpack)
data<-read.xlsx2("Twilight Imperium Data.xlsx", 1)
data$Game<-as.numeric(as.character(data$Game))
data$Score<-as.numeric(as.character(data$Score))
data$ScoreTransformed<-0
data<-data[!is.na(data$Game),]
players<-sort(unique(data$Player))
factions<-sort(unique(data$Faction))

#Our current point scoring system is you get a point for every point + a point for every person you beat
for (g in unique(data$Game)){
  game<-data[data$Game==g,]
  for (i in 1:nrow(game)){
    game$ScoreTransformed[i]<-game$Score[i]+sum(game$Score[i]>game$Score)
  }
  data$ScoreTransformed[data$Game==g]<-game$ScoreTransformed
}


player_prior<-function(thetas, lambda){
  prod(dexp(thetas, 1/(3*lambda)))
}

player_log_prior<-function(thetas, lambda){
  sum(dexp(thetas, 1/(3*lambda), log = T))
}

faction_prior<-function(thetas, lambda){
  prod(dexp(thetas, 1/(3*(1-lambda))))
}

faction_log_prior<-function(thetas, lambda){
  sum(dexp(thetas, 1/(3*(1-lambda)), log = T))
}

lambda_prior<-function(lambda){
  dbeta(lambda, 4,2)
}

lambda_log_prior<-function(lambda){
  dbeta(lambda, 4,2, log = T)
}

game_likelihood<-function(player_strengths, faction_strengths, scores){
  ddirichlet(scores/sum(scores), player_strengths+faction_strengths)
}

game_log_likelihood<-function(player_strengths, faction_strengths, scores){
  log(ddirichlet(scores/sum(scores), player_strengths+faction_strengths))
}

player_scores<-rep(2, length(players))
names(player_scores)<-players
faction_scores<-rep(1,length(factions))
names(faction_scores)<-factions

likelihood<-function(all_player_strengths, all_factions_strengths, data){
  
}

log_likelihood<-function(all_player_strengths, all_factions_strengths, data){
  
}

log_post