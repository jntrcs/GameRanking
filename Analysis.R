require(xlsx)
data<-read.xlsx2("Twilight Imperium Data.xlsx", 1)
data$Game<-as.numeric(as.character(data$Game))
data$Score<-as.numeric(as.character(data$Score))
data<-data[data$Real==1,]
data$ScoreTransformed<-0
#Our current point scoring system is you get a point for every point + a point for every person you beat
for (g in unique(data$Game)){
  game<-data[data$Game==g,]
  for (i in 1:nrow(game)){
    game$ScoreTransformed[i]<-game$Score[i]+sum(game$Score[i]>game$Score)
  }
  data$ScoreTransformed[data$Game==g]<-game$ScoreTransformed
}



require(R2jags)

mdl<-"model{
for (i in 1:nGames){
total[i]<-sum(strength[player.mat[i,1:numPlayers[i]]])+sum(fstrength[faction.mat[i,1:numPlayers[i]]])
}
for(i in 1:nGames){
for (j in 1:max(numPlayers)){
probs[i,j]<-ifelse(j > numPlayers[i], 0, (strength[player.mat[i,j]]+fstrength[faction.mat[i,j]])/total[i])
}
game[i,1:numPlayers[i]] ~ dmulti(probs[i,1:numPlayers[i]], totalPoints[i])
}
for (i in 1:nPlayers){
strength[i]~dexp(1/(3*lambda))
}
for (i in 1:nFactions){
fstrength[i]~dexp(1/(3*(1-lambda)))
}
lambda~dbeta(alpha, beta)
alpha~dgamma(10,1/2)
beta~dgamma(10/4, 1/4)
}
"

nGames<-length(unique(data$Game))
numPlayers<-aggregate(data$Score~data$Game, FUN=length)[,2]

players<-sort(unique(data$Player))
factions<-sort(unique(data$Faction))
nFactions<-length(factions)
nPlayers<-length(players)
totalPoints<-aggregate(data$ScoreTransformed~data$Game, FUN = sum)[,2]
#players.num<-as.numeric(players)
#factions.num<-as.numeric(factions)

game<-matrix(1, nrow=nGames, ncol=max(numPlayers))
player.mat<-matrix(1, nrow=nGames, ncol=max(numPlayers))
faction.mat<-matrix(1, nrow=nGames, ncol=max(numPlayers))

for (i in 1:nGames){
  game[i, 1:numPlayers[i]]<-data$ScoreTransformed[data$Game==i]
  player.mat[i, 1:numPlayers[i]]<-match(data$Player[data$Game==i], players)
  faction.mat[i, 1:numPlayers[i]]<-match(data$Faction[data$Game==i], factions)
}

parms=c("strength", "fstrength", "lambda", "alpha", "beta")
data.jags=c("nGames", "numPlayers", "game", "faction.mat", "player.mat", "nFactions", "nPlayers", "totalPoints")

writeLines(mdl, 'gameRank.jags')
jags.rank<-jags(data=data.jags, parameters.to.save = parms, inits=NULL, model.file = 'gameRank.jags',
                n.burnin = 5000, n.iter=55000, n.chains=2, n.thin = 1)
jags.rank
mcmc<-as.mcmc(jags.rank)
chains<-as.matrix(mcmc)

score<-apply(chains, 2, mean)
lowerQuantile<-apply(chains,2,quantile,.025)
upperQuantile<-apply(chains,2,quantile,.975)

fac.indices<-which(substr(names(score), 1,1)=="f")
fac.order<-sapply(regmatches(names(score)[fac.indices], gregexpr("[[:digit:]]+", names(score)[fac.indices])),as.numeric)
player.indices<-which(substr(names(score), 1,1)=="s")
player.order<-sapply(regmatches(names(score)[player.indices], gregexpr("[[:digit:]]+", names(score)[player.indices])),as.numeric)


Faction.Rank<-data.frame(Faction = factions[fac.order], Strength= score[fac.indices], LowerBound=lowerQuantile[fac.indices],
                         upperBound=upperQuantile[fac.indices])
Player.Rank<-data.frame(Player = players, Strength = score[player.indices], LowerBound=lowerQuantile[player.indices],
                        UpperBound=upperQuantile[player.indices])
Faction.Rank<-Faction.Rank[order(-Faction.Rank$Strength),]
Player.Rank<-Player.Rank[order(-Player.Rank$Strength),]
Faction.Rank
Player.Rank

lambda<-chains[,which(colnames(chains)=="lambda")]
hist(lambda)
