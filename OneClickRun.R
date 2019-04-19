require(xlsx)
require(MCMCpack)
data<-read.xlsx2("Twilight Imperium Data.xlsx", 1)
data$Game<-as.numeric(as.character(data$Game))
data$Score<-as.numeric(as.character(data$Score))
data$Score[data$Score==0]=.1
data$ScoreTransformed<-0
data<-data[!is.na(data$Game),]
data$Player<-as.character(data$Player)
data$Faction<-as.character(data$Faction)
players<-sort(unique(data$Player))
factions<-sort(unique(data$Faction))
nplayers<-length(players)
nfactions<-length(factions)
ngames<-length(unique(data$Game))

#Our current point scoring system is you get a point for every point + a point for every person you beat
for (g in unique(data$Game)){
  game<-data[data$Game==g,]
  for (i in 1:nrow(game)){
    game$ScoreTransformed[i]<-game$Score[i]+sum(game$Score[i]>game$Score)
  }
  for(i in 1:nrow(game)){
    game$scoreProp[i]<-game$ScoreTransformed[i]/sum(game$ScoreTransformed)
  }
  data$ScoreTransformed[data$Game==g]<-game$ScoreTransformed
  data$ScoreProp[data$Game==g]<-game$scoreProp
}

playerScores<-list()
for (p in players){
  playerScores[[p]]<-data$ScoreProp[data$Player==p]
}

factionScores<-list()
for (f in factions){
  factionScores[[f]]<-data$ScoreProp[data$Faction==f]
}

#Makes a list entry for every player and faction, with entries for what games they appeared in and 
#what other players and factions appeared in the game
gameList<-list()
for (p in players){
  gameList[[p]]<-list()
  games <- data$Game[data$Player==p]
  for (g in games){
    g<-as.character(g)
    gameList[[p]][[g]]<-list()
    gameList[[p]][[g]]$players<-match(data$Player[data$Game==g], players)
    gameList[[p]][[g]]$factions<-match(data$Faction[data$Game==g], factions)
    
  }
}
for (f in factions){
  gameList[[f]]<-list()
  games <- data$Game[data$Faction==f]
  for (g in games){
    g<-as.character(g)
    gameList[[f]][[g]]<-list()
    gameList[[f]][[g]]$players<-match(data$Player[data$Game==g], players)
    gameList[[f]][[g]]$factions<-match(data$Faction[data$Game==g], factions)
    
  }
}

#Gs are the log game scores
log_complete_cond_p<-function(lambda, Ss, lgamSumAll, lgamSumMy, theta){
  lgamSumAll - lgamSumMy  +theta*(sum(log(Ss))-1/(3*lambda))
}

log_complete_cond_f<-function(lambda, Ss, lgamSumAll, lgamSumMy, theta){
  lgamSumAll - lgamSumMy  +theta*(sum(log(Ss))-1/(3*(1-lambda)))
}

complete_cond_l<-function(nplayers, nfactions, sumThetaPs, sumThetaFs, lambda){
  lambda^(3-nplayers)*(1-lambda)^(1-nfactions)*exp(-1/(3*lambda)*sumThetaPs-1/(3*(1-lambda))*sumThetaFs) 
}

log_complete_cond_l<-function(nplayers, nfactions, sumThetaPs, sumThetaFs, lambda){
  (3-nplayers)*log(lambda)+(1-nfactions)*log(1-lambda)-1/(3*lambda)*sumThetaPs-1/(3*(1-lambda))*sumThetaFs
}

theta=6
scores<-c(theta, rep(2, 7), rep(1,18))
lgamall=lgamma(sum(scores[1:4]+scores[9:12]))
lgamSumMy<-lgamma(theta+1)
log_complete_cond_p(.7, .8, lgamall, lgamSumMy, theta)

iters=50000
chain<-matrix(0, nrow=iters, ncol=nfactions+nplayers+1)
colnames(chain)<-c(as.character(players), as.character(factions), "lambda")

chain[1,1:nplayers]<-rexp(nplayers, 1/(3*2/3))
chain[1,(nplayers+1):(nplayers+nfactions)]<-rexp(nfactions, 1/(3*1/3))
chain[1, nfactions+nplayers+1]<-rbeta(1,1,1)

accept_p=0
accept_f=0
accept_lambda<-0
for (i in 2:nrow(chain)){
  lastRow<-chain[i-1,]
  newRow<-lastRow
  for (j in 1:nplayers){
    prop<-lastRow[j]+rnorm(1,0, 3)
    if (prop<0) next #if the proposal is less than 0 there's no reason to compute, just skip
    lgam<-sum(sapply(gameList[[players[j]]], FUN = 
                       function(x)lgamma(sum(newRow[x$players]+newRow[x$factions+nplayers]))))
    tempRow<-newRow
    tempRow[j]<-prop
    lgam_prop<-sum(sapply(gameList[[players[j]]], FUN =
                            function(x)lgamma(sum(tempRow[x$players]+tempRow[x$factions+nplayers]))))
    
    factionStrengths<-newRow[nplayers+match(data$Faction[data$Player==players[j]], factions)]
    single_lgam_prop<-sum(lgamma(prop+factionStrengths))
    single_lgam<-sum(lgamma(lastRow[j]+factionStrengths))
    prop_dens<-log_complete_cond_p(lastRow[length(lastRow)], playerScores[[players[j]]], 
                                   lgam_prop, single_lgam_prop, prop)
    prev_dens<-log_complete_cond_p(lastRow[length(lastRow)], playerScores[[players[j]]], lgam,
                                   single_lgam, lastRow[j])
    
    if (log(runif(1))<prop_dens-prev_dens){
      newRow[j]<-prop
      accept_p=accept_p+1
    }
  }
  for (j in (nplayers+1):(nplayers+nfactions)){
    prop<-lastRow[j]+rnorm(1,0, 2)
    if (prop<0) next #just skip because prop should be rejected
    lgam<-sum(sapply(gameList[[factions[j-nplayers]]], FUN = 
                       function(x)lgamma(sum(newRow[x$players]+newRow[x$factions+nplayers]))))
    tempRow<-newRow
    tempRow[j]<-prop
    lgam_prop<-sum(sapply(gameList[[factions[j-nplayers]]], FUN =
                            function(x)lgamma(sum(tempRow[x$players]+tempRow[x$factions+nplayers]))))
    playerStrengths<-newRow[match(data$Player[data$Faction==factions[j-nplayers]], players)]
    single_lgam_prop<-sum(lgamma(prop+playerStrengths))
    single_lgam<-sum(lgamma(lastRow[j]+playerStrengths))
    prop_dens<-log_complete_cond_f(lastRow[length(lastRow)], factionScores[[factions[j-nplayers]]], 
                                   lgam_prop, single_lgam_prop, prop)
    prev_dens<-log_complete_cond_f(lastRow[length(lastRow)], factionScores[[factions[j-nplayers]]],
                                   lgam, single_lgam, lastRow[j])
    
    if (log(runif(1))<prop_dens-prev_dens){
      newRow[j]<-prop
      accept_f=accept_f+1
    }  
  }
  
  prop<-lastRow[length(lastRow)]+rnorm(1,0, .25)
  if (prop<1 & prop>0){
    prop_dens<-log_complete_cond_l(nplayers, nfactions, sum(newRow[1:nplayers]),
                                   sum(newRow[(nplayers+1):(nplayers+nfactions)]), prop)
    prev_dens<-log_complete_cond_l(nplayers, nfactions, sum(newRow[1:nplayers]),
                                   sum(newRow[(nplayers+1):(nplayers+nfactions)]), lastRow[length(lastRow)])
    if (log(runif(1))< prop_dens- prev_dens){
      accept_lambda=accept_lambda+1
      newRow[length(newRow)]<-prop
    }
  }
  
  
  chain[i,]<-newRow
}
accept_p/(nplayers)/iters
accept_f/(nfactions)/iters
accept_lambda/iters

nburn<-5000
chain=chain[-(1:nburn),]

require(coda)

n.eff<- effectiveSize(chain)
n.eff

final<-chain
stat.df<-data.frame(Unit=c(players, factions))
stat.df$Type<-c(rep("Player", 8), rep("Faction", 17))

stat.df$StrengthMean<-apply(final[,-26], 2, mean)
stat.df$Q25<-apply(final[,-26], 2, quantile, .25)
stat.df$Q75<-apply(final[,-26], 2, quantile, .75)
stat.df$Q25<-apply(final[,-26], 2, quantile, .25)
stat.df$Q75<-apply(final[,-26], 2, quantile, .75)
stat.df$Variance<-apply(final[,-26], 2, var)



justPlayers<-stat.df[stat.df$Type=="Player",]
justPlayers$Unit<-factor(justPlayers$Unit, levels=players[order(-justPlayers$Q25)])

best<-table(factor(apply(final[,1:8], 1,FUN=which.max ), levels=1:8))/nrow(final)
names(best)<-players
justPlayers$ProbBest<-round(c(best),2)
justPlayers$GamesPlayed<-factor(table(data$Player))

ggplot(justPlayers, aes( x=Unit))+geom_col(aes(y=StrengthMean, fill=GamesPlayed))+
  geom_errorbar(aes(ymin=Q25, ymax=Q75))+xlab("Player")+ylab("Strength")+
  scale_fill_discrete()+ggtitle("Player Strength Estimates with 50% Probability Intervals")+
  geom_text(aes(label=ProbBest, y= .15))+geom_label(aes(label="Probability of being the best:",
                                                        x=1.45, y=.5))

justFactions<-stat.df[stat.df$Type=="Faction",]
justFactions$Unit<-factor(justFactions$Unit, levels=factions[order(-justFactions$Q25)])
best<-table(apply(final[,9:25], 1,FUN=which.max ))/nrow(final)
names(best)<-factions
justFactions$ProbBest<-round(c(best),2)
justFactions$GamesPlayed<-factor(table(data$Faction))
ggplot(justFactions, aes( x=Unit))+geom_col(aes(y=StrengthMean, fill=GamesPlayed))+
  geom_errorbar(aes(ymin=Q25, ymax=Q75))+xlab("Faction")+ylab("Strength")+
  geom_text(aes(label=ProbBest, y= .05))+
  geom_label(aes(label="Probability of being the best:",x=2.8, y=.15))+
  ggtitle("Faction Strength Estimates with 50% Probability Intervals")

x=seq(0, 1, length=500)
db<-dbeta(x, 4,2)
ggplot() + geom_density(adjust=1.5,aes(x=final[,26], fill="Posterior", color="Posterior"), size=2)+
  xlab("Lambda") + geom_line(size=2,aes(x,db, color="Prior"))+ggtitle("Marginal of Lambda")+
  guides(fill=F,colour=guide_legend(title=""))

pmat=matrix(0, nrow=8, ncol=8)
for (i in 1:7){
  for(j in (i+1):8){
    pmat[i,j]<-mean(final[,i]>final[,j])
  }
}
pmat=round(pmat, 2)
colnames(pmat)<-players
rownames(pmat)<-players
pmat
