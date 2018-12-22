##AnalyzeResults.R
require(xlsx)
require(MCMCpack)
data<-read.xlsx2("Twilight Imperium Data.xlsx", 1)
data$Game<-as.numeric(as.character(data$Game))
data$Score<-as.numeric(as.character(data$Score))
data$ScoreTransformed<-0
data<-data[!is.na(data$Game),]
data$Player<-as.character(data$Player)
data$Faction<-as.character(data$Faction)
players<-sort(unique(data$Player))
factions<-sort(unique(data$Faction))
nplayers<-length(players)
nfactions<-length(factions)
ngames<-length(unique(data$Game))

load("FinalMatrix.RData")
#Produce player rankings
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

best<-table(apply(final[,1:8], 1,FUN=which.max ))/nrow(final)
names(best)<-players
justPlayers$ProbBest<-round(c(best),2)
justPlayers$GamesPlayed<-factor(table(data$Player))

#If they were to play an 8 player game, who would win?
averageFaction<-mean(final[,9:25])
winSim<-table(apply(final[,1:8], 1, FUN=function(thetas)which.max(rdirichlet(1, thetas+averageFaction))))
winSim<-winSim/nrow(final)
names(winSim)<-players
justPlayers$ProbWin<-c(winSim)

ggplot(justPlayers, aes( x=Unit))+geom_col(aes(y=StrengthMean, fill=GamesPlayed))+
  geom_errorbar(aes(ymin=Q25, ymax=Q75))+xlab("Player")+ylab("Strength")+
  scale_fill_discrete()+ggtitle("Player Strength Estimates with 50% Probability Intervals")+
  geom_text(aes(label=ProbBest, y= .15))+geom_label(aes(label="Probability of being the best:",
                                                       x=1.45, y=.5))+
  ggsave("PlayerStrength.jpg")

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
  ggtitle("Faction Strength Estimates with 50% Probability Intervals")+
  ggsave(filename = "FactionStrength.jpg")


x=seq(0, 1, length=500)
db<-dbeta(x, 4,2)
ggplot() + geom_density(adjust=1.5,aes(x=final[,26], fill="Posterior", color="Posterior"), size=2)+
  xlab("Lambda") + geom_line(size=2,aes(x,db, fill="Prior", color="Prior"))+ggtitle("Marginal of Lambda")+
  guides(fill=F,colour=guide_legend(title=""))+ggsave(file="LambdaPosterior.jpg")

best.factions<-table(apply(final[,9:25], 1,FUN=which.max ))/nrow(final)
names(best.factions)<-factions
best.factions

#If they were to play an 8 player game, who would win?
averageFaction<-mean(final[,9:25])
winSim<-table(apply(final[,1:8], 1, FUN=function(thetas)which.max(rdirichlet(1, thetas+averageFaction))))
winSim<-winSim/nrow(final)
names(winSim)<-players
winSim


###Create a visual matrix of the probability that person A is better than B

pmat=matrix(0, nrow=8, ncol=8)
for (i in 1:7){
  for(j in (i+1):8){
    pmat[i,j]<-mean(final[,i]>final[,j])
  }
}
pmat=round(pmat, 2)
colnames(pmat)<-players
rownames(pmat)<-players
xtable::xtable(pmat)

##Create a p-value test that tests whether the variance of the scores in the games is typical of what 
#we would see from our posterior predictive distribution

#2nd question, is the winner scoring what we would expect to see under our model

test.stat<-sum(sapply(1:9, FUN=function(i)var(data$ScoreProp[data$Game==i])))
winner.stat<-sum(sapply(1:9, FUN=function(i) max(data$ScoreProp[data$Game==i])))
dist<-numeric(nrow(final))
winner.dist<-numeric(nrow(final))
copy<-data
for (i in 1:nrow(final)){
  copy$PlayerStrength<-final[i, match(copy$Player, players)]
  copy$FactionStrength<-final[i, nplayers+match(copy$Faction, factions)]
  dist[i]<-sum(sapply(1:9, FUN=function(j) var(c(rdirichlet(1, copy$PlayerStrength[copy$Game==j]+copy$FactionStrength[copy$Game==j])))))
  winner.dist[i]<- sum(sapply(1:9, FUN=function(j) max(c(rdirichlet(1,copy$PlayerStrength[copy$Game==j]+copy$FactionStrength[copy$Game==j])))))
}

mean(dist>test.stat)
mean(winner.dist>winner.stat)
hist(dist, main="Testing whether we are seeing the correct variances between the scores", 
     xlab="Sum of the variances of the 9 games")
abline(v=test.stat, col="red", lwd=3)
hist(winner.dist, main="Testing how much the winner is dominating", xlab="Sum of the 9 winners scores")
abline(v=winner.stat, col="red", lwd=3)

playerProp<-rexp(nrow(final), 1/(3*final[,26]))
plot(density(playerProp))
abline(v=justPlayers$StrengthMean)


playerMax<-sapply(final[,26], FUN=function(lambda) max(rexp(8, lambda)))
hist(playerMax)
abline(v=max(justPlayers$StrengthMean))
playerMin<-sapply(final[,26], FUN=function(lambda) min(rexp(8, lambda)))
hist(playerMin)
abline(v=min(justPlayers$StrengthMean))
mean(min(justPlayers$StrengthMean)<playerMin)


#Orthogonal correlations: After accounting for the value of lambda, what are the correlations between players:
regMat<-apply(final[-1,1:8], 2, FUN=function(player)pexp(player, 1/(3*final[-2,26])))
head(regMat)
cor(regMat)
require(corrplot)
corrplot(cor(regMat[,1:8]))


#Sanity check! An average player should beat one of my 8 observed
#about half the time. Does that actually happen?

winRates<-rep(0, 8)
for (i in 1:8){
  lambdas<-sample(final[,26],10000, replace=T)
  fac_strength<-rexp(10000,1/(3*(1-lambdas)))
  strengths<- cbind(final[sample(1:180000, replace=T, 10000),i]+fac_strength,
                    rexp(10000, 1/(3*lambdas))+fac_strength)
  a=t(apply(strengths, 1, function(s)rdirichlet(1, s)))
  winRates[i]<-mean(a[,1]>a[,2])
}
winRates

