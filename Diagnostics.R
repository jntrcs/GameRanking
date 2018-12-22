#Diagnostics.R
chains<-list()
load("Chain1.Rdata")
chains[[1]]<-chain
load("Chain2.Rdata")
chains[[2]]<-chain
load("Chain3.Rdata")
chains[[3]]<-chain
load("Chain4.Rdata")
chains[[4]]<-chain


nburn<-5000
chains=lapply(chains, FUN=function(x) x[-(1:nburn),])

require(coda)

n.eff<- lapply(chains, effectiveSize)
add <- function(x) Reduce("+", x)
combined<-add(n.eff)

require(asbio)
?R.hat

rhats<-numeric(ncol(chains[[1]]))
for (i in 1:ncol(chains[[1]])){
  print(i)
  M=sapply(chains, FUN=function(x)x[,i])
  print(apply(M, 2, mean))
  rhats[i]<-R.hat(M, burn.in = 0)
}
combined=round(combined,0)
xtable::xtable(t(rbind(combined, rhats)), digits=c(0,0,3))

final<-chains[[1]]
for (i in 2:4){
  final<-rbind(final, chains[[i]])
}

save(final, file="FinalMatrix.RData")
