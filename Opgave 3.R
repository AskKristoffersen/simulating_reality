library(quantmod)
library(ggplot2)

library(reshape2)
#Henter S&P 100
getSymbols("^OEX",src="yahoo")
OEX
head(OEX)
tail(OEX)
qplot(OEX[,4])#Histogram OEX close


# Plot af closing prices
plot(1:dim(OEXclose)[1], OEXclose, type = "l", xlab = "Observations")
#3.8
OEXclose<-OEX$OEX.Close


logreturn<-c(NA,length(OEXclose))
si<-c(NA,length(OEXclose))
si1<-c(NA,length(OEXclose))
for (i in 2:length(OEXclose)){
  logreturn[i-1]<-log(OEXclose[[i]]/OEXclose[[i-1]])
  si[i]<-OEXclose[[i]]
  si1[i]<-OEXclose[[i-1]]
}

n<-length(logreturn)
T<-n/252
sum(logreturn)
muhat<-log(OEXclose[[length(OEXclose)]]/OEXclose[[1]])/T+(1/T*sum(logreturn^2))/2
muhat
mean(logreturn)
sigmahat<-sqrt(1/11*sum(logreturn^2))
sigmahat



u<-c(NA,length(OEXclose))
si<-c(NA,length(OEXclose))
sit<-c(NA,length(OEXclose))

for (i in 2:length(OEX[,4])) {
  u[i-1] <- log(OEX[[i,4]]/OEX[[i-1,4]])
  si[i] <- OEX[[i-1,4]]
  sit[i] <- OEX[[i,4]]
}

for (i in 2:length(OEXclose)){
  logreturn[i-1]<-log(OEXclose[[i]]/OEXclose[[i-1]])
  si[i]<-OEXclose[[i]]
  si1[i]<-OEXclose[[i-1]]
}

dt<-1/251
n<-length(OEXclose)

loglike <- function(pars, si. = si, sit. = sit) {
  mu <- pars[1]
  sigma2 <- pars[2]^2
  val.log.like <- n/2*log(2*pi) + n/2*log(sigma2*dt) + 1/(2*sigma2*dt) * sum((log(sit./si.)-(mu-sigma2/2)*dt)^2)
  return(val.log.like)
}
opti <- optim( c(0.1, 0.5), loglike)
pti
opti <- optim( c(0.1, 0.5),loglike)

optim(c(0,1,0,5),loglike)
?optim
loglike(c(-15,-10))

