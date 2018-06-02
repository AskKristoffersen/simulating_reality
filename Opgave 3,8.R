library(quantmod)
library(ggplot2)
library(reshape2)


#Henter S&P 100
getSymbols("^OEX",src="yahoo")
OEXclose<-OEX$OEX.Close
# Plot af closing prices
plot(1:dim(OEXclose)[1], OEXclose, type = "l", xlab = "Observations")
#3.8

#finder logreturn
logreturn<-c(NA,length(OEXclose))
for (i in 2:length(OEXclose)){
  logreturn[i-1]<-log(OEXclose[[i]]/OEXclose[[i-1]])
}

#finder teoretiske muhat og sigmahat
n<-length(logreturn)
T<-n/250

#Finder muhat og sigmahat
ahat<-1/n*log(OEXclose[[n]]/OEXclose[[1]])
bhat<-1/n*sum((logreturn-ahat)^2)*dt
sigmahat<-sqrt(bhat)/dt
muhat<-1/2*sigmahat^2+ahat/dt
#optimerer med loglikelihood
si<-c(NA,length(OEXclose)-1)
si1<-c(NA,length(OEXclose)-1)

for (i in 1:(length(OEXclose)-1)){
  si[i]<-OEXclose[[i+1]]
  si1[i]<-OEXclose[[i]]
}


dt<-1/250
n<-length(OEXclose)


loglike <- function(pars, si. = si, si1. = si1) {
  mu <- pars[1]
  sigma2 <- pars[2]^2
  val.log.like <- n/2*log(2*pi)+n/2*log(sigma2*dt)+1/(2*sigma2*dt)*sum((log(si./si1.)-(mu - 1/2*sigma2)*dt)^2)
  return(val.log.like)
}


opti<-optim(c(0.1,0.5),loglike)

muhat_loglike<-opti$par[1]
sigmahat_loglike<-opti$par[2]

muhat
muhat_loglike

sigmahat
sigmahat_loglike


#Fra 2010-2018
getSymbols("^OEX",src="yahoo", from="2010-01-04")

OEXclose<-OEX$OEX.Close

# Plot af closing prices
plot(1:dim(OEXclose)[1], OEXclose, type = "l", xlab = "Observations")

logreturn_2010<-c(NA,length(OEXclose))
for (i in 2:length(OEXclose)){
  logreturn_2010[i-1]<-log(OEXclose[[i]]/OEXclose[[i-1]])
}

#finder teoretiske muhat og sigmahat
n<-length(logreturn_2010)
T<-n/250

#Finder muhat og sigmahat for 2010 og frem
ahat_2010<-1/n*log(OEXclose[[n]]/OEXclose[[1]])
bhat_2010<-1/n*sum((logreturn_2010-ahat_2010)^2)*dt
sigmahat_2010<-sqrt(bhat_2010)/dt
muhat_2010<-1/2*sigmahat_2010^2+ahat_2010/dt

#optimerer med loglikelihood
si<-c(NA,length(OEXclose)-1)
si1<-c(NA,length(OEXclose)-1)

for (i in 1:(length(OEXclose)-1)){
  si[i]<-OEXclose[[i+1]]
  si1[i]<-OEXclose[[i]]
}


dt<-1/250
n<-length(OEXclose)


loglike <- function(pars, si. = si, si1. = si1) {
  mu <- pars[1]
  sigma2 <- pars[2]^2
  val.log.like <- n/2*log(2*pi)+n/2*log(sigma2*dt)+1/(2*sigma2*dt)*sum((log(si./si1.)-(mu - 1/2*sigma2)*dt)^2)
  return(val.log.like)
}


opti_2010<-optim(c(0.1,0.5),loglike)

muhat_loglike_2010<-opti_2010$par[1]
sigmahat_loglike_2010<-opti_2010$par[2]

muhat_2010
muhat_loglike_2010

sigmahat_2010
sigmahat_loglike_2010

#Forskel p? dem alle

muhat
muhat_loglike

muhat_2010
muhat_loglike_2010

sigmahat
sigmahat_loglike

sigmahat_2010
sigmahat_loglike_2010
