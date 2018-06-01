library(ggplot2)
library(quantmod)

source("functions/funktioner.R")
getSymbols("^OEX",src="yahoo",from="2010-06-01")
OEXclose<-OEX$OEX.Close
length(OEXclose)/8

#finder logreturn
logreturn<-c(NA,length(OEXclose))
for (i in 2:length(OEXclose)){
  logreturn[i-1]<-log(OEXclose[[i]]/OEXclose[[i-1]])
}

dt<-1/200

#finder true muhat
n<-length(logreturn)
T<-n/250
ahat<-1/n*sum(logreturn)
bhat<-1/n*sum((logreturn-ahat)^2)*dt
sigmahat<-sqrt(bhat*dt)
muhat<-1/2*sigmahat^2+ahat/dt
# finder true sigma


#simulerer 10000 nye datasæt
n<- 10000
sigma<-c(NA,n)
mu<-c(NA,n)
data<-Method2(mu=muhat,sigma=sigmahat,x0=OEXclose[[1]],n_sim=n,time_vector=seq(0,1,by=1/251))

logreturn_ny<-matrix(NA,252,n)
for (k in 1:10000){
  for (i in 2:252){
    logreturn_ny[i-1,k]<-log(data[i,k]/data[i-1,k])
    logreturn_ny[252,k]<-log(data[252,k]/data[251,k])
  }
  
}

logreturn_ny[2009,2]
dim(logreturn_ny)
for (i in 1:n){
  
  mu[i]<-log(data[252,i]/data[1,i])/(n*dt)+(1/(n*dt)*sum(logreturn_ny[,i]^2))/2
  sigma[i]<-sqrt(1/(n*dt)*sum(logreturn_ny[,i]^2))
}

log(data[51,1]/data[1,1])/T+1/(T*2)*sum(logreturn_ny[,1]^2)
#histogram af sigma
hist(sigma)
qplot(sigma, type="hist", fill=I("blue"))+geom_density()+geom_vline(xintercept=sigmahat,col="green", size=1)
var(sigma)
mean(sigma)
abs(sigmahat^2/(2*10000)-var(sigma))

qqnorm(sigma)
qqline(sigma)


x <- sigma
h<-hist(x, breaks=50, xlab="Sigma", ylab = "Frekvens", 
        main="Histogram med tæthed") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)
abline(v=sigmahat, lwd=3)
#histogram af mu
hist(mu)
qplot(mu, type="hist", fill=I("blue"))+stat_function(fun = dnorm, colour = "red",
                                                     arg = list(mean = mean(mu, na.rm = TRUE),
                                                                sd = sd(mu, na.rm = TRUE)))+geom_vline(xintercept=muhat,col="green", size=1)

qqnorm(mu)
qqline(mu)

mean(mu)
mu


h<-hist(mu, breaks=50, xlab="Sigma", ylab = "Frekvens", 
        main="Histogram med tæthed") 
xfit<-seq(min(mu,na.rm=T),max(mu,na.rm=T),length=40) 
yfit<-dnorm(xfit,mean=mean(mu,na.rm=T),sd=sd(mu,na.rm=T)) 
yfit <- yfit*diff(h$mids[1:2])*length(mu==NaN) 
lines(xfit, yfit, col="blue", lwd=2)
abline(v=muhat, lwd=3)

