library(ggplot2)
library(quantmod)

source("functions/funktioner.R")
getSymbols("^OEX",src="yahoo",from="2010-01-04")
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
ahat<-1/n*log(OEXclose[[n]]/OEXclose[[1]])
bhat<-1/n*sum((logreturn-ahat)^2)*dt
sigmahat<-sqrt(bhat)/dt
muhat<-1/2*sigmahat^2+ahat/dt
# finder true sigma


#simulerer 10000 nye datasæt
n<- 10000

data<-Method2(mu=muhat,sigma=sigmahat,x0=OEXclose[[1]],n_sim=n,time_vector=seq(0,8,by=dt))

logreturn_ny<-matrix(NA,length(data[,1]),n)
for (k in 1:n){
  for (i in 2:length(data[,1])){
    logreturn_ny[i-1,k]<-log(data[i,k]/data[i-1,k])
    logreturn_ny[length(data[,1]),k]<-log(data[length(data[,1]),k]/data[length(data[,1])-1,k])
  }
  
}
sigma<-rep(NA,n)
mu<-rep(NA,n)
a<-rep(NA,n)
b<-rep(NA,n)


for (i in 1:n){
  a[i]<-1/length(data[,1])*log(data[1601,i]/data[1,i])
  b[i]<-1/length(data[,1])*sum((logreturn_ny[,i]-a[i])^2)*dt
  sigma[i]<-sqrt(b[i])/dt
  mu[i]<-1/2*sigma[i]^2+a[i]/dt
}


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

h<-hist(mu, breaks=45, xlab="mu", ylab = "Frekvens", 
        main="Histogram med tæthed") 
xfit<-seq(min(mu),max(mu),length=40) 
yfit<-dnorm(xfit,mean=mean(mu),sd=sd(mu)) 
yfit <- yfit*diff(h$mids[1:2])*length(mu) 
lines(xfit, yfit, col="blue", lwd=2)
abline(v=muhat, lwd=3)

# finder varians op mod rigtig varians

truevar_mu<-1/length(data[,1])*(sigmahat^2/dt+sigmahat^4/2)
truevar_mu
var(mu)

truevar_sigma<-1/length(data[,1])*sigmahat^2/2
truevar_sigma
var(sigma)
