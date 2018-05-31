library(ggplot2)
library(quantmod)

getSymbols("^OEX",src="yahoo")
OEXclose<-OEX$OEX.Close


#finder logreturn
logreturn<-c(NA,length(OEXclose))
for (i in 2:length(OEXclose)){
  logreturn[i-1]<-log(OEXclose[[i]]/OEXclose[[i-1]])
}
tail(logreturn)

#finder true muhat
n<-length(logreturn)
T<-n/250
muhat<-log(OEXclose[[length(OEXclose)]]/OEXclose[[1]])/T+(1/T*sum(logreturn^2))/2
# finder true sigma
sigmahat<-sqrt(1/T*sum(logreturn^2))

#simulerer 10000 nye datasæt
n<- 10000
sigma<-c(NA,n)
mu<-c(NA,n)
data<-Method2(mu=muhat,sigma=sigmahat,x0=OEXclose[[1]],n_sim=n,time_vector=seq(0,1,by=1/50))

logreturn_ny<-matrix(NA,50,n)
for (k in 1:10000){
  for (i in 2:51){
    logreturn_ny[i-1]<-log(data[i,k]/data[i-1,k])
  }
  
}

logreturn_ny[50,2]
dim(logreturn_ny)
for (i in 1:n){
  
  mu[i]<-log(data[51,i]/data[1,i])/T+(1/T*sum(logreturn_ny[,i]^2))/2
  sigma[i]<-sqrt(1/T*sum(logreturn_ny[,i]^2))
}

tail(logreturn_ny[1,900])
#histogram af sigma
hist(sigma)
qplot(sigma, type="hist", fill=I("blue"))+geom_density()+geom_vline(xintercept=sigmahat,col="green", size=1)
var(sigma)
mean(sigma)
abs(sigmahat^2/2-var(sigma))

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
