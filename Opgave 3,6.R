library(quantmod)
library(ggplot2)
library(reshape2)
require(gridExtra)
#henter data
getSymbols("^OEX",src="yahoo")

OEXclose<-OEX$OEX.Close

#finder logreturn
logreturn<-c(NA,length(OEXclose))
for (i in 2:length(OEXclose)){
  logreturn[i-1]<-log(OEXclose[[i]]/OEXclose[[i-1]])
}


hist(logreturn, breaks=30)
x <- logreturn 
h<-hist(x, breaks=100, col="red", xlab="Log-afkast", ylab = "Frekvens", 
        main="Histogram med tæthed") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)


qqnorm(logreturn);qqline(logreturn)



