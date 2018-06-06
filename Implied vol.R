library(ggplot2)
library(quantmod)
library(pracma)
source("functions/funktioner.R")
# funktion til udregning af implied volatilitet
implied.vol <-
  function(s0, K, T, r, price){
    sigma <- 0.20
    sigma.up <- 10
    sigma.down <- 0.001
    count <- 0
    err <- Black_Scholes_callprice(T,s0,K,r,sigma,q=0) - price
    while(abs(err) > 0.00001 && count<1000){
      if(err < 0){
        sigma.down <- sigma
        sigma <- (sigma.up + sigma)/2
      }else{
        sigma.up <- sigma
        sigma <- (sigma.down + sigma)/2
      }
      err <- Black_Scholes_callprice(T,s0,K,r,sigma,q=0) - price
      count <- count + 1
    }

    if(count==1000){
      return(NA)
    }else{
      return(sigma)
    }
  }


implied_vol(s0=s0,K=strike,T=T_jun19,r=muhat,price=callprices)
getSymbols("^GSPC",src="yahoo",from="2017-12-10")
SP500<-GSPC$GSPC.Close
logreturn<-c(NA,length(SP500))
for(i in 2:length(SP500)){
  logreturn[i-1]<-log(SP500[[i]]/SP500[[i-1]])
}

n<-length(SP500)
dt<-1/250
ahat<-1/n*log(SP500[[length(SP500)]]/SP500[[1]])
bhat<-1/n*sum((logreturn-ahat)^2)*dt
sigmahat<-sqrt(bhat)/dt
muhat<-1/2*sigmahat^2+ahat/dt

option<-getOptionChain("^SPX",src="yahoo", NULL)
#Data for 21 Juni 2019
calls<-option$jun.21.2019$calls
callprices<-calls$Last
strike<-calls$Strike
T_jun19<-12.7/12
#Data for 20 december 2019
callprices2<-option$jun.15.2018$calls$Last
strike2<-option$jun.15.2018$calls$Strike
T_dec19<-0.5/12
s0<-SP500[[length(SP500)]]


vec_implied.vol<-Vectorize(implied.vol,vectorize.args = c("K","price"))

imp_vol<-vec_implied.vol(s0=s0,K=strike,T=T_jun19,r=muhat,price=callprices)
impvol2<-vec_implied.vol(s0=s0,K=strike2,T=T_dec19,r=muhat,price=callprices2)

qplot(strike,imp_vol, xlab ="Strike",ylab = "Implied Volatility", xlim = c(2000,4000))+ggtitle("Volatility Smile")+geom_smooth()
qplot(strike2,impvol2,  xlab ="Strike",ylab = "Implied Volatility")+ggtitle("Volatility Skew")+geom_smooth()


#5.9
qplot(imp_vol, geom="histogram")+geom_vline(xintercept=sigmahat, col="blue", size=1)
qplot(impvol2, geom="histogram")+geom_vline(xintercept=sigmahat, col="blue", size=1)

vol<-function(sigma,K,price){
  return(exp(-muhat*T_jun19)*(s0*exp((muhat)*T_jun19)*pnorm((log(s0/K)+(muhat+0.5*sigma^2)*T_jun19)/(sigma*sqrt(T_jun19)),0,1)-K*pnorm((log(s0/K)+(muhat+0.5*sigma^2)*T_jun19)/(sigma*sqrt(T_jun19))-sigma*sqrt(T_jun19),0,1))-price)
}
bisect(vol(sigma,K=4100,price=0.8),0,1)
