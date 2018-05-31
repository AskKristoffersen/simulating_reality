library(ggplot2)
library(quantmod)

## Black-Scholes Function
implied.vol <-
  function(s0, K, T, r, price){
    sigma <- 0.20
    sigma.up <- 10
    sigma.down <- 0.001
    count <- 0
    err <- Black_Scholes_callprice(T,s0,K,r,sigma,q=0) - price
    
    ## repeat until error is sufficiently small or counter hits 1000
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
    
    ## return NA if counter hit 1000
    if(count==1000){
      return(NA)
    }else{
      return(sigma)
    }
  }


getSymbols("^GSPC",src="yahoo",from="2017-12-10")
SP500<-GSPC$GSPC.Close
logreturn<-c(NA,length(SP500))
for(i in 2:length(SP500)){
  logreturn[i-1]<-log(SP500[[i]]/SP500[[i-1]])
}

T<-length(logreturn)/252
muhat<-log(SP500[[nrow(SP500)]]/SP500[[1]])/T+(1/T*sum(logreturn^2))/2

sigmahat<-sqrt(1/T*sum(logreturn^2))


option<-getOptionChain("^SPX",src="yahoo", NULL)
#Data for 21 Juni 2019
calls<-option$jun.21.2019$calls
callprices<-calls$Last
strike<-calls$Strike

#Data for 20 december 2019
callprices2<-option$dec.20.2019$calls$Last
strike2<-option$dec.20.2019$calls$Strike

s0<-SP500[[length(SP500)]]


vec_implied.vol<-Vectorize(implied.vol,vectorize.args = c("K","price"))

imp_vol<-vec_implied.vol(s0=s0,K=strike,T=T,r=muhat,price=callprices)
impvol2<-vec_implied.vol(s0=s0,K=strike2,T=T,r=muhat,price=callprices2)

qplot(strike,imp_vol, xlab ="Strike",ylab = "Implied Volatility", xlim = c(2000,4000))+ggtitle("Volatility Smile")+geom_smooth()+
qplot(strike2,impvol2,  xlab ="Strike",ylab = "Implied Volatility", xlim = c(2000,4000))+ggtitle("Volatility Skew")+geom_smooth()


#5.9
qplot(imp_vol, geom="histogram")+geom_vline(xintercept=sigmahat, col="blue", size=1)
qplot(impvol2, geom="histogram")+geom_vline(xintercept=sigmahat, col="blue", size=1)
