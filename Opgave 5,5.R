library(quantmod)
library(ggplot2)
require(gridExtra)
source("functions/funktioner.R")
#Vi finder Log return, muhat og sigmahat for data fra 2018
getSymbols("SPY",src="yahoo",from="2017-10-10")
SP500_2018_american<-SPY$SPY.Close
logreturn2018_american<-c(NA,length(SP500_2018_american))
for(i in 2:length(SP500_2018_american)){
  logreturn2018_american[i-1]<-log(SP500_2018_american[[i]]/SP500_2018_american[[i-1]])
}

T2018<-length(logreturn2018_american)/252

ahat<-1/n*log(SP500_2018_american[[length(SP500_2018_american)]]/SP500_2018_american[[1]])
bhat<-1/n*sum((logreturn2018_american-ahat)^2)*dt
sigmahat2018_<-sqrt(bhat)/dt
muhat2018<-1/2*sigmahat2018^2+ahat/dt

#vi finder priserne for optionerne hvis man købte dem i dag
american_options<-getOptionChain("SPY",src="yahoo", NULL)
american_calls<-american_options$dec.18.2020$calls
american_callprices<-american_calls$Last
strike<-american_calls$Strike

price2<-american_options$jun.21.2019$calls$Last
strike2<-american_options$jun.21.2019$calls$Strike
price3<-american_options$jun.15.2018$calls$Last
strike3<-american_options$jun.15.2018$calls$Strike
price4<-american_options$sep.20.2019$calls$Last
strike4<-american_options$sep.20.2019$calls$Strike


american_put<-american_options$dec.18.2020$puts
american_putprices<-american_put$Last
strike_put<-american_put$Strike

price2_put<-american_options$jun.21.2019$puts$Last
strike2_put<-american_options$jun.21.2019$puts$Strike
price3_put<-american_options$jun.15.2018$puts$Last
strike3_put<-american_options$jun.15.2018$puts$Strike
price4_put<-american_options$sep.20.2019$puts$Last
strike4_put<-american_options$sep.20.2019$puts$Strike


#Black scholes price of S&P 500 index
BS_prices_american<-Black_Scholes_callprice(T=T2018,SP500_2018_american[[length(SP500_2018_american)]],strike,muhat2018,sigmahat2018,q=0.0167)
BS2<-Black_Scholes_callprice(T=T2018,SP500_2018_american[[length(SP500_2018_american)]],strike2,muhat2018,sigmahat2018,q=0.0167)
BS3<-Black_Scholes_callprice(T=T2018,SP500_2018_american[[length(SP500_2018_american)]],strike3,muhat2018,sigmahat2018,q=0.0167)
BS4<-Black_Scholes_callprice(T=T2018,SP500_2018_american[[length(SP500_2018_american)]],strike4,muhat2018,sigmahat2018,q=0.0167)

BS_put_american<-Black_Scholes_putprices(T=T2018,SP500_2018_american[[length(SP500_2018_american)]],strike_put,muhat2018,sigmahat2018,q=0.0167)
BS2_put<-Black_Scholes_putprices(T=T2018,SP500_2018_american[[length(SP500_2018_american)]],strike2_put,muhat2018,sigmahat2018,q=0.0167)
BS3_put<-Black_Scholes_putprices(T=T2018,SP500_2018_american[[length(SP500_2018_american)]],strike3_put,muhat2018,sigmahat2018,q=0.0167)
BS4_put<-Black_Scholes_putprices(T=T2018,SP500_2018_american[[length(SP500_2018_american)]],strike4_put,muhat2018,sigmahat2018,q=0.0167)

#plots af forskel i priser

dec20<-qplot(BS_prices_american,american_callprices, xlab = "Black-Scholes", ylab = "Observerede")+geom_abline()+ggtitle("18 December 2020")
jun19<-qplot(BS2,price2, xlab = "Black-Scholes", ylab = "Observerede")+geom_abline()+ggtitle("21 Juni 2019")
jun18<-qplot(BS3,price3, xlab = "Black-Scholes", ylab = "Observerede")+geom_abline()+ggtitle("15 Juni 2018")
sep19<-qplot(BS4,price4, xlab = "Black-Scholes", ylab = "Observerede")+geom_abline()+ggtitle("20 September 2019")

dec20_put<-qplot(BS_put_american, american_putprices,xlab = "Black-Scholes", ylab = "Observerede")+geom_abline()+ggtitle("18 December 2020")
jun19_put<-qplot(BS2_put,price2_put,xlab = "Black-Scholes", ylab = "Observerede")+geom_abline()+ggtitle("21 Juni 2019")
jun18_put<-qplot(BS3_put,price3_put,xlab = "Black-Scholes", ylab = "Observerede")+geom_abline()+ggtitle("15 Juni 2018")
sep19_put<-qplot(BS4_put,price4_put,xlab = "Black-Scholes", ylab = "Observerede")+geom_abline()+ggtitle("20 September 2019")

#grid
grid.arrange(jun18,jun19,sep19,dec20, ncol=2, top="Sammenligning af BS og Amerikanske observerede priser call")

grid.arrange(jun18_put,jun19_put,sep19_put,dec20_put, ncol=2, top="Sammenligning af BS og Amerikanske observerede priser put")
