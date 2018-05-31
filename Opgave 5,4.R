library(quantmod)
library(ggplot2)
require(gridExtra)

#Vi finder Log return, muhat og sigmahat for data fra 2018
getSymbols("^GSPC",src="yahoo",from="2017-12-19")
SP500_2018<-GSPC$GSPC.Close
logreturn2018<-c(NA,length(SP500_2018))
for(i in 2:length(SP500_2018)){
  logreturn2018[i-1]<-log(SP500_2018[[i]]/SP500_2018[[i-1]])
}

T2018<-length(logreturn2018)/252
muhat2018<-log(SP500_2018[[nrow(SP500_2018)]]/SP500_2018[[1]])/T2018+(1/T2018*sum(logreturn2018^2))/2

sigmahat2018<-sqrt(1/T2018*sum(logreturn2018^2))

#vi finder priserne for optionerne hvis man købte dem i dag
option<-getOptionChain("^SPX",src="yahoo", NULL)


calls_juni15<-option$jun.15.2018$calls
calls_dec19<-option$dec.20.2019$calls
calls_juli31<-option$jul.31.2018$calls
calls_juni19<-option$jun.21.2019$calls



callprices_juni15<-calls_juni15$Last
callprices_dec19<-calls_dec19$Last
callprices_juli31<-calls_juli31$Last
callprices_juni19<-calls_juni19$Last



strike_juni15<-calls_juni15$Strike
strike_dec19<-calls_dec19$Strike
strike_juli31<-calls_juli31$Strike
strike_juni19<-calls_sep28$Strike


#Black scholes price of S&P 500 index
BS_prices_juni15<-Black_Scholes_callprice(
  T=T2018,SP500_2018[[length(SP500_2018)]],strike_juni15,muhat2018,sigmahat2018,q=0)
BS_prices_dec19<-Black_Scholes_callprice(
  T=T2018,SP500_2018[[length(SP500_2018)]],strike_dec19,muhat2018,sigmahat2018,q=0)
BS_prices_juli31<-Black_Scholes_callprice(
  T=T2018,SP500_2018[[length(SP500_2018)]],strike_juli31,muhat2018,sigmahat2018,q=0)
BS_prices_juni19<-Black_Scholes_callprice(
  T=T2018,SP500_2018[[length(SP500_2018)]],strike_juni19,muhat2018,sigmahat2018,q=0)



plot_jun_call<-qplot(BS_prices_juni15,callprices_juni15, xlab="Black-Scholes", ylab="Observerde")+geom_abline()+ggtitle("15 Juni 2018")
plot_dec19_call<-qplot(BS_prices_dec19,callprices_dec19,xlab="Black-Scholes", ylab="Observerde")+geom_abline()+ggtitle("20 December 2019")
plot_jul_call<-qplot(BS_prices_juli31,callprices_juli31,xlab="Black-Scholes", ylab="Observerde")+geom_abline()+ggtitle("31 Juli 2018")
plot_jun19_call<-qplot(BS_prices_juni19,callprices_juni19,xlab="Black-Scholes", ylab="Observerde")+geom_abline()+ggtitle("21 Juni 2019")


#gridarrange
grid.arrange(plot_jun_call,plot_jul_call,plot_jun19_call,plot_dec19_call, ncol=2,
             top = "Sammenligning af Black-Scholes og observered priser call")

#Data for Put optioner

putprices_juni15<-option$jun.15.2018$puts$Last
putprices_Juli31<-option$jul.31.2018$puts$Last
putprices_juni19<-option$jun.21.2019$puts$Last
putprices_dec19<-option$dec.20.2019$puts$Last


strike_putjuni15<-option$jun.15.2018$puts$Strike
strike_putjuli31<-option$jul.31.2018$puts$Strike
strike_putjuni19<-option$jun.21.2019$puts$Strike
strike_putdec19<-option$dec.20.2019$puts$Strike


#priser
BSput_juni15<-Black_Scholes_putprices(
  T=T2018,SP500_2018[[length(SP500_2018)]],strike_putjuni15,muhat2018,sigmahat2018,q=0)
BSput_juli31<-Black_Scholes_putprices(
  T=T2018,SP500_2018[[length(SP500_2018)]],strike_putjuli31,muhat2018,sigmahat2018,q=0)
BSput_juni19<-Black_Scholes_putprices(
  T=T2018,SP500_2018[[length(SP500_2018)]],strike_putjuni19,muhat2018,sigmahat2018,q=0)
BSput_dec19<-Black_Scholes_putprices(
  T=T2018,SP500_2018[[length(SP500_2018)]],strike_putdec19,muhat2018,sigmahat2018,q=0)


#plots
plot_jun_put<-qplot(BSput_juni15,putprices_juni15, xlab="Black-Scholes", ylab="Observerde")+geom_abline()+ggtitle("15 Juni 2018")
plot_jul_put<-qplot(BSput_juli31,putprices_Juli31,xlab="Black-Scholes", ylab="Observerde")+geom_abline()+ggtitle("31 Juli 2018")
plot_jun19_put<-qplot(BSput_juni19,putprices_juni19,xlab="Black-Scholes", ylab="Observerde")+geom_abline()+ggtitle("21 Juni 2019")
plot_dec_put<-qplot(BSput_dec19,putprices_dec19,xlab="Black-Scholes", ylab="Observerde")+geom_abline()+ggtitle("20 December 2019")

#gridarrange
grid.arrange(plot_jun_put,plot_jul_put,plot_jun19_put,plot_dec_put, ncol=2,
             top ="Sammenligning af Black-Scholes og observerede priser put")


#sammeligning af de forskellige mu og sigma regne ved forskellige tidsperioder:
getSymbols("^GSPC",src="yahoo",from="2018-17-04")
onemonth<-GSPC$GSPC.Close
logreturn_onemonth<-c(NA,length(onemonth))
for(i in 2:length(onemonth)){
  logreturn_onemonth[i-1]<-log(onemonth[[i]]/onemonth[[i-1]])
}

T_onemonth<-length(logreturn_onemonth)/252
muhat_onemonth<-log(onemonth[[nrow(onemonth)]]/onemonth[[1]])/T_onemonth+(1/T2018*sum(logreturn_onemonth^2))/2

sigmahat_onemonth<-sqrt(1/T_onemonth*sum(logreturn_onemonth^2))

#pris
Onemonth_prices<-Black_Scholes_callprice(
  T=T_onemonth,onemonth[[length(onemonth)]],strike_juni15,muhat_onemonth,sigmahat_onemonth,q=0)

put_month<-Black_Scholes_putprices(
  T=T_onemonth,onemonth[[length(onemonth)]],strike_putjuni15,muhat_onemonth,sigmahat_onemonth,q=0)

#plots
month_plot<-qplot(Onemonth_prices,callprices_juni15, xlab="Black-Scholes", ylab="Observerde")+geom_abline()+ggtitle("1 måned")

month_put<-qplot(put_month,putprices_juni15, xlab="Black-Scholes", ylab="Observerde")+geom_abline()+ggtitle("1 måned")

# 2010

getSymbols("^GSPC",src="yahoo",from="2016-05-19")
from2010<-GSPC$GSPC.Close
logreturn_2010<-c(NA,length(from2010))
for(i in 2:length(from2010)){
  logreturn_2010[i-1]<-log(from2010[[i]]/from2010[[i-1]])
}

T_2010<-length(logreturn_2010)/252
muhat_2010<-log(from2010[[nrow(from2010)]]/from2010[[1]])/T_2010+(1/T2018*sum(logreturn_2010^2))/2

sigmahat_2010<-sqrt(1/T_2010*sum(logreturn_2010^2))

prices2010<-Black_Scholes_callprice(
  T=T_2010,from2010[[length(from2010)]],strike_juni15,muhat_2010,sigmahat_2010,q=0)

put_2år<-Black_Scholes_putprices(
  T=T_2010,from2010[[length(from2010)]],strike_putjuni15,muhat_2010,sigmahat_2010,q=0)

#plot
plot2010<-qplot(prices2010,callprices_juni15, xlab="Black-Scholes", ylab="Observerde")+geom_abline()+ggtitle(" 2 år")

plot2years<-qplot(put_2år,putprices_juni15, xlab="Black-Scholes", ylab="Observerde")+geom_abline()+ggtitle(" 2 år")
#oneyear

getSymbols("^GSPC",src="yahoo",from="2017-05-19")
oneyear<-GSPC$GSPC.Close
logreturn_oneyear<-c(NA,length(oneyear))
for(i in 2:length(oneyear)){
  logreturn_oneyear[i-1]<-log(oneyear[[i]]/oneyear[[i-1]])
}

T_oneyear<-length(logreturn_oneyear)/252
muhat_oneyear<-log(oneyear[[nrow(oneyear)]]/oneyear[[1]])/T_oneyear+(1/T2018*sum(logreturn_oneyear^2))/2

sigmahat_oneyear<-sqrt(1/T_oneyear*sum(logreturn_oneyear^2))

#pris
oneyear_prices<-Black_Scholes_callprice(
  T=T_oneyear,oneyear[[length(oneyear)]],strike_juni15,muhat_oneyear,sigmahat_oneyear,q=0)

put_year<-Black_Scholes_putprices(
  T=T_oneyear,oneyear[[length(oneyear)]],strike_putjuni15,muhat_oneyear,sigmahat_oneyear,q=0)

#plots
year_plot<-qplot(oneyear_prices,callprices_juni15, xlab="Black-Scholes", ylab="Observerde")+geom_abline()+ggtitle("1 år")

year_put<-qplot(put_year,putprices_juni15, xlab="Black-Scholes", ylab="Observerde")+geom_abline()+ggtitle("1 år")

plot_jun_call<-qplot(BS_prices_juni15,callprices_juni15, xlab="Black-Scholes", ylab="Observerde")+geom_abline()+ggtitle("6 måneder")

plot_jun_put<-qplot(BSput_juni15,putprices_juni15, xlab="Black-Scholes", ylab="Observerde")+geom_abline()+ggtitle("6 måneder")
#gridarrange
grid.arrange(month_plot,plot_jun_call, year_plot, plot2010, ncol=2, top="Beregning af mu og sigma ved forskellige datastørrelser call")

grid.arrange(month_put,plot_jun_put,year_put, plot2years, ncol=2, top="Beregning af mu og sigma ved forskellige datastørrelser put")
