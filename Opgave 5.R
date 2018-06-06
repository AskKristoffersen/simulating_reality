library(ggplot2)
require(gridExtra)
library(quantmod)
source("functions/funktioner.R")

# Opgave 5.3

#starværdier
s0<-100
K<-80
r=0.06
T<- 1
sigma<-0.3
dt <- 1 / 50
n_sim<-10000



#Gør så funktionerne kan tage vektorer:
vec_MC_call<-Vectorize(MC_Callprice_method2,vectorize.args = c("T","s0","sigma","r"))
vec_MC_put<-Vectorize(MC_Putprice_method2,vectorize.args = c("T","s0","sigma","r"))
#ændrer på sigma
sigma_put_prices<-rep(NA,10)

sigma_put_prices[1:10]<-vec_MC_put(s0=s0 , r=r , n_sim = n_sim, K=K , T=T, dt=dt,sigma=seq(0.1,1,by=1/10))


sigmaput_plot<-qplot(seq(0.1,1,1/10),sigma_put_prices, xlab = "sigma", ylab = "pris")+geom_smooth()+ggtitle("Ændring af sigma")

BS_sigma_put<-Black_Scholes_putprices(T=T,s0=s0,K=K,r=r,sigma = seq(0.1,1,1/10),q=0)

sigma_fitput_plot<-qplot(BS_sigma_put,sigma_put_prices,xlab="Black-scholes priser", ylab="Monte Carlo priser")+geom_abline()+ggtitle("Ændring af sigma")

#Ændrer på s0
s0_put_prices<-rep(NA,10)
s0_put_prices[1:10]<-vec_MC_put(n_sim = n_sim, K=K , T=T, dt=dt,sigma=sigma, r=r, s0=seq(50,275,by=25))

s0put_plot<-qplot(seq(50,275,by=25),s0_put_prices, xlab = "S(t)", ylab = "Pris")+geom_smooth()+ggtitle("Ændring af S(t) ")

BS_s0_put<-Black_Scholes_putprices(T=T,s0=seq(50,275,by=25),K=K,r=r,sigma=sigma,q=0)

s0_fitput_plot<-qplot(s0_put_prices,BS_s0_put,xlab="Black-scholes priser", ylab="Monte Carlo priser")+geom_abline()+ggtitle("Ændring af S(t)")


#ændrer på r

r_put<-rep(NA,10)
r_put[1:10]<-vec_MC_put(n_sim = n_sim, K=K , T=T, dt=dt,sigma=sigma, s0=s0 , r=seq(0.01,0.10,by=1/100))

rput_plot<-qplot(seq(0.01,0.10,by=1/100), r_put, xlab= "r", ylab = "pris")+ geom_smooth()+ggtitle("Ændring af r")

BS_r_put<-Black_Scholes_putprices(T=T,s0=s0,K=K,r=seq(0.01,0.10,by=1/100),sigma=sigma,q=0)

r_fitput_plot<-qplot(r_put,BS_r_put,xlab="Black-scholes priser", ylab="Monte Carlo priser")+geom_abline()+ggtitle("Ændring af r")
#Ændrer på T
T_put<-rep(NA,10)

T_put[1:10]<-vec_MC_put(n_sim = n_sim, K=K , dt=dt,sigma=sigma, s0=s0, r=r, T=1:10)

Tput_plot<-qplot(1:10, T_put, xlab= "T", ylab = "pris")+geom_smooth()+ggtitle("Ændring af T")

BS_T_put<-Black_Scholes_putprices(T=1:10,s0=s0,K=K,r=r,sigma=sigma,q=0)

T_fitput_plot<-qplot(T_put,BS_T_put, xlab="Black-Scholes priser", ylab="Monte Carlo priser")+geom_abline()+ggtitle("Ændring af T")

#Call

#startværdier
s0<-100
K<-80
r=0.06
T<- 1
sigma<-0.3
dt <- 1 / 50
n_sim<-1000

#Ændrer på sigma
sigmacall_prices<-rep(NA,10)
sigmacall_prices[1:10]<-vec_MC_call(s0=s0 , r=r , n_sim = n_sim, K=K , T=T, dt=dt,sigma=seq(0.1,1,by=1/10))


sigmacall_plot<-qplot(seq(0.1,1,by=1/10), sigmacall_prices, xlab = "sigma", ylab = "pris")+geom_smooth()+ggtitle("Ændring af sigma")

BS_sigma_prices<-Black_Scholes_callprice(T=T,s0=s0,K=K,r=r,sigma = seq(0.1,1,1/10), q=0)

sigma_fitcall_plot<-qplot(sigmacall_prices,BS_sigma_prices,xlab="Black-scholes priser", ylab="Monte Carlo priser")+geom_abline()+ggtitle("Ændring af sigma")
#Ændrer på r
r_call_prices<-rep(NA,10)
r_call_prices<-vec_MC_call( s0=s0 , n_sim = n_sim, K=K , T=T, dt=dt,sigma=sigma, r=seq(0.01,0.10,by=1/100))


rcall_plot<-qplot(seq(0.01,0.10,by=1/100), r_call_prices, xlab= "r", ylab = "pris")+geom_smooth()+ggtitle("Ændring af r")

BS_r_call<-Black_Scholes_callprice(T=T,s0=s0,K=K,r=seq(0.01,0.10,by=1/100),sigma=sigma,q=0)

r_fitcall_plot<-qplot(r_call_prices,BS_r_call,xlab="Black-scholes priser", ylab="Monte Carlo priser")+geom_abline()+ggtitle("Ændring af r")
#Ændrer på s0
s0_call_prices<-rep(NA,10)
s0_call_prices[1:10]<-vec_MC_call( n_sim = n_sim, K=K , T=T, dt=dt,sigma=sigma, r=r, s0=seq(50,275,by=25))

s0call_plot<-qplot(seq(50,275,by=25),s0_call_prices, xlab = "S(t)", ylab = "Pris")+geom_smooth()+ggtitle("Ændring af S(t)")

BS_s0_prices<-Black_Scholes_callprice(T=T,seq(50,275,by=25),K=K,r=r,sigma=sigma,q=0)

s0_fitcall_plot<-qplot(s0_call_prices,BS_s0_prices,xlab="Black-scholes priser", ylab="Monte Carlo priser")+geom_abline()+ggtitle("Ændring af S(t)")
#Ændrer på T
T_call_prices<-rep(NA,10)
T_call_prices[1:10]<-vec_MC_call( n_sim = n_sim, K=K , dt=dt,sigma=sigma, r=r, s0=s0, T=1:10)

Tcall_plot<-qplot(seq(1,10,1),T_call_prices, xlab = "T", ylab = "pris")+geom_smooth()+ggtitle("Ændring af T")



T_BS_call<-Black_Scholes_callprice(T=1:10,s0=s0,K=K,r=r,sigma=sigma,q=0)

T_fitcall_plot<-qplot(T_BS_call, T_call_prices, xlab="Black-scholes priser", ylab="Monte Carlo priser")+geom_abline()+ggtitle("Ændring af T")

#grid plot af udviklingen put
grid.arrange(s0put_plot,Tput_plot,sigmaput_plot,rput_plot,ncol=2, top ="Udvikling af prisen for put option ved")

#grid plot af udviklingen call
grid.arrange(s0call_plot,Tcall_plot,sigmacall_plot,rcall_plot, ncol=2, top="Udvikling af prisen for call option ved")

#gridarrange sammenligning mellem MC og BS call
grid.arrange(s0_fitcall_plot,T_fitcall_plot,sigma_fitcall_plot,r_fitcall_plot,ncol=2, top= "Sammenligning af MC og BS priser call")

#gridarrange sammenligning mellem MC og BS put
grid.arrange(s0_fitput_plot,T_fitput_plot,sigma_fitput_plot,r_fitput_plot, ncol=2, top= "Sammenligning af MC og BS priser put" )



#opgave 5.4 blackscholes priser
#Vi finder Log return, muhat og sigmahat for data fra 2018
getSymbols("^GSPC",src="yahoo",from="2017-12-19")
SP500_2018<-GSPC$GSPC.Close
logreturn2018<-c(NA,length(SP500_2018))
for(i in 2:length(SP500_2018)){
  logreturn2018[i-1]<-log(SP500_2018[[i]]/SP500_2018[[i-1]])
}
T2018<-length(logreturn2018)/252
n<-length(logreturn2018)
dt<-1/250

ahat<-1/n*log(SP500_2018[[length(SP500_2018)]]/SP500_2018[[1]])
bhat<-1/n*sum((logreturn2018-ahat)^2)*dt
sigmahat2018<-sqrt(bhat)/dt
muhat2018<-1/2*sigmahat2018^2+ahat/dt

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
strike_juni19<-calls_juni19$Strike


#Black scholes price of S&P 500 index
BS_prices_juni15<-Black_Scholes_callprice(
  T=0.5/12,SP500_2018[[length(SP500_2018)]],strike_juni15,muhat2018,sigmahat2018,q=0.018)
BS_prices_dec19<-Black_Scholes_callprice(
  T=18/12,SP500_2018[[length(SP500_2018)]],strike_dec19,muhat2018,sigmahat2018,q=0.018)
BS_prices_juli31<-Black_Scholes_callprice(
  T=2/12,SP500_2018[[length(SP500_2018)]],strike_juli31,muhat2018,sigmahat2018,q=0.018)
BS_prices_juni19<-Black_Scholes_callprice(
  T=12/12,SP500_2018[[length(SP500_2018)]],strike_juni19,muhat2018,sigmahat2018,q=0.018)



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
  T=0.5/12,SP500_2018[[length(SP500_2018)]],strike_putjuni15,muhat2018,sigmahat2018,q=0.0182)
BSput_juli31<-Black_Scholes_putprices(
  T=2/12,SP500_2018[[length(SP500_2018)]],strike_putjuli31,muhat2018,sigmahat2018,q=0.0182)
BSput_juni19<-Black_Scholes_putprices(
  T=12/12,SP500_2018[[length(SP500_2018)]],strike_putjuni19,muhat2018,sigmahat2018,q=0.0182)
BSput_dec19<-Black_Scholes_putprices(
  T=18/12,SP500_2018[[length(SP500_2018)]],strike_putdec19,muhat2018,sigmahat2018,q=0.0182)


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
n<-length(onemonth)

ahat<-1/n*log(onemonth[[length(onemonth)]]/onemonth[[1]])
bhat<-1/n*sum((logreturn_onemonth-ahat)^2)*dt
sigmahat_onemonth<-sqrt(bhat)/dt
muhat_onemonth<-1/2*sigmahat_onemonth^2+ahat/dt

#pris
Onemonth_prices<-Black_Scholes_callprice(
  T=0.5/12,onemonth[[length(onemonth)]],strike_juni15,muhat_onemonth,sigmahat_onemonth,q=0.0182)

put_month<-Black_Scholes_putprices(
  T=0.5/12,onemonth[[length(onemonth)]],strike_putjuni15,muhat_onemonth,sigmahat_onemonth,q=0.0182)

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
n<-length(from2010)

ahat<-1/n*log(from2010[[length(from2010)]]/from2010[[1]])
bhat<-1/n*sum((logreturn_2010-ahat)^2)*dt
sigmahat_2010<-sqrt(bhat)/dt
muhat_2010<-1/2*sigmahat_2010^2+ahat/dt

prices2010<-Black_Scholes_callprice(
  T=0.5/12,from2010[[length(from2010)]],strike_juni15,muhat_2010,sigmahat_2010,q=0.0182)

put_2år<-Black_Scholes_putprices(
  T=0.5/12,from2010[[length(from2010)]],strike_putjuni15,muhat_2010,sigmahat_2010,q=0.0182)

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
n<-length(oneyear)
T_oneyear<-length(logreturn_oneyear)/252

ahat<-1/n*log(oneyear[[length(oneyear)]]/oneyear[[1]])
bhat<-1/n*sum((logreturn_oneyear-ahat)^2)*dt
sigmahat_oneyear<-sqrt(bhat)/dt
muhat_oneyear<-1/2*sigmahat_oneyear^2+ahat/dt
#pris
oneyear_prices<-Black_Scholes_callprice(
  T=0.5/12,oneyear[[length(oneyear)]],strike_juni15,muhat_oneyear,sigmahat_oneyear,q=0.0182)

put_year<-Black_Scholes_putprices(
  T=0.5/12,oneyear[[length(oneyear)]],strike_putjuni15,muhat_oneyear,sigmahat_oneyear,q=0.0182)

#plots
year_plot<-qplot(oneyear_prices,callprices_juni15, xlab="Black-Scholes", ylab="Observerde")+geom_abline()+ggtitle("1 år")

year_put<-qplot(put_year,putprices_juni15, xlab="Black-Scholes", ylab="Observerde")+geom_abline()+ggtitle("1 år")

plot_jun_call<-qplot(BS_prices_juni15,callprices_juni15, xlab="Black-Scholes", ylab="Observerde")+geom_abline()+ggtitle("6 måneder")

plot_jun_put<-qplot(BSput_juni15,putprices_juni15, xlab="Black-Scholes", ylab="Observerde")+geom_abline()+ggtitle("6 måneder")
#gridarrange
grid.arrange(month_plot,plot_jun_call, year_plot, plot2010, ncol=2, top="Beregning af mu og sigma ved forskellige datastørrelser call")

grid.arrange(month_put,plot_jun_put,year_put, plot2years, ncol=2, top="Beregning af mu og sigma ved forskellige datastørrelser put")



#Opgave 5.5 amerikanske optioner

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
n<-length(SP500_2018_american)
dt<-1/250

ahat<-1/n*log(SP500_2018_american[[length(SP500_2018_american)]]/SP500_2018_american[[1]])
bhat<-1/n*sum((logreturn2018_american-ahat)^2)*dt
sigmahat2018<-sqrt(bhat)/dt
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
BS_prices_american<-Black_Scholes_callprice(T=30/12,SP500_2018_american[[length(SP500_2018_american)]],strike,muhat2018,sigmahat2018,q=0.0167)
BS2<-Black_Scholes_callprice(T=12/12,SP500_2018_american[[length(SP500_2018_american)]],strike2,muhat2018,sigmahat2018,q=0.0167)
BS3<-Black_Scholes_callprice(T=0.5/12,SP500_2018_american[[length(SP500_2018_american)]],strike3,muhat2018,sigmahat2018,q=0.0167)
BS4<-Black_Scholes_callprice(T=15/12,SP500_2018_american[[length(SP500_2018_american)]],strike4,muhat2018,sigmahat2018,q=0.0167)

BS_put_american<-Black_Scholes_putprices(T=30/12,SP500_2018_american[[length(SP500_2018_american)]],strike_put,muhat2018,sigmahat2018,q=0.0167)
BS2_put<-Black_Scholes_putprices(T=12/12,SP500_2018_american[[length(SP500_2018_american)]],strike2_put,muhat2018,sigmahat2018,q=0.0167)
BS3_put<-Black_Scholes_putprices(T=0.5/12,SP500_2018_american[[length(SP500_2018_american)]],strike3_put,muhat2018,sigmahat2018,q=0.0167)
BS4_put<-Black_Scholes_putprices(T=15/12,SP500_2018_american[[length(SP500_2018_american)]],strike4_put,muhat2018,sigmahat2018,q=0.0167)

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


option<-getOptionChain("^SPX",src="yahoo", NULL)


calls_juni15<-option$jun.15.2018$calls
callprices_juni15<-calls_juni15$Last
qplot(callprices_juni15[1:150]/10,price3[1:150])

#Implied volatilitet


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


#opgave 5.10 Asian options


#Monte Carlo simulation af asiatiske Call optioner
MC_Asian_Callprice <- function(s0, K, sigma, r, n_sim, T) {
  dt = 1/1000
  simulated_paths <- simulated_paths2 <- AV_paths <- matrix(NA, 1000, n_sim)
  simulated_paths[1,] <- simulated_paths2[1,] <- AV_paths[1,] <- s0
  riemann_value <- c(NA, n_sim)
  asiancallprice <- c(NA, n_sim)
  
  for (k in 1:n_sim) {
    for (i in 2:1000) {
      z1 <- rnorm(1)
      z2 <- -z1
      simulated_paths[i, k] <- simulated_paths[i - 1, k] * exp((r-0.5*sigma^2)*dt + sigma * sqrt(dt) * z1)
      simulated_paths2[i, k] <- simulated_paths2[i - 1, k] * exp((r-0.5*sigma^2)*dt + sigma * sqrt(dt) * z2)
    }
    riemann_value[k] <- 1/2*(pmax(mean(simulated_paths[,k])-K,0) + pmax(mean(simulated_paths2[,k])-K,0))
    asiancallprice[k] <- exp(-r*(T))*riemann_value[k]
  }
  price <- mean(asiancallprice)
  return(price)
}

vec_asian<-Vectorize(MC_Asian_Callprice,c("s0","K","sigma","r"))

#Tjekker asiatiske priser med europæiske
strike<-seq(75,250,25)
Black_Scholes_callprice(T=1,s0=175,K=strike,r=0.02,sigma=0.2,q=0)
vec_asian(s0=175,r=0.02,sigma=0.2,n_sim=1000,T=1,K=strike)


#Monte Carlo simulation af asiatiske put optioner
MC_Asian_put<- function(s0, K, sigma, r, n_sim, T) {
  dt = 1/1000
  simulated_paths <- simulated_paths2 <- AV_paths <- matrix(NA, 1000, n_sim)
  simulated_paths[1,] <- simulated_paths2[1,] <- AV_paths[1,] <- s0
  riemann_value <- c(NA, n_sim)
  asiancallprice <- c(NA, n_sim)
  
  for (k in 1:n_sim) {
    for (i in 2:1000) {
      z1 <- rnorm(1)
      z2 <- -z1
      simulated_paths[i, k] <- simulated_paths[i - 1, k] * exp((r-0.5*sigma^2)*dt + sigma * sqrt(dt) * z1)
      simulated_paths2[i, k] <- simulated_paths2[i - 1, k] * exp((r-0.5*sigma^2)*dt + sigma * sqrt(dt) * z2)
    }
    riemann_value[k] <- 1/2*(pmax(K-mean(simulated_paths[,k]),0) + pmax(K-mean(simulated_paths2[,k]),0))
    asiancallprice[k] <- exp(-r*(T))*riemann_value[k]
  }
  price <- mean(asiancallprice)
  return(price)
}
vec_asian_put<-Vectorize(MC_Asian_put,c("s0","K","sigma","r"))

strike<-seq(75,250,25)
Black_Scholes_putprices(s0=100,r=0.02,sigma=0.2,T=1,K=strike,q=0)
vec_asian_put(s0=100,r=0.02,sigma=0.2,n_sim=1000,T=1,K=strike)


