library(ggplot2)
require(gridExtra)
#starværdier
s0<-100
K<-200
r=0.06
T<- 1
sigma<-0.3
dt <- 1 / 50
n_sim<-10000
n_paths<-10

MC_Callprice_method2(1000,0.06,0.3,5000,10,1000,1,1/50)

#Gør så funktionerne kan tage vektorer:
vec_MC_call<-Vectorize(MC_Callprice_method2,vectorize.args = c("T","s0","sigma","r"))
vec_MC_put<-Vectorize(MC_Putprice_method2,vectorize.args = c("T","s0","sigma","r"))
#ændrer på sigma
sigma_put_prices<-rep(NA,10)

sigma_put_prices[1:10]<-vec_MC_put(s0=s0 , r=r , n_sim = n_sim, n_paths = n_paths, K=K , T=T, dt=dt,sigma=seq(0.1,1,by=1/10))


sigmaput_plot<-qplot(seq(0.1,1,1/10),sigma_put_prices, xlab = "sigma", ylab = "pris")+geom_smooth()+ggtitle("Ændring af sigma")

BS_sigma_put<-Black_Scholes_putprices(T=T,s0=s0,K=K,r=r,sigma = seq(0.1,1,1/10),q=0)

sigma_fitput_plot<-qplot(BS_sigma_put,sigma_put_prices,xlab="Black-scholes priser", ylab="Monte Carlo priser")+geom_abline()+ggtitle("Ændring af sigma")

#Ændrer på s0
s0_put_prices<-rep(NA,10)
s0_put_prices[1:10]<-vec_MC_put(n_sim = n_sim, n_paths = n_paths, K=K , T=T, dt=dt,sigma=sigma, r=r, s0=seq(50,275,by=25))

s0put_plot<-qplot(seq(50,275,by=25),s0_put_prices, xlab = "S(t)", ylab = "Pris")+geom_smooth()+ggtitle("Ændring af S(t) ")

BS_s0_put<-Black_Scholes_putprices(T=T,s0=seq(50,275,by=25),K=K,r=r,sigma=sigma,q=0)

s0_fitput_plot<-qplot(s0_put_prices,BS_s0_put,xlab="Black-scholes priser", ylab="Monte Carlo priser")+geom_abline()+ggtitle("Ændring af S(t)")


#ændrer på r

r_put<-rep(NA,10)
r_put[1:10]<-vec_MC_put(n_sim = n_sim, n_paths = n_paths, K=K , T=T, dt=dt,sigma=sigma, s0=s0 , r=seq(0.01,0.10,by=1/100))

rput_plot<-qplot(seq(0.01,0.10,by=1/100), r_put, xlab= "r", ylab = "pris")+ geom_smooth()+ggtitle("Ændring af r")

BS_r_put<-Black_Scholes_putprices(T=T,s0=s0,K=K,r=seq(0.01,0.10,by=1/100),sigma=sigma,q=0)

r_fitput_plot<-qplot(r_put,BS_r_put,xlab="Black-scholes priser", ylab="Monte Carlo priser")+geom_abline()+ggtitle("Ændring af r")
#Ændrer på T
T_put<-rep(NA,10)

T_put[1:10]<-vec_MC_put(n_sim = n_sim, n_paths = n_paths, K=K , dt=dt,sigma=sigma, s0=s0, r=r, T=1:10)

Tput_plot<-qplot(1:10, T_put, xlab= "T", ylab = "pris")+geom_smooth()+ggtitle("Ændring af T")

BS_T_put<-Black_Scholes_putprices(T=1:10,s0=s0,K=K,r=r,sigma=sigma,q=0)

T_fitput_plot<-qplot(T_put,BS_T_put, xlab="Black-Scholes priser", ylab="Monte Carlo priser")+geom_abline()+ggtitle("Ændring af T")

#Call

#startværdier
s0<-100
K<-50
r=0.06
T<- 1
sigma<-0.3
dt <- 1 / 50
n_sim<-1000
n_paths<-5

#Ændrer på sigma
sigmacall_prices<-rep(NA,10)
sigmacall_prices[1:10]<-vec_MC_call(s0=s0 , r=r , n_sim = n_sim, n_paths = n_paths, K=K , T=T, dt=dt,sigma=seq(0.1,1,by=1/10))


sigmacall_plot<-qplot(seq(0.1,1,by=1/10), sigmacall_prices, xlab = "sigma", ylab = "pris")+geom_smooth()+ggtitle("Ændring af sigma")

BS_sigma_prices<-Black_Scholes_callprice(T=T,s0=s0,K=K,r=r,sigma = seq(0.1,1,1/10), q=0)

sigma_fitcall_plot<-qplot(sigmacall_prices,BS_sigma_prices,xlab="Black-scholes priser", ylab="Monte Carlo priser")+geom_abline()+ggtitle("Ændring af sigma")
#Ændrer på r
r_call_prices<-rep(NA,10)
r_call_prices<-vec_MC_call( s0=s0 , n_sim = n_sim, n_paths = n_paths, K=K , T=T, dt=dt,sigma=sigma, r=seq(0.01,0.10,by=1/100))


rcall_plot<-qplot(seq(0.01,0.10,by=1/100), r_call_prices, xlab= "r", ylab = "pris")+geom_smooth()+ggtitle("Ændring af r")

BS_r_call<-Black_Scholes_callprice(T=T,s0=s0,K=K,r=seq(0.01,0.10,by=1/100),sigma=sigma,q=0)

r_fitcall_plot<-qplot(r_call_prices,BS_r_call,xlab="Black-scholes priser", ylab="Monte Carlo priser")+geom_abline()+ggtitle("Ændring af r")
#Ændrer på s0
s0_call_prices<-rep(NA,10)
s0_call_prices[1:10]<-vec_MC_call( n_sim = n_sim, n_paths = n_paths, K=K , T=T, dt=dt,sigma=sigma, r=r, s0=seq(50,275,by=25))

s0call_plot<-qplot(seq(50,275,by=25),s0_call_prices, xlab = "S(t)", ylab = "Pris")+geom_smooth()+ggtitle("Ændring af S(t)")

BS_s0_prices<-Black_Scholes_callprice(T=T,seq(50,275,by=25),K=K,r=r,sigma=sigma,q=0)

s0_fitcall_plot<-qplot(s0_call_prices,BS_s0_prices,xlab="Black-scholes priser", ylab="Monte Carlo priser")+geom_abline()+ggtitle("Ændring af S(t)")
#Ændrer på T
T_call_prices<-rep(NA,10)
T_call_prices[1:10]<-vec_MC_call( n_sim = n_sim, n_paths = n_paths, K=K , dt=dt,sigma=sigma, r=r, s0=s0, T=1:10)

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
