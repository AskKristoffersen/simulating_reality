library(reshape2)
library(ggplot2)
require(gridExtra)
library(quantmod)
source("functions/funktioner.R")
#parametre
x0 <- 100
mu <- 0.02
sigma <- 0.85
n_sim <- 10
end_time <- 10
dt <- 1
time_vector <- seq(0, end_time, by = dt)


#method1
simulated_paths1<-matrix(NA,length(time_vector),n_sim)
for (i in 1:10){
  set.seed(i)
  simulated_paths1[,i] <- simulate_GBM(x0 = x0,
                                       mu = mu,
                                       sigma = sigma,
                                       n_sim = 1,
                                       time_vector = time_vector)
  
}

data_plot1 <- data.frame('time' = time_vector, "GBM" = simulated_paths1)
data_plot1 <- melt(data_plot1,  id = c('time'))

plot_gbm1 <- ggplot(data_plot1, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+ggtitle("Method A")

#method 2

simulated_paths2<-matrix(NA,length(time_vector),n_sim)
for (i in 1:10){
  set.seed(i)
  simulated_paths2[,i] <- Method2(x0 = x0,
                                  mu = mu,
                                  sigma = sigma,
                                  n_sim = 1,
                                  time_vector = time_vector)
  
}

data_plot2 <- data.frame('time' = time_vector, "GBM" = simulated_paths2)
data_plot2 <- melt(data_plot2,  id = c('time'))


plot_gbm2 <- ggplot(data_plot2, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+ggtitle("Method B")


#plot forskel på methode 1 og 2

grid.arrange(plot_gbm1,plot_gbm2, ncol=2, top="dt=1")
#plot med en af hver
plot(time_vector, simulated_paths1[, 1], type = "l")
lines(time_vector,simulated_paths2[, 1],col="red")


#1/10
dt <- 1/10
time_vector <- seq(0, end_time, by = dt)

simulated_paths3<-matrix(NA,length(time_vector),n_sim)
for (i in 1:10){
  set.seed(i)
  simulated_paths3[,i] <- simulate_GBM(x0 = x0,
                                       mu = mu,
                                       sigma = sigma,
                                       n_sim = 1,
                                       time_vector = time_vector)
  
}

data_plot3 <- data.frame('time' = time_vector, "GBM" = simulated_paths3)
data_plot3 <- melt(data_plot3,  id = c('time'))

plot_gbm3 <- ggplot(data_plot3, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+ggtitle("Method A")

simulated_paths4<-matrix(NA,length(time_vector),n_sim)
for (i in 1:10){
  set.seed(i)
  simulated_paths4[,i] <- Method2(x0 = x0,
                                  mu = mu,
                                  sigma = sigma,
                                  n_sim = 1,
                                  time_vector = time_vector)
  
}


data_plot4 <- data.frame('time' = time_vector, "GBM" = simulated_paths4)
data_plot4 <- melt(data_plot4,  id = c('time'))


plot_gbm4 <- ggplot(data_plot4, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+ggtitle("Method B")

#grid.arrange
grid.arrange(plot_gbm3, plot_gbm4, ncol=2, top="dt=1/10")
#1/100
dt <- 1/100
time_vector <- seq(0, end_time, by = dt)

simulated_paths5<-matrix(NA,length(time_vector),n_sim)
for (i in 1:10){
  set.seed(i)
  simulated_paths5[,i] <- simulate_GBM(x0 = x0,
                                       mu = mu,
                                       sigma = sigma,
                                       n_sim = 1,
                                       time_vector = time_vector)
  
}

data_plot5 <- data.frame('time' = time_vector, "GBM" = simulated_paths5)
data_plot5 <- melt(data_plot5,  id = c('time'))

plot_gbm5 <- ggplot(data_plot5, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+ggtitle("Method A")


simulated_paths6<-matrix(NA,length(time_vector),n_sim)
for (i in 1:10){
  set.seed(i)
  simulated_paths6[,i] <- Method2(x0 = x0,
                                  mu = mu,
                                  sigma = sigma,
                                  n_sim = 1,
                                  time_vector = time_vector)
  
}

data_plot6 <- data.frame('time' = time_vector, "GBM" = simulated_paths6)
data_plot6 <- melt(data_plot6,  id = c('time'))


plot_gbm6 <- ggplot(data_plot6, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+ggtitle("Method B")

grid.arrange(plot_gbm1,plot_gbm3,plot_gbm5,plot_gbm2,plot_gbm4,plot_gbm6, ncol=3)

#Opgave 4.7 simulation af MLE

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



# opgave 4.9
#eksempel 3.7
s0<-5
K<-10
r=0.06
sigma<-0.3
end_time <- 1
dt <- 1 / 50
n_sim<-10000
time_vector <- seq(0, end_time, by = dt)

#Figur 3.3
montecarlo_paths<-matrix(NA,nrow = 10000,ncol = 10)
endvalue<-matrix(NA,nrow = 10000,ncol = 10)
option<-matrix(NA,nrow = 10000,ncol = 10)

for (i in 1:10){
  set.seed(i)
  simulated_paths<-simulate_GBM(x0=s0,mu=r,sigma=sigma,n_sim=n_sim,time_vector = time_vector)
  endvalue[,i]<-simulated_paths[51,]
  option[,i]<-pmax(0,K-endvalue[,i])
  for (b in 1:10000){
    montecarlo_paths[b,i]<- exp(-r)*mean(option[1:b,i])
  }
}


data_plot_3.3 <- data.frame("sim"=1:10000, "montecarlo" = montecarlo_paths)
data_plot_3.3 <- melt(data_plot_3.3,  id = c('sim'))


plot_3.3 <- ggplot(data_plot_3.3, aes(sim, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+ylim(4.2,4.7)+
  ggtitle("Eulers metode")


# figur 3.4
AV_option_3.4<-matrix(NA,nrow = 10000,ncol = 10)
antithetic_paths<-matrix(NA,10000,10)

for (i in 1:10){
  set.seed(i)
  AV_paths<-simulate_GBM_AV_euler(x0=s0,mu=r,sigma=sigma,n_sim=n_sim,time_vector = time_vector)
  sim1<-AV_paths[[1]]
  sim2<-AV_paths[[2]]
  AV_option_3.4[,i]<-1/2*(pmax(0,K-sim1[51,]) + pmax(0,K-sim2[51,]))
  for (b in 1:n_sim){
    antithetic_paths[b,i]<-exp(-r)*mean(AV_option_3.4[1:b,i])
  }
}


data_plot_3.4 <- data.frame("sim"=1:10000, "antithetic" = antithetic_paths)
data_plot_3.4<- melt(data_plot_3.4,  id = c('sim'))

plot_3.4 <- ggplot(data_plot_3.4, aes(sim, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+ylim(4.2,4.7)+
  ggtitle("Eulers metode med AV")


#3.5
montecarlo_paths_3.5<-matrix(NA,nrow = 10000,ncol = 10)
endvalue_3.5<-matrix(NA,nrow = 10000,ncol = 10)
option_3.5<-matrix(NA,nrow = 10000,ncol = 10)

for (i in 1:10){
  set.seed(i)
  simulated_paths_3.5<-Method2(x0=s0,mu=r,sigma=sigma,n_sim=n_sim,time_vector = time_vector)
  endvalue_3.5[,i]<-simulated_paths_3.5[51,]
  option_3.5[,i]<-pmax(0,K-endvalue_3.5[,i])
  for (b in 1:10000){
    montecarlo_paths_3.5[b,i]<- exp(-r)*mean(option_3.5[1:b,i])
  }
}
data_plot_3.5 <- data.frame("sim"=1:10000, "Montecarlo" = montecarlo_paths_3.5)
data_plot_3.5<- melt(data_plot_3.5,  id = c('sim'))

plot_3.5 <- ggplot(data_plot_3.5, aes(sim, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+ylim(4.2,4.7)+
  ggtitle("Analytisk løsning")


#3.6


AV_option_3.6<-matrix(NA,nrow = 10000,ncol = 10)
antithetic_paths_3.6<-matrix(NA,10000,10)

for (i in 1:10){
  set.seed(i)
  AV_paths_3.6<-Method2_AV(x0=s0,mu=r,sigma=sigma,n_sim=n_sim,time_vector = time_vector,dt=dt)
  AV_paths1<-AV_paths_3.6[[1]]
  AV_paths2<-AV_paths_3.6[[2]]
  AV_option_3.6[,i]<-1/2*(pmax(0,K-AV_paths1[51,])+pmax(0,K-AV_paths2[51,]))
  for (b in 1:n_sim){
    antithetic_paths_3.6[b,i]<-exp(-r)*mean(AV_option_3.6[1:b,i])
  }
}

data_plot_3.6<- data.frame("sim"=1:10000, "anithetic" = antithetic_paths_3.6)
data_plot_3.6<- melt(data_plot_3.6,  id = c('sim'))

plot_3.6 <- ggplot(data_plot_3.6, aes(sim, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+ylim(4.2,4.7)+
  ggtitle("Analytisk Løsning med AV")

grid.arrange(plot_3.3,plot_3.4,plot_3.5,plot_3.6,ncol=2)

grid.arrange(plot_3.3, plot_3.4, ncol=2)

grid.arrange(plot_3.5,plot_3.6, ncol=2)
