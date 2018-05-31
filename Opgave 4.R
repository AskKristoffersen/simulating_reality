require(gridExtra)
library(reshape2)
library(ggplot2)
#Opgave 4.6
x0 <- 100
mu <- 0.03
sigma <- 0.1
n_sim <- 10
end_time <- 1
dt <- 1 / 200
time_vector <- seq(0, end_time, by = dt)
simulated_paths <- simulate_GBM(x0 = x0,
                                mu = mu,
                                sigma = sigma,
                                n_sim = n_sim,
                                time_vector = time_vector)
plot_gbm <- ggplot(data_plot, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")
plot_gbm
#Figur 3.3
s0<-5
K<-10
r=0.06
sigma<-0.3
end_time <- 1
dt <- 1 / 50
n_sim<-10000
time_vector <- seq(0, end_time, by = dt)
montecarlo_paths<-matrix(NA,nrow = 10000,ncol = 10)
endvalue<-matrix(NA,nrow = 10000,ncol = 10)
option<-matrix(NA,nrow = 10000,ncol = 10)
for (i in 1:10){
  simulated_paths<-simulate_GBM(x0=s0,mu=r,sigma=sigma,n_sim=n_sim,time_vector = time_vector)
  endvalue[,i]<-simulated_paths[51,]
  option[,i]<-pmax(0,K-endvalue[,i])
 for (b in 1:10000){
  montecarlo_paths[b,i]<- exp(-r)*mean(option[1:b,i])
}
}
head(montecarlo_paths)
plot(1:10000,option)
data_plot <- data.frame("sim"=1:10000, "montecarlo" = montecarlo_paths)
data_plot <- melt(data_plot,  id = c('sim'))

plot_gbm <- ggplot(data_plot, aes(sim, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+ylim(4.2,4.7)
plot_gbm
# figur 3.4
montecarlo_paths<-matrix(NA,nrow = 10000,ncol = 10)
endvalue<-matrix(NA,nrow = 10000,ncol = 10)
option<-matrix(NA,nrow = 10000,ncol = 10)
endvalue_minus<-matrix(NA,10000,10)
option_minus<-matrix(NA,10000,10)
antithetic_paths<-matrix(NA,10000,10)
antithetic<-matrix(NA,10000,10)

for (i in 1:10){
  set.seed(i)
  simulated_paths<-simulate_GBM(x0=s0,mu=r,sigma=sigma,n_sim=n_sim,time_vector = time_vector)
  endvalue[,i]<-simulated_paths[51,]
  option[,i]<-pmax(0,K-endvalue[,i])
  
  
  simulated_paths_minus<-simulate_GBM_minus(x0=s0,mu=r,sigma=sigma,n_sim=n_sim,time_vector = time_vector)
    endvalue_minus[,i]<-simulated_paths_minus[51,]
  option_minus[,i]<-pmax(0,K-endvalue[,i])
  antithetic[,i]<-0.5*(option[,i]+option_minus[,i])
  for (b in 1:10000){
    antithetic_paths[b,i]<-exp(-r)*mean(antithetic[1:b,i])
  }
}


data_plot_3.4 <- data.frame("sim"=1:10000, "antithetic" = antithetic_paths)
data_plot_3.4antithetic <- melt(data_plot_antithetic,  id = c('sim'))

plot_3.4 <- ggplot(data_plot_3.4, aes(sim, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+ylim(4.2,4.7)
grid.arrange(plot_gbm,plot_gbm_antithetic,ncol=2)
#3.5
montecarlo_paths<-matrix(NA,nrow = 10000,ncol = 10)
endvalue<-matrix(NA,nrow = 10000,ncol = 10)
option<-matrix(NA,nrow = 10000,ncol = 10)
dim(simulated_paths)
for (i in 1:10){
  for (b in 1:10000){
    endvalue[,i]<-s0*exp((r-(sigma^2)/2)+sigma*rnorm(1,0,1))
    option[,i]<-pmax(0,K-endvalue[,i])
    montecarlo_paths[b,i]<- exp(-r)*mean(option[1:b,i])
  }
}