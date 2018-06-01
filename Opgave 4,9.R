require(gridExtra)
library(reshape2)
library(ggplot2)
#Figur 3.3
#eksempel 3.7
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
