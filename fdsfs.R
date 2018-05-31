library(quantmod)
library(ggplot2)
library(reshape2)
#Henter S&P 100
getSymbols("^OEX",src="yahoo")
OEX
head(OEX)
tail(OEX)
qplot(OEX[,4])#Histogram OEX close

#Log return
u<-rep(NA,length(OEX[,4]))
OEXclose<-OEX$OEX.Close

for (i in 2:length(OEX[,4])){
  u[1]<-log(OEXclose[[2]]/OEXclose[[1]])
  u[i]<-log(OEXclose[[i]]/OEXclose[[i-1]])
}

logreturn<-c(NA,length(u)-1)
for (i in 1:length(u)-1){
  logreturn[i]<-u[i+1]
}
head(OEXclose)
# Plot af closing prices
plot(1:dim(OEXclose)[1], OEXclose, type = "l", xlab = "Observations")
#simulating paths of GBM of OEXclose
eleven_years<-OEXclose[1:2201,]
x0 <- OEXclose[[1]]
mu <- 0.02
sigma <- 0.5
n_sim <- 10
end_time <- 11
dt <- 1 / 200
time_vector <- seq(0, end_time, by = dt)
simulated_paths <- simulate_GBM(x0 = x0,
                                mu = mu,
                                sigma = sigma,
                                n_sim = n_sim,
                                time_vector = time_vector)
plot(time_vector, simulated_paths[, 1], type = "l")
data_plot <- data.frame('time' = time_vector, "GBM" = simulated_paths)
data_plot <- melt(data_plot,  id = c('time'))

plot_gbm <- ggplot(data_plot, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")
plot_gbm

dim(simulated_paths)
end_values<-simulated_paths[2201,1:10]
GBM_price<-mean(end_values)#Mean of simulated paths
Observed_price<-eleven_years[[2201]]#Observed value
abs(GBM_price-Observed_price)
 