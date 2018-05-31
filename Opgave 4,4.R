#Opgave 4.4
library(reshape2)
library(ggplot2)
require(gridExtra)
x0 <- 100
mu <- 0.03
sigma <- 0.5
n_sim <- 10
end_time <- 10
dt <- 1
time_vector <- seq(0, end_time, by = dt)


#method1
simulated_paths1 <- simulate_GBM(x0 = x0,
                                 mu = mu,
                                 sigma = sigma,
                                 n_sim = n_sim,
                                 time_vector = time_vector)
data_plot1 <- data.frame('time' = time_vector, "GBM" = simulated_paths1)
data_plot1 <- melt(data_plot1,  id = c('time'))

plot_gbm1 <- ggplot(data_plot1, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+ggtitle("Method A")

#method 2

simulated_paths2 <- Method2(x0 = x0,
                                mu = mu,
                                sigma = sigma,
                                n_sim = n_sim,
                                time_vector = time_vector)

data_plot2 <- data.frame('time' = time_vector, "GBM" = simulated_paths2)
data_plot2 <- melt(data_plot2,  id = c('time'))


plot_gbm2 <- ggplot(data_plot2, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+ggtitle("Method B")


#plot forskel på methode 1 og 2

grid.arrange(plot_gbm1,plot_gbm2, ncol=2)
#plot med en af hver
plot(time_vector, simulated_paths1[, 1], type = "l")
lines(time_vector,simulated_paths2[, 1],col="red")


#1/10
dt <- 1/10
time_vector <- seq(0, end_time, by = dt)

simulated_paths3 <- simulate_GBM(x0 = x0,
                                 mu = mu,
                                 sigma = sigma,
                                 n_sim = n_sim,
                                 time_vector = time_vector)
data_plot3 <- data.frame('time' = time_vector, "GBM" = simulated_paths3)
data_plot3 <- melt(data_plot3,  id = c('time'))

plot_gbm3 <- ggplot(data_plot3, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+ggtitle("Method A")


simulated_paths4 <- Method2(x0 = x0,
                            mu = mu,
                            sigma = sigma,
                            n_sim = n_sim,
                            time_vector = time_vector)

data_plot4 <- data.frame('time' = time_vector, "GBM" = simulated_paths4)
data_plot4 <- melt(data_plot4,  id = c('time'))


plot_gbm4 <- ggplot(data_plot4, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+ggtitle("Method B")

#1/100
dt <- 1/100
time_vector <- seq(0, end_time, by = dt)

simulated_paths5 <- simulate_GBM(x0 = x0,
                                 mu = mu,
                                 sigma = sigma,
                                 n_sim = n_sim,
                                 time_vector = time_vector)
data_plot5 <- data.frame('time' = time_vector, "GBM" = simulated_paths5)
data_plot5 <- melt(data_plot5,  id = c('time'))

plot_gbm5 <- ggplot(data_plot5, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+ggtitle("Method A")


simulated_paths6 <- Method2(x0 = x0,
                            mu = mu,
                            sigma = sigma,
                            n_sim = n_sim,
                            time_vector = time_vector)

data_plot6 <- data.frame('time' = time_vector, "GBM" = simulated_paths6)
data_plot6 <- melt(data_plot6,  id = c('time'))


plot_gbm6 <- ggplot(data_plot6, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+ggtitle("Method B")

grid.arrange(plot_gbm1,plot_gbm3,plot_gbm5,plot_gbm2,plot_gbm4,plot_gbm6, ncol=3)

