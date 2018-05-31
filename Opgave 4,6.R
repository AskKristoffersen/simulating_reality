library(ggplot2)
library(reshape2)


#parametre
x0 <- 100
mu <- 0.03
sigma <- 0.5
n_sim <- 10
end_time <- 10
dt <- 1 
time_vector <- seq(0, end_time, by = dt)


simulated_paths <- simulate_GBM(x0 = x0,
                                mu = mu,
                                sigma = sigma,
                                n_sim = n_sim,
                                time_vector = time_vector)

data_plot <- data.frame('time' = time_vector, "GBM" = simulated_paths)
data_plot <- melt(data_plot,  id = c('time'))

plot_gbm <- ggplot(data_plot, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+
  ggtitle("Method A")
plot_gbm
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
plot_gbm2

grid.arrange(plot_gbm, plot_gbm2, ncol=2)
