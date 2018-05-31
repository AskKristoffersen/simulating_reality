library(reshape2)
library(ggplot2)
#Opgave 4.6

x0 <- 50
mu <- 0.06
sigma <- 0.3
n_sim <- 50
end_time <- 1
dt <- 0.01
error<-function(mu, sigma, n_sim, end_time, dt, x0){
  time_vector <- seq(0, end_time, by = dt)
  
  n_time  <- length(time_vector) # number of time points
  
  simulated_paths <- matrix(NA, n_time, n_sim)
  simulated_paths[1,] <- x0
  simulated_paths2 <- matrix(NA, n_time, n_sim)
  simulated_paths2[1,] <- x0
  for (k in 1:n_sim) {
    for (i in 2:n_time) {
      w1<-rnorm(1,0,1)
      dx <- mu * simulated_paths[i - 1, k] * dt + simulated_paths[i - 1, k] * sigma * sqrt(dt) * w1
      simulated_paths[i, k] <- simulated_paths[i - 1, k] + dx
      simulated_paths2[i, k] <- simulated_paths[i - 1, k]*exp((mu-0.5*sigma^2)*dt+sigma*sqrt(dt)*w1)
    }
  }
  return(1/n_time*sum(abs(simulated_paths2[n_time,]-simulated_paths[n_time,])))
}
p<-error(mu=mu, sigma=sigma, n_sim=n_sim, end_time=end_time, dt=dt, x0=x0)
vec_error<-Vectorize(error, c("dt"))
vec_error(dt=c(0.01,0.005,0.002,0.001,0.0005),mu=mu, sigma=sigma, n_sim=n_sim, end_time=end_time, x0=x0)

error<-matrix(NA,5, 5)
error[5,2]<-1/n_sim*sum(abs(simulated_paths2[n_time,]-simulated_paths[n_time,]))

