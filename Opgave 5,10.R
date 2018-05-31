#Monte Carlo simulation af asiatiske Call optioner
MC_Asian_Callprice <- function(s0, r, sigma, n_sim,K, time_vector, dt) {
  
  n_time  <- length(time_vector) # number of time points
  
  simulated_paths <- simulated_paths2 <- AV_paths <- matrix(NA, n_time, n_sim)
  simulated_paths[1,] <- simulated_paths2[1,] <- AV_paths[1,] <- s0
  
  riemann_value <- c(NA, n_sim)
  asiancallprice <- c(NA, n_sim)
  
  for (k in 1:n_sim) {
    for (i in 2:n_time) {
      z1 <- rnorm(1)
      z2 <- -z1
      simulated_paths[i, k] <- simulated_paths[i - 1, k]*exp((r-0.5*sigma^2)*dt+sigma*sqrt(dt)*z1)
      simulated_paths2[i, k] <- simulated_paths2[i - 1, k]*exp((r-0.5*sigma^2)*dt+sigma*sqrt(dt)*z2)
      AV_paths[i, k] <- (simulated_paths[i, k] + simulated_paths2[i, k])/2
    }
    riemann_value[k] <- sum(simulated_paths[,k]*dt)
    asiancallprice[k] <- exp(-r*(T))*pmax(riemann_value[k]-K,0)
  }
  
  price <- mean(asiancallprice)
  
  return(price)
}
vec_asian<-Vectorize(MC_Asian_Callprice,c("s0","K","sigma","r"))

#Tjekker asiatiske priser med europæiske
strike<-seq(75,250,25)
Black_Scholes_callprice(T=1,s0=200,K=75,r=0.02,sigma=0.2,q=0)
vec_asian(s0=200,r=0.02,sigma=0.2,n_sim=10000,time_vector = seq(0,1,1/200),K=75, dt=1/200)

#Monte Carlo simulation af asiatiske put optioner
MC_Asian_Callprice <- function(s0, r, sigma, n_sim, time_vector,K) {
  
  n_time  <- length(time_vector) # number of time points
  
  simulated_paths <- simulated_paths2 <- AV_paths <- matrix(NA, n_time, n_sim)
  simulated_paths[1,] <- simulated_paths2[1,] <- AV_paths[1,] <- s0
  
  riemann_value <- c(NA, n_sim)
  asiancallprice <- c(NA, n_sim)
  
  for (k in 1:n_sim) {
    for (i in 2:n_time) {
      z1 <- rnorm(1)
      z2 <- -z1
      simulated_paths[i, k] <- simulated_paths[i - 1, k]*exp((r-0.5*sigma^2)*dt+sigma*sqrt(dt)*z1)
      simulated_paths2[i, k] <- simulated_paths2[i - 1, k]*exp((r-0.5*sigma^2)*dt+sigma*sqrt(dt)*z2)
      AV_paths[i, k] <- (simulated_paths[i, k] + simulated_paths2[i, k])/2
    }
    riemann_value[k] <- sum(simulated_paths[,k]*dt)
    asiancallprice[k] <- exp(-r*(T))*pmax(riemann_value[k]-K,0)
  }
  
  price <- mean(asiancallprice)
  
  return(price)
}
