#Monte Carlo simulation af asiatiske Call optioner
MC_Asian_call <- function(S0, K, sigma, r, n_sim, T) {
  dt = 1/1000
  simulated_paths <- simulated_paths2 <- AV_paths <- matrix(NA, 1000, n_sim)
  simulated_paths[1,] <- simulated_paths2[1,] <- AV_paths[1,] <- S0
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

simulate_asian_call(200,20,0.2,0.02,1000,1)
vec_asian<-Vectorize(MC_Asian_Callprice,c("s0","K","sigma","r"))

#Tjekker asiatiske priser med europæiske
strike<-seq(75,250,25)
Black_Scholes_callprice(T=1,s0=100,K=100,r=0.02,sigma=0.2,q=0)
MC_Asian_Callprice(s0=200,r=0.02,sigma=0.2,n_sim=1000,time_vector = seq(0,1,by=1/1000),K=20, dt=1/1000)


#Monte Carlo simulation af asiatiske put optioner
MC_Asian_put<- function(S0, K, sigma, r, n_sim, T) {
  dt = 1/1000
  simulated_paths <- simulated_paths2 <- AV_paths <- matrix(NA, 1000, n_sim)
  simulated_paths[1,] <- simulated_paths2[1,] <- AV_paths[1,] <- S0
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
