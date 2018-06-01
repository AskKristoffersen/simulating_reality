Method2 <- function(x0, mu, sigma, n_sim, time_vector) {
  
  n_time  <- length(time_vector) # number of time points
  
  simulated_paths <- matrix(NA, n_time, n_sim)
  simulated_paths[1,] <- x0
  
  for (k in 1:n_sim) {
    for (i in 2:n_time) {
      simulated_paths[i, k] <- simulated_paths[i - 1, k]*exp((mu-0.5*sigma^2)*dt+sigma*sqrt(dt)*rnorm(1,0,1))
    }
  }
  
  return(simulated_paths)
  
}

Method2_AV <- function(x0, mu, sigma, n_sim, time_vector,dt) {
  
  n_time  <- length(time_vector) # number of time points
  
  simulated_paths <- simulated_paths2 <- AV_paths <- matrix(NA, n_time, n_sim)
  simulated_paths[1,] <- simulated_paths2[1,] <- AV_paths[1,] <- x0
  
  
  for (k in 1:n_sim) {
    for (i in 2:n_time) {
      z1 <- rnorm(1)
      z2 <- -z1
      simulated_paths[i, k] <- simulated_paths[i - 1, k]*exp((mu-0.5*sigma^2)*dt+sigma*sqrt(dt)*z1)
      simulated_paths2[i, k] <- simulated_paths2[i - 1, k]*exp((mu-0.5*sigma^2)*dt+sigma*sqrt(dt)*z2)
    }
  }
  
  return(list(simulated_paths,simulated_paths2))
  
}
