#s0 er stock price value
#t er sluttidspunkt
#K er striken
Black_Scholes_callprice<-function(T,s0,K,r,sigma,q){
  d1<-(log(s0/K)+(r-q+0.5*sigma^2)*T)/(sigma*sqrt(T))
  d2<-d1-sigma*sqrt(T)
  call_price<-exp(-r*T)*(s0*exp((r-q)*T)*pnorm(d1,0,1)-K*pnorm(d2,0,1))
  return(call_price)
}




Black_Scholes_putprices<-function(T,s0,K,r,sigma,q){
  d1<-(log(s0/K)+(r-q+0.5*sigma^2)*T)/(sigma*sqrt(T))
  d2<-d1-sigma*sqrt(T)
  call_price<-exp(-r*T)*(s0*exp((r-q)*T)*pnorm(d1,0,1)-K*pnorm(d2,0,1))
  put_price<-call_price-exp(-T*q)*s0+K*exp(-r*T)
  return(put_price)
}


#Montecarlo put pricing med metode 1 uden varians reduktion
MC_putprice_method2<-function(s0,r,sigma,n_sim,K,T,dt){
  
  time_vector <- seq(0, T, by = dt)
  
  option<-rep(NA,n_sim)
  
  
  simulated_paths<-Method2_AV(x0=s0,mu=r,sigma=sigma,n_sim=n_sim,time_vector = time_vector,dt=dt)
  sim1<-simulated_paths[[1]]
  sim2<-simulated_paths[[2]]
  option<-1/2*(pmax(0,K-sim1[length(time_vector),])+pmax(0,K-sim2[length(time_vector),]))
  montecarlo_paths<- exp(-r*T)*mean(option)
  return(montecarlo_paths)
}

#Montecarlo Call pris med metode 1 uden varians reduktion.

MC_Callprice_method2<-function(s0,r,sigma,n_sim,K,T,dt){
  
  time_vector <- seq(0, T, by = dt)
  
  option<-rep(NA,n_sim)
  
  
  simulated_paths<-Method2_AV(x0=s0,mu=r,sigma=sigma,n_sim=n_sim,time_vector = time_vector,dt=dt)
  sim1<-simulated_paths[[1]]
  sim2<-simulated_paths[[2]]
  option<-1/2*(pmax(0,sim1[length(time_vector),]-K)+pmax(0,sim2[length(time_vector),]-K))
  montecarlo_paths<- exp(-r*T)*mean(option)
  return(montecarlo_paths)
}




#' Simulate GBM
#'
#' @param x0 - inital value of stochastic process (numeric)
#' @param mu - drift (numeric)
#' @param sigma - volatility (numeric)
#' @param n_sim - number of simulations (integer)
#' @param time_vector - time points for each simulation (numeric vector)
#'
#' @return matrix containing simulated paths in each column (numeric matrix)

simulate_GBM <- function(x0, mu, sigma, n_sim, time_vector) {
  
  n_time  <- length(time_vector) # number of time points
  
  simulated_paths <- matrix(NA, n_time, n_sim)
  simulated_paths[1,] <- x0
  
  for (k in 1:n_sim) {
    for (i in 2:n_time) {
      dx <- mu * simulated_paths[i - 1, k] * dt + simulated_paths[i - 1, k] * sigma * sqrt(dt) * rnorm(1, 0, 1)
      simulated_paths[i, k] <- simulated_paths[i - 1, k] + dx
    }
  }
  
  return(simulated_paths)
  
}

simulate_GBM_AV_euler <- function(x0, mu, sigma, n_sim, time_vector) {
  
  n_time  <- length(time_vector) # number of time points
  
  simulated_paths <- simulated_paths2 <- AV_paths <- matrix(NA, n_time, n_sim)
  simulated_paths[1,] <- simulated_paths2[1,] <- AV_paths[1,] <- x0
  
  
  for (k in 1:n_sim) {
    for (i in 2:n_time) {
      z1 <- rnorm(1)
      z2 <- -z1
      dx1 <- mu * simulated_paths[i - 1, k] * dt + simulated_paths[i - 1, k] * sigma * sqrt(dt) * z1
      simulated_paths[i, k] <- simulated_paths[i - 1, k] + dx1
      dx2 <- mu * simulated_paths2[i - 1, k] * dt + simulated_paths2[i - 1, k] * sigma * sqrt(dt) * z2
      simulated_paths2[i, k] <- simulated_paths2[i - 1, k] + dx2
    }
  }
  
  return(list(simulated_paths,simulated_paths2))
  
}
#Analytiske metode
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

