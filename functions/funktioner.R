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
MC_Putprice_method2<-function(s0,r,sigma,n_sim,K,T,dt){
  
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

# funktion til udregning af implied volatilitet
implied.vol <-
  function(s0, K, T, r, price){
    sigma <- 0.20
    sigma.up <- 10
    sigma.down <- 0.001
    count <- 0
    err <- Black_Scholes_callprice(T,s0,K,r,sigma,q=0) - price
    while(abs(err) > 0.00001 && count<1000){
      if(err < 0){
        sigma.down <- sigma
        sigma <- (sigma.up + sigma)/2
      }else{
        sigma.up <- sigma
        sigma <- (sigma.down + sigma)/2
      }
      err <- Black_Scholes_callprice(T,s0,K,r,sigma,q=0) - price
      count <- count + 1
    }
    
    if(count==1000){
      return(NA)
    }else{
      return(sigma)
    }
  }


#simulation af assets
Assets<-function(dt=1/12,s0=100,r0=0.01,A0=1000,a=0.15,b=0.042,lambda=-0.23,sigma_r=0.01,rho=-0.15,mu=0.09,sigma=0.2, weights,n=1000){
  B <- (1 / a) * (1 - exp(-a * 1:10))
  xij <- rep(1/10,10)
  
  A <- matrix(NA,m+1,n)  
  A[1,] <- A0
  
  r <- matrix(0,m+1,n)  
  r[1,] <- r0
  
  for(j in 1:n){
    for(i in 2:121){
      dw1<-rnorm(n=121,mean=0,sd=1)
      dw3<-rho*dw1+sqrt(1-rho^2)*rnorm(n=121,mean=0,sd=1)
      dr <- a*(b-r[i-1,j])*dt + sigma_r*sqrt(dt)*dw1[i-1]
      r[i,j] <- r[i-1,j] + dr
      dA <-A[i-1,j]* (weights[1] *r[i-1,j]  * dt + weights[2] *(mu*dt+sigma*sqrt(dt)*dw3[i-1]) + 
                        sum(xij * weights[3] * ((r[i-1,j] - lambda * sigma_r * B) * dt - sigma_r * B *sqrt(dt)* dw1[i-1])))
      A[i,j] <- A[i-1,j] + dA
    }
  } 
  return(list(A,r))
}

#liabilities
L<-function(t){
  Liability<-premium*(1+q)^t
  return(Liability) 
}

#nulkupon priser

VasicekZCBprice <- 
  function(r0=0.01, a=0.15, b=0.042, sigma_r=0.01, T=10,t,lambda=-0.23,r){
    b.vas <- (1/a)*(1-exp(-(T-t)*a)) 
    a.vas <- (sigma_r^2/(2*a^2)-b+(lambda*sigma_r)/a)*((T-t)-b.vas)-sigma_r^2/(4*a)*b.vas^2
    return(exp(a.vas-b.vas*r))
  }
