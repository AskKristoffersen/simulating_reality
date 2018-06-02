
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
