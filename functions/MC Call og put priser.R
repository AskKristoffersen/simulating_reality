
#Montecarlo put pricing med metode 1 uden varians reduktion
MC_Putprice_method1<-function(s0,r,sigma,n_sim,T,n_paths,K,dt){
  time_vector <- seq(0, T, by = dt)
  
montecarlo_paths<-matrix(NA,nrow = n_sim,ncol = n_paths)
endvalue<-matrix(NA,nrow = n_sim,ncol = n_paths)
option<-matrix(NA,nrow = n_sim,ncol = n_paths)

for (i in 1:n_paths){
  simulated_paths<-simulate_GBM(x0=s0,mu=r,sigma=sigma,n_sim=n_sim,time_vector = time_vector)
  endvalue[,i]<-simulated_paths[nrow(simulated_paths),]
  option[,i]<-pmax(0,K-endvalue[,i])
  for (b in 1:n_sim){
    montecarlo_paths[b,i]<- exp(-r)*mean(option[1:b,i])
  }
}
print(mean(montecarlo_paths[nrow(montecarlo_paths),]))
}

#Montecarlo Call pris med metode 1 uden varians reduktion.

MC_Callprice_method1<-function(s0,r,sigma,n_sim,n_paths,K,T,dt){
  
  time_vector <- seq(0, T, by = dt)
  
  montecarlo_paths<-matrix(NA,nrow = n_sim,ncol = n_paths)
  endvalue<-matrix(NA,nrow = n_sim,ncol = n_paths)
  option<-matrix(NA,nrow = n_sim,ncol = n_paths)
  
  for (i in 1:n_paths){
    simulated_paths<-simulate_GBM(x0=s0,mu=r,sigma=sigma,n_sim=n_sim,time_vector = time_vector)
    endvalue[,i]<-simulated_paths[nrow(simulated_paths),]
    option[,i]<-pmax(0,endvalue[,i]-K)
    for (b in 1:n_sim){
      montecarlo_paths[b,i]<- exp(-r)*mean(option[1:b,i])
    }
  }
  print(mean(montecarlo_paths[nrow(montecarlo_paths),]))
}

