library(reshape2)
library(ggplot2)
require(gridExtra)
library(pracma)
# simulerer assets

#Model parameter
s0<- 100
r0<- 0.01
a<- 0.15
b<- 0.042
lambda<- -0.23
sigma_r<- 0.01
rho<- -0.15
mu<-0.107
sigma<-0.145
A0<-1000

n <- 10    # MC simulation trials
T <- 10    # total time
m <- 120   # subintervals
dt <- T/m 
  


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
#plotter 100% i banken
A<-Assets(weights = c(10/10,0/10,0/10),n=10)

time_vector<-seq(0,T,by=dt)
data_plot<- data.frame("time"=time_vector, "paths" = A[[1]])
data_plot<- melt(data_plot,  id = c('time'))

plot_bank <- ggplot(data_plot, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+
ggtitle("100 % Banken ")

#plotter 100% aktier
A<-Assets(weights = c(0/10,10/10,0/10),n=10)

time_vector<-seq(0,T,by=dt)
data_plot<- data.frame("time"=time_vector, "paths" = A[[1]])
data_plot<- melt(data_plot,  id = c('time'))

plot_aktier <- ggplot(data_plot, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+
  ggtitle("100 % Aktier ")

#plotter 100% i obligationer

A<-Assets(weights = c(0/10,0/10,10/10),n=10)

time_vector<-seq(0,T,by=dt)
data_plot<- data.frame("time"=time_vector, "paths" = A[[1]])
data_plot<- melt(data_plot,  id = c('time'))

plot_obli <- ggplot(data_plot, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+
  ggtitle("100 % Obligationer ")





#6.3
#Quantiles for 90% bond 10 % stok
list<-Assets(weights = c(0/10,1/10,9/10))
A<-list[[1]]

meanvector<-rep(NA,121)
for (i in 1:121){
  meanvector[i]<-mean(A[i,])
}

qs <- apply(A, 1, quantile, probs=c(0.005, 0.5, 0.995))
quantiles<-t(rbind(qs,meanvector))
colnames(quantiles)<-c("0.5%","50%", "99.5%","Mean")


time_vector<-seq(0,T,by=dt)
data_plot_6.3<- data.frame("time"=time_vector, "quantiles" = quantiles)
data_plot_6.3<- melt(data_plot_6.3,  id = c('time'))

plot_A_quantiles <- ggplot(data_plot_6.3, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  ggtitle("90% bonds og 10% aktier  ")

plot_A_quantiles+scale_color_hue(name = "Quantiles",labels = c("0.5%","50%", "99.5%","Mean"))

# 90%aktier 10% bonds
list<-Assets(weights = c(0/10,9/10,1/10))
A<-list[[1]]

meanvector<-rep(NA,121)
for (i in 1:121){
  meanvector[i]<-mean(A[i,])
}

qs <- apply(A, 1, quantile, probs=c(0.005, 0.5, 0.995))
quantiles<-t(rbind(qs,meanvector))
colnames(quantiles)<-c("0.5%","50%", "99.5%","Mean")


time_vector<-seq(0,T,by=dt)
data_plot_6.3<- data.frame("time"=time_vector, "quantiles" = quantiles)
data_plot_6.3<- melt(data_plot_6.3,  id = c('time'))

plot_A_quantiles <- ggplot(data_plot_6.3, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  ggtitle("90% aktier 10% obligationer  ")

plot_A_quantiles+scale_color_hue(name = "Quantiles",labels = c("0.5%","50%", "99.5%","Mean"))

#fordelingsplot 10000 simulationer 90 %akiter


n<-10000
list<-Assets(weights = c(0/10,9/10,1/10),n=n)
A<-list[[1]]

asset_dist <- data.frame(year = factor(rep(c("1","3","6","10"), each=n)), 
                         value = c(A[13,], A[37,], A[73,], A[121,]))
ggplot(asset_dist, aes(x=value, fill=year)) + geom_density(alpha=.3)+ggtitle("Fodelingsplot med 90% aktier og 10% obligationer")

#90%obligationer

n<-10000
list<-Assets(weights = c(0/10,1/10,9/10),n=n)
A<-list[[1]]

asset_dist <- data.frame(year = factor(rep(c("1","3","6","10"), each=n)), 
                         value = c(A[13,], A[37,], A[73,], A[121,]))
ggplot(asset_dist, aes(x=value, fill=year)) + geom_density(alpha=.3)+ggtitle("Fodelingsplot med 90% obligationer og 10% aktier")

# Opgave 6.4

n<-1000

premium<-1000
q<-0.025


L<-function(t){
  Liability<-premium*(1+q)^t
  return(Liability) 
}
L_T<-L(10)

probality_bonds<-rep(NA,11)

list<-Assets(A0=1000,weights = c(0/10,0/10,10/10),n=n)
A<-list[[1]]
probality_bonds[1]<-sum(A[121,]<L(10))/n

list<-Assets(A0=1000,weights = c(0/10,1/10,9/10),n=n)
A<-list[[1]]
probality_bonds[2]<-sum(A[121,]<L(10))/n

list<-Assets(A0=1000,weights = c(0/10,2/10,8/10),n=n)
A<-list[[1]]
probality_bonds[3]<-sum(A[121,]<L(10))/n

list<-Assets(A0=1000,weights = c(0/10,3/10,7/10),n=n)
A<-list[[1]]
probality_bonds[4]<-sum(A[121,]<L(10))/n

list<-Assets(A0=1000,weights = c(0/10,4/10,6/10),n=n)
A<-list[[1]]
probality_bonds[5]<-sum(A[121,]<L(10))/n

list<-Assets(A0=1000,weights = c(0/10,5/10,5/10),n=n)
A<-list[[1]]
probality_bonds[6]<-sum(A[121,]<L(10))/n

list<-Assets(A0=1000,weights = c(0/10,6/10,4/10),n=n)
A<-list[[1]]
probality_bonds[7]<-sum(A[121,]<L(10))/n

list<-Assets(A0=1000,weights = c(0/10,7/10,3/10),n=n)
A<-list[[1]]
probality_bonds[8]<-sum(A[121,]<L(10))/n

list<-Assets(A0=1000,weights = c(0/10,8/10,2/10),n=n)
A<-list[[1]]
probality_bonds[9]<-sum(A[121,]<L(10))/n

list<-Assets(A0=1000,weights = c(0/10,9/10,1/10),n=n)
A<-list[[1]]
probality_bonds[10]<-sum(A[121,]<L(10))/n

list<-Assets(A0=1000,weights = c(0/10,10/10,0/10),n=n)
A<-list[[1]]
probality_bonds[11]<-sum(A[121,]<L(10))/n

Aktieandel<-seq(0,100,10)
qplot(Aktieandel,probality_bonds)+ggtitle("P(A(T)<L(T))")
#6.6

VasicekZCBprice <- 
function(r0=0.01, a=0.15, b=0.042, sigma_r=0.01, T=10,t,lambda=-0.23,r){
  b.vas <- (1/a)*(1-exp(-(T-t)*a)) 
  a.vas <- (sigma_r^2/(2*a^2)-b+(lambda*sigma_r)/a)*((T-t)-b.vas)-sigma_r^2/(4*a)*b.vas^2
  return(exp(a.vas-b.vas*r))
}




list<-Assets(weights = c(0,1/10,9/10), A0=1000)
A<-list[[1]]
r<-list[[2]]

#Bonus quantile plot

B<-matrix(NA,121,n)
for (j in 0:n-1){
  for (i in 1:121){
    B[i,j]<-max(A[i,j]-VasicekZCBprice(t=i/12,r=r[i,j])*L_T,0)
  }
}

meanvector_B<-rep(NA,121)
for(i in 1:121){
  meanvector_B[i]<-mean(B[i,])
}

qs <- apply(B, 1, quantile, probs=c(0.005, 0.5, 0.995))
quantiles<-t(rbind(qs,meanvector_B))
colnames(quantiles)<-c("0.5%","50%", "99.5%","Mean")


time_vector<-seq(0,T,by=dt)
data_plot_6.5<- data.frame("time"=time_vector, "quantiles" = quantiles)
data_plot_6.5<- melt(data_plot_6.5,  id = c('time'))

Quantilplot_B <- ggplot(data_plot_6.5, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  ggtitle("B fraktiler ")

Quantilplot_B+scale_color_hue(name = "Quantiles",labels = c("0.5%","50%", "99.5%","Mean"))

#SCR function

Loss<-matrix(NA,109,n)
for (j in 1:n){
  for (i in 1:109){
    Loss[i,j]<-B[i,j]-exp(-mean((r[i:(i+11),])))*B[i+12,j]
  }
}

meanvector_Loss<-rep(NA,109)
for (i in 1:109){
  meanvector_Loss[i]<-mean(Loss[i,])
}


qs <- apply(Loss, 1, quantile, probs=c(0.005, 0.5, 0.995))
quantiles<-t(rbind(qs,meanvector_Loss))
colnames(quantiles)<-c("0.5%","50%", "99.5%","Mean")

SCR<-quantiles[,3]

qplot(seq(0,9,dt), SCR)+geom_line()+ggtitle("SCR plot")

CR<-matrix(NA,109,n)

for(j in 1:n){
  for (i in 1:109){
    CR[i,j]<-B[i,j]/SCR[i]
  }
}



qs <- apply(CR, 1, quantile, probs=c(0.005, 0.5, 0.995))
quantiles<-t(qs)
colnames(quantiles)<-c("0.5%","50%", "99.5%")

time_vector<-seq(0,9,by=dt)
data_plot_6.6<- data.frame("time"=time_vector, "quantiles" = quantiles)
data_plot_6.6<- melt(data_plot_6.6,  id = c('time'))

Quantilplot_CR <- ggplot(data_plot_6.6, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  ggtitle("Fraktiler CR  ")

Quantilplot_CR+scale_color_hue(name = "Quantiles",labels = c("0.5%","50%", "99.5%","Mean"))

TF<-CR<1


for (k in 1:n){
  for(i in 1:109){
    if (TF[i,k]==TRUE){
      TF[i:109,k]=TRUE
    }
  }
}
TF[,1]
prob<-rep(NA,109)
prob[1]<-0
for (i in 1:109){
  prob[i]<-sum(TF[i,])/n
}




qplot(time_vector, prob, ylab="Sandynligheden for insolvens")+ggtitle("Sandynligheden for insolvens til tid t")

#ZCP hedge

A0ny<-1000-VasicekZCBprice(t=0/12,r=r0)*L_T
invest<-Assets(A0=A0ny,weights = c(0,1/10,9/10))
Assets_invest<-invest[[1]]
rZCB<-invest[[2]]

#plot assest
time_vector<-seq(0,T,by=dt)
data_plot<- data.frame("time"=time_vector, "paths" = Assets_invest)
data_plot<- melt(data_plot,  id = c('time'))

plot <- ggplot(data_plot, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  theme(legend.position = "none")+
  ggtitle("Plot ")
plot

Assets_values<-matrix(NA,121,n)
for (k in 1:n){
  for (i in 1:121){
    Assets_values[i,k]<-Assets_invest[i,k]+VasicekZCBprice(t=i-1/12,r=rZCB[i,k])*L_T 
  }
}




B_ZCB<-matrix(NA,121,n)
for (j in 1:n){
  for (i in 1:121){
    B_ZCB[i,j]<-max(Assets_values[i,j]-VasicekZCBprice(t=i-1/12,r=rZCB[i,j])*L_T,0)
  }
}

meanvector_B_ZCB<-rep(NA,121)
for(i in 1:121){
  meanvector_B_ZCB[i]<-mean(B_ZCB[i,])
}

qs <- apply(B_ZCB, 1, quantile, probs=c(0.005, 0.5, 0.995))
quantiles<-t(rbind(qs,meanvector_B_ZCB))
colnames(quantiles)<-c("0.5%","50%", "99.5%","Mean")


time_vector<-seq(0,T,by=dt)
data_plot_ZCB<- data.frame("time"=time_vector, "quantiles" = quantiles)
data_plot_ZCB<- melt(data_plot_ZCB,  id = c('time'))

Quantilplot_B_ZCB <- ggplot(data_plot_ZCB, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  ggtitle("Fraktilplot nulkupon obligation hedge  ")

Quantilplot_B_ZCB+scale_color_hue(name = "Quantiles",labels = c("0.5%","50%", "99.5%","Mean"))

#SCR function

Loss_ZCB<-matrix(NA,109,n)
for (j in 1:n){
  for (i in 1:109){
    Loss_ZCB[i,j]<-B_ZCB[i,j]-exp(-mean((rZCB[i:(i+11),])))*B_ZCB[i+12,j]
  }
}


meanvector_Loss_ZCB<-rep(NA,109)
for (i in 1:109){
  meanvector_Loss_ZCB[i]<-mean(Loss_ZCB[i,])
}


qs <- apply(Loss_ZCB, 1, quantile, probs=c(0.005, 0.5, 0.995))
quantiles<-t(rbind(qs,meanvector_Loss_ZCB))
colnames(quantiles)<-c("0.5%","50%", "99.5%","Mean")

SCR_ZCB<-quantiles[,3]

qplot(seq(0,9,dt), SCR_ZCB)+geom_line()+ggtitle("SCR plot nulkupon hedge")

CR_ZCB<-matrix(NA,109,n)

for(j in 1:n){
  for (i in 1:109){
    CR_ZCB[i,j]<-B_ZCB[i,j]/SCR_ZCB[i]
  }
}



qs <- apply(CR_ZCB, 1, quantile, probs=c(0.005, 0.5, 0.995))
quantiles<-t(qs)
colnames(quantiles)<-c("0.5%","50%", "99.5%")

time_vector<-seq(0,9,by=dt)
data_plot_CR_ZCB<- data.frame("time"=time_vector, "quantiles" = quantiles)
data_plot_CR_ZCB<- melt(data_plot_CR_ZCB,  id = c('time'))

Quantilplot_CR_ZCB <- ggplot(data_plot_CR_ZCB, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  ggtitle("Fraktilplot nulkupon hedge CR ")

Quantilplot_CR_ZCB+scale_color_hue(name = "Quantiles",labels = c("0.5%","50%", "99.5%","Mean"))


#Zero Coupon swap hedge

Assets_swap<-function(dt=1/12,s0=100,r0=0.01,A0=1000,a=0.15,b=0.042,lambda=-0.23,sigma_r=0.01,rho=-0.15,mu=0.09,sigma=0.2, weights=c(0,1/10,9/10),n=1000){
  B <- (1 / a) * (1 - exp(-a * 1:10))
  xij <- rep(1/10,10)
  
  A <- matrix(NA,m+1,n)  
  A[1,] <- A0
  
  r <- matrix(0,m+1,n)  
  r[1,] <- r0
  
  betaling<-matrix(0,m+1,n)
  betaling[1,]<-VasicekZCBprice(t=1/12,r=r0)*L_T*r0*dt
  
  
  for(j in 1:n){
    for(i in 2:121){
      dw1<-rnorm(n=121,mean=0,sd=1)
      dw3<-rho*dw1+sqrt(1-rho^2)*rnorm(n=121,mean=0,sd=1)
      dr <- a*(b-r[i-1,j])*dt + sigma_r*sqrt(dt)*dw1[i-1]
      r[i,j] <- r[i-1,j] + dr
      dA <-A[i-1,j]* (weights[1] *r[i-1,j]  * dt + weights[2] *(mu*dt+sigma*sqrt(dt)*dw3[i-1]) + 
                        sum(xij * weights[3] * ((r[i-1,j] - lambda * sigma_r * B) * dt - sigma_r * B *sqrt(dt)* dw1[i-1])))
      betaling[i,j]<-VasicekZCBprice(t=1/12,r=r0)*L_T*r[i-1,j]*dt
      A[i,j] <- A[i-1,j] + dA - betaling[i,j]
    }
  } 
  return(list(A,r,betaling))
}
swap_invest<-Assets_swap()
r_swap<-swap_invest[[2]]
Aswap_invest<-swap_invest[[1]]
betaling<-swap_invest[[3]]



Nyassets_swap<-matrix(NA,121,n)
for(k in 1:n){
  for(i in 1:121){
    Nyassets_swap[i,k]<-Aswap_invest[i,k]+VasicekZCBprice(t=i/12,r=r_swap[i,k])*L_T
  }
}


B_swap<-matrix(NA,121,n)
for (j in 1:n){
  for (i in 1:121){
    B_swap[i,j]<-max(Nyassets_swap[i,j]-(VasicekZCBprice(t=i/12,r=r_swap[i,j])*L_T+VasicekZCBprice(t=1/12,r=r0)*L_T),0)
  }
}

meanvector_B_swap<-rep(NA,121)
for(i in 1:121){
  meanvector_B_swap[i]<-mean(B_swap[i,])
}

qs <- apply(B_swap, 1, quantile, probs=c(0.005, 0.5, 0.995))
quantiles<-t(rbind(qs,meanvector_B_swap))
colnames(quantiles)<-c("0.5%","50%", "99.5%","Mean")


time_vector<-seq(0,T,by=dt)
data_plot_swap<- data.frame("time"=time_vector, "quantiles" = quantiles)
data_plot_swap<- melt(data_plot_swap,  id = c('time'))

Quantilplot_B_swap <- ggplot(data_plot_swap, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  ggtitle("Quantile plot B_swap ")

Quantilplot_B_swap+scale_color_hue(name = "Quantiles",labels = c("0.5%","50%", "99.5%","Mean"))


#SCR

Loss_swap<-matrix(NA,109,n)
for (j in 1:n){
  for (i in 1:109){
    Loss_swap[i,j]<-B_swap[i,j]-exp(-mean((r_swap[i:(i+11),])))*B_swap[i+12,j]
  }
}

meanvector_Loss_swap<-rep(NA,109)
for (i in 1:109){
  meanvector_Loss_swap[i]<-mean(Loss_swap[i,])
}


qs <- apply(Loss_swap, 1, quantile, probs=c(0.005, 0.5, 0.995))
quantiles<-t(rbind(qs,meanvector_Loss_swap))
colnames(quantiles)<-c("0.5%","50%", "99.5%","Mean")

SCR_swap<-quantiles[,3]

qplot(seq(0,9,dt), SCR_swap)+geom_line()+ggtitle("SCR plot nulkupon swap hedge")

CR_swap<-matrix(NA,109,n)

for(j in 1:n){
  for (i in 1:109){
    CR_swap[i,j]<-B_swap[i,j]/SCR_swap[i]
  }
}



qs <- apply(CR_swap, 1, quantile, probs=c(0.005, 0.5, 0.995))
quantiles<-t(qs)
colnames(quantiles)<-c("0.5%","50%", "99.5%")

time_vector<-seq(0,9,by=dt)
data_plot_CR_swap<- data.frame("time"=time_vector, "quantiles" = quantiles)
data_plot_CR_swap<- melt(data_plot_CR_swap,  id = c('time'))

Quantilplot_CR_swap <- ggplot(data_plot_CR_swap, aes(time, value)) +
  geom_line(aes(colour = variable)) +
  ggtitle("Quantile plot CR ZCB  swap ")

Quantilplot_CR_swap+scale_color_hue(name = "Quantiles",labels = c("0.5%","50%", "99.5%","Mean"))
#sandsynlighedsplot
TF_swap<-CR_swap<1


for (k in 1:n){
  for(i in 1:109){
    if (TF_swap[i,k]==TRUE){
      TF_swap[i:109,k]=TRUE
    }
  }
}
TF[,1]
prob_swap<-rep(NA,109)
prob_swap[1]<-0
for (i in 1:109){
  prob_swap[i]<-sum(TF_swap[i,])/n
}


qplot(time_vector, prob_swap, ylab ="sandsynlighed for insolvens")+ggtitle("Sandsynlighedsplot")

#6.9

Assets_underQ<-function(dt=1/12,s0=100,r0=0.01,A0=1000,a=0.15,b=0.042,lambda=-0.23,sigma_r=0.01,rho=-0.15,mu=0.09,sigma=0.2, weights,n=1000){
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
      dr <- a*(b-(lambda*sigma_r)/a-r[i-1,j])*dt + sigma_r*sqrt(dt)*dw1[i-1]
      r[i,j] <- r[i-1,j] + dr
      dA <-A[i-1,j]* (weights[1] *r[i-1,j]  * dt + weights[2] *(r[i-1,j]*dt+sigma*sqrt(dt)*dw3[i-1]) + 
                        sum(xij * weights[3] * (r[i-1,j] * dt - sigma_r * B *sqrt(dt)* dw1[i-1])))
      A[i,j] <- A[i-1,j] + dA
    }
  } 
  return(list(A,r))
}

list<-Assets_underQ(weights = c(0,1/10,9/10))
AQ<-list[[1]]
bonusoption<-rep(NA,1000)
for (i in 1:1000){
  bonusoption[i]<-max(AQ[121,i]-L(10),0)
}
ydelse<-VasicekZCBprice(t=0,r=r0)*mean(bonusoption)


L<-function(t){
  Liability<-premium*(1+q)^t
  return(Liability) 
}

bonusoptions2<-matrix(NA,1000,10)
for (k in 1:1000){
  for (i in 1:10){
    bonusoptions2[k,i]<-max(AQ[i*12,k]-L(i),0)
  }
}
meanbonus<-rep(NA,10)
for (i in 1:10){
  meanbonus[i]<-mean(bonusoptions2[,i])
}
diskon<-VasicekZCBprice(t=1,r=r0)*meanbonus
sum(diskon)
# finder eta

bisect(function(eta){return(VasicekZCBprice(t=0,r=r0)*(L(10)+eta*(mean(bonusoption)))-1000)},0,1)

  