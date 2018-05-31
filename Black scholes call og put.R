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
