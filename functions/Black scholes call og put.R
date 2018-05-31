#s0 er stock price value
#t er sluttidspunkt
#K er striken
# dette er uden divedender og med startidspunkt 0
Black_Scholes_callprice<-function(T,s0,K,r,sigma){
  d1<-(log(s0/K)+(r+0.5*sigma^2)*T)/(sigma*sqrt(T))
  d2<-d1-sigma*sqrt(T)
  call_price<-exp(-r*T)*(s0*exp(r*T)*pnorm(d1,0,1)-K*pnorm(d2,0,1))
  return(call_price)
}
Black_Scholes_callprice(T=0.25,s0=40,K=40,r=0.08,sigma=0.3)

Black_Scholes_putprices<-function(T,s0,K,r,sigma){
  d1<-(log(s0/K)+(r+0.5*sigma^2)*T)/(sigma*sqrt(T))
  d2<-d1-sigma*sqrt(T)
  call_price<-exp(-r*T)*(s0*exp(r*T)*pnorm(d1,0,1)-K*pnorm(d2,0,1))
  put_price<-call_price-exp(-T)*s0+K*exp(-r*T)
  return(put_price)
}