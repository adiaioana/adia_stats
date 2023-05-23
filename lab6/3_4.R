tconfidence_interval=function(es_mean,es_sd,alfa,n){
  critical_t=qt(1-alfa/2, n-1)
  a=es_mean-critical_t*es_sd/sqrt(n)
  b=es_mean+critical_t*es_sd/sqrt(n)
  interval=c(a,b) 
  return (interval)
}


x=scan("history.txt"); 
m=mean(x)
s=sd(x)
n=length(x)
alfa=0.05
print(tconfidence_interval(m,s,alfa,n))