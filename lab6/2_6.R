

zconfidence_interval=function(s_mean,alfa,sigma,n){
  critical_z=qnorm(1-alfa/2, 0, 1)
  a=s_mean-critical_z*sigma/sqrt(n)
  b=s_mean+critical_z*sigma/sqrt(n)
  interval=c(a,b) 
  return (interval)
}

x=scan("history.txt"); 
m=mean(x)

#2.5
print(zconfidence_interval(m,alfa=0.05,sigma=10,n=25))