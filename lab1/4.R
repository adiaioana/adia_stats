newx=function(n,p){
  if(n==0)
    n=18
  if(p==0)
    p=0.25
  set.seed(0) #make this example reproducible
  x=seq(0,n,1)
  vec=dbinom(x,n,p)
  #plot (k, vec, type = "l", ylab = "log density")
  barplot(vec)
  
}


newx(18,0.25)