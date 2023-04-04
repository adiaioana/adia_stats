maxbin=function(n,p){
  if(n==0)
    n=18
  if(p==0)
    p=0.25
  set.seed(0) #make this example reproducible
  x=seq(0,n,1)
  vec=dbinom(x,n,p)
  mx=max(vec)
  #plot (k, vec, type = "l", ylab = "log density")
  return (mx)
}

wow=maxbin(40,0.5)

suma=function(n,p,k){
  
  set.seed(0) #make this example reproducible
  x=seq(0,n,1)
  vec=dbinom(x,n,p)
  
  for(i in 1:k)
    s=s+vec[i]
  
  return (s)
}

wow1=suma(40,0.5,2)

inter=function(n,p,k,m){
  
  set.seed(0) #make this example reproducible
  x=seq(0,n,1)
  vec=dbinom(k:m,n,p)
  
  return (vec)
}

wow2=inter(40,0.5,20,23)