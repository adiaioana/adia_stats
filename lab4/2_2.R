MC_impr=function(N){ 
  sum=0; 
  for(i in 1:N)
  { 
    u=rexp(1,1); 
    sum=sum+exp(-2*u*u)/(3*exp(-3*u)); 
  } 
  return(sum/N); 
}
MC_avg=function(k,N){ 
  estimates=0
  for(i in 1:k) 
    estimates[i]=MC_impr(N); 
  print(mean(estimates)); 
  print(sd(estimates));
}

MC_avg(30,50000)