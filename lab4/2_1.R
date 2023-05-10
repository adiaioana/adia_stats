MC_b=function(N)
{ 
sum=0; 
for(i in 1:N)
{ 
u=runif(1,1,4);
sum=sum+exp(u); 
} 
return(3*sum/N); 
}


MC_d=function(N, lim)
{ 
  sum=0; 
  for(i in 1:N)
  { 
    u=runif(1,1,lim);
    sum=sum+(1/(4*u*u-1)); 
  } 
  return((lim-1)*sum/N); 
}

val_b=51.87987
x_b=MC_b(100000)
err_abs_b=abs(x_b-val_b)
err_rel_b=err_abs_b/abs(val_b)

MC_d_avg=function(k,N){ 
  estimates=0;
  for(i in 1:k)
    estimates[i]=MC_d(k,N); 
  print(mean(estimates)); 
  print(sd(estimates));
  return (MC_d(k,N));
}

val_d=log(3)/4
x_d=MC_d_avg(1000,5000)
val_d
x_d
err_abs_d=abs(x_d-val_d)
err_rel_d=err_abs_d/abs(val_d)