#B1
LLNGeometric=function(N,P){ 
  nn=length(N)
  np=length(P)
  mat=matrix(
    nrow=nn,
    ncol=np
  )
  rownames(mat)=N
  colnames(mat)=P
  
  for(i in 1:nn)
    for(j in 1:np)
      mat[i,j]=(mean(rgeom(N[i],P[j])))
  
  return (mat)
}
#n din {5000,10000,100000,500000}
#p din {0.2,0.6,0.6,0.8}
N=c(5000,10000,100000,500000)
P=c(0.2,0.6,0.6,0.8)
print(LLNGeometric(N,P))

#B2
TLCStudent=function(N_val, z_val,r,n){
  Ex=0
  VarX=r/(r-2)
  
  st_dev=sqrt(VarX)
  expectation=0 
  
  
  mat=matrix(NA,nrow=length(N_val),ncol=length(z_val))
  rownames(N_val)
  colnames(z_val)
  
  for(i in 1:length(N_val))
    for(j in 1:length(z_val))
    {
      N_b2=N_val[i]
      z=z_val[j]
      
      upper_bound=z*st_dev/sqrt(n)+expectation
      sum=0
      for(k in 1:N_b2)
      {
        xn=mean(rt(n, df=r))
        if(xn<=upper_bound)
          sum=sum+1
      }
      clt_aprox=sum/N_b2
      
      #eroarea absoluta
      err_abs=abs(clt_aprox-pt(z, df=r))
      mat[i,j]=err_abs
    }
  
  return (mat)
}
N=c(5000,10000,20000)
Z=c(-1.5,0,1.5)
print(TLCStudent(N,Z,r=10,n=50))

#B3

#P(h<=X<k) cu X=B(n,p)
prob_B3=function(n,p,h,k)
{
  expectation=n*p; 
  variance=n*p*(1-p); 
  standarddeviation=sqrt(variance); 
  
  z_h=(h-n*p)/standarddeviation
  z_k=(k-n*p)/standarddeviation
  
  prob_k=pnorm(z_k)
  prob_h=pnorm(z_h)
  aprox_prob=prob_k-prob_h+dbinom(h,n,p)
  return (aprox_prob)
}
print(prob_B3(100,0.5,30,70))
