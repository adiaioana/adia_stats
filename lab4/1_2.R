#1_1
sphere_vol=function(N){ 
  NC=0; 
  for(i in 1:N)
  { 
    x=runif(1,-1,1); 
    y=runif(1,-1,1); 
    z=runif(1,-1,1);
    if(x*x+y*y+z*z<=1) 
      NC=NC+1; 
  }
  return(8*NC/N);
}
#1_2
parabol_aria=function(N) 
{
  NC=0
  for(i in 1:N)
  {
    x=runif(1,0,2); 
    y=runif(1,0,2); 
    if(-2*x*x+5*x-2>=y)
      NC=NC+1
  }
  return (4*NC/N);
}

aria=1.125
y=parabol_aria(10000)
y
abs_err=abs(y)-aria;
rel_err=abs_err-aria*100
