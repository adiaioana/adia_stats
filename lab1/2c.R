newx=function(n){
  if(n==0)
    n=scan("data.txt", skip=2)
  x=scan("data.txt",skip=1)
  sst=0
  sdr=0
  y=vector(length = n-1)
  i=n-1
  while(i>0){
    if(i!=n-1)
      y[i]=y[i+1]+x[i+1]
    else
      y[i]=x[n]
    i=i-1
  }
  i=1
  while(i<n){
    sst=sst+x[i]
    y[i]=sst/y[i]
    i=i+1
  }
  return (y)
}

vec=newx(5)
print(vec)