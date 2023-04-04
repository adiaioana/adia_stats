newx=function(n){
  if(n==0)
    n=scan("data.txt", skip=2)
  x=scan("data.txt",skip=1)
  mn=x[1]
  mx=x[1]
  y=vector(length=n-1)
  y[n-1]=x[n]
  i=n-2
  while(i>0) {
    if(x[i+1]>y[i+1])
      y[i]=x[i+1]
    else
      y[i]=y[i+1]
    i=i-1
  }
  
  i=1
  while(i<n) {
    if(x[i]<mn)
      mn=x[i]
    y[i]=mn/y[i]
    i=i+1
  }
  return (y)
}

vec=newx(5)
print(vec)