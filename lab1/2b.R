newx=function(n){
  if(n==0)
    n=scan("data.txt", skip=2)
  x=scan("data.txt", skip=1)
  mn=x[1]
  mx=x[1]
  y=seq(1,n,n)
  for (i in 1:n) {
    if(mn>x[i])
      mn=x[i]
    if(mx<x[i])
      mx=x[i]
  }
  for (i in 1:n) {
    y[i]=(x[i]-mn)/mx
  }
  print (y)
  return (y)
}

b=newx(5)