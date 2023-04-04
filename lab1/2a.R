newx=function(n){
  x=scan("data.txt")
  s=0
  y=seq(1,n,n)
  for (i in 1:n) {
    s=s+x[i]
    y[i]=x[i]/s
  }
  return (y)
}

newx(5)
