modul=function(vec)
{
  y=0
  pas=table(vec)
  n=length(pas)
  for (i in 1:n)
    if(y<pas[i])
    {
      y=pas[i]
      el=names(pas[i])
    }
  return (el)
}

inp=c(3,6,4,3,6,7,8,5)
ans=modul(inp)