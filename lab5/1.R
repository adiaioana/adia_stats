simulare = function (mat)
{
  set.seed(123)
  U=runif(1,0,1)
  k=ncol(mat)
  sum=0
  
  for(i in 1:k)
  {
    if(U-sum>=0 && mat[2,i]>U-sum)
      return (mat[1,i])
    sum=sum+mat[2,i]
  }
  
  return (-1000)
}
generare_mat = function (k) # generez variabila X
{
  set.seed(123)
  var=runif(k-1,0,1) 
  var[k]=1
  acc=var[order(var)]
  x=var
  st=0
  
  A=matrix(acc,2,k)
  for(i in 1:k){
    A[1,i]=as.integer(runif(1,-k,k))
    A[2,i]= acc[i] - st
    st=acc[i]
  }
  return (A)
}
k=5
B=generare_mat(5)
X=simulare(B)
print(X)
print (B)




# verific suma prob sa fie 1
sum=0
for(i in 1:k){
  sum=sum+B[2,i]
}

#print(sum)