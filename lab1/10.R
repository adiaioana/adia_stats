zece=function(n,k){
set.seed(0) #make this example reproducible
vec=vector(length=n)
for(i in 1:n) {
  vec[i]=dpois(x=i,lambda=k)
}
#plot (k, vec, type = "l", ylab = "log density")
barplot(vec)
}

zece(2,2)
