noua=function(n,k){
  
set.seed(0)
vec=vector(length=n)
for(i in 1:n) {
  vec[i]=dgeom(i,prob=k)
}
#plot (k, vec, type = "l", ylab = "log density")
barplot(vec)
}
noua(18,0.2)
