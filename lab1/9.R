n=5
set.seed(0) #make this example reproducible
vec=vector(length=n)
for(i in 1:n) {
  vec[i]=dgeom(i,prob=0.2)
}
#plot (k, vec, type = "l", ylab = "log density")
barplot(vec)