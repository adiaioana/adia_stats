
opa = function(b) {
mn=b[1]
mx=b[1]
nr<- length(b)
s=b[1]
nr40m=(b[1]<40)
nr40M=(b[1]>=40)
for(i in 2:length(b)) {
  if(b[i] > mx)
    mx = b[i]
  if(b[i] < mn)
    mn = b[i]
  s=s+b[i]
  if(b[i] > 39)
    nr40M=nr40M+1 
    else
    nr40m=nr40m+1
}
medar=s/nr #media aritmetica
pr40m=100*nr40m/nr #procent nr. <40
nx=mn/mx #raport minim/maxim
rez=seq(1,7,7)

rez= c(mn,mx,medar,s,nx,nr40M,pr40m)
print(rez)
return (1)
}

K= c(46, 33, 39, 37, 46, 30, 48, 32, 49, 35, 30, 4)
opa(K)
