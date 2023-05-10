N=1000

n1 = 1e4 # primul => nr clienti
x1 = rexp(n1, rate = 4) 


n2 = n1*3 # al doilea => de 3 ori mai multi clienti
x2 = rexp(n2, rate = 12)

p=0.75 #probabilitatea de a fi servit de primul mecanic
mec = sample(c(1, 2), size = n1 + n2, replace = TRUE, prob = c(p, 1-p));

x=c(x1[mec==1],x2[mec==2])
length(x)=N
mean(x)