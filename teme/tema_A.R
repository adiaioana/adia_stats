#A1)a)
repr_grafic=function(n,p,k,lambda)
{
  #pentru distributia binomiala
  x=seq(k,n,1)
  y_binom=dbinom(x,n,p)
  
  #pentru distributia geometrica
  x=seq(k,n,1)
  y_geom=dgeom(x,p)
  
  #pentru distributia Poisson
  x=seq(k,n,1)
  y_pois=dpois(x,lambda)
  
  plot(x,y_pois, type="l", main="A1a", col="green", ylim=c(0,max(y_pois, y_geom, y_binom)),
       xlab="k:n", ylab="Probability",lwd=3)
  
  lines(x,y_geom, col="red", lwd=3)
  lines(x,y_binom, col="blue", lwd=3)
}
repr_grafic(10,0.4,3,1)

#A1)b)
n=20
p=0.5

x=dgeom(1:n, p)
#probabilitatea X imp
p_odd=sum(dgeom(seq(1, n, by=2), p))
# probabilitatea (X>=4) = 1 - probl (X<3)
p_morethaneq4=1-pgeom(2,p)#sau (1-p)^3
# probabilitatea (X<=20)
p_lessthaneq20=pgeom(20,p)

#A1)c)
smallest_k=function(lambda){
  k=0
  wish=1e-7
  #incrementez k pana am probabilitatea <10^-7
  while (ppois(k, lambda, lower.tail = FALSE) >= wish) {
    k <- k + 1
  }
  return (k)
}

print(smallest_k(3))

#A2)a)
grades_A2_a=function(file) {
  grades=read.csv(file)
  #mediana
  print(median(grades[["P"]]))
  print(median(grades[["S"]]))
  #media
  print(mean(grades[["P"]]))
  print(mean(grades[["S"]]))
  #deviatia standard
  print(sd(grades[["P"]]))
  print(sd(grades[["S"]]))
  #cvartilele
  print(quantile(grades[["P"]]))
  print(quantile(grades[["S"]]))
}

grades_A2_a("note.csv")

#A2)b)
grades_A2_b=function(file,letter)
{
  grades=read.csv(file)
  #o sa folosesc Tukey
  q1=quantile(grades[[letter]],0.25)
  q3=quantile(grades[[letter]],0.75)
  IQR= q3 - q1
  lower_bound=q1-1.5 * IQR;
  upper_bound=q3+1.5 * IQR;
  
  grades_filtrat=grades[grades[[letter]]>=lower_bound 
                        & grades[[letter]]<=upper_bound, ]
  return (grades_filtrat)
}
grades=read.csv("note.csv")
new_grades_S=grades_A2_b("note.csv","S")
new_grades_P=grades_A2_b("note.csv","P")

#A2)c)
breaks <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
hist(grades[["P"]], breaks = breaks, main = "Distributie frecv. notelor P", xlab = "interval note")
hist(grades[["S"]], breaks = breaks, main = "Distributie frecv. notelor S", xlab = "interval note")
