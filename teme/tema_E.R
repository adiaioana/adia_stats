tconfidence_interval=function(es_mean,es_sd,alfa,n){
  critical_t=qt(1-alfa/2,n-1)
  a=es_mean-critical_t*es_sd/sqrt(n)
  b=es_mean+critical_t*es_sd/sqrt(n)
  interval=c(a,",",b) 
  return (interval)
}
#E1
m=138
s=11
n=10
alfa=c(0.90,0.95,0.99)

for(A in alfa){
  cat("Intervalul de incredere pentru ", A*100," este ",
        tconfidence_interval(m,s,1-A,n),"\n")  
}
#E2
m=18
s=1.2
n=256
alfa_E2=0.05
cat("Intervalul de incredere pentru 95% este ",
    tconfidence_interval(m,s,alfa_E2,n),"\n") 

#E3
n=153
nemultumiti=17
p0=0.12
p_prim=nemultumiti/n
z_score=(p_prim-p0)/sqrt(p0*(1-p0)/n)
# pentru 5% nivel semnificatie
alfa=0.05
critical_z=qnorm(alfa,0,1)

if(z_score<critical_z)
  print("se poate respinge");
else
  print("nu se poate respinge")

# pentru 1% nivel semnificatie
alfa=0.01
critical_z=qnorm(alfa,0,1)
if(z_score<critical_z)
  print("se poate respinge");
else
  print("nu se poate respinge")