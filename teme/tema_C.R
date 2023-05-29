#C1
volume_estimated = function(N, a)
{
  xmin= - sqrt(a)
  xmax= -xmin
  ymin= - sqrt(a)
  ymax= -ymin
  zmin=0
  zmax=a
  
  count=0
  for(i in 1:N){ 
    x=runif(1, xmin,xmax)
    y=runif(1, ymin,ymax)
    z=runif(1, zmin,zmax)
    if(z>=x*x+y*y && z<=a)
      count=count+1
  }
  
  dom_vol=4*a*a
  rez= dom_vol * (count/N)
  return (rez)
}

eroare_relativa=function(est_val, val_exact)
{
  return (abs(est_val-val_exact)/val_exact)
}

a_sample=c(2,4,10)
N_sample=c(10000,20000,50000)

for(a in a_sample){
  volum=a*a*pi
  # raza= sqrt(a), inaltimea=a iar vol = r^2*h*pi
  for(n in N_sample) {
    cat("Pentru a= ", a, " si N = ", n, 
        "; eroarea relativa este ", eroare_relativa(volume_estimated(n,a),volum),"\n")
  }
}

#C2
#cum x>=0 si y>=0 => 12-3x>=0, deci x<=4, acum am apromixa ca y<=10/3 din 3y<=x+6
ntries=100000
ymax=0
#in plus, luam 1000 de nr random x in 0,4 si vedem unde vine y max si y min pentru estimare
for(i in 1:ntries)
{
  x=runif(1,0,4)
  m1= 12-3*x
  m2=(x+6)/3
  ymax=max(ymax,min(m1,m2))
}
# ymax obtinem 2.9999 aproximativ de aici y va fi mai egal decat 3
# deci [0,4]x[0,3] va include toate punctele cu siguranta
# mai iteram similar prin val lui x si obtinem un maxim de 3.76, dar pt y=0 oricum obtinem x<=4

a=c=0
b=4
d=3

ar_estimated = function(N, a,b,c,d)
{
  xmin= a
  xmax=b
  ymin=c
  ymax=d
  
  count=0
  for(i in 1:N){ 
    x=runif(1, xmin,xmax)
    y=runif(1, ymin,ymax)
    if(y<=12-3*x && 3*y<=x+6)
      count=count+1
  }
  
  dom_ar=abs((b-a)*(d-c))
  rez= dom_ar * (count/N)
  return (rez)
}

cat("Aria estimata prin MC va fi ", ar_estimated(200000,a,b,c,d) )

#C3
functia_a = function (x) {
  return ((x+1)/sqrt(4-x*x))
}

functia_b =function(x){
  return (1/(x*x+4))
}

functia_c = function(x) {
  return (x*exp(x))
}

MC_integr=function(N,lim,f){
  set.seed(123)
  x=runif(N,lim[1],lim[2])
  medie=mean(f(x))
  return ((lim[2]-lim[1])*medie)
}
#setam capetele integralei si valorile lor conf enunt
lim_a=c(-1,1)
lim_b=c(-100000,0)
lim_c=lim_b
val_exacta_a=pi/3
val_exacta_b=pi/4
val_exacta_c=-1


cat("Pentru (a), rezultatul exact este ", val_exacta_a, 
    " iar aprox MC este ", MC_integr(100000,lim_a,functia_a),"\n")

cat("Pentru (b), rezultatul exact este ", val_exacta_b, 
    " iar aprox MC este ", MC_integr(100000,lim_b,functia_b),"\n")

cat("Pentru (c), rezultatul exact este ", val_exacta_c, 
    " iar aprox MC este ", MC_integr(100000,lim_c,functia_c),"\n")

#C4
#4a
simulare_nr_zile=function(m,n,p,q,nr_trials)
{
  nr_zile=0
  while(nr_trials>0)
  {
    Z=0
    conturi_false=m
    while(conturi_false>0)
    {
      Z=Z+1
      conturi_false_noi=rbinom(1,n,p)
      conturi_false_inchise=sum(runif(conturi_false) < q)
      conturi_false=conturi_false+conturi_false_noi-conturi_false_inchise
      #debug> cat("CF> ",conturi_false, " ; CF_new> ", conturi_false_noi, " ; CF_closed ", conturi_false_inchise ,"\n")
    }
    
    #debug> print(Z)
    nr_zile[nr_trials]=Z
    nr_trials=nr_trials-1
  }
  # print(nr_zile)
  return (mean(nr_zile))
}

m=1000
n=10
p=0.2
q=0.5
print(simulare_nr_zile(m,n,p,q,100))

#4b

simulare_4b=function(m,n,p,q,nr_trials)
{
  count=0
  nr_zile=0
  for(i in 1:nr_trials)
  {
    Z=0
    conturi_false=m
    while(Z<40)
    {
      Z=Z+1
      conturi_false_noi=rbinom(1,n,p)
      conturi_false_inchise=sum(runif(conturi_false) < q)
      conturi_false=conturi_false+conturi_false_noi-conturi_false_inchise
      #debug> cat("CF> ",conturi_false, " ; CF_new> ", conturi_false_noi, " ; CF_closed ", conturi_false_inchise ,"\n")
    }
    if(conturi_false<=50000)
      count=count+1
  }
  return (count/nr_trials)
}


m=100000
n=500
p=0.5
q=0.1
prob=simulare_4b(m,n,p,q,10)
print(prob)

#4c
conf=0.99
err=0.01

low_bound=prob-err
upp_bound=prob+err
if(low_bound>=0 && upp_bound<=1 && (prob<=upp_bound && prob>=low_bound)){
  
  print(prob)
  cat("intervalul de incredere> ", c(low_bound, upp_bound))
}

