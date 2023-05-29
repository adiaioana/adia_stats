#D1a
#generez un x
set.seed(123)
are_el_maj=function(x,k) {
  for(i in 1:k) {
    fr=0
    n=length(x)
    a=sample(1:n,1,replace=FALSE)
    for(el in x)
      if(el==x[a])
        fr=fr+1
   #pentru debug> cat("Pentru ", x[a], " frecventa este ", fr,"la testul ", i, "\n")
    if(fr>n/2+1)
      return (x[a])
  }
  return ("x nu are M-element")
}
x_dim=1000 # iau n 1000 si o sa iau el din x din 1:n ca oricum sunt n elem, 
#si nu despre valoare e vorba
#am generat x desi nu era nevoie
x=sample(2:x_dim,x_dim, replace=TRUE) # setez toate elementele in afara de cel majoritar care il pun cu codul 1

fr_m_element=sample(x_dim/2+1:x_dim,1)
#iau aleatoriu fr elementului majoritar
pozitii_m_element=sample(1:x_dim, fr_m_element,replace=F)
for(i in pozitii_m_element)
  x[i]=1 # pun elmaj
# pentru debug> cat("sirul are ", x_dim, " elemente si M-elementul are frecv ", fr_m_element, "si sirul e ",x)

k=sample(1:100000,1) #aleg constanta k cu semnificatia din enunt
print(are_el_maj(x,k))

#D1b

#cum stim ca probl. <1/2^k, luam k=[loq2(10^7)] + 1 (adica 24)
k=as.integer(log2(10^7))+1
print(are_el_maj(x,k))

#D2

element_ith=function(i,A) {
  z=sample(A,1,replace=FALSE)
  a_small=A[A<z]
  a_big=A[A>z]
  n=length(A)
  if(length(a_small)>i)
    return (element_ith(i,a_small))
  else if(n>i+length(a_big))
      return (z)
  else
    return (element_ith(i-n+length(a_big),a_big))
}

Aa=sample(1:1000,1000,replace=T)
sort(Aa,decreasing=FALSE)
print(element_ith(10,Aa))

#D3a
MC_D3=function(n,S,a) {
  m=as.integer(a*log(n))
  Sprim=sample(S,m,replace=F)
  sort(Sprim,decreasing=F)
  return (median(Sprim))
}

print(MC_D3(length(Aa),Aa,4))

#D3b

#1-2/n^2 > 1- 10^(-7)

D3_b = function()
{
  i=1
  while(i<=10^6)
  {
    aux=1-2/(i*i)
    if(aux>1-10^(-7))
      return (i);
    i=i+1
  }
  return (-1)
}
print(D3_b())