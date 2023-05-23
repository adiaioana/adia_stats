alfa=0.05
n=150
defecte=20
p0=0.1
p_prim=defecte/n
z_score=(p_prim-p0)/sqrt(p0*(1-p0)/n)
critical_z=qnorm(1-alfa,0,1)
comp=c(z_score,critical_z)
print(comp)
#z_score<critical_z => nu esxista dovezi sa se respinga