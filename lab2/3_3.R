outliers_mean=function(vec)
{
  m=mean(vec)
  s=sd(vec)
  outliers=vector() 
  j=0
  for(i in 1:length(vec))
    if(vec[i]>m-2*s && vec[i]<m+2*s)
    { 
      j=j+1
      outliers[j]=vec[i] 
    } 
  outliers
}
outliers_iqr=function(vec)
{
  q1=quartile(vec,type=1)
  q3=quartile(vec,type=3)
  q=IQR(vec)
  lb=q1-1.5*q
  ub=q3+1.5*q
  outliers=vector() 
  j=0
  for(i in 1:length(vec))
    if(vec[i]>lb&&vec[i]<ub)
    { 
      j=j+1
      outliers[j]=sample[i] 
    } 
  outliers
}


sample=scan("sample2.txt", skip=0)
answer1=outliers_mean(sample)
answer2=outliers_iqr(sample)
answer3=summary(sample)