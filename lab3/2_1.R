LLNPoisson=function(lambda,n)
{ 
  sum=0; 
  for(i in 1:n)
  { u=rpois(1,lambda); 
  sum=sum+u; } 
  return(sum/n); 
  
}

LLNExp=function(lambda,n)
{
  return(mean(rexp(n,lambda)));
}

LLNB=function(lambda,n)
{
  return(mean(rbinom(n,lambda)));
}

LLNExp(2,3)