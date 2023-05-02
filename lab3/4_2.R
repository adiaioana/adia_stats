binomialprobability=function(n,p,k){ 
  expectation=n*p; 
  variance=n*p*(1-p); 
  standarddeviation=sqrt(variance); 
  q=standarddeviation/(k+0.5); 
  return(pnorm(q)); }

binomialprobability(5,0.3,0.00001)