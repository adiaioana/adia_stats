CLT_Student = function(alfa,lambda, n, N, z){
  expectation = alfa/lambda;
  st_dev = alfa/lambda;
  upper_bound = z * st_dev/sqrt(n) + expectation;
  sum = 0;
  for(i in 1:N) {
    x_n = mean(rexp(n, lambda));
    if(x_n <= upper_bound)
      sum = sum + 1
  }
  return(c(sum/N,pnorm(z)))
}
CLT_Student(2,3,50,5000,-1.5)