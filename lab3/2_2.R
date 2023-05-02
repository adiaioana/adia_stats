LLNStudent=function(n,r)
{
  vec=rt(n,r);
  plot(vec)
  return(mean(vec));
}

ex=LLNStudent(100000,4)
ex