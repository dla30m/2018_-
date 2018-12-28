f = function(lan){
  x = 0
  i = 1
  repeat{
    x = x + rexp(1,lan)
    if(x>1){
      return(i)
    }
    i = i+1
  }
}
h = function(t,lan){
  output = NULL
  for(i in 1:t){
    output = c(output,f(lan))
  }
  return(output)
}

lan = 100
Nsim = 10^4
a = proc.time();
k = h(Nsim, lan)
proc.time()-a;

a = proc.time();
l = g(Nsim, lan)
proc.time()-a;
system.time(g(Nsim, lan))
#Example 14
g = function(t, lan){
  spread=3*sqrt(lan)
  tmp = round(seq(max(0,lan-spread),lan+spread,1))
  prob=ppois(tmp, lan)
  X=rep(0,t)
  for (i in 1:t){
    u=runif(1)
    X[i]=tmp[1]+sum(prob < u)
  }
  return(X)
}
lambda=100
spread=3*sqrt(lambda)
t=round(seq(max(0,lambda-spread),lambda+spread,1))
t