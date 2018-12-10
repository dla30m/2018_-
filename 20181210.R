#Example 21
#dgamma(x,3/2,1)
f = function(x){
  return( 2/sqrt(pi)*x^(1/2)*exp(-x) )
}
#dexp(x,2/3)
g = function(x){
  return( 2/3*exp(-2*x/3) )
}

Nsim=2500
k = 1/gamma(3/2)
M = optimize(f=function(x){f(x)/g(x)},interval=c(0,100),maximum=TRUE)$objective
M
M = optimize(f=function(x){dgamma(x,3/2,1)/dexp(x,2/3)},interval=c(0,100),maximum=TRUE)$objective
M
u = runif(Nsim)
y = rexp(Nsim,2/3)
y = -3/2*log(runif(Nsim))
x = y[u<(2*exp(1)*y/3)^(1/2)*exp(-y/3)] #accepted subsample
mean(x)

x = y[u <f(y)/g(y)/M ]
mean(x)