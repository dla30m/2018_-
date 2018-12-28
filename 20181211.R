#Exercise 7.1
g = function(x){
  return(exp(-x)/(1-exp(-0.05)))
}
M = optimize(f=function(x){g(x)},interval=c(0,0.05),maximum=TRUE)$objective
x = NULL
repeat{
  u = runif(1) 
  y = runif(1,max=0.05) #generation from g
  if(u<g(y)/0.05){
    x=c(x,y)
  }
  if(length(x)==1000){
    break
  }
}
mean(x)

install.packages('Ryacas')
library(Ryacas)
x = Sym('x')
a = Integrate(x*g(x),x=x,a=0,b=0.05)
-(1.05 * exp(-0.05))/0.048770575499286 - -1/0.048770575499286
eval(as.expression(a))
#Exercise 7.2(a)
g = function(x){
  return(x^2+5/3*x^4)
}
M = optimize(f=function(x){g(x)},interval=c(0,1),maximum=TRUE)$objective
u = runif(Nsim)*M #uniform over (0,M)
y = runif(Nsim) #generation from g
x = y[u < g(y)] #accepted subsample
mean(x)
