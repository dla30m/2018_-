#Eercise 2.1
m = 10000
u = runif(m)
v = numeric(m)
for (i in 1:m){
  b = function(x){ (exp(x)-1)/(exp(1)-1)-u[i] }
  v[i] = uniroot(f = b,lower = 0 ,upper = 1)$root
}
hist(v,breaks = seq(0,1,length.out = 12),freq = F ,main = "Exercise 2.1")

integrand <- function(x) { exp(x)/(exp(1)-1)}
integrate(integrand)
#Eercise 2.1
u = runif(1)
b = function(x){ (exp(x)-1)/(exp(1)-1) - u }
v = uniroot(f = b,lower = 0 ,upper = 1)$root
v
#Eercise 2.2
u = runif(1)
u
if(u<1/4){
  b = function(x){ ((x*x)/4 - x +1) - u }
  v = uniroot(f = b,lower = 0 ,upper = 1/4)$root
}else{
  b = function(x){ ( x - x*x/12 -9/4) - u }
  v = uniroot(f = b,lower = 0 ,upper = 3/4)$root
}
v
