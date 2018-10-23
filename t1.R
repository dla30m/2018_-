u = runif(1)
u
if(u<1/2){
  b = function(x){ exp(2*x)/2 - u }
  v = uniroot(f = b,lower = -100000 ,upper = 0)$root
}else{
  b = function(x){ (2-exp(-2*x))/2 - u }
  v = uniroot(f = b,lower = 0 ,upper = 100000)$root
}
v

m = 10000
u = runif(m)
v = numeric(m)
for (i in 1:m){
  if(u[i]<1/2){
    b = function(x){ exp(2*x)/2 - u[i] }
    v[i] = uniroot(f = b,lower = -100000 ,upper = 0)$root
  }else{
    b = function(x){ (2-exp(-2*x))/2 - u[i] }
    v[i] = uniroot(f = b,lower = 0 ,upper = 100000)$root
  }
}
hist(v,freq = F ,main = "Exercise 1")
mean(v)
var(v)

