set.seed(1234)
#Example 4
#Gamma X = -1/lambda * log(U)
gam = function(lan){
  run = runif(20,0,1)
  x = -(1/lan)*log(prod(run))
  x
}
x = gam(2)
x
#§ä¥­§¡­È
qu = function(n){
  a = 0
  for (i in 1:n){
    a = a + gam(2)
  }
  a = a/n
  a
}
ans = qu(100)
ans

#ClassWork 1
run1 = runif(2,0,1)
t = -1*log(prod(run1))
x = t*runif(1,0,1);x
y = t-x;y

#ClassWork 3
CW3 = function(k){
  run1 = runif(k)
  t = -log(prod(run1))
  run2 = c(0,sort(runif(k-1)),1) # sort ±Æ§Ç
  x = numeric(k)
  for (i in 1:k){
    x[i] = t*(run2[i+1]-run2[i])
  }
  x
}
x = CW3(5)
x

run2 = runif(5);run2
run2 = sort(run2);run2
run2 = c(0,run2,1);run2

#Eercise 2.1
#method 1
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
#method2
u = runif(1)
b = function(x){ (exp(x)-1)/(exp(1)-1) - u }
v = uniroot(f = b,lower = 0 ,upper = 1)$root
v

#Eercise 2.2
u = runif(1)
if(u<1/4){
  b = function(x){ ((x*x)/4 - x +1) - u }
  v = uniroot(f = b,lower = 0 ,upper = 1/4)$root
}else{
  b = function(x){ ( x - x*x/12 -9/4) - u }
  v = uniroot(f = b,lower = 0 ,upper = 3/4)$root
}
v
