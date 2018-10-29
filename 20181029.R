#Example 12
#method 1
EX12 = function(n,p){
  u = runif(1)
# print(u)
  c = p/(1-p)
  i = 0
  pr = (1 - p)^n
  F = pr
  while(u>=F){
    pr = (c*(n - i)/(i + 1))*pr
    i = i+1
    F = F+pr
  }
  X = i
  re = c(X,pr,F)
  return(X)
}
EX12(10,0.3)
m = 100
b = numeric(m)
for(i in 1:m){
   b[i] = EX12(10,0.3)
}
mean(b)


#method 2
EX12 = function(n,p){
  u = runif(1)
  print(u)
  c = p/(1-p)
  i = 0
  pr = (1 - p)^n
  F = pr
  repeat{
    if(u<F){
      X = i
      re = c(X,pr,F)
      return(re)
    }
    pr = (c*(n - i)/(i + 1))*pr
    i = i+1
    F = F+pr
  }
}
EX12(10,0.3)


#Example 13
EX13 = function(lam){
  u = runif(1)
# print(u)
  i = 0
  p = exp(-lam)
  F = p
  while(u>=F){
    p = lam*p/(i+1)
    i = i+1
    F = F+p
  }
  X = i
  re = c(X,p,F)
  return(X)
}
EX13(0.5)
m = 100
pa = numeric(m)
for(i in 1:m){
   pa[i] = EX13(0.8)
}
mean(pa)

#Example 15
U = runif(1)
P = 0.3
Q = 1-P
X = ceiling(log(U)/log(Q));X
X = floor(log(U)/log(Q))+1;X

#Example 16(Negative Binomial)
EX16 = function(r,p){
  u = runif(1)
  print(u)
  j = r
  pr = p^j
  F = pr
  while(u>=F){
    pr = j*(1-p)/(j+1-r)*pr
    j = j+1
    F = F+pr
  }
  X = j
  re = c(X,p,F)
  return(X)
}
EX16(10,0.1)
m = 100
negbin = numeric(m)
for(i in 1:m){
   negbin[i] = EX16(10,0.1)
}
mean(negbin)

