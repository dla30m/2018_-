#Exercise 6.2
n = 10;p = 1/3;k = 5
#pbinom P(X<=x)
alpha = 1- pbinom(k-1,n,p)
f = function(k,n,p,x){
  xb = dbinom(x,n,p)
  alpha = 1- pbinom(k-1,n,p)
  return(xb/alpha)
}
f(k,n,p,8)

#Inverse Transformtion
# t ¦¸¼Æ
f = function(t,k,n,p){
  output = NULL
  xb = NULL
  for(j in k:n){
    xb = c(xb,dbinom(j,n,p))
  }
  alpha = 1- pbinom(k-1,n,p)
  xb = xb/alpha
  xb = cumsum(xb)
  for(j in 1:t){
    U1=runif(1)
    for(i in 1:(n-k+1)){
      if(U1 < xb[i]){
        output = c(output,i+k-1)
        break
      }
    }
  }
  return(output)
}
f(50,5,10,1/3)

#Accept-Reject Method
f = function(t,k,n,p){
  output = NULL
  xb = NULL
  for(j in k:n){
    xb = c(xb,dbinom(j,n,p))
  }
  alpha = 1- pbinom(k-1,n,p)
  xb = xb/alpha
  maxxb = max(xb*(k+1))
  q = 1/(k+1)
  for(j in 1:t){
    repeat{
      U1=runif(1)
      U1=floor(U1*(n-k+1))+1
      U2=runif(1)
      if(U2 < (xb[U1] / (maxxb*q)) ){
        output = c(output,U1+k)
        break
      }
    }
  }
  return(output)
}
f(50,2,10,0.4)
