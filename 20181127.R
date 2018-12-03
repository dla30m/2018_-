#Example 18
f = function(pj){
  repeat{
    U1=runif(1)
    U1=floor(U1*10)+1
    U2=runif(1)
    if( U2< pj[U1]/0.12 ){
      return(U1)
    }
  }
}
pj = c(0.11,0.12,0.09,0.08,0.12,0.10,0.09,0.09,0.10,0.10)
f(pj)
t = replicate(100,f(pj))
mean(t)
table(t)
table(t)/100

#Exercise 6.1
f = function(t,lan,k){
  output = NULL
  g = function(i){
    return( exp(-lan)*(lan^i)/factorial(i) )
  }
  sumg = 0
  for(j in 0:k){
    sumg = g(j)+sumg
  }
  for(j in 1:t){
    repeat{
      U1=runif(1)
      U1=floor(U1*(k+1))
      U2=runif(1)
      if(U2<g(U1)/sumg){
        output = c(output,U1)
        break
      }
    }
  }
  return(output)
}
table(f(10000,20,10))
tabulate(f(10000,20,10),10)
as.numeric(  table( f(100,20,10) )   )

#Exercise 6.1 Accept-Reject Method
f = function(t,lan,k){
  output = NULL
  dp = dpois(0:k,lan)
  sumdp = sum(dp)
  dp = dp/sumdp
  maxdp = max(dp*(k+1))
  q = 1/(k+1)
  for(j in 1:t){
    repeat{
      U1=runif(1)
      U1=floor(U1*(k+1))+1
      U2=runif(1)
      if(U2 < (dp[U1] / (maxdp*q)) ){
        output = c(output,U1)
        break
      }
    }
  }
  return(output)
}
table(f(10000,20,10))

#Exercise 6.1 Inverse Transformtion
f = function(t,lan,k){
  output = NULL
  dp = dpois(0:k,lan)
  print(dp)
  cumdp = cumsum(dp)
  print(cumdp)
  sumdp = sum(dp)
  cumdp = cumdp/sumdp
  for(j in 1:t){
    U1=runif(1)
    for(i in 1:k+1){
      if(U1<cumdp[i]){
        output = c(output,i)
        break
      }
    }
  }
  return(output)
}
table(f(10000,20,10))