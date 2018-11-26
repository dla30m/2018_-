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

#Exercise 6.1
f = function(t,lan,k){
  output = NULL
  sumg = 0
  for(j in 0:k){
    sumg = dpois(j,lan)+sumg
  }
  MAX = 0
  for(j in 0:k){
    if(MAX < dpois(j,lan)/sumg){
       MAX = dpois(j,lan)/sumg
    }
  }
  for(j in 1:t){
    repeat{
      U1=runif(1)
      U1=floor(U1*(k+1))
      U2=runif(1)
      if(U2<dpois(U1,lan)/sumg){
        output = c(output,U1)
        break
      }
    }
  }
  return(output)
}
#Exercise 6.1 Inverse Transformtion
f = function(t,lan,k){
  output = NULL
  dp = dpois(0:k,lan)
  cumdp = cumsum(dp)
  sumdp = sum(dp)
  cumdp = cumdp/sumdp
  for(j in 1:t){
    repeat{
      U1=runif(1)
      U1=floor(U1*(k+1))
      U2=runif(1)
      if(U2<cumdp[U1]){
        output = c(output,U1)
        break
      }
    }
  }
  return(output)
}
table(f(10,20,10))
