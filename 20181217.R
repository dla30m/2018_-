#Example 22 
f = function(x){
  return( 2*exp(-x^2/2)/sqrt(2*pi) )
}
g = function(x){
  return( exp(-x) )
}
Nsim = 2500
M = optimize(f=function(x){f(x)/g(x)},interval=c(0,100),maximum=TRUE)$objective
#method 1
h = function(n){
  x = NULL
  repeat{
    u = runif( (n-length(x)) )
    y = rexp( (n-length(x)), 1)
    x = c(x,y[u < exp(-(y-1)^2/2)])
    if(length(x)>=n){
      break
    }
  }
  return(x)
}
mean(h(1000))

#method 2
h = function(n){
  x = NULL
  repeat{
    u = rexp(n-length(x),1)
    y = rexp(n-length(x),1)
    x = c(x,y[u > (y-1)^2/2])
    if(length(x)==n){
      break
    }
  }
  return(x)
}
mean(h(1000))

#method 3
h = function(n){
  x = NULL
  y = NULL
  repeat{
    y1 = rexp(n-length(x),1)
    y2 = rexp(n-length(x),1)
    j = y2 > (y1-1)^2/2
    y = c(y,(y2[j]- (y1[j]-1)^2/2) )
    x = c(x,y2[y1 > (y2-1)^2/2])
    u = runif(n-length(x))
    i = (u > 1/2)
    x[i] = -x[i]
    if(length(x)==n){
      break
    }
  }
  return(list(xl = x, yl = y))
}
mean(h(10000)$xl)
mean(h(10000)$yl)


mean(h(1000))

a = c(1,5,3,8,4,8,5,1)
i = (a>5)
i
a[i] = -a[i]
a