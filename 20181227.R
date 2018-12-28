#Test 2.1(a)
par(mfrow=c(1,2))
f = function(x){
  return( 2*exp(-x^2/2)/sqrt(2*pi) )
}
g = function(x){
  return( exp(-x) )
}
M = optimize(f=function(x){f(x)/g(x)},interval=c(0,100),maximum=TRUE)$objective
#n is time
h = function(n){
  x = NULL
  z = NULL
  U = NULL
  Y = NULL
  repeat{
    u = runif( (n-length(x)) )
    y = rexp( (n-length(x)), 1)
    U = c(U,u)
    Y = c(Y,y)
    x = c(x, y[u < exp(-(y-1)^2/2)])
    z = c(z, u[u < exp(-(y-1)^2/2)])
    if(length(x)>=n){
      break
    }
  }
  return(list( x = x, z = z, U = U,  Y = Y))
}
hh = h(1000)
Y = hh$Y
U = hh$U
U = U*M*g(Y)
U = c(U,U)
Y = c(-Y,Y)
x = hh$x
z = hh$z
z = z*M*g(x)
x = c(-x,x)
z = c(z,z)
plot( Y, U, col = "grey")
points( x, z, col = "red")
b = seq( -7, 7, length.out = 10000)
lines( b, f(b), col = "yellow")
c = seq( 0, 7, length.out = 10000)
lines( c, M*g(c), col = "yellow")
lines( -c, M*g(c), col = "yellow")

g = function(x){
  return( exp(-abs(x))/2 )
}
M = optimize(f=function(x){dnorm(x)/g(x)},interval=c(-100,100),maximum=TRUE)$objective

h = function(n){
  g = function(x){
    return( exp(-abs(x))/2 )
  }
  M = optimize(f=function(x){dnorm(x)/g(x)},interval=c(-100,100),maximum=TRUE)$objective
  x = NULL
  z = NULL
  U = NULL
  Y = NULL
  repeat{
    u = runif( n-length(x) )
    y = rexp(  n-length(x), 1)
    v = runif(n-length(x))
    temp = y
    i = v<1/2
    temp[i] = -temp[i]
    U = c(U,u*M*g(y))
    Y = c(Y,temp)
    j = u < dnorm(y)/g(y)/M
    tmp = y[j]
    v = runif(sum(j))
    i = v<1/2
    tmp[i] = -tmp[i]
    x = c(x, tmp)
    z = c(z, u[j]*M*g(tmp))
    if(length(x)==n){
      break
    }
  }
  return(list( x = x, z = z, U = U,  Y = Y))
}
hh = h(1000)
Y = hh$Y
U = hh$U
x = hh$x
z = hh$z
plot( Y, U, col = "grey")
points( x, z, col = "red")
b = seq( -7, 7, length.out = 10000)
lines( b, dnorm(b), col = "yellow")
c = seq( 0, 7, length.out = 10000)
lines( c, M*g(c), col = "yellow")
lines( -c, M*g(c), col = "yellow")
lines( c, M*g(c), col = "yellow")