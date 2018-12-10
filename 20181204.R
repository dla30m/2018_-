#Example 19
f = function(t){
  M = optimize(f=function(x){dbeta(x,2,4)},interval=c(0,1),maximum=TRUE)$objective
  output = NULL
  while(length(output)<=2500){
    U1 = runif(1)
    U2 = runif(1)
    if(U2 < (20*U1*(1-U1)^3/M)){
      output = c(output)
    }
  }
  return(output)
}

#Example 20

Nsim=2500
a = 2.7 ; b = 6.3
M = optimize(f=function(x){dbeta(x,2.7,6.3)},interval=c(0,1),maximum=TRUE)$objective
u = runif(Nsim,max=M) #uniform over (0,M)
y = runif(Nsim) #generation from g
x = y[u<dbeta(y,a,b)] #accepted subsample
x
v = u[u<dbeta(y,a,b)]
plot(y,u,col = "grey")
points(x,v,col = "red")
z = seq(0,1,by=0.0001)
z = seq(0,1,length.out=10000)
lines(z,dbeta(z,2.7,6.3))

