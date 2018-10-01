#Example 1
n = 3
Nsim = 10^4
U = runif(Nsim)
X = U ^ (1/n)
mean(X)
hist(X,freq = F ,main = "Example 2.1")
hist(X,freq = T ,main = "Example 2.1")

#Example 2
m = 10000
u = runif(m); x = u^2
xx = seq(0, 1, by=.001)
cut.u = (0:10)/10; cut.x = cut.u^2
cut.u
par(mfrow=c(1,2))
hist(u, breaks=cut.u, prob=T, ylim=c(0,10))
hist(u, breaks=cut.u, prob=R, ylim=c(0,10))
lines(xx, dunif(xx), col="blue")
hist(x, breaks=cut.x, prob=T, ylim=c(0,10))
lines(xx, .5*xx^(-.5), col="blue")
par(mfrow=c(1,1))

qbeta(runif(10),3,1)

#Example 3
Nsim=10^4 #number of random variables
U=runif(Nsim)
X=-log(U) #transforms of uniforms
Y=rexp(Nsim) #exponentials from R

par(mfrow=c(1,2))                      #合併圖在同一的地方一列二行
hist(X,freq=F,main="Exp from Uniform")
hist(Y,freq=F,main="Exp from R")

#Eercise 1.1
m = 10000
u = runif(m)
v = numeric(m)
for (i in 1:m){
  b = function(x){(x^2+x)/2-u[i]}
  v[i] <- uniroot(f = b,lower = 0 ,upper = 1)$root
}
hist(v,breaks = seq(0,1,length.out = 12),freq = F ,main = "Exercise 1.1")

#Eercise 1.2(a)
m = 100
u = runif(m)
v = numeric(m)
for (i in 1:m){
  b = function(x){  1/(1+exp((-1)*(x)))  -u[i]}
  v[i] <- uniroot(f = b,lower = -10000 ,upper = 10000)$root
}
hist(v,breaks = seq(-10,10),freq = F ,main = "Exercise 1.2(a)")

#hist(qlogis(u))

#Eercise 1.2(b)
m = 100
u1 = runif(m)
v1 = numeric(m)
for (i in 1:m){
  b = function(x){  1/2+1/pi*atan(x)  -u1[i]}
  v1[i] <- uniroot(f = b,lower = -10000 ,upper = 10000)$root
}
hist(v1,breaks = seq(-100,100),freq = F ,main = "Exercise 1.2(b)")
