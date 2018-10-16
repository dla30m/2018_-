#Example 9 P.10
set.seed(1234)
nsim=10000
u1=runif(nsim)
u2=runif(nsim)
X1=sqrt(-2*log(u1))*cos(2*pi*u2)
X2=sqrt(-2*log(u1))*sin(2*pi*u2)
U=array(0,dim=c(nsim,1))
for(i in 1:nsim)
U[i]=sum(runif(12,-.5,.5))
par(mfrow=c(1,3))
hist(X1)
hist(U)

a=3
mean(X1>=-1.96 & X1<=1.96)
mean(X1>=-6 & X1<=6)
mean(U>a)
m = 100000; n = 12
u = runif(m*n)
UNI = matrix(u, nrow=m)
z = rowSums(UNI) - 6
cut = c(min(z)-.5, seq(-2, 2, by=.5), max(z)+.5)
hist(z, breaks=cut, ylim=c(0,.4), prob=T)
zz = seq(min(z), max(z), by=.01)
lines(zz, dnorm(zz), col="blue")
mean(z>=-2 & z<=2)
E = m*diff(pnorm(c(-Inf, seq(-2, 2, by=.5), Inf))); E
N = hist(z, breaks=cut, plot=F)$counts; N
Q = sum(((N-E)^2)/E); Q; qchisq(c(.025,.975), 9)
