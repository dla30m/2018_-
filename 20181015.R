
#Example 8
#自己
#method 1
set.seed(1234)
m = 2*50000; z = numeric(m)
u1 = runif(m/2); u2 = runif(m/2)
z1 = sqrt(-2*log(u1)) * cos(2*pi*u2) # half of normal variates
z2 = sqrt(-2*log(u1)) * sin(2*pi*u2) # other half
z[seq(1, m, by = 2)] = z1 # interleave
z[seq(2, m, by = 2)] = z2 # two halves

#開始貴爾檢定  E((N-E)/E)^2  expectation
E = m*diff(pnorm(c(-Inf, seq(-2, 2, by=.5), Inf))); E
N = hist(z, breaks=cut, plot=F)$counts; N
Q = sum(((N-E)^2)/E); Q; qchisq(c(.025,.975), 9)
#dnorm rnorm pnorm

#method 2
set.seed(1234)
z02 = numeric(m)
z1 = numeric(m/2)
z2 = numeric(m/2)
a = proc.time()
for(i in 1:m/2){
  v1 = 2*runif(1)-1
  v2 = 2*runif(1)-1
  s = v1^2+v2^2
  if(s<=1){
    z1[i] = v1*sqrt(-2*log(s)/s)
    z2[i] = v2*sqrt(-2*log(s)/s)
  }else{
    i = i-1
  }
}
proc.time()-a
z02[seq(1, m, by = 2)] = z1 # interleave
z02[seq(2, m, by = 2)] = z2 # two halves

E2 = m*diff(pnorm(c(-Inf, seq(-2, 2, by=.5), Inf))); E2
N2 = hist(z02, breaks=cut, plot=F)$counts; N2
Q2 = sum(((N2-E2)^2)/E2); Q2; qchisq(c(.025,.975), 9)

#圖型
par(mfrow=c(1,2))
cut = c(min(z)-.5, seq(-2, 2, by=.5), max(z)+.5)
hist(z, breaks=cut, ylim=c(0,.5), prob=T,main="Ex8 method 1")
zz = seq(min(z), max(z), by=.01)
lines(zz, dnorm(zz), col="blue")
hist(z02, breaks=cut, ylim=c(0,.5), prob=T,main="Ex8 method 2")
zz = seq(min(z02), max(z02), by=.01)
lines(zz, dnorm(zz), col="blue")


#學長陳彥霖
par(mfrow=c(1,2))
# method 1
set.seed(1234)
m = 2*50000; z = numeric(m)
u1 = runif(m/2); u2 = runif(m/2)
z1 = sqrt(-2*log(u1)) * cos(2*pi*u2) # half of normal variates
z2 = sqrt(-2*log(u1)) * sin(2*pi*u2) # other half
z[seq(1, m, by = 2)] = z1 # interleave
z[seq(2, m, by = 2)] = z2 # two halves
cut = c(min(z)-.5, seq(-2, 2, by=.5), max(z)+.5)
hist(z, breaks=cut, ylim=c(0,.4), prob=T,main = 'method 1')
zz = seq(min(z), max(z), by=.01)
lines(zz, dnorm(zz), col="blue")

E = m*diff(pnorm(c(-Inf, seq(-2, 2, by=.5), Inf))); E
N = hist(z, breaks=cut, plot=F)$counts; N
Q = sum(((N-E)^2)/E); Q; qchisq(c(.025,.975), 9)

# method 2 Not good
set.seed(1234)
n <- 5e4
t <- 0
X <- NULL
Y <- NULL
while (t < n){
  V1 <- runif(1,-1,1)
  V2 <- runif(1,-1,1)
  S <- V1^2 + V2^2
  if (S <= 1){
    X <- c(X, V1*sqrt(-2*log(S)/S))
    Y <- c(Y, V2*sqrt(-2*log(S)/S))
  }
  t <- length(X)
}

# method 3
set.seed(1234)
n <- 5e4
t <- 0
X <- NULL
Y <- NULL
while (t < n){
  V1 <- runif(n-t,-1,1)
  V2 <- runif(n-t,-1,1)
  S <- V1^2 + V2^2
  i <- which(S <= 1)
  X <- c(X, V1[i]*sqrt(-2*log(S[i])/S[i]))
  Y <- c(Y, V2[i]*sqrt(-2*log(S[i])/S[i]))
  t <- length(X)
}

Z <- c(X,Y)
cut <- c(min(Z)-.5, seq(-2, 2, by=.5), max(Z)+.5)
hist(Z, breaks = cut, ylim = c(0,.4), prob = T,main = 'method 3')
zz <- seq(min(Z), max(Z), by = .01)
lines(zz, dnorm(zz), col="blue")

E <- 2*n*diff(pnorm(c(-Inf, seq(-2, 2, by=.5), Inf))); E
N <- hist(Z, breaks=cut, plot=F)$counts; N
Q <- sum(((N-E)^2)/E); Q; qchisq(c(.025,.975), 9)
