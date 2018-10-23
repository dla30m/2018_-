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
