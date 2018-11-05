Exercise 4.2
#method 1
EX4 = function(n){
  u = sample(1:n)
  a = 0
  for(i in 1:n){
    if(u[i]==i){
      a=a+1
    }
  }
  return(a)
}

#proc.time()  ®É¶¡
a <- proc.time()
t = replicate(10000,EX4(100))
mean(t)
var(t)
proc.time()-a

#method 2
EX4 = function(n){
  u = sample(1:n)
  return(sum(u==1:n))
}
EX4(100)

sample
sample.int(100,100)
sample(1:n)


#Example 17
u1 = runif(1)
u2 = runif(1)
if(u1<0.5){
  X2 = floor(u2*10)+1
}else{
  X2 = floor(u2*5)+6
}
X2

#Exercise 5.1
u1 = runif(1)
u2 = runif(1)
if(u1<0.55){
  X2 = floor(u2*5)+5
}else{
  X2 = floor(u2*5)+6
}
X2

#Homework 2
Nsim=10^4
n=6;p=.3
y=rgamma(Nsim,n,rate=p/(1-p))
x=rpois(Nsim,y)
hist(x,main="",freq=F,col="grey",breaks=40)
lines(1:50,dnbinom(1:50,n,p),lwd=2,col="sienna")

#Exercise 5.2
ex = function(a){
  u = runif(1)
  if(u<0.225){
    return(6)
  }else if(u<0.4){
    return(7)
  }else if(u<0.6){
    return(8)
  }else if(u<0.825){
    return(9)
  }else{
    return(10)
  }
}

EX = function(){
  u1 = runif(1)
  u2 = runif(1)
  if(u1<0.6){
    return(floor(u2*10)+1)
  }else{
    return(ex())
  }
}
EX()
t = replicate(10000,EX())
mean(t)
hist(t,break=seq(0.5,10))
