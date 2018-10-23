#P12 3 Discrete Distributions
x = 1:5 ; x
cumsum(dbinom(x,5,0.6))
dbinom(x,5,0.6)
plot(x,cumsum(dbinom(x,5,0.6)),type="s",ylab="CDF")#type = "s" 階梯圖
points(x,cumsum(dbinom(x,5,0.6)),pch=)#pch 點的圖形
abline(0.2,0,col="blue")
abline(1,0,col="yellow")
?pch
u=cumsum(dbinom(x,5,0.6));u
plot(u,x,type="S",ylab="Quantile")
points(u,x,pch=20)

#Example 10
EX = function(n){
  p = c(0.2,0.15,0.25,0.4) ;p
  q =cumsum(p)
  if(n < q[1]){
	return(1)
  }else if(n < q[2]){
	return(2)
  }else if(n < q[3]){
	return(3)
  }else{
	return(4)
  }
}
x = runif(100)
v = sapply(x,EX)
mean(v)
hist(v,breaks = seq(0.5,4.5))

#Example 11
f = function(m,n){
  x = runif(m)
  floor(x*n)+1
}
v = f(100,10)
mean(v)
hist(v,breaks = seq(0.5,10.5))




