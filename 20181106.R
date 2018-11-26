a = c(0.06,0.06,0.06,0.06,0.06,0.15,0.13,0.14,0.15,0.13)
a
z = NULL
for(i in 1:10000){
	z[i] = sample(1:10,1,prob = a)
}
hist(z,breaks = seq(0,10,by=1))

#Exercise 5.3
fx = function(j){
  return( (1/2)^(j+1) + (1/2)*(2^(j-1))/(3^j) )
}
ex = function(){
  u = runif(1)
  a = 1
  k = 0
  repeat{
    k = k+fx(a)
    if(u < k ){
      return(a)
    }
    a = a+1;
  }
}

t = replicate(10000,ex())
mean(t)
hist(t)$count