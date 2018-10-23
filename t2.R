tt = function(al,be){
  dist = 0
  repeat {
    u = runif(1)
    v = runif(1)
    t = u^(1/al)+v^(1/be)
    if( t<=1 ){
      dist = u^(1/al)/( u^(1/al)+v^(1/be) )
      break
    }
  }
  return(dist)
}
tt(2,2)

m = 10000
bet = numeric(m)
for(i in 1:m){
  t = tt(2,2)
  if( t == 0 ){
    i = i-1
  }else{
    bet[i] = t  
  }
}
hist(bet,prob=T,main = 'method 1')
zz = seq(min(bet), max(bet), by=.01)
lines(zz, dbeta(zz,2,2), col="blue")
#bet
?