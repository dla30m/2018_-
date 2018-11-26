#Exercise 5.4
#r顏色種類 n顆球 抽m顆球
f = function(r,m,n){
  v = NULL
  ball = sample(1:r,n,replace=T)
  which = sample(1:n,m)
  for(i in 1:r){
    v = c(v,sum( ball[which]==i ))
  }
  return(v)
}
f(5,10,50)