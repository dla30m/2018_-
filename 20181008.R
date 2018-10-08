set.seed(1234)
#Gamma X = -1/lambda * log(U)
gam = function(lan){
  run = runif(20,0,1)
  x = -(1/lan)*log(prod(run))
  x
}
x = gam(2)
x
#§ä¥­§¡­È
qu = function(n){
  a = 0
  for (i in 1:n){
    a = a + gam(2)
  }
  a = a/n
  a
}
ans = qu(100)
ans
