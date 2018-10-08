set.seed(1234)
run1 = runif(2,0,1)
t = -1*log(prod(run1))
x = t*runif(1,0,1);x
y = t-x;y

CW3 = function(k){
  run1 = runif(k)
  t = -log(prod(run1))
  run2 = c(0,sort(runif(k-1)),1) # sort ±Æ§Ç
  x = numeric(k)
  for (i in 1:k){
    x[i] = t*(run2[i+1]-run2[i])
  }
  x
}
x = CW3(5);x
x

run2 = runif(5);run2
run2 = sort(run2);run2
run2 = c(0,run2,1);run2
