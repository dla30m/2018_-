m = 10000
x = numeric(m)
for (i in 1:m){
  ace = sample(1:52,4)
  x[i] = sum(ace<=4)
}
summary(as.factor(x))
summary(as.factor(x))/m
round(choose(4, 0)*choose(48, 4)/choose(52, 4), 4)
mean(x==0)
