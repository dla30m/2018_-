#一次骰兩顆骰子總數和的機率
m = 10000
x = numeric(m)
for (i in 1:m){
 x[i] = sum(sample(1:6,2,repl = T))
}
summary(as.factor(x))
sim = round(summary(as.factor(x))/m, 3)
exa = round(c(1:6, 5:1)/36, 3); rbind(sim, exa)

#總數和等於六和十二的機率
mean(x==6 | x==12) # '|'是或

#60個人在不同天生日的機率
n = 1:60
p = numeric(60)
for (i in n){
 q = prod(1-(0:(i-1))/365)
 p[i] = 1-q
}
plot(n,p)
plot(n,p,col = c('red','blue')) # col 交替顏色
abline(h=1/exp(1),col = "darkgreen")
abline(a = 0.2,b = 1, col = "red")
