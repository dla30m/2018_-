#p7 Example 3.(a)(b)
x = numeric(1e4)
y = rep(1:13,4)
for (i in 1:1e4){
  a = sample(y,5)
  x[i] = sum(a==1)
}
mean(x==0)

round(choose(4, 0)*choose(48, 5)/choose(52, 5), 4)

for (i in 1:1e4){
  a = sample(y,5)
  x[i] = all(a >1)
}
mean(x)
x