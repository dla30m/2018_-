#睹计贺
set.seed(1234)
#uniformity 10计 01ぇい
run = runif(10,0,1)
#キА计 = 羆/
mean(run) = sum(run)/length(run)
#跑钵计
var(run) = (sum(run^2) - (sum(run)^2)/length(run)) / (length(run) - 1)
#俱计
round ; floor ; ceiling
#睹计俱计110ぇい
round(runif(10,0,1)*10+1)
#ㄢ秖い絋粄Τ碭计
x=runif(10,0,1)
y=floor(x*10)+1;y
a=runif(10,0,1)
b=floor(x*10)+1;b
b=floor(a*10)+1;b
sum(y==b)
