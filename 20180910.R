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
> x=runif(10,0,1)
> y=floor(x*10)+1;y
 [1] 2 7 7 7 9 7 1 3 7 6
> a=runif(10,0,1)
> b=floor(x*10)+1;b
 [1] 2 7 7 7 9 7 1 3 7 6
> b=floor(a*10)+1;b
 [1]  7  6  3 10  3  9  3  3  2  3
> sum(y==b)
 [1] 1
