set.seed(1234)
b = round(runif(10,0,1)*20+1);b
d = sum(b[b%%2==0])/sum((b%%2==0));d

f = 0;for (i in 1:100){
   b = round(runif(10,0,1)*20+1)
   d = sum(b[b%%2==0])/sum((b%%2==0))
   f = f+d
}
f=f/100
f