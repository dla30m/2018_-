#積分函數
#要下載Ryacas
install.packages("Ryacas")
library(Ryacas)
x = Sym('x')
Integrate( exp(x),x,0,x )
Integrate( (x-2)/2,x,2,x )

#出來是字串無法令成函數
