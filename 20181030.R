#Exercise 4.4
u = sample(2:12,5,rep=T);u
u[1]

EX4.4.1 = function(){
  ar = numeric(11)
  i = 0
  repeat{
    i = i+1
    u = sample(1:6,1)
    v = sample(1:6,1)
#   print(u+v)
    ar[u+v-1] = 1
#   print(ar)
    if(sum(ar)==11) 
      break;
  }
  return(i)
}

EX4.4.2 = function(k){
  ar = numeric(k)
  for(i in 1:k){
    ar[i] = EX4.4.1()
  }
  return(ar)
}
EX4.4.2(100)