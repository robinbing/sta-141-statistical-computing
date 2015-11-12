#All these three functions are based on what we derive before.

#function dtriang()

dtriang = function(a = 0 , b = 2 , c=1 , X){
  fun = c( 1:length(X))
  for( i in 1:length(X) ){
    if (X[i] >= a & X[i] < c) 
      fun[i] = 2*(X[i] - a) / ( (b - a) * (c - a) )
    else if(X[i] >= c & X[i] <= b)
      fun[i] = 2*(b - X[i]) / ( (b - a) * (b - c) )
    else
      fun[i] = 0
  }
  fun
}

plot(dtriang(,,,seq(0,2,0.001)), main = ("function dtriang"))

#function ptriang()


ptriang = function(a = 0 ,b = 2 ,c = 1 , X){
  Fun=c(1:length(X))
  for( i in 1:length(X) ){
    if( X[i] <= a)
      Fun[i] = 0
    else if (X[i] >= a & X[i] < c) 
      Fun[i] = (X[i] - a)*dtriang(a = a, b = b, c = c, X = X[i]) / 2
    else if(X[i] >= c & X[i] <= b)
      Fun[i] = (b - a )*dtriang(a = a, b = b, c =c , X = c) / 2 - (b - X[i]) * 
        dtriang(a = a, b = b, c = c, X = X[i]) / 2
    else
      Fun[i] = 1
  }
  Fun
}

plot(ptriang(,,,seq(0,2,0.001)), main = ("function ptriang"))
#function rtriang()

#function inverse CDF
inCDF = function (a = 0 , b = 2 , c = 1, X){
  inF = c(1:length(X))
  for( i in 1:length(X) ) {
    if( X[i] < 0)  return(sprintf( "x<%f" , a ))
    else if(X[i] >= ptriang(a = a, b = b, c = c, X = a) & X[i] < ptriang(a = a, b = b, c = c, X = c))
      inF[i] = a + sqrt( (b - a) * (c - a) * X[i])
    else if(X[i] >= ptriang(a = a, b = b, c = c, X = c) & X[i] <= ptriang(a = a, b = b, c = c, X = b))
      inF[i] = b - sqrt( (b - a) * (b - c) * (1 - X[i]))
    else if(X[i] > 1) return(sprintf( "x>%f" , b ))
  }
  inF
}

#use inCDF to samply from a Triangular distribution
rtriang = function (n = 1, a = 0, b = 2,c = 1){
  x = runif(n,0,1)
  inCDF(a = a, b = b, c = c, X = x)
}

plot(density(rtriang(100000)), main = ("function rtriang"))
