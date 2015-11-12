#get the plot of nodeDensity
x <- y <-seq(1,100,1)
z = outer(x,y,nodeDensity)


persp(x,y,z,theta=-30,phi=30,expand=0.6,col="lightblue")
persp(x,y,z,theta=0,phi=30,expand=0.6,col="lightblue")

filled.contour(x, y, z,color.palette=heat.colors)


#two way to find the max number in the density
##############
#the first one 
nodeDensity
#then we will find hiDensity = a * cos(river[hiArea] * pi / (2 * band)) + b,
#and obviously it's at most 4
mx=4
###############
#the second way
nDensity = function(x,y) {
  z = - nodeDensity(x,y) 
  return(z)
  }

nlm(nDensity , p=50 , y =40)
mx=round(-nlm(nDensity , p=50 , y =40)$minimum)     #take the value a bit more than what we get

#sampleAR is the function which uses Acceptance/Reject to sample
sampleAR = function( fun , n ){
  
  samples = matrix(c(1:(2*n)) , nrow=2)                        #initialize parameters 
  acceptnum = 0
  totalnum = 0
  
  while(acceptnum < n){
    totalnum = totalnum + 1
    
    x = runif(1 ,0 ,100 )
    y = runif(1 ,0 ,100 )
    u = runif(1 ,0 ,mx )           
    if( u <= fun(x,y)){
      acceptnum = acceptnum + 1
      samples[1 , acceptnum] = x
      samples[2 , acceptnum] = y
    }
  }
  
    return(list(totalnum , samples) )
}


ff = sampleAR(nodeDensity, 30000)

#efficience 
eff = dim(ff[[2]])[2] / ff[[1]]
eff

#plot for samples
library(MASS)
tt=kde2d(ff[[2]][1,],ff[[2]][2,],n=300,h=8)
persp(tt,theta=60,phi=30,expand=0.6,col="lightgreen")

tt=kde2d(ff[[2]][1,],ff[[2]][2,],n=300,h=4)
filled.contour(tt,,color.palette=heat.colors)
