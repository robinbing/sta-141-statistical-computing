#the first algorithm to generate sample
#inputs are the numbers of sample, probilities of events, 
#and mean and standard of normal distributions, function 
#return is numSample random numbers

mixture1 = function (numSample , prob , meanSd){
  #use sample to choose distribution from k norems
  knorm=sample(c(1:length(prob)) , size = numSample , replace = TRUE , prob = prob)
 
  #use sapply to get the distrbution
  sapply(1:numSample , function(p) rnorm( 1, meanSd[1 , knorm[p]] , meanSd[2 , knorm[p]]))
} 

#the second algorithm to generate sample
#inputs are the numbers of sample, probilities of events, 
#and mean and standard of normal distributions, function 
#return is numSample random numbers

select = function( n = 10000, probs )
{
  CDF = c(0, cumsum( probs ))
  u = runif(n)
  
  g = cut(u, CDF)
  names(probs)[g]
} 


mixture2 = function (numSample , probs , meanSd){
  gg=select(numSample, probs)
  
  gg = c(gg , names(probs))               #Let all events show up in gg, so we can use sapply to manipulate 
  num = table(gg)
  
  sapply(1:length(probs), function(p) rnorm(as.vector(num[p]-1) , meanSd[1,p] , meanSd[2,p]))
}



#give the k normal distributions
meanSd = matrix( c(-3,10,6,1,2,3), nrow = 2, ncol = 3, byrow = F)

#obtain the sample
tt1 = mixture1(10000 , c(a=.1,b=.4,c=.5) , meanSd)
tt2 = mixture2(10000 , c(a=.1,b=.4,c=.5) , meanSd)

#plot the distribution we obtain to figure out whether the two results are the same
plot( density(tt1) , main = "distribution" )
lines( density(unlist(tt2)) , col = "blue" )
legend( "topleft" , legend = c("distribution of algorithm1","distribution of algorithm2"),
       col = c("black","blue") , cex = .5, lty = c(1,1) , lwd = c(2,.7) , title = "Line Types", 
       xjust = 0 , yjust = 0,text.width=20)

#use qqplot to identify
qqplot(tt1,unlist(tt2))
abline(a = 0,b = 1)

#plot the time to compare algorithm1 and algorithm2
x=c(1, 2, 5, 10, 100, 1000, 10000)

time_for_1=sapply(1:length(x), function(p) system.time(replicate(100,mixture1(x[p],c(a=.1,b=.4,c=.5) , meanSd)))/100)
time_for_2=sapply(1:length(x), function(p) system.time(replicate(100,mixture2(x[p],c(a=.1,b=.4,c=.5) , meanSd)))/100)

plot( time_for_1[3,] , main=("elapsed"), xlab="kth test" , ylab="time" , col='red', pch = 19,type="b")
points(time_for_2[3,] , pch = 19,type="b")

legend( "topleft" , legend = c("time of algorithm1","time of algorithm2"),
        col = c("red","black") , cex = .5, lty = c(1,1) , lwd = c(2,.7) , title = "Line Types", 
        xjust = 0 , yjust = 0,text.width=3)


##########################################################
#consider about the parameters in the component densities 
meanSd1 = matrix( c(-3,10,6,1,2,3), nrow = 2, ncol = 3, byrow = F)
meanSd2 = matrix( c(-9,100,36,1,4,9), nrow = 2, ncol = 3, byrow = F)
meanSd3 = matrix( c(-9^9,10^9,6^9,1,4^9,9^9), nrow = 2, ncol = 3, byrow = F)
time_for_1 = sapply(1:length(x), function(p) system.time(replicate(100,mixture1(x[p],c(a =.1,b =.4,c =.5) , meanSd1)))/100)
time_for_2 = sapply(1:length(x), function(p) system.time(replicate(100,mixture1(x[p],c(a =.1,b =.4,c =.5) , meanSd2)))/100)
time_for_3 = sapply(1:length(x), function(p) system.time(replicate(100,mixture1(x[p],c(a =.1,b =.4,c =.5) , meanSd3)))/100)

plot( time_for_1[3,] , main = ("elapsed for algorithm1"), xlab = "kth test" , ylab = "time" , col = "red", pch = 19, type = "b")
points(time_for_2[3,] , pch = 19,type = "b")
points(time_for_3[3,] , pch = 19,type = "b" ,col = "blue")

legend( "topleft" , legend = c("component1","component2(bigger number)","component3(biggest number)"),
        col = c("red","black","blue") , cex = .5, lty = c(1,1) , lwd = c(2,.7) , title = "Line Types", 
        xjust = 0 , yjust = 0,text.width=3)

#the sencond algorith
meanSd1 = matrix( c(-3,10,6,1,2,3), nrow = 2, ncol = 3, byrow = F)
meanSd2 = matrix( c(-9,100,36,1,4,9), nrow = 2, ncol = 3, byrow = F)
meanSd3 = matrix( c(-9^9,10^9,6^9,1,4^9,9^9), nrow = 2, ncol = 3, byrow = F)
time_for_1 = sapply(1:length(x), function(p) system.time(replicate(100,mixture1(x[p],c(a =.1,b =.4,c =.5) , meanSd1))) / 100)
time_for_2 = sapply(1:length(x), function(p) system.time(replicate(100,mixture1(x[p],c(a =.1,b =.4,c =.5) , meanSd2))) / 100)
time_for_3 = sapply(1:length(x), function(p) system.time(replicate(100,mixture1(x[p],c(a =.1,b =.4,c =.5) , meanSd3))) / 100)

plot( time_for_1[3,] , main = ("elapsed for algorithm2"), xlab = "kth test" , ylab = "time" , col = 'red', pch = 19,type = "b")
points(time_for_2[3,] , pch = 19, type = "b")
points(time_for_3[3,] , pch = 19,type = "b" ,col = "blue")

legend( "topleft" , legend = c("component1","component2(bigger number)","component3(biggest number)"),
        col = c("red","black","blue") , cex = .5, lty = c(1,1) , lwd = c(2,.7) , title = "Line Types", 
        xjust = 0 , yjust = 0,text.width=3)

#######################################################
#consider the number of components in the mixture
meanSd1 = matrix( c(-3,10,6,1,2,3), nrow = 2, ncol = 3, byrow = F)
meanSd2 = matrix( c(-3,10,6,5,7,9,-1,3,9,1,2,3), nrow = 2, ncol = 6, byrow = F)
meanSd3 = matrix( c(-3,10,6,5,7,9,,20,11,2,7,13,10,-1,3,9,1,2,3), nrow = 2, ncol = 9, byrow = F)

time_for_1 = sapply(1:length(x), function(p) system.time(replicate(100,mixture1(x[p],c(a =.1,b =.4,c =.5) , meanSd1)))/100)
time_for_2 = sapply(1:length(x), function(p) system.time(replicate(100,mixture1(x[p],c(a =.1,b =.4,c =.5) , meanSd2)))/100)
time_for_3 = sapply(1:length(x), function(p) system.time(replicate(100,mixture1(x[p],c(a =.1,b =.4,c =.5) , meanSd3)))/100)

plot( time_for_1[3,] , main = ("elapsed for algorithm2"), xlab = "kth test" , ylab = "time" , col = 'red', pch = 19,type = "b")
points(time_for_2[3,] , pch = 19,type="b")
points(time_for_3[3,] , pch = 19,type="b" ,col="blue")

legend( "topleft" , legend = c("k=3","k=6","k=9"),
        col = c("red","black","blue") , cex = .5, lty = c(1,1) , lwd = c(2,.7) , title = "Line Types", 
        xjust = 0 , yjust = 0,text.width=3)


#the second algorithm
meanSd1 = matrix( c(-3,10,6,1,2,3), nrow = 2, ncol = 3, byrow = F)
meanSd2 = matrix( c(-3,10,6,5,7,9,-1,3,9,1,2,3), nrow = 2, ncol = 6, byrow = F)
meanSd3 = matrix( c(-3,10,6,5,7,9,,20,11,2,7,13,10,-1,3,9,1,2,3), nrow = 2, ncol = 9, byrow = F)

time_for_1 = sapply(1:length(x), function(p) system.time(replicate(100,mixture1(x[p],c(a =.1,b =.4,c =.5) , meanSd1)))/100)
time_for_2 = sapply(1:length(x), function(p) system.time(replicate(100,mixture1(x[p],c(a =.1,b =.4,c =.5) , meanSd2)))/100)
time_for_3 = sapply(1:length(x), function(p) system.time(replicate(100,mixture1(x[p],c(a =.1,b =.4,c =.5) , meanSd3)))/100)

plot( time_for_1[3,] , main = ("elapsed for algorithm1"), xlab = "kth test" , ylab = "time" , col = 'red', pch = 19,type = "b")
points(time_for_2[3,] , pch = 19,type = "b")
points(time_for_3[3,] , pch = 19,type = "b" ,col = "blue")

legend( "topleft" , legend = c("k=3","k=6","k=9"),
        col = c("red","black","blue") , cex = .5, lty = c(1,1) , lwd = c(2,.7) , title = "Line Types", 
        xjust = 0 , yjust = 0,text.width=3)
