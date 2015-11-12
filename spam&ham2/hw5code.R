library(rpart)
load("E:/STA141/ASSIGNMENT/5/trainVariables hehe.rda")
load("E:/STA141/ASSIGNMENT/5/testData.rda")
load("E:/STA141/ASSIGNMENT/5/blindTestData.rda")
t= trainVariables
tes = testVariables
bl = blindTestVariables

#replace na by mean of corresponding column
for(i in 1: 29){
  t[which(is.na(t[,i])),i] = mean(t[,i] , na.rm = TRUE)
}
for(i in 1: 29){
  tes[which(is.na(tes[,i])),i] = mean(tes[,i] , na.rm = TRUE)
}

for(i in 1: 29){
  bl[which(is.na(bl[,i])),i] = mean(bl[,i] , na.rm = TRUE)
}




split_index = 
    #
    #split the trainDate
    #n£ºthe number of data set in cross validation
    #trainVariables: the trainVariables which need to be splitted
    #
function(n,trainVariables){
  index=0
  
  groups = rep(1:n, length=length(trainVariables[[1]]))
  
  for(i in 1:n){
    index[i] =list(which(groups==i))
  }
  index
}



result = 
  #
  #the result and prediction err of the model
  #testVariables: the test data set
  #pred: the prediction value of each model
  #
  function(testVariables , pred){
    l1=l2=l3=l4=0
    
    for(i in 1:length(testVariables$isSpam)){
      if(testVariables$isSpam[i]==TRUE & pred[i]==TRUE) l1 = l1+1
      if(testVariables$isSpam[i]==TRUE & pred[i]==FALSE) l2 = l2+1
      if(testVariables$isSpam[i]==FALSE & pred[i]==TRUE) l3 = l3+1
      if(testVariables$isSpam[i]==FALSE & pred[i]==FALSE) l4 = l4+1
    }
    
    err = (l2+l3)/length(testVariables$isSpam)
    result = data.frame(spam = c(l1,l2), ham = c(l3,l4))
    rownames(result) = c("spam","ham")
    list(err,result)
  }


#model of classification tree
classification =  
    #
    #function for creating classification tree model to classify
    #trainVariables:variables in training dataset
    #testVariables: variables in testing dataset
    #first create the tree, then prune the tree, finally do prediction
    #
function(trainVariables , testVariables){
  model = rpart(isSpam~. , method = "class" ,data = trainVariables)
  pmodel = prune(model,cp = 
                 model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
  
  pred = as.logical(predict(pmodel , testVariables,type = "class"))
  pred
}




classification_test = 
    #
    #function to test model classification tree
    #use classification() to test testdataset
    #
function(testVariables , trainVariables){
  pred = classification(trainVariables,testVariables) 
  result = result(testVariables,pred)
  result
}

main_ct = 
    #
    #cross validation for classification tree model
    #t: the trainVariables
    #splitindex: the index of the trainVriables
    #n:the number to split
    #
function(t , splitindex,¡¡n){
  r=0
  sum_c = 0
  
  for(i in 1:n){
    tt = t[unlist(splitindex[i]),]
    ttr = t[-unlist(splitindex[i]),]
    r[i] = list(classification_test(testVariables = tt , ttr))
    print(r[i])
  }
  
  for(i in 1:n){
    sum_c = r[[i]][[1]]+sum_c
  }
  sum_c/n
}



#model of k-NN(this code from dicussion7 and i understand this code)
knn = 
    #
    #k: the number of the nearest neighbors
    #test.index: the indext of the cases which need to be test
    #distmatrix: the matrix of distence which we calculate before
    #
function(k, test.index , distmatrix){
  ord = distmatrix[ ,test.index]
  nn = apply(ord , 2 , drop.first , drop = test.index)
  nn = nn[seq(k),]
  
}

drop.first = function(x,drop){
  x[-match(drop,x)]
}



knn_test = 
    #
    #test_index: the index of test cases
    #ord.mat: pre_computed matrix
    #k: the number of the nearest neighbors
    #data: dataset(trainVariables or testVariables)
function(test_index,ord.mat,k,data){
    pred = 0
    
    knn_num = knn(k,test_index,ord.mat ) 
    
    for(i in 1:length(knn_num[1 , ])){
      xx = table(c( data[knn_num[ , i] , ]$isSpam , FALSE , TRUE ))
      if(xx['TRUE'] / xx['FALSE'] >= .5) pred[i] = TRUE
      else pred[i] = FALSE
    }
    
    result(data[test_index , ] , pred)
  }

main_knn(n,k,splitindex,ord.mat,t)



main_knn = 
    #
    #n: the number of splitted datasets
    #k: the number of the nearest nighbors
    #ord.mat: the matrix of distance
    #data: dataset
    #
function(n, k , splitindex , ord.mat,data){
  sum_k = 0
  
  r_knn=0
  for(i in 1:n){
    tt = unlist(splitindex[i])
    r_knn[i] =list(knn_test(tt,ord.mat,k,data))
  }
  
  for(i in 1:n){
    sum_k = r_knn[[i]][[1]]+sum_k
  }
  sum_k/n
  
}


plotmix =
    #
    #function to plot
    #
function(a,b)
{
  plot(density(a))
  lines(density(b),col = "blue")
}


#training data: cross validation
#data using :trainingVariables
n = 5
splitindex = split_index(n,t)

#model classification
main_ct(t, splitindex,n)    

#model k-nn
t1 = t[-30]
scale_t = scale(t1)
dists_t = dist(scale_t,"manhattan")
dists_t = as.matrix(dists_t)
ord.mat = apply(dists_t , 2, order)

k=2
main_knn(n,k,splitindex,ord.mat,t)

#test which k is best
eff = 0
for(k in 2:20){
  eff[k]=main_knn(n,k,splitindex,ord.mat,t)
}

plot(sapply(2:20 , function(i) main_knn(n,i,splitindex,ord.mat,t)) , xlim= c(2,20), type = "b")


#testData
#model1
classification_test(tes,t)

#model2
a = rbind(tes,t)
scale_a = scale(a[-30])
dists_a = dist(scale_a,"manhattan")
dists_a = as.matrix(dists_a)
ord.mat_a = apply(dists_a , 2, order)
k =2
knn_test(1:2000,ord.mat_a , k , a)
################################################
knn_test2 = 
  #
  #test_index: the index of test cases
  #ord.mat: pre_computed matrix
  #k: the number of the nearest neighbors
  #data: dataset(trainVariables or testVariables)
  function(test_index,ord.mat,k,data){
    pred = 0
    
    knn_num = knn(k,test_index,ord.mat ) 
    
    for(i in 1:length(knn_num[1 , ])){
      xx = table(c( data[knn_num[ , i] , ]$isSpam , FALSE , TRUE ))
      if(xx['TRUE'] / xx['FALSE'] >= .5) pred[i] = TRUE
      else pred[i] = FALSE
    }
    
    pred
  }
################################################
#explore the misclassified observations
table(tes$isSpam)
resct =  classification(t,tes)
ctmis_f = which(resct[1:1511]==TRUE) 
ctmis_t = which(resct[1512:2000]==FALSE) 
#subset the table
#aaa is the mis-classify of ham
#bbb is correct mail of ham
#ccc is the mis-classify of spam
#ddd is correct mail of spam
aaa = data.frame(tes[ctmis_f,])
bbb = data.frame(tes[1:1511,][-ctmis_f,])

ccc = data.frame(tes[ctmis_t,])
ddd = data.frame(tes[1512:2000,][-ctmis_t,])

par(mfrow = c(3,4))
sapply(1:12 , function(i) plotmix(ccc[,i],ddd[,i]))
sapply(13:24 , function(i) plotmix(ccc[,i],ddd[,i]))
sapply(25:29 , function(i) plotmix(ccc[,i],ddd[,i]))


#kNN
resknn = knn_test2(1:2000 , ord.mat_a , k , a)
knnmis_f = which(resknn[1:1511]==TRUE) 
knnmis_t = which(resknn[1512:2000]==FALSE) 

aaa1 = data.frame(tes[knnmis_f,])
bbb1 = data.frame(tes[1:1511,][-knnmis_f,])
ccc1 = data.frame(tes[ctmis_t,])
ddd1 = data.frame(tes[1512:2000,][-ctmis_t,])



par(mfrow = c(3,4))
sapply(1:12 , function(i) plotmix(aaa1[,i],bbb[,i]))
sapply(13:24 , function(i) plotmix(aaa1[,i],bbb[,i]))
sapply(25:29 , function(i) plotmix(aaa1[,i],bbb[,i]))

#find both classifiers' mis-classify 
which(resct[1:1511] + resknn[1:1511]==2)
which(resct[1511:2000] + resknn[1511:2000]==0)

###################################
knn_test1 = 
  #
  #test_index: the index of test cases
  #ord.mat: pre_computed matrix
  #k: the number of the nearest neighbors
  #data: dataset(trainVariables or testVariables)
  function(test_index,ord.mat,k,data){
    pred = 0
    
    knn_num = knn(k,test_index,ord.mat ) 
    
    for(i in 1:length(knn_num[1 , ])){
      xx = table(c( data[knn_num[ , i] , ]$isSpam , FALSE , TRUE ))
      if(xx['1'] / xx['0'] >= .5) pred[i] = TRUE
      else pred[i] = FALSE
    }
    
    pred
  }
######################################
#test blind
#knn
load("E:/STA141/ASSIGNMENT/5/blindTestData.rda")
bl = blindTestVariables
temp = data.frame(c(1:808))
colnames(temp) = "isSpam"
bl = cbind(bl,temp)

b = rbind(bl,t)
scale_b = scale(b[-30])
dists_b = dist(scale_b,"manhattan")
dists_b = as.matrix(dists_b)
ord.mat_b = apply(dists_b , 2, order)

k = 2
p = knn_test1(1:808,ord.mat_b , k , b)
table(p)

#classification tree
table(classification(trainVariables = t,testVariables = bl))
