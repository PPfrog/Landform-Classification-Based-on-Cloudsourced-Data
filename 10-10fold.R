# library(ISLR)
# library(class)
# library(e1071)
# 
# #KNN vs. 10-fold CV
# setwd("~/Desktop/ERG 2050/project")
# train = read.csv("training.csv", head = T)
# attach(train)
# error.set = matrix(nrow = 10, ncol =10)
# for(i in 1:10){
#   print(i)
#   set.seed(i)
#   kfolds = tune.knn(train[,-c(1,2)], class, k = c(1:10))
#   for(j in 1:10){
#     error.set[i,j] = kfolds$performances$error[j]
#   }
# }

library(MASS)
library(caret)
setwd("~/Desktop/ERG 2050/project")
train = read.csv("training.csv", head = T)
attach(train)
error.set = rep(0,10)
cv.error = rep(0,10)
for(i in 1:10){
  print(i)
  set.seed(i)
  folds = createFolds(1:nrow(train))
  orchard.index = which(train$class=='orchard')
  orchard.data = train[orchard.index,]
  for(j in 1:10){
    test.index = unlist(folds[j])
    test.data=rbind(orchard.data,train[test.index,])
    qda.train=qda(class~.,data=train[-test.index,])
    qda.pred = predict(qda.train,train[test.index,-1])
    cv.error[j] = 1 - mean(qda.pred$class == train[test.index,1])
  }
  error.set[i] = mean(cv.error)
}