library(ISLR)
library(class)

#KNN vs. Validation Set Approach
setwd("~/Desktop/ERG 2050/project")
train = read.csv("training.csv", head = T)
attach(train)
set.seed(119)
train.index = sample(1:10545, 9000)
train.x = cbind(train[,-1][,-1])[train.index,]
test.x = cbind(train[,-1][,-1])[-train.index,]
train.class = class[train.index]
test.class = class[-train.index]
error.set = c()
k.set = c(1:10)
for(i in 1:10){
  set.seed(1)
  best.knn.pred = knn(train.x, test.x, train.class, k = i)
  best.error = 1 - mean(best.knn.pred == test.class)
  error.set[i] = best.error
  }
plot(x = k.set, y = error.set, xlab = "K", ylab = "Validation Set Approach Error",type = 'b')
error.set[4]

#KNN vs. LOOCV
loocv.error.set = c()
for(i in 1:10){
  set.seed(2)
  best.knn.pred = knn.cv(train[,-c(1,2)], class, k = i)
  best.error = 1 - mean(best.knn.pred == class)
  loocv.error.set[i] = best.error
}
plot(x =k.set, y = loocv.error.set, xlab = "K", ylab = "LOOCV Error Rate",type = 'b')
loocv.error.set[3]

#KNN vs. 10-fold C.V.

library(e1071)
set.seed(1)
kfolds = tune.knn(train[,-c(1,2)], class, k = c(1:10))
summary(kfolds)
plot(result.kfolds$k, result.kfolds$error, xlab = "K", ylab =  "10-fold CV Error Rate", type = "b")

#KNN vs. Boostrap