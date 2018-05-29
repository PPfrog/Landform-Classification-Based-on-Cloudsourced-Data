library(ISLR)
library(class)

#KNN vs. Validation Set Approach
setwd("~/Desktop/ERG 2050/project")
train = read.csv("training.csv", head = T)
attach(train)
error.set = matrix(nrow = 10, ncol =10)
for(i in 1:100){
  print(i)
  set.seed(i)
  train.index = sample(1:10545, 9000)
  train.x = cbind(train[,-1][,-1])[train.index,]
  test.x = cbind(train[,-1][,-1])[-train.index,]
  train.class = class[train.index]
  test.class = class[-train.index]
  k.set = c(1:10)
  for(j in 1:10){
    set.seed(1)
    best.knn.pred = knn(train.x, test.x, train.class, k = j)
    best.error = 1 - mean(best.knn.pred == test.class)
    error.set[i,j] = best.error
  }
}