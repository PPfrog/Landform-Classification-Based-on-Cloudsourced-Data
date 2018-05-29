#Tree classification

library(tree)
library(ISLR)
attach(train)

tree.train = tree(class~., train)
plot(tree.train)
text(tree.train, pretty = 0)
cv.train = cv.tree(tree.train, FUN = prune.misclass)
par(mfrow=c(1,2))
plot(cv.train$size, cv.train$dev, type = "b")
plot(cv.train$k, cv.train$dev, type = "b")

tree.train.com = tree(class~., train, control = tree.control(minsize = 2, mincut = 1, nobs = 10545, mindev = 0))
cv.train.com = cv.tree(tree.train.com, FUN=prune.misclass)
cv.train.com
com.best = cv.train.com$size[43]
com.best
prune.train.com = prune.misclass(tree.train.com, best = com.best)
tree.pred.com = predict(prune.train.com, test[,-1],type = "class")
table(tree.pred.com,test[,1])
mean(tree.pred.com == test[,1])




