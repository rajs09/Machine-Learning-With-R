Library(ISLR)
Library(tree)
#change continous data to categorical data
#append column of categorical data and remove independent variable 
train<-sample(1*nrow(),factor*nrow())
saletree<-tree(categoricaldata~.,dataset)
plot(saletree)
text(saletree,pretty = 0)
test_sale<-predict(saletree,testing_data,type="class")
mean(test_sale!=testing_high)
cv_saletree <- cv.tree(saletree, FUN = prune.misclass)
names(cv_saletree)
plot(cv_saletree$size,cv_saletree$dev, type="b")
#prune the tree
prune_sale <- prune.misclass(saletree, best=9)
plot(prune_sale)
text(prune_sale, pretty=0)
#check the misfit error
prune_predict <- predict(prune_sale, testing_data, type ="class")
prune_predict
table(prune_predict)
mean(prune_predict!= testing_high)