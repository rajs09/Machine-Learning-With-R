str(heart_tidy)
head(heart_tidy)
tail(heart_tidy)
nrow(heart_tidy)
ncol(heart_tidy)
load(caret)
library(caret)
set.seed(3033)
train=createDataPartition(y=heart_tidy$V14,p=0.7,list=FALSE)
trd<-heart_tidy[train,]
td<-heart_tidy[-train,]
dim(trd);dim(td)
anyNA(heart_tidy)
??dim
str(heart_tidy)
trd[["V14"]]<-factor(trd[["V14"]])
td[["V14"]]<-factor(td[["V14"]])
trctl<-trainControl(method="repeatedcv",number=10,repeats=3)
set.seed(3233)
svm_linear<-train(V14~.,data=trd,method="svmLinear",trControl=trctl,preProcess=c("center","scale"),tuneLength=10)
svm_linear
test_pred<-predict(svm_linear,newdata=td)
test_pred
confusionMatrix(test_pred,td$V14)
#tuning linear svmmodel with different cost values
grid=expand.grid(C=c(0,0.01,0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,5))
set.seed(3233)
svm_linear_grid=train(V14~.,data=trd,method="svmLinear",trControl=trctl,
                      preprocess=c("center","scale"),tuneGrid=grid,tuneLength=10)
svm_linear_grid
plot(svm_linear_grid)
test_pred_grid=predict(svm_linear_grid,newdata=td)
test_pred_grid
confusionMatrix(test_pred_grid,td$V14)
#svm  non linear kernel
set.seed(3233)
svm_radial_grid=train(V14~.,data=trd,method="svmRadial",trControl=trctl,
                      preProcess=c("center","scale"),tuneLength=10)
svm_radial_grid
plot(svm_radial_grid)
test_pred_radial=predict(svm_radial_grid,newdata=td)
confusionMatrix(test_pred_radial,td$V14)
grid_radial=expand.grid(sigma=c(0,0.01,0.02,0.025,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1
                                ,0.25,0.5,0.75,0.9),
                        C=c(0,0.01,0.05,0.1,0.25,0.5,0.75,1,1.5,2,5))
set.seed(3233)
svm_radial=train(V14~.,data=trd,method="svmRadial",trControl=trctl,
                 preProcess=c("center","scale"),tuneGrid=grid_radial,tuneLength=10)
svm_radial
test_pred_radial1=predict(svm_radial,newdata=td)
confusionMatrix(test_pred_radial1,td$V14)
plot(svm_radial)
library(ggplot2)
qplot(Petal.Length,Petal.Width,data=iris,color=Species)
library(e1071)
svm_model<-svm(Species~.,data=iris)
svm_model
summary(svm_model)
plot(svm_model,data=iris,Petal.Width~Petal.Length,slice=list(Sepal.Width=3,Sepal.Length=4))
pred<-predict(svm_model,iris)
summary(pred)
pred
#Accuracy of model
#confusion matrix
tab<-table(Predicted=pred,Actual=iris$Species)
tab
#misclassification rate
#accuracy
sum_tab<-sum(diag(tab)/sum(tab))
1-sum_tab
#linearsvmmodel
svm_model_linear<-svm(Species~.,data=iris,kernel="linear")
summary(svm_model_linear)
plot(svm_model_linear,data=iris,Petal.Width~Petal.Length,slice=list(Sepal.Width=3,Sepal.Length=4))
pred_linear<-predict(svm_model_linear,iris)
summary(pred_linear)
pred_linear
taba<-table(Predicted=pred_linear,Actual=iris$Species)
sum_taba<-sum(diag(taba)/sum(taba))
sum_taba

#polynomial svm
svm_model_polynomial<-svm(Species~.,data=iris,kernel="polynomial")
summary(svm_model_polynomial)
plot(svm_model_polynomial,data=iris,Petal.Width~Petal.Length,slice=list(Sepal.Width=3,Sepal.Length=4))
pred_polynomial<-predict(svm_model_polynomial,iris)
summary(pred_polynomial)
tabas<-table(Predicted=pred_polynomial,Actual=iris$Species)
sum_tabas<-sum(diag(tabas)/sum(tabas))
sum_tabas

#Tune the model
tmodel1<-tune(svm,Species~.,data=iris,ranges=list(epsilon=seq(0,1,0.1),cost=2^(2:7)))
plot(tmodel1)
summary(tmodel1)

#cost value change
tmodel<-tune(svm,Species~.,data=iris,ranges=list(epsilon=seq(0,1,0.1),cost=2^(2:7)))
plot(tmodel)
summary(tmodel)

mymodel<-tmodel1$best.model
summary(mymodel)
plot(mymodel,data=iris,Petal.Width~Petal.Length,slice=list(Sepal.Width=3,Sepal.Length=4))
pred_mymodel<-predict(mymodel,iris)
summary(pred_mymodel)
tabas<-table(Predicted=pred_mymodel,Actual=iris$Species)
sum_tabas<-sum(diag(tabas)/sum(tabas))
sum_tabas



###########heart data

svm_model<-svm(V14~.,data=heart_tidy)
svm_model
summary(svm_model)

plot(svm_model,data=heart_tidy,Petal.Width~Petal.Length,slice=list(Sepal.Width=3,Sepal.Length=4))
pred<-predict(tmodel1,heart_tidy)
summary(pred)
heart_tidy$V14=factor(heart_tidy[["V14"]])
svm_model<-svm(V14~.,data=heart_tidy)
tmodel1<-tune(svm,V14~.,data=heart_tidy,ranges=list(epsilon=seq(0,1,0.1),cost=2^(2:6)))
plot(tmodel1)
summary(tmodel1)
tabas<-table(Predicted=pred,Actual=heart_tidy$V14)
sum_tabas<-sum(diag(tabas)/sum(tabas))
sum_tabas


mymodel<-tmodel1$best.model
summary(mymodel)
plot(mymodel,data=heart_tidy,Petal.Width~Petal.Length,slice=list(Sepal.Width=3,Sepal.Length=4))
pred_mymodel<-predict(mymodel,heart_tidy)
summary(pred_mymodel)
tabas<-table(Predicted=pred_mymodel,Actual=heart_tidy$V14)
tabas
sum_tabas<-sum(diag(tabas)/sum(tabas))
summary(sum_tabas)
summary(pred_mymodel)
summary(tmodel1)
