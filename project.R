setwd("C:/Users/Maryam/Desktop/Datascientists/Practical Machine Learning")

library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(randomForest)
set.seed(123456)

train<-read.csv("pml-training.csv", na.strings = c("NA", "DIV/0!", ""))
test<-read.csv("pml-testing.csv", na.strings = c("NA", "DIV/0!", ""))

dim(train)
dim(test)
str(train)
str(test)

train_data<-sapply(colnames(train), function(x) if(sum(is.na(train[,x]))>0.75*nrow(train)) {
  return(T)
} else{
    return(F)
  }
)

train<-train[, !train_data]
train<-train[, -(1:6)]

train_set<-createDataPartition(y=train$classe, p=0.7, list=FALSE)
training<-train[train_set,]
testing<-train[-train_set, ]

model_1<-randomForest(classe~., data=training, method="class")
predict_1<-predict(model_1, testing, type="class")
confusionMatrix(predict_1, testing$classe)

model_2<-rpart(classe~.,data=training, method = "class")
predict_2<-predict(model_2, testing, type="class")
confusionMatrix(predict_2, testing$classe)
fancyRpartPlot(model_2)


predict_test<-predict(model_1, newdata=test)
predict_test