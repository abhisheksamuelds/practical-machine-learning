library(kernlab)
library(caret)
library(corrplot)
library(randomForest)

data_training <- read.csv("D:/Learning/Statictics/Practical Machine Learning/Human Activity Recognition/input/pml-training.csv", na.strings= c("NA",""," "))
data_testing <- read.csv("D:/Learning/Statictics/Practical Machine Learning/Human Activity Recognition/input/pml-testing.csv", na.strings= c("NA",""," "))

data_training_NAs <- apply(data_training, 2, function(x) {sum(is.na(x))})
data_training_clean <- data_training[,which(data_training_NAs == 0)]
data_training_clean <- data_training_clean[8:length(data_training_clean)]

data_test_NAs <- apply(data_testing, 2, function(x) {sum(is.na(x))})
data_test_clean <- data_testing[,which(data_test_NAs == 0)]
data_test_clean <- data_test_clean[8:length(data_test_clean)]

inTrain <- createDataPartition(y = data_training_clean$classe, p = 0.75, list = FALSE)
training <- data_training_clean[inTrain, ]
crossval <- data_training_clean[-inTrain, ]

model <- randomForest(classe ~ ., data = training)

predictTrain <- predict(model, training)
confusionMatrix(training$classe, predictTrain)

predictCrossVal <- predict(model, crossval)
confusionMatrix(crossval$classe, predictCrossVal)

predictTest <- predict(model, data_test_clean)
predictTest

write.pml.predictions <- function(x) {
n = length(x)
for(i in 1:n){
filename = paste0("D:/Learning/Statictics/Practical Machine Learning/Human Activity Recognition/problem_id_",i,".txt")
write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
}
}

write.pml.predictions(predictTest)