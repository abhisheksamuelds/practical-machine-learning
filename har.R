library(kernlab)
library(caret)
library(randomForest)

data_training <- read.csv("D:/Learning/Statictics/Practical Machine Learning/Human Activity Recognition/input/pml-training.csv", na.strings= c("NA",""," "))
data_testing <- read.csv("D:/Learning/Statictics/Practical Machine Learning/Human Activity Recognition/input/pml-testing.csv", na.strings= c("NA",""," "))

data_training_filter <- apply(data_training, 2, function(x) {sum(is.na(x))})
data_training_clean <- data_training[,which(data_training_filter == 0)]
data_training_clean <- data_training_clean[8:length(data_training_clean)]

data_test_filter <- apply(data_testing, 2, function(x) {sum(is.na(x))})
data_testing_clean <- data_testing[,which(data_test_filter == 0)]
data_testing_clean <- data_testing_clean[8:length(data_testing_clean)]

inTrain <- createDataPartition(y = data_training_clean$classe, p = 0.75, list = FALSE)
training <- data_training_clean[inTrain, ]
crossValidation <- data_training_clean[-inTrain, ]

model <- randomForest(classe ~ ., data = training)

predictTraining <- predict(model, training)
confusionMatrix(training$classe, predictTraining)

predictCrossValidation <- predict(model, crossValidation)
confusionMatrix(crossValidation$classe, predictCrossValidation)

predictTesting <- predict(model, data_testing_clean)
predictTesting

write.pml.predictions <- function(x) {
n = length(x)
for(i in 1:n){
filename = paste0("D:/Learning/Statictics/Practical Machine Learning/Human Activity Recognition/problem_id_",i,".txt")
write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
}
}

write.pml.predictions(predictTesting)
