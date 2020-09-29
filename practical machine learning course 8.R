library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(party)
library(partykit)

trainfile <- 'pml-training.csv'
testfile <- 'pml-testing.csv'
training <- read.csv(trainfile, header=TRUE, na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(testfile, header=TRUE, na.strings=c("NA", "#DIV/0!", ""))

dim(training)
dim(testing)

Tr1 <- createDataPartition(training$classe, p=0.75, list=FALSE)
Train <- training[Tr1, ]
Test <- training[-Tr1, ]
dim(Train)
dim(Test)

nzv <- nearZeroVar(Train, saveMetrics=TRUE)
Train <- Train[,nzv$nzv==FALSE]

nzv <- nearZeroVar(Test,saveMetrics=TRUE)
Test <- Test[,nzv$nzv==FALSE]

Train <- Train[c(-1)]

clean_train <- Train
for(i in 1:length(Train)) {
  if( sum( is.na( Train[, i] ) ) /nrow(Train) >= .6) {
    for(j in 1:length(clean_train)) {
      if( length( grep(names(Train[i]), names(clean_train)[j]) ) == 1)  {
        clean_train <- clean_train[ , -j]
      }   
    } 
  }
}
# Set the cleansed data set back to the original variable, thus overwriting the original.
Train <- clean_train
```

# Next, I transform the data on the myTesting and testing data sets.
```{r}
clean1 <- colnames(Train)

# remove the classe column
clean2 <- colnames(Train[, -58]) 

# only allow columns in Test that are also in Train
Test <- Test[clean1]         

# only allow testing that are also in Train
testing <- testing[clean2]             

dim(Test)

dim(testing)

for (i in 1:length(testing) ) {
  for(j in 1:length(Train)) {
    if( length( grep(names(Train[i]), names(testing)[j]) ) == 1)  {
      class(testing[j]) <- class(Train[i])
    }      
  }      
}

# To get the same class between testing and Train
testing <- rbind(Train[2, -58] , testing)
testing <- testing[-1,]

set.seed(156272)
# Create a decision tree first
dt1 <- rpart(classe ~ ., data=Train, method="class")
# Next, create predictions from the results of the decision tree model.
predict1 <- predict(dt1, Test, type = "class")

cm <- confusionMatrix(predict1, as.factor(Test$classe))

cm$overall

plot(cm$table, col = cm$byClass, main = paste("Decision Tree  - Confusion Matrix. Accuracy =", round(cm$overall['Accuracy'], 4)))

set.seed(156272)
rf1 <- randomForest(as.factor(classe) ~ ., data = Train)
rf_pred <- predict(rf1, Test, type = "class")
confmat <- confusionMatrix(rf_pred, as.factor(Test$classe))
# And display the confusion matrix with the results/statistics. I'm especially
# interested in the accuracy of the model and its ability to predict.
confmat

oos <- (1 - round(confmat$overall['Accuracy'], 4))*100
final_p <- predict(rf1, testing, type = "class")
final_p