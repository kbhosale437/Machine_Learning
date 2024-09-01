library(forecast)
library(caret)
library(FNN)
library(rpart) 
library(rpart.plot) ### rpart and rpart.plot libraries are for the tree analysis
library(randomForest)### for random forest
library(e1071) ## for Bayes classifier

## Import data - HW_1 data
HW_1 <- read.csv("/Users/karanbhosale/Documents/Machine learning/HW1.csv")
## Create training and Validation data sets
train.index <- sample(rownames(HW_1), dim(HW_1)[1] * 0.6)
valid.index <- setdiff(rownames(HW_1), train.index)
train.data <- HW_1[train.index, ]
valid.data <- HW_1[valid.index, ]


#2. KNN Analysis:
housing.df <- read.csv("/Users/karanbhosale/Documents/Machine learning/HW1.csv")
## create training and validation data sets
train.index <- sample(rownames(housing.df), 0.6*dim(housing.df)[1])  
valid.index <- setdiff(rownames(housing.df), train.index)  
train.df <- housing.df[train.index, ]
valid.df <- housing.df[valid.index, ]

# Normalize Data 
train.norm.df <- train.df
valid.norm.df <- valid.df
norm.values <- preProcess(housing.df[, 2:6], method = c("center", "scale"))
train.norm.df[, 2:6] <- predict(norm.values, train.df[, 2:6])
valid.norm.df[, 2:6] <- predict(norm.values, valid.df[, 2:6])


# initialize a data frame with two columns: k, and accuracy
accuracy.df <- data.frame(k = seq(1, 30, 1), accuracy = rep(0, 30))

# compute  for different k on validation data
for (i in 1:30) {
  knn.pred <- knn(train.norm.df[, 2:6], valid.norm.df[, 2:6], 
                  cl = train.norm.df[, 1], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, as.factor(valid.norm.df[, 1]))$overall[1] 
}

max(accuracy.df[,2])

# say we find the optimal k is 23, then we use k=23 for the final classifier

knn.pred.final<-knn(train.norm.df[, 2:6], valid.norm.df[, 2:6], 
                    cl = train.norm.df[, 1], k = 23)

# the final classification valid.df$Knn_pred<-knn.pred.final
actual_values1 <- factor(valid.norm.df[, 1])

predictions1 <- factor(knn.pred.final, levels = levels(actual_values1)) # Ensure predictions are factors with correct levels

# Create confusion matrix
cm1 <- confusionMatrix(predictions1, actual_values1)
cm1

#3.Bayes  analysis

HW_1 <- read.csv("/Users/karanbhosale/Documents/Machine learning/HW1.csv")



HW_1$VALUE<-as.factor(HW_1$VALUE)
HW_1$YEAR<-as.factor(HW_1$YEAR)
HW_1$AREA<-as.factor(HW_1$AREA)
HW_1$FLOORS<-as.factor(HW_1$FLOORS)
HW_1$ROOMS<-as.factor(HW_1$ROOMS)
HW_1$REMODEL<-as.factor(HW_1$REMODEL)

train.index <- sample(rownames(HW_1), dim(HW_1)[1] * 0.6)
valid.index <- setdiff(rownames(HW_1), train.index)
train.data <- HW_1[train.index, ]
valid.data <- HW_1[valid.index, ]

copy_train<-train.index
copy_valid<-valid.index
copy_train.d<-train.data
copy_valid.d<-valid.data

selected.var <- c(1, 3, 5, 6) # select outcome and predictor variables


# run Naive Bayes
HW_1.nb <- naiveBayes(VALUE ~ ., data = copy_train.d)

## predict probabilities
pred.prob <- predict(HW_1.nb, newdata = copy_valid.d, type = "raw")
pred_valid.df<-cbind(copy_valid.d,pred.prob)

## predict class
pred.class <- predict(HW_1.nb, newdata = copy_valid.d)
pred_valid.df<-cbind(pred_valid.df,pred.class)

# confusion matrix - validation
pred.class <- predict(HW_1.nb, newdata = copy_valid.d)
confusionMatrix(pred.class, copy_valid.d$VALUE)

#4.Tree Analysis

HW_1 <- read.csv("/Users/karanbhosale/Documents/Machine learning/HW1.csv")

train_index<-sample(rownames(HW_1), dim(HW_1)[1]*0.6)
valid_index<-setdiff(rownames(HW_1),train_index)
train_data<-HW_1[train_index, ]
valid_data<-HW_1[valid_index, ]

### Train the model using the training data
mytree<- rpart(VALUE ~  YEAR + AREA + FLOORS + ROOMS + REMODEL , data = train_data, method = "class")

prp(mytree)  ## plot the tree

### Predict using the validation data
predicted_values <- predict(mytree, newdata=valid_data, type = "class")

### Confusion matrix
confusionMatrix(relevel(as.factor(predicted_values), "High"), 
                relevel(as.factor(valid_data$VALUE), "High"))


#5.Random forest

HW_1 <- read.csv("/Users/karanbhosale/Documents/Machine learning/HW1.csv")

## since we are doing a classification random forest now,
## we need to convert the outcome variable to factor (i.e., categorical) type

train_data$VALUE<-as.factor(train_data$VALUE) 
valid_data$VALUE<-as.factor(valid_data$VALUE) 

### Train the model using the training data
myforest <- randomForest(VALUE ~  FLOORS + YEAR + AREA + ROOMS + REMODEL, data = train_data) 

### Predict using the validation data
predicted_values_forest <- predict(myforest, newdata=valid_data)

### Confusion matrix
confusionMatrix(relevel(as.factor(predicted_values_forest), "High"), 
                relevel(as.factor(valid_data$VALUE), "High"))

