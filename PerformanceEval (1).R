library(forecast)
library(caret)

### Prediction errors
## import data - Toyota data
## specify the file path based on where you put the data file
toyota_data<-read.csv("/Users/karanbhosale/Documents/Machine learning/ToyotaCorolla (1).csv")

## create training and validation data sets
train.index<-sample(rownames(toyota_data), dim(toyota_data)[1]*0.6)
valid.index<-setdiff(rownames(toyota_data),train.index)
train.data<-toyota_data[train.index, ]
valid.data<-toyota_data[valid.index, ]

## run a linear regression model
reg <- lm(Price~., data = train.data[,-c(1,2,8,11)])
## "score" data (that is, making prediction) using validation set
pred_v <- predict(reg, newdata = valid.data[,-c(1,2,8,11)])
## check prediction errors
accuracy(pred_v,valid.data$Price)


#### Confusion matrix
owner.df <- read.csv("/Users/karanbhosale/Documents/Machine learning/ownerExample.csv")
confusionMatrix(relevel(as.factor(ifelse(owner.df$Probability>0.5, 'owner', 'nonowner')),ref="owner"), 
                relevel(as.factor(owner.df$Class),ref="owner"))
confusionMatrix(relevel(as.factor(ifelse(owner.df$Probability>0.25, 'owner', 'nonowner')),ref="owner"), 
                relevel(as.factor(owner.df$Class),ref="owner"))
confusionMatrix(relevel(as.factor(ifelse(owner.df$Probability>0.75, 'owner', 'nonowner')),ref="owner"), 
                relevel(as.factor(owner.df$Class),ref="owner"))



#### Performance Evaluation Practice

## import data - prediction performance evaluation
## specify the file path based on where you put the data file
data1<-read.csv("/Users/karanbhosale/Documents/Machine learning/eval_prediction.csv")

## RMSE and other error metrics
accuracy(data1$predicted_price, data1$actual_price)


## import data - classification performance evaluation
## specify the file path based on where you put the data file
data2<-read.csv("/Users/karanbhosale/Documents/Machine learning/eval_classification.csv")

## confusion matrix and accuracy
confusionMatrix(relevel(as.factor(data2$predicted_class),"1"), 
                relevel(as.factor(data2$actual_class),"1"))