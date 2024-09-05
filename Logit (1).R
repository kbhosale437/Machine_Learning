library(caret)
library(forecast) 


##### Toyota Practice (linear regression)

toyota_data<- read.csv("/Users/karanbhosale/Documents/Machine learning/ToyotaCorolla (1).csv")

### Partition data first
train_index<-sample(rownames(toyota_data), dim(toyota_data)[1]*0.6)
valid_index<-setdiff(rownames(toyota_data),train_index)
train_data<-toyota_data[train_index, ]
valid_data<-toyota_data[valid_index, ]

### Run linear regression model on the training data
mymodel<-lm(Price~Age_08_04+HP+KM+Weight, data=train_data)
summary(mymodel)  ## regression results

### Predict using the validation data
predicted_price<- predict(mymodel, newdata=valid_data)

### Prediction errors
accuracy(predicted_price,valid_data$Price)




##### Cereals Example

cereal_data<- read.csv("Cereals_classification.csv")

### Partition data first
train_index<-sample(rownames(cereal_data), dim(cereal_data)[1]*0.6)
valid_index<-setdiff(rownames(cereal_data),train_index)
train_data<-cereal_data[train_index, ]
valid_data<-cereal_data[valid_index, ]

### Run logit regression model on the training data
mymodel<-glm(high_rating~calories+protein+fat+fiber+sugars+vitamins, data=train_data, family='binomial')
summary(mymodel)  ## regression results

### Predict using the validation data
predicted_values<- predict(mymodel, type="response", newdata=valid_data)

### Confusion matrix
confusionMatrix(relevel(as.factor(ifelse(predicted_values>0.5,1,0)),"1"),
                relevel(as.factor(valid_data$high_rating),"1"))





####### Universal Bank Practice

bank_data<- read.csv("UniversalBank.csv")

### Partition data first
train_index<-sample(rownames(bank_data), dim(bank_data)[1]*0.6)
valid_index<-setdiff(rownames(bank_data),train_index)
train_data<-bank_data[train_index, ]
valid_data<-bank_data[valid_index, ]

### Run logit regression model on the training data
## you can try other predictor variables if you like
mymodel<-glm(Personal.Loan~CCAvg+Age+Income+Family+Experience, data=train_data, family='binomial')
summary(mymodel)  ## regression results

### Predict using the validation data
predicted_values<- predict(mymodel, type="response", newdata=valid_data)

### Confusion matrix
confusionMatrix(relevel(as.factor(ifelse(predicted_values>0.5,1,0)),"1"),
                relevel(as.factor(valid_data$Personal.Loan),"1"))