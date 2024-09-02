library(e1071)
library(caret)
library(forecast)
library(neuralnet)


HW_2<- read.csv("/Users/karanbhosale/Documents/Machine learning/HW2.csv")

HW_2$Flight.Status<-as.factor(HW_2$Flight.Status)
HW_2$Delay <- ifelse(HW_2$Flight.Status == "delayed",1,0)

### Partition data first
train_index<-sample(rownames(HW_2), dim(HW_2)[1]*0.6)
valid_index<-setdiff(rownames(HW_2),train_index)
train_data<-HW_2[train_index, ]
valid_data<-HW_2[valid_index, ]

### Run logit regression model on the training data
mymodel<-glm(Delay ~ CRS_DEP_TIME + DISTANCE + Weather + DAY_WEEK , data=train_data, family='binomial')
summary(mymodel)  

## regression results

### Predict using the validation data
predicted_values<- predict(mymodel, type="response", newdata=valid_data)

### Confusion matrix
confusionMatrix(relevel(as.factor(ifelse(predicted_values > 0.5, 1, 0)),"1"),
                relevel(as.factor(valid_data$Delay),"1"))

#Q2

HW_2 <- read.csv("/Users/karanbhosale/Documents/Machine learning/HW2.csv")

varlist<-c(1,2,4)

# partition
train.index <- sample(rownames(HW_2), 0.6*dim(HW_2)[1])  
valid.index <- setdiff(rownames(HW_2), train.index)  
train.df <- HW_2[train.index, varlist]
valid.df <- HW_2[valid.index, varlist]


# normalize
norm.values <- preProcess(HW_2[,varlist], method="range")
train.norm.df <- predict(norm.values, train.df)
valid.norm.df <- predict(norm.values, valid.df)

HW_2$ontime<- ifelse(HW_2$Flight.Status=="ontime",1,0)

set.seed(1)
nn <- neuralnet(ontime ~ DAY_WEEK  + DISTANCE + CRS_DEP_TIME, data = HW_2, linear.output=FALSE, hidden = 3)

# display weights
nn$weights

# plot network
plot(nn)

## use compute() function to predict for neural nets
predict <- compute(nn, HW_2[,1:4])
confusionMatrix(relevel(as.factor(ifelse(predict$net.result > 0.5,"1", "0")),ref="1"),
                relevel(as.factor(HW_2$ontime),ref="1"))    


#2
set.seed(1)
nn1 <- neuralnet(ontime ~  DISTANCE + DAY_WEEK + CRS_DEP_TIME , data = HW_2, linear.output=FALSE, hidden = 3)

# display weights
nn1$weights

# plot network
plot(nn1)


## use compute() function to predict for neural nets
predict <- compute(nn1, HW_2[,1:4])
confusionMatrix(relevel(as.factor(ifelse(predict$net.result > 0.5,"1", "0")),ref="1"),
                relevel(as.factor(HW_2$ontime),ref="1"))


#3
HW_2 <- read.csv("/Users/karanbhosale/Documents/Machine learning/HW2.csv")

varlist<-c(2,3,4,5)  ## feel free to include other variables

# Since we are doing classification SVM now, we first convert the outcome to factor
HW_2$Flight.Status<-as.factor(HW_2$Flight.Status)

# partition the data
train.index<-sample(rownames(HW_2), dim(HW_2)[1]*0.6)
valid.index<-setdiff(rownames(HW_2), train.index)
train.df<-HW_2[train.index,varlist]
valid.df<-HW_2[valid.index,varlist]



# support vector machine
svm2 <- svm(Flight.Status ~., data = train.df)

prediction2<-predict(svm2, valid.df)


# confusion matrix for validation data
confusionMatrix(as.factor(prediction2), as.factor(valid.df$Flight.Status))

