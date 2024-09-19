library(e1071)
library(caret)
library(forecast)


### Toyota Practice (Practice 1 in lecture notes)

toyota.df <- read.csv('/Users/karanbhosale/Documents/Machine learning/ToyotaCorolla (1).csv')


varlist<-c(3,4,9,12,14,17,19,21,25,26,28,30,34,39)

# partition
train.index <- sample(rownames(toyota.df), 0.6*dim(toyota.df)[1])  
valid.index <- setdiff(rownames(toyota.df), train.index)  
train.df <- toyota.df[train.index, varlist]
valid.df <- toyota.df[valid.index, varlist]


# support vector regression
svm1 <- svm(Price ~ ., data = train.df)


prediction1<-predict(svm1, valid.df)

# RMSE and other measures
accuracy(prediction1, valid.df$Price)




### Universal Bank Practice (Practice 2 in lecture notes)

bank.df <- read.csv("/Users/karanbhosale/Documents/Machine learning/UniversalBank.csv")

varlist<-c(2,3,4,6,7,10)  ## feel free to include other variables

# Since we are doing classification SVM now, we first convert the outcome to factor
bank.df$Personal.Loan<-as.factor(bank.df$Personal.Loan)

# partition the data
train.index<-sample(rownames(bank.df), dim(bank.df)[1]*0.6)
valid.index<-setdiff(rownames(bank.df), train.index)
train.df<-bank.df[train.index,varlist]
valid.df<-bank.df[valid.index,varlist]



# support vector machine
svm2 <- svm(Personal.Loan ~., data = train.df)

prediction2<-predict(svm2, valid.df)


# confusion matrix for validation data
confusionMatrix(as.factor(prediction2), as.factor(valid.df$Personal.Loan))
