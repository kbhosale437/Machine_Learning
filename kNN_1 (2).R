library(caret)
library(forecast)
library(FNN)

#### Riding mower example
mower.df <- read.csv("/Users/karanbhosale/Documents/Machine learning/RidingMowers.csv")
train.index <- sample(rownames(mower.df), 0.6*dim(mower.df)[1])  
valid.index <- setdiff(rownames(mower.df), train.index)  
train.df <- mower.df[train.index, ]
valid.df <- mower.df[valid.index, ]

## suppose we have a new household to classify
new.df <- data.frame(Income = 60, Lot_Size = 20)

## here's a plot to show the data
# (not required for kNN analysis, just for illustrative purpose)
plot(Lot_Size ~ Income, data=train.df, pch=ifelse(train.df$Ownership=="Owner", 1, 3))
text(train.df$Income, train.df$Lot_Size, rownames(train.df), pos=4)
text(60, 20, "X")
legend("topright", c("owner", "non-owner", "newhousehold"), pch = c(1, 3, 4))


# initialize normalized training & validation data
train.norm.df <- train.df
valid.norm.df <- valid.df

# use preProcess() from the caret library to normalize Income and Lot_Size
norm.values <- preProcess(mower.df[, 1:2], method=c("center", "scale"))
train.norm.df[, 1:2] <- predict(norm.values, train.df[, 1:2])
valid.norm.df[, 1:2] <- predict(norm.values, valid.df[, 1:2])
new.norm.df <- predict(norm.values, new.df)

# use knn() to do kNN analysis
# knn() is available in library FNN
library(FNN)

# an example of kNN using k=3 to classify the new household
nn <- knn(train = train.norm.df[, 1:2], test = new.norm.df, 
          cl = train.norm.df[, 3], k = 3)

# show the 3 nearest neighbors in the training set
rownames(train.df)[attr(nn, "nn.index")]


# initialize a data frame with two columns: k, and accuracy
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

# run kNN for different k on validation data
for(i in 1:14) {
  knn.pred <- knn(train.norm.df[, 1:2], valid.norm.df[, 1:2], 
                  cl = train.norm.df[, 3], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, as.factor(valid.norm.df[, 3]))$overall[1]
}

# check the accuracy table, it shows which k gives the highest accuracy
accuracy.df

# say we find the optimal k is 4, then we can classify the new household using k=4

knn.pred.new <- knn(train.norm.df[, 1:2], new.norm.df, 
                    cl = train.norm.df[, 3], k = 4)
rownames(train.df)[attr(knn.pred.new, "nn.index")]

