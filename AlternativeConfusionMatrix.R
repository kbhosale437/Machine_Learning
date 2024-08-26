# Load necessary libraries
library(class)
library(caret)

# Load the data
housing.df <- read.csv("/Users/karanbhosale/Documents/Machine learning/WestRoxburyKNN.csv")

# Split the data into training and validation sets
set.seed(123)  # Setting seed for reproducibility
train.index <- sample(rownames(housing.df), 0.6 * nrow(housing.df))  
valid.index <- setdiff(rownames(housing.df), train.index)  
train.df <- housing.df[train.index, ]
valid.df <- housing.df[valid.index, ]

# Normalize the data
norm.values <- preProcess(housing.df[, 1:3], method = c("center", "scale"))
train.norm.df <- train.df
valid.norm.df <- valid.df
train.norm.df[, 1:3] <- predict(norm.values, train.df[, 1:3])
valid.norm.df[, 1:3] <- predict(norm.values, valid.df[, 1:3])

# Run KNN with k=3
knn.pred <- knn(train.norm.df[, 1:3], valid.norm.df[, 1:3], 
                cl = train.norm.df[, 4], k = 3)

# Build and display confusion matrix with table() function
cm <- table(knn.pred, as.factor(valid.norm.df[, 4]))
print(cm)

# Calculate accuracy
accuracy <- sum(diag(cm)) / sum(cm)
print(paste("Accuracy:", round(accuracy, 4)))

# To get an accuracy table for choosing k:
accuracy.df <- data.frame(k = seq(1, 30, 1), accuracy = rep(0, 30))

for(i in 1:30) {
  knn.pred <- knn(train.norm.df[, 1:3], valid.norm.df[, 1:3], 
                  cl = train.norm.df[, 4], k = i)
  cm <- table(knn.pred, as.factor(valid.norm.df[, 4]))
  accuracy.df[i, 2] <- sum(diag(cm)) / sum(cm)
}

print(accuracy.df)
