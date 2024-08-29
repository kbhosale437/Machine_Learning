library(e1071) ## for Bayes classifier
library(caret)

delays.df <- read.csv("/Users/karanbhosale/Documents/Machine learning/FlightDelays.csv")

# change variables to categorical first
delays.df$DAY_WEEK <- as.factor(delays.df$DAY_WEEK)
delays.df$Flight.Status<-as.factor(delays.df$Flight.Status)

selected.var <- c(2, 4, 8, 10, 13) # select outcome and predictor variables

# create training and validation sets
train.index <- sample(rownames(delays.df), dim(delays.df)[1]*0.6) 
valid.index<-setdiff(rownames(delays.df), train.index)
train.df <- delays.df[train.index, selected.var]
valid.df <- delays.df[valid.index, selected.var]

# run Naive Bayes
delays.nb <- naiveBayes(Flight.Status ~ ., data = train.df)

## predict probabilities
pred.prob <- predict(delays.nb, newdata = valid.df, type = "raw")
pred_valid.df<-cbind(valid.df,pred.prob)

## predict classes
pred.class <- predict(delays.nb, newdata = valid.df)
pred_valid.df<-cbind(pred_valid.df,pred.class)

# confusion matrix - validation
pred.class <- predict(delays.nb, newdata = valid.df)
confusionMatrix(pred.class, valid.df$Flight.Status)


## Practice: automobile accidents
accidents.df <- read.csv("accidents.csv")

# create a new outcome variable first
accidents.df$INJURY <- ifelse(accidents.df$MAX_SEV_IR>0, "yes", "no")

# convert predictor variables to categorical
for (i in c(1:dim(accidents.df)[2])){
  accidents.df[,i] <- as.factor(accidents.df[,i])
}

# create training and validation sets
train.index <- sample(rownames(accidents.df), dim(accidents.df)[1]*0.6)  
valid.index <- setdiff(rownames(accidents.df), train.index)  
train.df <- accidents.df[train.index, ]
valid.df <- accidents.df[valid.index, ]

vars <- c("INJURY", "HOUR_I_R",	"ALIGN_I"	,"WRK_ZONE",	"WKDY_I_R",
          "INT_HWY",	"LGTCON_I_R",	"PROFIL_I_R",	"SPD_LIM",	"SUR_COND",
          "TRAF_CON_R",	"TRAF_WAY",	"WEATHER_R")

nb <- naiveBayes(INJURY ~ ., data = train.df[, vars])

# confusion matrix - validation
confusionMatrix(predict(nb, valid.df[, vars]),valid.df$INJURY)

