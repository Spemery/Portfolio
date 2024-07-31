rm(list=ls())
library(FNN)
library(tidyverse)


churndata <- read.csv("churndata.csv")
head(churndata)
str(churndata)

###Prepare the data: Clean up, check the data types
churndata$churn <- factor(churndata$churn)
churndata$vmail <- factor(churndata$vmail)
churndata$area <- factor(churndata$area)

sum(is.na(churndata))
churndata <- na.omit(churndata)


## Scale the numerical predictors
churndata[c(4:17)] <- scale(churndata[c(4:17)])

###Create training and test sets
set.seed(467) 
churndata <- mutate(churndata, id=1:nrow(churndata)) 
nTrain = trunc(0.7*nrow(churndata))
train <- slice_sample(churndata, n = nTrain)
test <- anti_join(churndata, train, by = 'id') 
train <- select(train, -id)
test <- select(test, -id)
set.seed(seed=NULL) 

###########################
### Logistic Regression ###
###########################
### Build a logreg model with the train set

glm.model1 <- glm(churn ~ ., data = train, family = binomial)
summary(glm.model1)

glm.final <- step(glm.model1,direction="backward")
summary(glm.final)

### Make a prediction for the test set using the best-fitting model
# type="response" outputs probabilities P(Y = 1|X) instead of the log-odds (logit).
glm.probs <- predict(glm.final, newdata = test, type = "response")
# Convert the predicted probabilities into class labels No or Yes
glm.pred <- rep("No", nrow(test))
glm.pred[glm.probs > .5] <- "Yes"


glm.table <- table(test$churn, glm.pred, dnn = c("Actual", "Predicted"))
glm.table

### Overall error rate
(glm.table[1, 2]+glm.table[2, 1])/sum(glm.table)
###Type I error
glm.table[1, 2]/sum(glm.table[1, ])
###Type II error
glm.table[2, 1]/sum(glm.table[2, ])
###Power
glm.table[2, 2]/sum(glm.table[2,])


###########
### KNN ###
###########


train.x <- train[4:17]
test.x <- test[4:17]

train.y <- train$churn
test.y <- test$churn

# Make a prediction for the observations in the test set, based on the train set

kset <- seq(1, 19, by = 2)
error.rate <- rep(0, length(kset))      
  
for(i in kset) {
  knn.pred <- knn(train.x, test.x, train.y,k = i)
  error.rate[i%/%2 + 1] <- mean(test.y != knn.pred)
}

bestk <- kset[which.min(error.rate)]
bestk
minerror <- min(error.rate)
minerror
print(paste("Best test k is", bestk, "with a test error rate of", minerror))

### Predict the response variable using the k value that results in the lowest error
knn.pred <- knn(train.x, test.x, train.y, k = bestk)

knn.table <- table(test.y, knn.pred , dnn=c("Actual", "Predicted"))
knn.table

### Overall error rate
(knn.table[1, 2]+knn.table[2, 1])/sum(knn.table)
### Type I error
knn.table[1, 2]/sum(knn.table[1, ])
### Type II error
knn.table[2, 1]/sum(knn.table[2, ])
### Power
knn.table[2, 2]/sum(knn.table[2,])

