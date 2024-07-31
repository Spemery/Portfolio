library(tidyverse)
library(FNN)

library(readxl)
mydata <- read_excel("~/Desktop/ACAC Data/DataSince2020.xlsx", 
                     sheet = "All Data", col_types = c("text", 
                                                       "text", "numeric", "text", "numeric", 
                                                       "text", "text", "text", "numeric", 
                                                       "text", "text", "text", "text"))
mydata<-select(mydata, -Membership, -UserName)
head(mydata)
str(mydata)
mydata$Gender<-factor(mydata$Gender)
mydata$Status<-factor(mydata$Status)
mydata[3]<-as.numeric(scale(mydata[3]))
mydata[5]<-as.numeric(scale(mydata[5]))
mydata[8]<-as.numeric(scale(mydata[8]))
head(mydata)
mydata<-data.frame(mydata)
head(mydata)

set.seed(467) 
mydata <- mutate(mydata, id=1:nrow(mydata)) 
nTrain = trunc(0.7*nrow(mydata))
train <- slice_sample(mydata, n = nTrain)
test <- anti_join(mydata, train, by = 'id') 
train <- select(train, -First, -Last, -Reason)
test <- select(test, -First, -Last, -Reason)
set.seed(seed=NULL)
str(train)
contrasts(test$Status)
glm.model <- glm(Status ~ . -id, data = train, family = binomial)
summary(glm.model)


### Make a prediction for the test set using the best-fitting model
# type="response" outputs probabilities P(Y = 1|X) instead of the log-odds (logit).
glm.probs <- predict(glm.model, newdata = test, type = "response")
# Convert the predicted probabilities into class labels No or Yes
glm.pred <- rep("Cancel", nrow(test))
glm.pred[glm.probs < .3] <- "Active"

### Create the confusion matrix and calculate error rates
# You can rename the rows and columns (do so only if it is asked in the question)
glm.table <- table(test$Status, glm.pred, dnn = c("Actual", "Predicted"))
glm.table

### Overall error rate
(glm.table[1, 2]+glm.table[2, 1])/sum(glm.table)
###Type I error (False Positive)
glm.table[1, 2]/sum(glm.table[1, ])
###Type II error (False Negative)
glm.table[2, 1]/sum(glm.table[2, ])
###Power
glm.table[2, 2]/sum(glm.table[2,])

#####Predict

library(readxl)
nd <- read_excel("~/Desktop/ACAC Data/DataSince2020.xlsx", 
                 sheet = "report-05-31-2024", col_types = c("text", 
                                                            "text", "numeric", "text", "numeric", 
                                                            "text", "text", "numeric", "text", 
                                                            "text", "text"))
view(nd)
nd<-select(nd, -Membership1, -UserName1)
nd<-rename(nd, UGName = Column1)
nd<-rename(nd, Corporate = Column2)
view(nd)
nd$Gender<-factor(nd$Gender)
nd[3]<-as.numeric(scale(nd[3]))
nd[5]<-as.numeric(scale(nd[5]))
nd[7]<-as.numeric(scale(nd[7]))
head(nd)
nd<-data.frame(nd)
nd <- mutate(nd, id=1:nrow(nd)) 
head(nd)
str(nd)
str(train1)
nd1 <- select(nd, -First, -Last)
glm.probs1 <- predict(glm.model1, newdata = nd1, type = "response")
glm.pred1 <- rep("Cancel", nrow(nd1))
glm.pred1[glm.probs1 < .3] <- "Active"
str(glm.pred1)

nd$preds<-glm.pred1
head(nd)
view(nd)

install.packages("openxlsx")
library(openxlsx)
write.xlsx(nd, "output.xlsx")
