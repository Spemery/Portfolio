library(readxl)
mydata <- read_excel("~/Desktop/ACAC Data/referRdata.xlsx", 
                     col_types = c("text", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "text", "text", "numeric", "numeric", 
                                   "numeric", "text"))
View(mydata)

head(mydata)
mydata$Gender<-as.factor(mydata$Gender)
mydata$Referred<-as.factor(mydata$Referred)
mydata[2:6]<-(scale(mydata[2:6]))
mydata[9:11]<-(scale(mydata[9:11]))
head(mydata)
#mydata<-select(mydata, -Names)
mydata<-na.omit(mydata)
head(mydata)

set.seed(467)
nTrain1 = trunc(0.7*nrow(mydata))
train1 <- slice_sample(mydata, n = nTrain1)
test1 <- anti_join(mydata, train1, by = 'Name') 
train1 <- select(train1, -Name)
test1 <- select(test1, -Name)
set.seed(seed=NULL)
str(train1)
view(test1)

#train.x<-select(train1, -Referred, -Club, -Gender)
#test.x<-select(test1, -Referred, -Club, -Gender)
#train.y<-train1$Referred
#test.y<-test1$Referred
contrasts(mydata$Referred)

mod1 <- glm(Referred ~ . -Club -DaysSince -PT -SPA -PL -GuestVisits, data = train1, family = binomial)
summary(mod1)

mod1 <- glm(Referred ~ . -Club -Gender -Checkins -Tenure -DaysSince -PIL -SPA, data = train1, family = binomial)
summary(mod1)


### Make a prediction for the test set using the best-fitting model
# type="response" outputs probabilities P(Y = 1|X) instead of the log-odds (logit).
glm.probs1 <- predict(mod1, newdata = test1, type = "response")
# Convert the predicted probabilities into class labels No or Yes
glm.pred1 <- rep("No", nrow(test1))
glm.pred1[glm.probs1 > .01] <- "Yes"
#print(glm.probs1)

### Create the confusion matrix and calculate error rates
# You can rename the rows and columns (do so only if it is asked in the question)
glm.table1 <- table(test1$Referred, glm.pred1, dnn = c("Actual", "Predicted"))
glm.table1

### Overall error rate
(glm.table1[1, 2]+glm.table1[2, 1])/sum(glm.table1)
###Type I error (False Positive)
glm.table1[1, 2]/sum(glm.table1[1, ])
###Type II error (False Negative)
glm.table1[2, 1]/sum(glm.table1[2, ])
###Power
glm.table1[2, 2]/sum(glm.table1[2,])

library(readxl)
nd <- read_excel("~/Desktop/ACAC Data/referRdata.xlsx", 
                 sheet = "Sheet2", col_types = c("text", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "text", "text", 
                                                 "numeric", "numeric", "numeric", 
                                                 "text"))
View(nd)

nd$Gender<-as.factor(nd$Gender)
nd$Referred<-as.factor(nd$Referred)
nd[2:6]<-(scale(nd[2:6]))
nd[9:11]<-(scale(nd[9:11]))
head(nd)
#mydata<-select(mydata, -Names)
mydata<-na.omit(mydata)

glm.probs <- predict(mod1, newdata = nd, type = "response")
glm.pred <- rep("No", nrow(nd))
glm.pred[glm.probs > .015] <- "Yes"
nd$preds<-glm.pred
nd$probs<-glm.probs
glm.pred
head(nd, 10)
view(nd)
library(openxlsx)
write.xlsx(nd, "referMod1.xlsx")
