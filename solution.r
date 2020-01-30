library(tidyverse)
library(carets)
train_data <- read.csv("train.csv", stringsAsFactors = F)
#View(head(train_data))
#summary(train_data)

#create a title column, for gender plus age
train_data$Title <- str_replace(train_data$Name, "^(.*),", "") 
train_data$Title <- str_replace(train_data$Title, "\\.(.*)", "") 
train_data$Title <- str_replace(train_data$Title, " ", "") 

train_data$Title <- as.factor(train_data$Title)
#reconcile to Mr, Mrs, Master, Miss
train_data$Title <- str_replace_all(train_data$Title, c("Capt"= "Mr", "Col"= "Mr", "Don"= "Mr", "Jonkheer" = "Mr", "Major" = "Mr", "Rev"= "Mr", "Sir"= "Mr"))
train_data$Title <- str_replace_all(train_data$Title, c("Lady"= "Mrs", "Mlle"= "Miss", "Mme"= "Mrs", "Ms" = "Miss", "the Countess" = "Mrs"))
train_data$Title[which(train_data$Title == "Dr" & train_data$Sex == "female")]="Mrs"
train_data$Title[which(train_data$Title == "Dr" & train_data$Sex == "male")]="Mr"
train_data$Title <- as.factor(train_data$Title)

table(train_data$Title, train_data$Sex)
#get family size
train_data$Fsize <- train_data$Parch + train_data$SibSp + 1
train_data$Pclass <- as.factor(train_data$Pclass)
train_data$Sex <- as.factor(train_data$Sex)
train_data$Embarked <- as.factor(train_data$Embarked)

train_data$Survived <- ifelse(train_data$Survived==1, "Y", "N")
train_data$Survived <- as.factor(train_data$Survived)

summary(train_data)
#exploratory analysis on data
#ggplot(train_data)+geom_boxplot(aes(x=Embarked, y=Survived, color=Sex))
ggplot(train_data)+geom_bar(aes(x=Embarked))
#by popular demand, replace the 2 missing values with s
train_data$Embarked[which(train_data$Embarked=="")]="S" 

#how does sex relate with survival
ggplot(train_data)+geom_bar(aes(x=Sex, fill=Survived))

#how does age relate with survival...average age of 28 for both male and female survived.
ggplot(train_data)+geom_boxplot(aes(x=Sex, y=Age, fill=Survived))

#how does family size relate with survival..
ggplot(train_data)+geom_bar(aes(x=Fsize,fill=Survived))


#how does title relate with survival...average age of 28 for both male and female survived.
ggplot(train_data)+geom_bar(aes(x=Title, fill=Survived))


#create a new variable that shows if its single
#train_data$Single <- ifelse(train_data$Fsize==1, "Y", "N")
#train_data$Single <- as.factor(train_data$Single)
ggplot(train_data)+geom_bar(aes(x=Single, fill=Survived))

#class(train_data$Cabin)
#replace cabin with deck no
#train_data$Cabin <- str_replace_all(train_data$Cabin, "\\d", "")

#create fare per passenger
train_data$f_per <- train_data$Fare / train_data$Fsize 

summary(train_data)
train_data <- droplevels(train_data)

pre_proc_mod <- preProcess(train_data, method = c("medianImpute", "center", "scale"))
pre_proc_data <- predict(pre_proc_mod, train_data)
sum(is.na(pre_proc_data))

pre_proc_data <- droplevels(pre_proc_data)
summary(pre_proc_data)


#partition data
partition <- createDataPartition(pre_proc_data$Survived, p=1, list = FALSE)
trainSet <- pre_proc_data[partition,]
testSet <- pre_proc_data1



str(pre_proc_data)
#predictor variable
predictor <- c("Pclass","Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "Fsize", "Title", "f_per")
#outcome variable
outcome <- "Survived"

#set training controls parameters
trControl <- trainControl(method = "cv", number = 10, savePredictions = "final", classProbs = T)

#train using glm model
model_glm <-   train(trainSet[,predictor], trainSet[,outcome], method = "glm", trControl = trControl, tuneLength = 3)
testSet$pred_glm <- predict(object = model_glm, testSet[,predictor])
confusionMatrix(testSet[,outcome], testSet$pred_glm)
#81.53%

#train using rf model
model_rf <-   train(trainSet[,predictor], trainSet[,outcome], method = "rf", trControl = trControl, tuneLength = 3)
testSet$pred_rf <- predict(object = model_rf, testSet[,predictor])
confusionMatrix(testSet[,outcome], testSet$pred_rf)
#82.43%



#train using gbm model
model_gbm <-   train(trainSet[,predictor], trainSet[,outcome], method = "gbm", trControl = trControl, tuneLength = 3)
testSet$pred_gbm <- predict(object = model_gbm, testSet[,predictor])
confusionMatrix(testSet[,outcome], testSet$pred_gbm)
#85.59%


#averaging both rf and glm
testSet$pred_rf_prob <- predict(model_rf, testSet[,predictor], type = "prob")
testSet$pred_glm_prob <- predict(model_glm, testSet[,predictor], type = "prob")
testSet$pred_gbm_prob <- predict(model_gbm, testSet[,predictor], type = "prob")

testSet$pred_avg <- (testSet$pred_rf_prob$Y + testSet$pred_glm_prob$Y + testSet$pred_gbm_prob$Y)/2

testSet$pred_avg <- ifelse(testSet$pred_avg > 0.5 , "Y", "N")
testSet$pred_avg <- as.factor(testSet$pred_avg)
confusionMatrix(testSet[,outcome], testSet$pred_avg)
#81.08%



#weighted avg
testSet$pred_w_avg <- 0.22*(testSet$pred_rf_prob$Y) + 0.18*testSet$pred_glm_prob$Y + 0.60*testSet$pred_gbm_prob$Y

testSet$pred_w_avg <- ifelse(testSet$pred_w_avg > 0.5 , "Y", "N")
testSet$pred_w_avg <- as.factor(testSet$pred_w_avg)
confusionMatrix(testSet[,outcome], testSet$pred_w_avg)
#84.68%

#using highest votes
testSet$pred_votes <-  ifelse(testSet$pred_rf == "Y" & testSet$pred_gbm == "Y", "Y", ifelse(testSet$pred_glm=="Y" & testSet$pred_rf == "Y", "Y", ifelse(testSet$pred_glm=="Y" & testSet$pred_gbm == "Y", "Y", "N")))
testSet$pred_votes <- as.factor(testSet$pred_votes)
confusionMatrix(testSet[,outcome], testSet$pred_votes)
#83.78%


#stacking
#use the predictions from rf and glm model

trainSet$gbm_oob <- model_gbm$pred$Y[order(model_gbm$pred$rowIndex)]
testSet$gbm_oob <- testSet$pred_gbm_prob$Y

trainSet$glm_oob <- model_glm$pred$Y[order(model_glm$pred$rowIndex)]
testSet$glm_oob <- testSet$pred_glm_prob$Y

trainSet$rf_oob <- model_rf$pred$Y[order(model_rf$pred$rowIndex)]
testSet$rf_oob <- testSet$pred_rf_prob$Y

stack_predicator <- c("glm_oob", "gbm_oob", "rf_oob")
model_stack_gb <- train(trainSet[,stack_predicator], trainSet[, outcome], method = "gbm", trControl = trControl, tuneLength = 3  )
testSet$pred_stack_gbm <- predict(model_stack_gb, testSet[, stack_predicator])
confusionMatrix(testSet$pred_stack_gbm, testSet[, outcome])
#84.23%

model_stack_glm <- train(trainSet[,stack_predicator], trainSet[, outcome], method = "glm", trControl = trControl, tuneLength = 3  )
testSet$pred_stack_glm <- predict(model_stack_glm, testSet[, stack_predicator])
confusionMatrix(testSet$pred_stack_glm, testSet[, outcome])
#82.88%





#using stacked glm gave the highest accuracy
#to predict
#take in data, format, preprocess, train


d_test <- read.csv("test.csv", stringsAsFactors = F)
d_test$Pclass <- as.factor(d_test$Pclass)
d_test$Sex <- as.factor(d_test$Sex)
d_test$Age <- as.numeric(d_test$Age)

d_test$Title <- str_replace(d_test$Name, "^(.*),", "") 
d_test$Title <- str_replace(d_test$Title, "\\.(.*)", "") 
d_test$Title <- str_replace(d_test$Title, " ", "") 

d_test$Title <- as.factor(d_test$Title)
#reconcile to Mr, Mrs, Master, Miss
d_test$Title <- str_replace_all(d_test$Title, c("Capt"= "Mr", "Col"= "Mr", "Don"= "Mr", "Jonkheer" = "Mr", "Major" = "Mr", "Rev"= "Mr", "Sir"= "Mr"))
d_test$Title <- str_replace_all(d_test$Title, c("Lady"= "Mrs", "Mlle"= "Miss", "Mme"= "Mrs", "Ms" = "Miss", "the Countess" = "Mrs", "Mra"="Mrs"))
d_test$Title[which(d_test$Title == "Dr" & d_test$Sex == "female")]="Mrs"
d_test$Title[which(d_test$Title == "Dr" & d_test$Sex == "male")]="Mr"
d_test$Title <- as.factor(d_test$Title)

table(d_test$Title, d_test$Sex)
#get family size
d_test$Fsize <- d_test$Parch + d_test$SibSp + 1
d_test$f_per <- d_test$Fare / d_test$Fsize
d_test$Pclass <- as.factor(d_test$Pclass)
d_test$Sex <- as.factor(d_test$Sex)
d_test$Embarked <- as.factor(d_test$Embarked)

d_test$Embarked[which(d_test$Embarked=="")]="S" 


#create a new variable that shows if its single
#d_test$Single <- ifelse(d_test$Fsize==1, "Y", "N")
#d_testSingle <- as.factor(d_test$Single)

d_test <- droplevels(d_test)



table(d_test$Title, d_test$Sex)
d_test$Title <- as.factor(d_test$Title)

d_test$Embarked <- as.factor(d_test$Embarked)

summary(d_test)
#pre-process data
pre_proc_mod1 <- preProcess(d_test, method = c("medianImpute", "center", "scale"))
pre_proc_data1 <- predict(pre_proc_mod1, d_test)
sum(is.na(pre_proc_data1))
summary(pre_proc_data1)

sub <- read.csv("gender_submission.csv")
ans1 <- cbind(PassengerId = d_test$PassengerId, Survived = testSet$pred_glm)

ans1 <- as.data.frame(ans1)
ans1$Survived <- ifelse(ans1$Survived==2, 1, 0)

write_csv(ans1, "result.csv")
