adult <- read.csv("~/R/4032/adult.data", header=FALSE, strip.white=TRUE)
adult.test <- read.csv("~/R/4032/adult.test", header=FALSE, strip.white=TRUE)

adult$V1 <- gsub("?",NA,adult$V1, fixed = TRUE)
adult$V2 <- gsub("?",NA,adult$V2, fixed = TRUE)
adult$V3 <- gsub("?",NA,adult$V3, fixed = TRUE)
adult$V4 <- gsub("?",NA,adult$V4, fixed = TRUE)
adult$V5 <- gsub("?",NA,adult$V5, fixed = TRUE)
adult$V6 <- gsub("?",NA,adult$V6, fixed = TRUE)
adult$V7 <- gsub("?",NA,adult$V7, fixed = TRUE)
adult$V8 <- gsub("?",NA,adult$V8, fixed = TRUE)
adult$V9 <- gsub("?",NA,adult$V9, fixed = TRUE)
adult$V10 <- gsub("?",NA,adult$V10, fixed = TRUE)
adult$V11 <- gsub("?",NA,adult$V11, fixed = TRUE)
adult$V12 <- gsub("?",NA,adult$V12, fixed = TRUE)
adult$V13 <- gsub("?",NA,adult$V13, fixed = TRUE)
adult$V14 <- gsub("?",NA,adult$V14, fixed = TRUE)

df <- na.omit(adult)

adult.test$V1 <- gsub("?",NA,adult.test$V1, fixed = TRUE)
adult.test$V2 <- gsub("?",NA,adult.test$V2, fixed = TRUE)
adult.test$V3 <- gsub("?",NA,adult.test$V3, fixed = TRUE)
adult.test$V4 <- gsub("?",NA,adult.test$V4, fixed = TRUE)
adult.test$V5 <- gsub("?",NA,adult.test$V5, fixed = TRUE)
adult.test$V6 <- gsub("?",NA,adult.test$V6, fixed = TRUE)
adult.test$V7 <- gsub("?",NA,adult.test$V7, fixed = TRUE)
adult.test$V8 <- gsub("?",NA,adult.test$V8, fixed = TRUE)
adult.test$V9 <- gsub("?",NA,adult.test$V9, fixed = TRUE)
adult.test$V10 <- gsub("?",NA,adult.test$V10, fixed = TRUE)
adult.test$V11 <- gsub("?",NA,adult.test$V11, fixed = TRUE)
adult.test$V12 <- gsub("?",NA,adult.test$V12, fixed = TRUE)
adult.test$V13 <- gsub("?",NA,adult.test$V13, fixed = TRUE)
adult.test$V14 <- gsub("?",NA,adult.test$V14, fixed = TRUE)

df.test <- na.omit(adult.test)



summary(df)
df$V14 <- as.factor(df$V14)
#Remove trailing dot from test data
df.test$V15 <- gsub("[^<>=50K]", "", df.test$V15)
df.test$V15 <- as.factor(df.test$V15)
dfc <- rbind(df, df.test)
str(dfc)


#qualification
qualification <- dfc$V4
qualification[grepl("th", qualification)] <- "HS-dropout"
dfc$qualification <- as.factor(qualification)

qualification <- df.test$V4
qualification[grepl("th", qualification)] <- "HS-dropout"
df.test$qualification <- as.factor(qualification)


#Bin Age
age <- dfc$V1
age2 <- as.numeric(age)
ageCat <- cut(age2, seq(10,90,10))
ageCat <- as.factor(ageCat)
dfc$ageCat <- ageCat

age <- df.test$V1
age2 <- as.numeric(age)
ageCat <- cut(age2, seq(10,90,10))
ageCat <- as.factor(ageCat)
df.test$ageCat <- ageCat

dfc$V5 <- NULL

#Bin Hours
hours <- dfc$V13
hoursbin <- cut(hours, c(1,39,99))
dfc$hoursbin <- hoursbin
levels(hoursbin)
hours <- df.test$V13
hoursbin <- cut(age2, c(1,39,99))
df.test$hoursbin <- hoursbin


dfc$V1 <-as.numeric(dfc$V1)
dfc$V3 <-as.numeric(dfc$V3)
dfc$V13 <-as.numeric(dfc$V13)
dfc$V12 <-as.numeric(dfc$V12)
dfc$V11 <-as.numeric(dfc$V11)
dfc$V10 <- as.factor(dfc$V10)
dfc$V14 <- as.factor(dfc$V14)
dfc$V9 <- as.factor(dfc$V9)
dfc$V8 <- as.factor(dfc$V8)
dfc$V7 <- as.factor(dfc$V7)
dfc$V6 <- as.factor(dfc$V6)
dfc$V2 <- as.factor(dfc$V2)
str(dfc)
str(df.test)
df.test$V1 <-as.numeric(df.test$V1)
df.test$V3 <-as.numeric(df.test$V3)
df.test$V13 <-as.numeric(df.test$V13)
df.test$V12 <-as.numeric(df.test$V12)
df.test$V11 <-as.numeric(df.test$V11)
df.test$V10 <- as.factor(df.test$V10)
df.test$V14 <- as.factor(df.test$V14)
df.test$V9 <- as.factor(df.test$V9)
df.test$V8 <- as.factor(df.test$V8)
df.test$V7 <- as.factor(df.test$V7)
df.test$V6 <- as.factor(df.test$V6)
df.test$V2 <- as.factor(df.test$V2)
df.test$V5 <- NULL
str(df.test)



library(ggplot2)
library(randomForest)

#Random forest
#rf.train.1[!complete.cases(rf.train.1),]

rf.label <- as.factor(dfc$V15)

rf.train.1 <- dfc[, c("V11", "V12", "V13", "V2", "V7", "qualification", "V6", "V8", "ageCat", "V10")]
set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)



Prediction <- predict(rf.1, df.test)
mean(df.test$V15==Prediction)

#Cross Validation
library(caret)
library(doSNOW)
set.seed(2348)
#cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)
ctrl.1 <- trainControl(method = "cv", number = 10)

# Set up doSNOW package for multi-core training. This is helpful as we're going
# to be training a lot of trees.
# NOTE - This works on Windows and Mac, unlike doMC
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)


# Set seed for reproducibility and train
set.seed(34324)
cv.1 <- train(x = rf.train.1, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.1)

#Shutdown cluster
stopCluster(cl)
cv.1
cv.1$finalModel
rf.prediction <- predict(cv.1, dfc[30162:45222,])
confusionMatrix(data = rf.prediction, reference = dfc[30162:45222,]$V15, mode = "everything")

#ctrl <- trainControl(method = "cv", savePred=T, classProb=T)
#cv.2 <- train(rf.label~., data=rf.train.1 , method = "svmLinear", trControl = ctrl)
library(e1071)
cv.2 <- naiveBayes(rf.label~., data=rf.train.1)
pred <- predict(cv.2, df.test)
mean(pred==df.test$V15)


rf.train.2 <- rf.train.1
rf.train.2$capgain[which(rf.train.2$V11 < 25000)] <- "less than 25k"
rf.train.2$capgain[which(rf.train.2$V11 >= 25000 & rf.train.2$V11 < 35000)] <- "25k to 35k"
rf.train.2$capgain[which(rf.train.2$V11 < 50000 & rf.train.2$V11 >= 35000)] <- "35k to 50k"
rf.train.2$capgain[which(rf.train.2$V11 >= 50000)] <- "more than 50k"
rf.train.2$capgain <- as.factor(rf.train.2$capgain)


rf.train.2$caploss[which(rf.train.2$V12 < 25000)] <- "less than 25k"
rf.train.2$caploss[which(rf.train.2$V12 >= 25000 & rf.train.2$V12 < 35000)] <- "25k to 35k"
rf.train.2$caploss[which(rf.train.2$V12 < 50000 & rf.train.2$V12 >= 35000)] <- "35k to 50k"
rf.train.2$caploss[which(rf.train.2$V12 >= 50000)] <- "more than 50k"
rf.train.2$caploss <- NULL
rf.train.2$V11 <- NULL
rf.train.2$V12 <- NULL
rf.train.2$V13 <- NULL
rf.train.2$hoursbin <- dfc$hoursbin


train_control <- trainControl(method="cv", number=10)
fit <- train(x = rf.train.2, y = rf.label, method = "nb", trControl=train_control)
fit



levels(rf.label)[1] <- "no"
levels(rf.label)[2] <- "yes"
library(mlbench)
library(FSelector)
weights<- chi.squared(V15~., dfc2)
print(weights)
subset<- cutoff.k(weights, 5)
f<- as.simple.formula(subset, "Class")
print(f)


dfc$V3 <- as.integer(dfc$V3)
dfc$weightbin <- cut(dfc$V3, 50)
weights<- chi.squared(V15~., dfc)
print(weights)
