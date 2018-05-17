library(ggplot2)
library(randomForest)
library(mlbench)
library(FSelector)
library(caret)
library(doSNOW)
library(e1071)

str(df.test)
str(dfc)

weights<- chi.squared(V15~., dfc)
weights
subset<- cutoff.k(weights, 10)
f<- as.simple.formula(subset, "Class")
print(f)


# dfc$totalcap[which( (dfc$V11 - dfc$V12) < 0)] <- "negative"
# dfc$totalcap[which( (dfc$V11 - dfc$V12) >= 0 &  (dfc$V11 - dfc$V12) <= 10000)] <- "0 to 10k"
# dfc$totalcap[which( (dfc$V11 - dfc$V12) > 10000 &  (dfc$V11 - dfc$V12) <= 20000 )] <- "10k to 20k"
# dfc$totalcap[which( (dfc$V11 - dfc$V12) > 20000 &  (dfc$V11 - dfc$V12) <= 30000 )] <- "20k to 30k"
# dfc$totalcap[which( (dfc$V11 - dfc$V12) > 30000 &  (dfc$V11 - dfc$V12) <= 40000 )] <- "30k to 40k"
# dfc$totalcap[which( (dfc$V11 - dfc$V12) > 40000 &  (dfc$V11 - dfc$V12) < 50000 )] <- "40k to 50k"
# dfc$totalcap[which( (dfc$V11 - dfc$V12) >= 50000 )] <- "more than 50k"
# dfc$totalcap <- as.factor(dfc$totalcap)
# str(dfc$totalcap)
# levels(dfc$totalcap)
# dfc[is.na(dfc$totalcap),]

dfc$totalcap <- ""
dfc$totalcap[which( (dfc$V11 - dfc$V12) < 0)] <- "negative"
dfc$totalcap[which( (dfc$V11 - dfc$V12) == 0) ] <- "0"
dfc$totalcap[which( (dfc$V11 - dfc$V12) > 0 &  (dfc$V11 - dfc$V12) <= 25000 )] <- "0 to 25k"
dfc$totalcap[which( (dfc$V11 - dfc$V12) > 25000 &  (dfc$V11 - dfc$V12) <= 50000 )] <- "25k to 50k"
dfc$totalcap[which( (dfc$V11 - dfc$V12) >= 50000 )] <- "more than 50k"
dfc$totalcap <- as.factor(dfc$totalcap)
str(dfc$totalcap)
levels(dfc$totalcap)
dfc[is.na(dfc$totalcap),]

dfc$capgains <- as.factor(dfc$V11)


classes <- dfc$V15
nb.train.1 <- dfc[, c("V8", "V6", "totalcap", "qualification", "V7")]
set.seed(34324)
train_control <- trainControl(method="cv", number=10)
nb.model.1 <- train(x = nb.train.1, y = classes, method = "nb", trControl=train_control)

nb.model.prediction <- predict(nb.model.1, dfc[30162:45222,])
mean(nb.model.prediction == dfc[30162:45222,]$V15)
confusionMatrix(data = nb.model.prediction, reference = dfc[30162:45222,]$V15, mode = "prec_recall")




classes<-ifelse(classes=="<=50K","no","yes")
classes <- as.factor(classes)



library(class)
#SVM
nb.train.1 <- dfc[, c("V8", "V6", "V11", "qualification", "V7")]
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

nb.train.1$V11 <- normalize(nb.train.1$V11)


set.seed(34324)
train_control <- trainControl(method="cv", number=10)
train_control <- trainControl(method="none")
nb.model.1 <- train(classes~., data=nb.train.1, method = "svmRadial", trControl = trainControl(), tuneGrid = NULL, tuneLength = 3)

str(dfc)


data(iris)
str(iris)
table(iris$Species)
gp <- runif(nrow(iris))
iris2 <- iris[order(gp),]
iris2
str(iris2)
summary(iris2[,1:4])
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
normalize(c(1,2,3,4,5))
iris_n <- as.data.frame(lapply(iris2[,c(1,2,3,4)], normalize))
str(iris_n)
iris_train <- iris_n[1:129,]
iris_test <- iris_n[130:150,]
iris_train_target <- iris2$Species[1:129]
iris_test_target <- iris2$Species[130:150]

train_control <- trainControl(method="none")
knn.model.1 <- train(x = iris_train, y = iris_train_target, method = "svmRadial", trControl = trainControl(), tuneGrid = NULL, tuneLength = 3)
knn.model.1
