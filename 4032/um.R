library(e1071)
plot(iris)

s <- sample(150, 100)
col = c("Petal.Length", "Petal.Width", "Species")
iris_train <- iris[s, col]
iris_test <- iris[-s, col]

svmfit <- svm(Species ~., data=iris_train, kernel="linear", cost=100, scale= FALSE)
svmfit
plot(svmfit, iris_train[,col])

tuned <- tune(svm, Species~., data=iris_train, kernel="linear", ranges=list(cost=c(0.001, 0.001, 0.1, 1, 10, 100)))
summary(tuned)

p <- predict(svmfit, iris_test[,col], type="class")
plot(p)
table(p, iris_test[,3])
mean(p==iris_test[,3])

library(ggplot2)
library(caret)
ctrl <- trainControl(method = "cv", savePred=T, classProb=T)
mod <- train(Species~., data=iris, method = "svmLinear", trControl = ctrl)
head(mod$pred)
mod


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

require(class)
m1 <- knn.cv(train=iris_train, cl=iris_train_target, k=13)
m1
table(iris_test_target, m1)
mean(iris_test_target == m1)
attributes(.Last.value)

country.freq = table(dfc$V14)
barplot(country.freq) 


ggplot(dfc, aes(country.freq)) + geom_line() 

frequency(dfc$V14)

qplot(as.factor(dfc$V14), geom="histogram", stat="count")

library(ggplot2)
library(plyr)

country <- dfc$V14
country <- arrange(count(dfc, "V14"), desc(freq))
country$V14 <- factor(country$V14, levels = country$V14[order(country$freq)])


ggplot(country, aes(x=V14, y= freq))+ geom_bar(stat = "identity") + coord_flip() + theme_bw() + xlab("country") + ylab("freq") + ggtitle("Countries Frequency")

ggplot(country, aes(x=V14, y= freq))+ geom_bar(stat = "identity") + coord_flip() + theme_bw() + xlab("country") + ylab("freq") + ggtitle("Countries Frequency")


ggplot(country, aes(y=V14, x= freq, group=1))+ geom_line()

#ggplot(dfc, aes(x=V6 fill=V8)) + geom_bar()
ggplot(dfc, aes(x = V6, fill = V8)) +geom_bar()
ggplot(dfc, aes(x = V6, fill = V15)) +geom_bar()

ggplot(dfc, aes(x = V7, fill = V15)) +geom_bar() + coord_flip()  + xlab("Occupation") + labs(fill = "Class")
ggplot(dfc, aes(x = V2, fill = V15)) +geom_bar() + coord_flip()  + xlab("Work class") + labs(fill = "Class")
ggplot(dfc, aes(x = qualification, fill = V15)) +geom_bar() + coord_flip()  + xlab("Qualification") + labs(fill = "Class")
ggplot(dfc, aes(x = hoursbin, fill = V15)) +geom_bar() + coord_flip()  + xlab("Hours") + labs(fill = "Class")


ggplot(dfc, aes(x = hoursbin, fill = hoursbin)) +geom_bar() + coord_flip()  + xlab("Hours") + labs(fill = "Class") + facet_grid(. ~ V15)

ggplot(dfc, aes(V2, fill = V2)) +geom_bar() + coord_flip()  + xlab("Occupation") + facet_grid(. ~ V15)

ggplot(dfc, aes(x = V8, fill = V15)) +geom_bar() + coord_flip() + ylab("Occupation")

ggplot(dfc, aes(x = V7, fill=factor(V7))) +geom_bar() + coord_flip() + facet_wrap(~ V15)
