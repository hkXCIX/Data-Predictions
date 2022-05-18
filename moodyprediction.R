getwd()

library(rpart)
library(rpart.plot)

moody <- read.csv("M2022train.csv")

tree <- rpart(Grade ~ ., data = moody,method = "class",control = rpart.control(minsplit = 100))
rpart.plot(tree)

pred <- predict(tree, moody, type="class")
head(pred)
mean(moody$Grade==pred)
CrossValidation::cross_validate(moody,tree,5,0.9)


myprediction$Grade <-decision
error <- mean(moody$Grade!= predict$Grade)
error

