getwd()

library(rpart)
library(rpart.plot)

moody <- read.csv("M2022train.csv")
summary(moody$Grade)

table(moody$Grade)

myprediction <- moody
decision <- rep('F', nrow(myprediction))

decision[myprediction$Score>30 & myprediction$Major=='Psychology'] <- 'D'
decision[myprediction$Score>40 & myprediction$Major=='Economics'] <- 'D'
decision[myprediction$Score>54 & myprediction$Major=='CS'] <- 'D'
decision[myprediction$Score>43 & myprediction$Major=='Statistics'] <- 'D'

decision[myprediction$Score>50 & myprediction$Major=='Psychology'] <- 'C'
decision[myprediction$Score>55 & myprediction$Major=='Economics'] <- 'C'
decision[myprediction$Score>64 & myprediction$Major=='CS'] <- 'C'
decision[myprediction$Score>62 & myprediction$Major=='Statistics'] <- 'C'

decision[myprediction$Score>57 & myprediction$Major=='Psychology'] <- 'B'
decision[myprediction$Score>65 & myprediction$Major=='Economics'] <- 'B'
decision[myprediction$Score>75 & myprediction$Major=='CS'] <- 'B'
decision[myprediction$Score>72 & myprediction$Major=='Statistics'] <- 'B'

decision[myprediction$Score>70 & myprediction$Major=='Psychology'] <- 'A'
decision[myprediction$Score>75 & myprediction$Major=='Economics'] <- 'A'
decision[myprediction$Score>90 & myprediction$Major=='CS'] <- 'A'
decision[myprediction$Score>83 & myprediction$Major=='Statistics'] <- 'A'

rpartModel<-rpart(Grade~., data=moody[moody$Score<=50,],control = rpart.control(minsplit = 10))
nrow(moody[moody$Score<=50,])
rpart.plot(rpartModel)
pred_rpartModel <- predict(rpartModel, newdata=moody[moody$Score<=50,], type="class")
pred_yourModel <- decision[moody$Score>50]

CrossValidation::cross_validate(moody, rpartModel, 5, 305/700)

decision[myprediction$Score>50] <- pred_yourModel
decision[myprediction$Score<=50] <-as.character(pred_rpartModel )

myprediction$Grade <-decision
error <- mean(moody$Grade!= myprediction$Grade)
error


# repeating code for files to submit
nograde <- read.csv("M2022testSNoGrade.csv")
submit <- read.csv("M2022submissionS.csv")

myprediction <- nograde
decision <- rep('F', nrow(myprediction))

decision[myprediction$Score>30 & myprediction$Major=='Psychology'] <- 'D'
decision[myprediction$Score>40 & myprediction$Major=='Economics'] <- 'D'
decision[myprediction$Score>54 & myprediction$Major=='CS'] <- 'D'
decision[myprediction$Score>43 & myprediction$Major=='Statistics'] <- 'D'

decision[myprediction$Score>50 & myprediction$Major=='Psychology'] <- 'C'
decision[myprediction$Score>55 & myprediction$Major=='Economics'] <- 'C'
decision[myprediction$Score>64 & myprediction$Major=='CS'] <- 'C'
decision[myprediction$Score>62 & myprediction$Major=='Statistics'] <- 'C'

decision[myprediction$Score>57 & myprediction$Major=='Psychology'] <- 'B'
decision[myprediction$Score>65 & myprediction$Major=='Economics'] <- 'B'
decision[myprediction$Score>75 & myprediction$Major=='CS'] <- 'B'
decision[myprediction$Score>72 & myprediction$Major=='Statistics'] <- 'B'

decision[myprediction$Score>70 & myprediction$Major=='Psychology'] <- 'A'
decision[myprediction$Score>75 & myprediction$Major=='Economics'] <- 'A'
decision[myprediction$Score>90 & myprediction$Major=='CS'] <- 'A'
decision[myprediction$Score>83 & myprediction$Major=='Statistics'] <- 'A'

rpartModelSubmit<-rpart(Grade~., data=moody[moody$Score<=50,],control = rpart.control(minsplit = 10))
pred_rpartModelSubmit <- predict(rpartModelSubmit, newdata=moody[moody$Score<=50,], type="class")
pred_yourModelSubmit <- decision[moody$Score>50]

decision[myprediction$Score>50] <- pred_yourModelSubmit
decision[myprediction$Score<=50] <-as.character(pred_rpartModelSubmit )

myprediction$Grade <-decision

submit$Grade <- decision
submit

write.csv(submit, 'M2022submissionS.csv', row.names=FALSE)
