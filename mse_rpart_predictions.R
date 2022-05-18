getwd()

library(rpart)
library(rpart.plot)
library(ModelMetrics)

train <- read.csv("Earnings_Train2022-1.csv")
summary(train)

# gpa alone does not seem to affect earnings, in fact means got lower with higher gpas
gpa1 <- subset(train, train$GPA > 2.210)
mean1 <- mean(gpa1$Earnings)
mean1

gpa2 <- subset(train, train$GPA > 2.5)
mean2 <- mean(gpa2$Earnings)
mean2

gpa3 <- subset(train, train$GPA > 2.8)
mean3 <- mean(gpa3$Earnings)
mean3

# does not tell much, credits should not be a significant factor
plot(train$Number_Of_Credits,train$Earnings,ylab="Earnings",xlab="Credits",main="Credits vs. Earnings",col="red")

# some factor in connections
plot(train$Number_Of_Professional_Connections,train$Earnings,ylab="Earnings",xlab="Connections",main="Connections vs. Earnings",col="red")

# not much effect
plot(train$Graduation_Year,train$Earnings,ylab="Earnings",xlab="Connections",main="Graduation Year vs. Earnings",col="red")

# not much effect
plot(train$Number_Of_Parking_Tickets,train$Earnings,ylab="Earnings",xlab="Connections",main="Number of Parking Tickets vs. Earnings",col="red")

# majors: STEM, Humanities, Professional, Vocational, Business, Other
# earning: Vocational > Professional > Humanities > Business > STEM > Other
# major plays some role
stem <- subset(train, train$Major == 'STEM')
mean_stem <- mean(stem$Earnings)
mean_stem

humanities <- subset(train, train$Major == 'Humanities')
mean_humanities <- mean(humanities$Earnings)
mean_humanities

professional <- subset(train, train$Major == 'Professional')
mean_professional <- mean(professional$Earnings)
mean_professional

vocational <- subset(train, train$Major == 'Vocational')
mean_vocational <- mean(vocational$Earnings)
mean_vocational

business <- subset(train, train$Major == 'Buisness')
business
mean_business <- mean(business$Earnings)
mean_business

other <- subset(train, train$Major == 'Other')
other
mean_other <- mean(other$Earnings)
mean_other

# stem mse
v<-sample(1:nrow(stem))
v[1:5]
trainScrambled<-stem[v, ]
n <- 100
trainSample<-trainScrambled[nrow(trainScrambled)-n:nrow(trainScrambled), ]
testSample <- trainScrambled[1:n,]
lm.tree <- lm(Earnings~GPA+Number_Of_Professional_Connections+Graduation_Year+Number_Of_Credits+Number_Of_Parking_Tickets,  data=trainSample)
pred2 <- predict(lm.tree,newdata=testSample)
mse(testSample$Earnings,pred2)

# humanities mse
v<-sample(1:nrow(humanities))
v[1:5]
trainScrambled<-humanities[v, ]
n <- 100
trainSample<-trainScrambled[nrow(trainScrambled)-n:nrow(trainScrambled), ]
testSample <- trainScrambled[1:n,]
lm.tree <- lm(Earnings~GPA+Number_Of_Professional_Connections+Graduation_Year+Number_Of_Credits+Number_Of_Parking_Tickets,  data=trainSample)
pred2 <- predict(lm.tree,newdata=testSample)
mse(testSample$Earnings,pred2)

# professional mse
v<-sample(1:nrow(professional))
v[1:5]
trainScrambled<-professional[v, ]
n <- 100
trainSample<-trainScrambled[nrow(trainScrambled)-n:nrow(trainScrambled), ]
testSample <- trainScrambled[1:n,]
lm.tree <- lm(Earnings~GPA+Number_Of_Professional_Connections+Graduation_Year+Number_Of_Credits+Number_Of_Parking_Tickets,  data=trainSample)
pred2 <- predict(lm.tree,newdata=testSample)
mse(testSample$Earnings,pred2)

# vocational mse
v<-sample(1:nrow(vocational))
v[1:5]
trainScrambled<-vocational[v, ]
n <- 100
trainSample<-trainScrambled[nrow(trainScrambled)-n:nrow(trainScrambled), ]
testSample <- trainScrambled[1:n,]
lm.tree <- lm(Earnings~GPA+Number_Of_Professional_Connections+Graduation_Year+Number_Of_Credits+Number_Of_Parking_Tickets,  data=trainSample)
pred2 <- predict(lm.tree,newdata=testSample)
mse(testSample$Earnings,pred2)

# business mse
v<-sample(1:nrow(business))
v[1:5]
trainScrambled<-business[v, ]
n <- 100
trainSample<-trainScrambled[nrow(trainScrambled)-n:nrow(trainScrambled), ]
testSample <- trainScrambled[1:n,]
lm.tree <- lm(Earnings~GPA+Number_Of_Professional_Connections+Graduation_Year+Number_Of_Credits+Number_Of_Parking_Tickets,  data=trainSample)
pred2 <- predict(lm.tree,newdata=testSample)
mse(testSample$Earnings,pred2)

# other mse
v<-sample(1:nrow(other))
v[1:5]
trainScrambled<-other[v, ]
n <- 100
trainSample<-trainScrambled[nrow(trainScrambled)-n:nrow(trainScrambled), ]
testSample <- trainScrambled[1:n,]
lm.tree <- lm(Earnings~GPA+Number_Of_Professional_Connections+Graduation_Year+Number_Of_Credits+Number_Of_Parking_Tickets,  data=trainSample)
summary(lm.tree)
pred2 <- predict(lm.tree,newdata=testSample)
mse(testSample$Earnings,pred2)


submit <- read.csv("earning_submission2.csv")
test <- read.csv("Earnings_Test_Students-1.csv")

teststem <- subset(test, test$Major == "STEM")
tree <- lm(Earnings~GPA+Number_Of_Professional_Connections+Graduation_Year+Number_Of_Credits+Number_Of_Parking_Tickets,  data=stem)
prediction <- predict(tree, newdata=test)
nrow(teststem)
submit[1:2000,]$Earnings<-prediction[1:2000]

testhumanities <- subset(test, test$Major == "Humanities")
tree <- lm(Earnings~GPA+Number_Of_Professional_Connections+Graduation_Year+Number_Of_Credits+Number_Of_Parking_Tickets,  data=humanities)
prediction<- predict(tree, newdata=test)
nrow(testhumanities)
submit[2001:4000,]$Earnings<-prediction[2001:4000]

testvocational <- subset(test, test$Major == "Vocational")
tree <- lm(Earnings~GPA+Number_Of_Professional_Connections+Graduation_Year+Number_Of_Credits+Number_Of_Parking_Tickets,  data=vocational)
prediction<- predict(tree, newdata=test)
nrow(testvocational)
submit[4001:6000,]$Earnings<-prediction[4001:6000]

testprofessional <- subset(test, test$Major == "Professional")
tree <- lm(Earnings~GPA+Number_Of_Professional_Connections+Graduation_Year+Number_Of_Credits+Number_Of_Parking_Tickets,  data=professional)
prediction<- predict(tree, newdata=test)
nrow(testprofessional)
submit[6001:8000,]$Earnings<-prediction[6001:8000]

testbusiness <- subset(test, test$Major == "Buisness")
tree <- lm(Earnings~GPA+Number_Of_Professional_Connections+Graduation_Year+Number_Of_Credits+Number_Of_Parking_Tickets,  data=business)
prediction<- predict(tree, newdata=test)
nrow(testbusiness)
submit[8001:9000,]$Earnings<-prediction[8001:9000]

testother <- subset(test, test$Major == "Other")
tree <- lm(Earnings~GPA+Number_Of_Professional_Connections+Graduation_Year+Number_Of_Credits+Number_Of_Parking_Tickets,  data=other)
prediction<- predict(tree, newdata=test)
nrow(testother)
submit[9001:10000,]$Earnings<-prediction[9001:10000]


write.csv(submit, 'earning_submission.csv', row.names=FALSE)
