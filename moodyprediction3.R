getwd()

moody <- read.csv("M2022train.csv")
summary(moody)

#majority A or F
#B, C, D somewhat evenly distributed
table(moody$Grade)

#A range from 85-100
agrademoody <- subset(moody, moody$Grade == 'A')
min(agrademoody$Score)
summary(agrademoody)
#table(grademoody)

#B range from 63-98
bgrademoody <- subset(moody, moody$Grade == 'B')
min(bgrademoody$Score)
max(bgrademoody$Score)
summary(bgrademoody)

#C range from 50-99
cgrademoody <- subset(moody, moody$Grade == 'C')
min(cgrademoody$Score)
max(cgrademoody$Score)
summary(cgrademoody)

#D range from 21-97
dgrademoody <- subset(moody, moody$Grade == 'D')
min(dgrademoody$Score)
max(dgrademoody$Score)
summary(dgrademoody)

#F range from 18-88
fgrademoody <- subset(moody, moody$Grade == 'F')
min(fgrademoody$Score)
max(fgrademoody$Score)
summary(fgrademoody)

#guarantees based from 1st quartile lows
#F has to be 20 or less
#D has to be 49 or less
#C has to be 62 or less
#B has to be 84 or less
#A has to be 85 or more

#refining based on majors
#refining by majors instead of seniority since seniority may not be as indicative
table(moody[moody$Score>70 & moody$Major=='Psychology',]$Grade)
table(moody[moody$Score>75 & moody$Major=='Economics',]$Grade)
table(moody[moody$Score>90 & moody$Major=='CS',]$Grade)
table(moody[moody$Score>83 & moody$Major=='Statistics',]$Grade)

table(moody[moody$Score>57 & moody$Major=='Psychology',]$Grade)
table(moody[moody$Score>65 & moody$Major=='Economics',]$Grade)
table(moody[moody$Score>75 & moody$Major=='CS',]$Grade)
table(moody[moody$Score>72 & moody$Major=='Statistics',]$Grade)

table(moody[moody$Score>50 & moody$Major=='Psychology',]$Grade)
table(moody[moody$Score>55 & moody$Major=='Economics',]$Grade)
table(moody[moody$Score>64 & moody$Major=='CS',]$Grade)
table(moody[moody$Score>62 & moody$Major=='Statistics',]$Grade)

table(moody[moody$Score>30 & moody$Major=='Psychology',]$Grade)
table(moody[moody$Score>40 & moody$Major=='Economics',]$Grade)
table(moody[moody$Score>54 & moody$Major=='CS',]$Grade)
table(moody[moody$Score>43 & moody$Major=='Statistics',]$Grade)

table(moody[moody$Score<30 & moody$Major=='Psychology',]$Grade)
table(moody[moody$Score<40 & moody$Major=='Economics',]$Grade)
table(moody[moody$Score<54 & moody$Major=='CS',]$Grade)
table(moody[moody$Score<43 & moody$Major=='Statistics',]$Grade)


#applying predictor to submission file

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


myprediction$Grade <-decision
error <- mean(moody$Grade!= myprediction$Grade)
error

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

submit$Grade <- decision
submit

write.csv(submit, 'M2022submissionS.csv', row.names=FALSE)
