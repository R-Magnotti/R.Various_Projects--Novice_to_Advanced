#Author: Richard Magnotti

library(DMwR)

data("algae")
#access algae from DMwR

summary(algae)
str(algae)
#summary of algae

#33 NA's out of 200 entries

algae2 = na.omit(algae)
#using naomit to purge NA's from the data frame
summary(algae2)
#shows that there are no more NA's
#but that there are also now only 184 entries 

algaeSub1 = algae[c(4:11)]
summary(algaeSub1)
#to create a new data set just including data attached to NA's

newCl = replace(algaeSub1$Cl,is.na(algaeSub1$Cl),mean(na.omit(algaeSub1$Cl)))
#to create a sub data frame that consists of column Cl without NA's

summary(newCl)
#to show a summary of the sub data frame to demonstrate the omission of NA's

cleanAlgae1 = knnImputation(algae)
#using KNN imputation to make new data frame with default values
cleanAlgae2 = knnImputation(algae, k=5)
#KNN imputation with k equal to 5
summary(cleanAlgae1)
summary(cleanAlgae2)
cleanAlgae3 = knnImputation(algae,k=5,meth='median')
cleanAlgae4 = knnImputation(algae,k=5,meth='median')
#now using median instead of mean
summary(cleanAlgae3)
summary(cleanAlgae4)

qqnorm(algae$a7)
qqline(algae$a7)
#variable a7 does not seem to be normally ditributed based on the graphs
#(lin is not touched by many points and points don't follow a line)

shapiro.test(algae$a7)
#the shapiro test says that the data is NOT normally distributed
#we know this because the w value should be as close to 1 as possible
#while our w value is .5 

ks.test(algae$a7, "pnorm")
#the ks test says that algae$a7 is not normalized




