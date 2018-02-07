#Author: Richard Magnotti

crime = read.csv("Major Crimes in Seattle.csv",header=T)

#strutcture/summary of raw data
str(crime)
summary(crime)

crime.sub1 = crime[c(2:11)]
summary(crime.sub1)

crime.scale = as.data.frame(scale(crime.sub1))
str(crime.scale)

var.crime1 = apply(crime.scale, 2, var)
summary(var.crime1)

mean.crime = apply(crime.scale, 2, mean)
summary(mean.crime)

var.crime2 = apply(crime.sub1,2,var)
summary(var.crime2)

sd.crime = apply(crime.sub1, 2, sd)
summary(sd.crime) 
sd.crime

cor(crime.scale)
pairs(crime.sub1)
#very highh correlation between: 
#VIOENT_CRIMES_TOTAL & ROBBERY, VIOENT_CRIMES_TOTAL & ASSAULT, VIOENT_CRIMES_TOTAL & BURGLARY,
#VIOENT_CRIMES_TOTAL & LARCENY_THEFT, VIOENT_CRIMES_TOTAL & VEHICLE_THEFT, 
#VIOENT_CRIMES_TOTAL & PROPERTY_CRIMES_TOTAL, VIOENT_CRIMES_TOTAL & MAJOR_CRIMES_TOTAL,
#ROBBERY & ASSAULT, ROBBERY & BURGLARY, ROBBERY & LARCENY THEFT, ROBBERY & PROPERTY_CRIMES_TOTAL,
#ROBBEY & MAJOR_CRIMES_TOTAL, ASSAULT & BURGLARY, ASSAULT & LARCENY_THEFT, ASSAULT & VEHICLE_THEFT,
#ASSULT & PROPERTY_CRIMES_TOTAL, ASSAULT & MAJOR_CRIMES_TOTAL, etc...

#must input scaled numeric subset
pca1 = prcomp(crime.scale)
pca1
summary(pca1)
#ust imput scaled numeric subset
pca2 = princomp(crime.scale)
pca2
summary(pca2)
loadings(pca2)

library(randomForest)
forest.2000 = randomForest(PRECINCT ~ ., data = crime, importance = T, ntree = 2000)
importance(forest.2000)
varImpPlot(forest.2000)
#random forest finds important: LARCENY_THEFT, VEHICLE_THEFT, PROPERTY_CRIMES_TOTAL, BURGLARY, and 
#MAJOR_CRIMES_TOTAL

intPrecinct = as.integer(crime$PRECINCT)
stepdata= cbind(intPrecinct,crime.sub2)
str(stepdata)

model = lm(intPrecinct ~ ., data =  stepdata)
step(model)
#linear model found 4 important variables (RAPE, ROBBERY, LARCENY_THEFT, MAJOR_CRIMES_TOTAL)

#in order to be able to eliminate any variables, there should be some agreement between 
#the results of random forest and the linear model. However, they slightly disagree. Therefore,
#we need to look at the respective std. deviations of each variable to help see what we can eliminate. 
#VEHICLE_THEFT, MAJOR_CRIMES_TOTAL, PROPERTY_CRIMES_TOTAL, LARCENY_THEFT, and BURGLARY are the 
#variables with the highest standard deviation (we want as high S.D. as possible to get the biggest spread
#in values...aka the biggest effect on precint). Because random forest and lm don't completely agree, and 
#the standard deviations are more in favor of random forest, we can put a little more weight in the 
#results of random forest. Not to mention, by looking at cor function we can eliminate important
# variables that are highly correlated. Some variables found important in random forest not found 
# in lm have a high correlation coefficient (e.g. LARCENY_THEFT and ROBBERY (.88 cor coeff.)). So,
# for for example choosing LARCENY_THEFT over ROBBERY is supported by random forest, std. dev., and also 
# cor function. Therefore, our final data frame (based on the criteria just stated) will include important
# variables: MAJOR_CRIMES_TOTAL, PROPERTY_CRIMES_TOTAL, BURGLARY, and LARCENY_THEFT.

crime.important = crime[c(7,8,10,11)]
summary(crime.important)



