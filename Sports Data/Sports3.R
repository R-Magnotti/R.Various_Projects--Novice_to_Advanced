#Author: Richard Magnotti

library(class)
library(MASS)
library(rpart)
library(C50)
library(fpc)
library(cluster)

sports  = read.csv("sports.csv",header=T) 
summary(sports)

sports.sub1 = sports[c(1:8)]
str(sports.sub1)

sports.norm = scale(sports.sub1)
summary(sports.norm)
sports.data.frame = as.data.frame(sports.norm)
str(sports.data.frame)

#from the look of the summary of sports.norm, we identify 2 potential outliers with MAX.'s > 3
#these are Agility and Decision_Making

boxplot.stats(sports.sub1$Agility) #outliers: 73,76,80 
boxplot.stats(sports.sub1$Decision_Making) #outliers: 103,103,103,98,89,103,99,91,98,101,103,101,94,98,100,92,101

#second linear model for predicting Agility
model = lm(sports.data.frame$Agility ~ ., data =  sports.data.frame)
model
#lm identifies the important prediction variables to be: Strength, Injury, Endurance, and Age

intPS = as.integer(sports$Prime_Sport)
stepdata= cbind(intPS,sports.sub1)
str(stepdata)

model2 = lm(intPS ~ ., data =  stepdata)
model2
#lm2 predicts the most important variables to be Injury, Strength, Agility, Quickness, and Decision_Making

library(randomForest)
forest.2000 = randomForest(Prime_Sport ~ ., data = sports, importance = T, ntree = 2000)
importance(forest.2000)
varImpPlot(forest.2000)
#random forest shows that the most important variables in classifying prime_sport to be:
#Agility, Decision_Making, Age, Strength, Vision, and Quickness 
#this compares to step in asmuch as they have in common:
#Strength, Agility, Quickness, and Decision_Making

#use KNN
str(sports.data.frame) #493 observables
numObs = seq(1,493)
trainKNN1 = subset(sports.data.frame, numObs%%4!=0)
str(trainKNN1)
trainKNN2 = subset(sports, numObs%%4!=0)
str(trainKNN2)
testKNN1 = subset(sports.data.frame, numObs %% 4 == 0)
str(testKNN1)
testKNN2 = subset(sports, numObs %% 4 == 0)
str(testKNN2)

train.prime_sport = subset(sports$Prime_Sport,numObs %% 4 != 0)
test.prime_sport = subset(sports$Prime_Sport, numObs %% 4 == 0)

KCat = knn(trainKNN1, testKNN1, train.prime_sport, k = 3)
table(KCat,test.prime_sport)
# ~35% accuracy 

#C50
train.sports.num = trainKNN2[,1:8]
test.sports.num = testKNN2[,1:8]

model.C50 = C5.0(train.sports.num, train.prime_sport)
plot(model.C50)
predict.C50 = predict(model.C50, test.sports.num)
table(predict.C50, test.prime_sport)
# ~40% accuracy 
#C50 is a bit more accurate than KNN!!

#KMEANS
sports.cluster = cbind(intPS,sports.norm)
str(sports.cluster)
sports.clust.scale = as.data.frame(scale(sports.cluster))
str(sports.clust.scale)
sports.ch = kmeansruns(sports.clust.scale, krange = 1:15, criterion ="ch")
sports.ch #k=2

#WARD hclust method
dmatrix= dist(sports.clust.scale, method = "euclidean")

fith.sports = hclust(dmatrix, method="ward.D") # uses ANOVA to form clusters
plot(fith.sports)
#best k value is 2
rect.hclust(fith.sports, k = 2, border = "red")
h.groups = cutree(fith.sports, k = 2)
h.groups




