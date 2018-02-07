#Author: Richard Magnotti

library(class)
library(MASS)
library(rpart)

sports  = read.csv("sports.csv",header=T) 
summary(sports)

sports.sub1 = sports[c(1:8)]
str(sports.sub1)

sports.norm = as.data.frame(scale(sports.sub1))
str(sports.norm)

numObs = seq(1,493)

train1 = subset(sports.norm, numObs%%4!=0)
str(train1)
train2 = subset(sports, numObs%%4!=0)
str(train2)
test1 = subset(sports.norm, numObs %% 4 == 0)
str(test1)
test2 = subset(sports, numObs %% 4 == 0)
str(test2)

train.prime_sport = subset(sports$Prime_Sport,numObs %% 4 != 0)
test.prime_sport = subset(sports$Prime_Sport, numObs %% 4 == 0)

#train.data = cbind(train.prime_sport, train1)
#ldaSports.full= lda(train.prime_sport ~ ., data = train.data)
#ldaPrime.Class.full = predict(ldaSports.full, test1)
#table(ldaPrime.Class.full, test.prime_sport) 
#could not get LDA to work properly. Issue with ldaPrime variable

KCat = knn(train1, test1, train.prime_sport, k = 7)
table(KCat,test.prime_sport) # ~ 37% accuracy

rpart.prime = rpart(Prime_Sport ~ ., data = train2)
plot(rpart.prime)
text(rpart.prime)
printcp(rpart.prime)
summary(rpart.prime)
#important variables:
#agility, endurance, strength, decision_making

predict.rpart = predict(rpart.prime, test2, type = "class")
table(predict.rpart)
table(predict.rpart,test2$Prime_Sport) # ~ 49% accuracy

#(could not get LDA to work properly). KNN=7 provided ~ 37% accuracy vs CART with a ~ 49% accucary. 
#CART is the most accurate among the classification methods






