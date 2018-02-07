#Author: Richard Magnotti

library(class)

cars = read.csv("cars.txt",header=T)
summary(cars)

cars.sub1 = cars[c(1:7)]
summary(cars.sub1)

numObs = seq(1,261)

train.cars = subset(cars, numObs%%4!=0)
str(train.cars)

test.cars = subset(cars, numObs %% 4 == 0)
str(test)

cars.train.class = subset(cars$brand, numObs %% 4 != 0)
cars.test.class = subset(cars$brand, numObs %% 4 == 0)

library(rpart)
library(rpart)

rpart.brand = rpart(brand ~ ., data = train.cars)

plot(rpart.brand)
text(rpart.brand)
printcp(rpart.brand)
#important variables are cubinches, hp, mpg, weightlbs
summary(rpart.brand)

predict.rpart = predict(rpart.brand, test.cars, type = "class")
table(predict.rpart)
table(predict.rpart,test.cars$brand) # ~ 66% accuracy

#install.packages("C50")
library(C50) #to double check C50 was installed

train.cars.num = train.cars[,1:7]
test.cars.num = test.cars[,1:7]
model.C50 = C5.0(train.cars.num, cars.train.class)

plot(model.C50)
predict.C50 = predict(model.C50, test.cars.num)
table(predict.C50, cars.test.class) # ~ 75% accuracy
#the accuracy of C50 is noticably greater

#install.packages("randomForest")
library("randomForest") #to double check random_forest was installed

forest.default.500 = randomForest(brand ~ ., data = train.cars, importance = T)
forest.default.500
plot(forest.default.500)
summary(forest.default.500)
importance(forest.default.500) 
# important variables are mpg, cylinders, cubicinches, hp, weightlbs, time.to.60

predict.default = predict(forest.default.500, test.cars, type = "class")
table(predict.default)
table(predict.default,test.cars$brand) # ~ 86% accuracy 
#random forest has a MUCH higher accuracy than the CART and C50 algorithm (11% and 19% respectively)
