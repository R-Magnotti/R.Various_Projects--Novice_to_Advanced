#Author: Richard Magnotti

library(class)

cars = read.csv("cars.txt",header=T)
summary(cars)

cars.sub1 = cars[c(1:7)]
summary(cars.sub1)

cars.norm = as.data.frame(scale(cars.sub1)) #z-score method
summary(cars.norm)

intBrand = as.integer(cars$brand) #stepwise regression with brand as an integer

stepdata= cbind(intBrand,cars.norm)
str(stepdata)

model = lm(intBrand ~ ., data =  stepdata)
model

stepped = step(model) #shows the best subset of the numeric variable
stepped

cars.sub2 = cars.norm[c(1,3,4,5,7)]
str(cars.sub2)
numObs = seq(1,261)

train = subset(cars.sub2, numObs%%4!=0)
str(train)

test = subset(cars.sub2, numObs %% 4 == 0)
str(test)

carsTrain = subset(cars$brand, numObs %% 4 != 0)
carsTest = subset(cars$brand, numObs %% 4 == 0)

KCat0 = knn(train, test, carsTrain, k = 1)
KCat1 = knn(train, test, carsTrain, k = 3)
KCat2 = knn(train, test, carsTrain, k = 5)
KCat3 = knn(train, test, carsTrain, k = 7)
KCat4 = knn(train, test, carsTrain, k = 9)

table(KCat0)
table(KCat1)
table(KCat2)
table(KCat3)
table(KCat4)

#calculations done on calculator
table(KCat0,carsTest) # ~ 69% accuracy
table(KCat1,carsTest) # ~ 63% 
table(KCat2,carsTest) # ~ 58%
table(KCat3,carsTest) # ~ 65%
table(KCat4,carsTest) # ~ 66%
#KCat0 has highest accuracy
#AKA KCat0 has the best value of k

KCv0 = knn.cv(cars.sub2, cars$brand, k = 1)
KCv1 = knn.cv(cars.sub2, cars$brand, k = 3)
KCv2 = knn.cv(cars.sub2, cars$brand, k = 5)
KCv3 = knn.cv(cars.sub2, cars$brand, k = 7)
KCv4 = knn.cv(cars.sub2, cars$brand, k = 9)

#calculations done on calculator
table(KCv0,cars$brand) # ~ 75% accuracy
table(KCv1,cars$brand) # ~ 75%
table(KCv2,cars$brand) # ~ 73%
table(KCv3,cars$brand) # ~ 75% 
table(KCv4,cars$brand) # ~ 74%

#accuracy values for knn.cv show a slightly higher accuracy compared to knn method
