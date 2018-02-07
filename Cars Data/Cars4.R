#Author: Richard Magnotti

#_______________________________________________________________________________________________________
#first iteration using important variables
#CART found mpg, cubinches, hp, wightlbs, and year to be the most important

cars.sub1 = cars[c(1, 3, 4, 5, 7)]
cars.min = apply(cars.sub1,2,min)
cars.max = apply(cars.sub1,2,max)
cars.range = (cars.max - cars.min)
cars.norm = data.frame(scale(cars.sub1, center=cars.min, scale=cars.range))
summary(cars.norm)
str(cars.norm)
#to normalize the data from the numeric subset and display it
#using the min/max method

brandI=brandII=brandIII=as.integer(cars$brand)
for (i in 1:261)
{
  +	(brandI[i]=if(cars$brand[i] == " Europe.") 1 else 0)
  +	(brandII[i]=if(cars$brand[i] == " Japan.") 1 else 0)
  +	(brandIII[i]=if(cars$brand[i] == " US.") 1 else 0)
}

clBrand = cbind(brandI,brandII,brandIII)

numObs = seq(1,261)
train.NN = subset(cars.norm, numObs%%4!=0)
str(train.cars)
test.NN = subset(cars.norm, numObs %% 4 == 0)
str(test)

brand.train.NN = subset(clBrand, numObs %% 4 != 0)
brand.test.NN = subset(clBrand, numObs %% 4 == 0)

brand.train.NN.matrix = as.matrix(brand.train.NN)
brand.test.NN.matrix = as.matrix(brand.test.NN)

train.NN.matrix = as.matrix(train.NN)
test.NN.matrix = as.matrix(test.NN)

#install.packages("neural")
library(neural)

neurons = 6

nn.neural = mlptrain(train.NN.matrix, neurons, brand.train.NN.matrix, it=20)
nn.neural

nn.neural.out = mlp(test.NN.matrix, nn.neural$weight,nn.neural$dist, nn.neural$neurons, nn.neural$actfns)
nn.neural.out.round= round(nn.neural.out, digits=0)
#to round output to whole number
nn.neural.out.round

table(nn.neural.out.round,brand.test.NN)
table(brand.test.NN)

#_______________________________________________________________________________________________________
#second iteration using all numeric variables

cars.sub1 = cars[c(1:7)]
cars.min = apply(cars.sub1,2,min)
cars.max = apply(cars.sub1,2,max)
cars.range = (cars.max - cars.min)
cars.norm = data.frame(scale(cars.sub1, center=cars.min, scale=cars.range))
summary(cars.norm)
str(cars.norm)
#to normalize the data from the numeric subset and display it
#using the min/max method

brandI=brandII=brandIII=as.integer(cars$brand)
for (i in 1:261)
{
  +	(brandI[i]=if(cars$brand[i] == " Europe.") 1 else 0)
  +	(brandII[i]=if(cars$brand[i] == " Japan.") 1 else 0)
  +	(brandIII[i]=if(cars$brand[i] == " US.") 1 else 0)
}

clBrand = cbind(brandI,brandII,brandIII)

numObs = seq(1,261)
train.NN = subset(cars.norm, numObs%%4!=0)
str(train.cars)
test.NN = subset(cars.norm, numObs %% 4 == 0)
str(test)

brand.train.NN = subset(clBrand, numObs %% 4 != 0)
brand.test.NN = subset(clBrand, numObs %% 4 == 0)

brand.train.NN.matrix = as.matrix(brand.train.NN)
brand.test.NN.matrix = as.matrix(brand.test.NN)

train.NN.matrix = as.matrix(train.NN)
test.NN.matrix = as.matrix(test.NN)

neurons = 10

nn.neural = mlptrain(train.NN.matrix, neurons, brand.train.NN.matrix, it=20)
nn.neural

nn.neural.out = mlp(test.NN.matrix, nn.neural$weight,nn.neural$dist, nn.neural$neurons, nn.neural$actfns)
nn.neural.out.round= round(nn.neural.out, digits=0)
#to round output to whole number
nn.neural.out.round

table(nn.neural.out.round,brand.test.NN)
table(brand.test.NN)
