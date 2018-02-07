#Author: Richard Magnotti
library(class)
library(MASS)
library(rpart)

crime = read.csv("Major Crimes in Seattle.csv",header=T)

#strutcture/summary of raw data
str(crime)
summary(crime)

crime.sub1 = crime[c(2:11)]

#normalize by z-score
crime.norm1 = scale(crime.sub1)
boxplot(crime$VEHICLE_THEFT)
boxplot.stats(crime$VEHICLE_THEFT)

#find correlations
plot(crime.sub1)
#from looking at the plot, the following have strong correlation values:
#robbery & assault, roberry & violent_crimes_total, burglary & vehicle_theft, 
#larceny_theft & property_crimes_total, vehicle_theft & property_crimes_total
#property_crimes_total&major_crimes_total

cor(crime.sub1)

#we found no NA's/missing data by looking at the summary

#normalize via min-max
crime.min = apply(crime.sub1,2,min)
crime.max = apply(crime.sub1,2,max)
crime.range = (crime.max - crime.min)
crime.norm2 = data.frame(scale(crime.sub1, center=crime.min, scale=crime.range))
summary(crime.norm2)
str(crime.norm2)

#already normalized by z-score, but let' turn it into a data frame for convenience
crime.sub2 = as.data.frame(crime.sub1)

#stepwise regression with PRECINCT as an integer
intPrecinct = as.integer(crime$PRECINCT)
stepdata= cbind(intPrecinct,crime.sub2)
str(stepdata)

model = lm(intPrecinct ~ ., data =  stepdata)
model

#shows the best subset of the numeric variable
stepped = step(model) 
stepped
#the most important numeric variables are rape, robbery larceny_theft, and major_crimes_total
#larceny_theft is the most important variable

#now we define a multiple linear regression model with 
#the output variables as a function of theother numeric variables
crime.sub3 = crime.sub2[c(2,3,7,10)]

#summary of linear model 
summary(model)

#second linear model for predicting VIOLENT_CRIMES_TOTAL
model2 = lm(crime.sub2$VIOLENT_CRIMES_TOTAL ~ ., data =  crime.sub2)
model2
#beta values: 1.139e-14, 1.000e+00, 1.000e+00, 
#1.000e+00, 1.000e+00, 1.340e-15, 1.475e-15, 1.486e-15, -1.444e-15

obsnum = seq(1,627)

#test and train 
train1 = subset(crime.sub3, obsnum %% 4 != 0)
test1 = subset(crime.sub3, obsnum %% 4 == 0)
str(test1)
str(train1)
train2 = subset(crime$PRECINCT, obsnum %% 4!=0)
test2 = subset(crime$PRECINCT, obsnum %% 4 == 0)
train.data = cbind(train2, train1)
test.data = cbind(test2, test1)

#use KNN
KCat0 = knn(train1, test1, train2, k = 1) # ~38% accuracy
KCat1 = knn(train1, test1, train2, k = 3) # ~38% accuracy
KCat2 = knn(train1, test1, train2, k = 5) # ~40% accuracy
KCat3 = knn(train1, test1, train2, k = 7) # ~41% accuracy
KCat4 = knn(train1, test1, train2, k = 9) # ~44% accuracy
#KCat4 has highest accuracy of ~44% (calculated by calculator)

table(KCat0,test2)
table(KCat1,test2) 
table(KCat2,test2) 
table(KCat3,test2) 
table(KCat4,test2)

#use LDA 
ldaPrecinct.full= lda(train2 ~ ., data = train.data)
ldaPrecinctClass.full = predict(ldaPrecinct.full, test1)
table(ldaPrecinctClass.full$class, test2)
#LDA predictor has an accuracy of ~44% (calculated by calculator)

#CART
library(C50) #to double check C50 was installed
train3 = subset(crime, obsnum %% 4 != 0)
test3 = subset(crime, obsnum %% 4 == 0)
train.crime.num = train3[,2:11]
test.crime.num = test3[,2:11]
model.C50 = C5.0(train.crime.num, train2)
plot(model.C50)
predict.C50 = predict(model.C50, test.crime.num)
table(predict.C50, test2)
#C50 predicted with an accuracy of ~46%

#back propogation method, AKA neural nets
precinctI=precinctII=precinctIII=precinctIV=precinctV=precinctVI=as.integer(crime$PRECINCT)
for (i in 1:627)
{
  +	(precinctI[i]=if(crime$PRECINCT[i] == "EAST") 1 else 0)
  +	(precinctII[i]=if(crime$PRECINCT[i] == "NORTH") 1 else 0)
  +	(precinctIII[i]=if(crime$PRECINCT[i] == "SOUTH") 1 else 0)
  + (precinctIV[i]=if(crime$PRECINCT[i] == "SW") 1 else 0)
  + (precinctV[i]=if(crime$PRECINCT[i] == "UNKNOWN") 1 else 0)
  + (precinctVI[i]=if(crime$PRECINCT[i] == "WEST") 1 else 0)
}

clPrecinct = cbind(precinctI,precinctII,precinctIII,precinctIV,precinctV,precinctVI)

precinct.crimetrain.NN = subset(clPrecinct, numObs %% 4 != 0)
precinct.crimetest.NN = subset(clPrecinct, numObs %% 4 == 0)

precinct.train.NN.matrix = as.matrix(precinct.crimetrain.NN)
precinct.test.NN.matrix = as.matrix(precinct.crimetest.NN)

train.NN.matrix = as.matrix(train1)
test.NN.matrix = as.matrix(test1)

library(neural)

neurons = 7

nn.neural = mlptrain(train.NN.matrix, neurons, precinct.train.NN.matrix, it=20)
#using 20 interations to save output time
nn.neural

nn.neural.out = mlp(test.NN.matrix, nn.neural$weight,nn.neural$dist, nn.neural$neurons, nn.neural$actfns)
nn.neural.out.round= round(nn.neural.out, digits=1)
#to round output to whole number

table(nn.neural.out.round,precinct.crimetest.NN)
#NN predictor has an accuracy ~83% !!

#KNN and LDA have the lowest accuracy of ~44%
#LDA, KNN, and CART have all similar percentages, 
#while neural nets has an exponentially higher at 83%!!

#KMEANS
crime.int = as.integer(crime$PRECINCT)
crime.cluster = cbind(crime.int,crime.sub1)
str(crime.cluster)
crime.scale=as.data.frame(scale(crime.cluster))
str(crime.scale)

#compute WSS for 1-15 cluster centers
wss=(nrow(crime.scale) - 1) * sum(apply(crime.scale, 2, var))

for(i in 1:15)
{wss[i]=sum(kmeans(crime.scale,center=i)$withinss)}

plot(1:15, wss, type="b", xlab="Number of Clusters", 
     ylab="Within Cluster Sum of Sq Error or WSS")
#looks like the point of diminishing return is about 6 clusters, k~6
#however, because our crime.ch and crime.asw k-values match, we need not use the wss k-value

#use KMEANSRUNS ro find potential k vlues to run KMEANS
#install.packages("fpc")
library(fpc)
crime.ch = kmeansruns(crime.scale, krange = 1:15, criterion ="ch")
crime.ch
crime.ch$bestk #k=2
crime.asw = kmeansruns(crime.scale, krange = 1:15, criterion ="asw")
crime.asw$bestk #k=2
#the two k-values agree, k=2

fit.crime = kmeans(crime.scale,2,nstart = 100,iter.max = 100)
print.cluster = function(labels, k){
  for( i in 1:k){
    print(paste("Cluster #", i))
    print(crime[labels==i,c(1:7)])
  }
}

groups.fit.crime = fit.crime$cluster
fit.crime
fit.crime$cluster
print.cluster(groups.fit.crime, 2)
fit.crime$centers
#because we have only one k-value, we need only run KMEANS once,
#therefore, k=2 by default provides the best results 




