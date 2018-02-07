#Author: Richard Magnotti

#import sports.csv/normalize via min-max
sports  = read.csv("sports.csv",header=T) 
summary(sports)

sports.sub1 = sports[c(1:8)]
str(sports.sub1)

sports.min = apply(sports.sub1,2,min)
sports.max = apply(sports.sub1,2,max)
sports.range = (sports.max - sports.min)
sports.norm = data.frame(scale(sports.sub1, center=sports.min, scale=sports.range))
summary(sports.norm)

Prime_SportI=Prime_SportII=Prime_SportIII=Prime_SportIV=as.integer(sports$Prime_Sport)
int.sport = as.integer(sports$Prime_Sport)
head(int.sport)
for (i in 1:493)
{
  +	(Prime_SportI[i]=if(int.sport[i] == 1) 1 else 0)
  +	(Prime_SportII[i]=if(int.sport[i] == 2) 1 else 0)
  +	(Prime_SportIII[i]=if(int.sport[i] == 3) 1 else 0)
  + (Prime_SportIV[i]=if(int.sport[i] == 4) 1 else 0)
}

clPrime = cbind(Prime_SportI,Prime_SportII,Prime_SportIII,Prime_SportIV)

#create train subset
numObs = seq(1,493)
train.sports = subset(sports.norm, numObs%%4!=0)
str(train.sports)

#create test subset
test.sports = subset(sports.norm, numObs %% 4 == 0)
str(test.sports)

#create prime sport train/test vectors
prime.train.sports = subset(clPrime, numObs %% 4 != 0)
prime.test.sports = subset(clPrime, numObs %% 4 == 0)

#convert to matrix format/run mlp/mlptrain
prime.train.sports.matrix = as.matrix(prime.train.sports)
prime.test.sports.matrix = as.matrix(prime.test.sports)
train.sports.matrix = as.matrix(train.sports)
test.sports.matrix = as.matrix(test.sports)

neurons = 7
sports.neural = mlptrain(train.sports.matrix, neurons, prime.train.sports.matrix, it=20)
sports.neural

sports.neural.out = mlp(test.sports.matrix, sports.neural$weight,sports.neural$dist, sports.neural$neurons, sports.neural$actfns)
sports.neural.out.round = round(sports.neural.out, digits=0)
#to round output to whole number
sports.neural.out.round

#confusion matrix of sports neural network
table(sports.neural.out.round,prime.test.sports)
table(prime.test.sports)

#normalized data frame that includes integer version of primeSport
sports.int = as.integer(sports$Prime_Sport)
sports.cluster = cbind(sports.int,sports.sub1)
str(sports.cluster)
sports.scale=as.data.frame(scale(sports.cluster))
str(sports.scale)

#compute WSS form 1-15
wss=(nrow(sports.scale) - 1) * sum(apply(sports.scale, 2, var))
for(i in 1:15)
{wss[i]=sum(kmeans(sports.scale,center=i)$withinss)}

plot(1:15, wss, type="b", xlab="Number of Clusters", 
     ylab="Within Cluster Sum of Sq Error or WSS")

#use kmeans .ch method
#install.packages("fpc")
library(fpc)
sports.ch = kmeansruns(sports.scale, krange = 1:15, criterion ="ch")
sports.ch
sports.ch$bestk
#the best k value is k=2

#run kmeans
fit.sports = kmeans(sports.scale,2,nstart = 100,iter.max = 100)
print.cluster = function(labels, k){
  for( i in 1:k){
    print(paste("Cluster #", i))
    print(sports[labels==i,c(1:7)])
  }
}
fit.sports









