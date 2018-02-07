#Author: Richard Magnotti
#to source this, Cars1 needs to be sourced first

library(class)
library(MASS)

cars.norm
cars.sub2

train2 = subset(cars.sub2, numObs%%4!=0)
str(train)
test2 = subset(cars.sub2, numObs %% 4 == 0)
str(test)

train3 = subset(cars$brand, numObs%%4!=0)
test3 = subset(cars$brand, numObs%%4==0)

train.data = cbind(train3, train2)
test.data = cbind(test3, test2)
ldaBrand.full= lda(train3 ~ ., data = train.data)
ldaBrandClass.full = predict(ldaBrand.full, test2)

table(ldaBrandClass.full$class, test3)
# ~ 78.46% accuracy

cars.sub2

brand.full.cv = cbind(cars$brand, cars.sub2)
ldaBrandCV = lda(cars$brand ~ mpg + cubicinches + hp + weightlbs + year, data=brand.full.cv, CV =T)
table(ldaBrandCV$class, cars$brand)
# ~ 76% accuracy

#lda has a higher accuracy (78%) than lda with cv=T (76%). However, both are within the same 
#magnetude and the difference is not large