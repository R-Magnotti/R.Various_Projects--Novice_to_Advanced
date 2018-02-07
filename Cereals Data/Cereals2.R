#Author: Richard Magnotti

cereals = read.table("cereals_dat.txt",header=T)

t.test(cereals$RATING)
#mean = 42.6657
#95% confidence rating = 39.47736 & 45.85405

mean(cereals$RATING)
#the mean of cereals$RATING is 42.6657
#this is the same as the t test mean

wilcox.test(cereals$RATING)
#the Willcox test shows p-value = 2.512e-14
#while the t.test shows p-value < 2.2e-16
#there is a small discrepency since the p-values differ

cereals.sample.1 = cereals[sample(1:nrow(cereals), 20, replace=F),]
cereals.sample.2 = cereals[sample(1:nrow(cereals), 20, replace=F),]

t.test(cereals.sample.1$RATING)
t.test(cereals.sample.2$RATING)
#sample 1 has a mean of 45.58223  and a 95% confidence rating of 36.97893 54.18552
#sample 2 has a mean of 39.70148  and a 95% confidence rating of 33.58643 45.81653

t.test(cereals.sample.1$RATING,cereals.sample.2$RATING)
#the difference in means is NOT equal to zero

wilcox.test(cereals.sample.1$RATING,cereals.sample.2$RATING)
#with a p-value of 0.2516, 

cereals.sub1 = cereals.sample.1[c(4:16)]
cereals.sub2 = cereals.sample.2[c(4:16)]

lm1 = lm(RATING ~ ., data=cereals.sub1)
summary(lm1)
#based on this linear model, we now know that SHELF, WEIGHT, and CUPS are the
#least significant of the variables in relation to affect on RATING

lm2 = lm(RATING ~ CALORIES+PROTEIN+FAT+SODIUM+FIBER+CARBO+SUGARS+POTASS+VITAMINS, data = cereals.sub1)
#lm2 is a linear model with only the variables that are sginificantly influential to RATING
#the standard error and multiple r-squared values are the same as the previous linear model
summary(lm2)

prediction1 = 5.493e+01 + (-2.227e-01*5.02971) + 
  (3.273e+00*5.02971) + (-1.691e+00*5.02971) + 
  (-5.449e-02*5.02971) + (3.443e+00*5.02971) + 
  (1.092e+00*5.02971) + (-7.249e-01*5.02971) +
  (-3.399e-02*5.02971) + (-5.121e-02*5.02971)

prediction1

prediction2 = predict(lm2, newdata=data.frame(CALORIES=-2.227e-01*5.02971,PROTEIN=3.273e+00,
                                              FAT=-1.691e+00*5.02971, SODIUM=-5.449e-02*5.02971,
                                              FIBER=3.443e+00*5.02971, CARBO=1.092e+00*5.02971,
                                              SUGARS=-7.249e-01*5.02971, POTASS=-3.399e-02*5.02971,
                                              VITAMINS=-5.121e-02*5.02971),interval="confidence")
summary(prediction2)


