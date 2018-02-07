#Author: Richard Magnotti

cereals = read.table("cereals_dat.txt",header=T) 
#to input the data file and set it as a a class

summary(cereals)
#to print a STATISTICAL summary of the data in "cereals_dat.txt"

str(cereals)
#to give the sturcture of the object - what types of variables in object 

cereals.sub1 = cereals[c(4:16)]
#to create a subset object out of the "cereals" class 
#in the form of a vector
#(the numbers in the braces represent chosen columns)

cereals.sub2 = cereals[c(8,16)]
#to create a vector of columns 8 and 16

plot(cereals.sub2)
#to plot the 8 and 16 columns of the cereals data set (aka fiber and rating)
#the correlation that we see is that as cereals' fiber increases, so does its rating 

pairs(cereals.sub1)
#to plot all the variables in pairs to look for correlations
#with RATING, there is a negative correlation with SUGARS and with CUPS

cor(cereals.sub1)
#to make a "cor" ouput (correlation)
#It looked to me like only SUGAR and CUPS had a negative correlation with
#RATING. However, it seems like both CUPS and SUGAR, as well as WEIGHT, VITAMINS,
#SODIUM, FAT,and CALORIES all had negative correlatoins with RATING

hist(cereals$RATING)
#to make a histogram of the data associated with the RATING variable
#(the '$' is used to access an element of a list and its data)
#All-Bran_with_Extra_Fiber appears to be the outlier based on the histogram

cereals.min = apply(cereals.sub1,2,min)
cereals.max = apply(cereals.sub1,2,max)
cereals.range = (cereals.max - cereals.min)
cereals.norm = data.frame(scale(cereals.sub1, center=cereals.min, scale=cereals.range))
summary(cereals.norm)
#to normalize the data from the numeric subset and diplay it
#using the min/max method

znorm = scale(cereals.sub1)
summary(znorm)
#to nromalize the data of the numeric subset using the z-score norm

boxplot.stats(cereals.sub1$RATING)
#to get a numerical value for outlying values
#outlier is 93.70491

cereals.sub3 = subset(cereals.sub1$RATING,cereals.sub1$RATING<93.70491)
RATING=cereals.sub3
#to remove the outlier from the RATING variable data 

boxplot.stats(RATING)
#to show that the outlier has been removed from RATING's data 

cloud(RATING ~ cereals.sub1$FIBER + cereals.sub1$SUGARS, main = "3D Plot of RATING vs FIBER and SUGARS")
#to make a plot of RATING vs FIBER and SUGARS

scatterplot3d(cereals.sub1$RATING,cereals.sub1$FIBER,cereals.sub1$SUGARS,main = "3D Plot of RATING vs FIBER and SUGARS")
#To make a 3D scatterplot of the same data

table(cereals$TYPE,cereals$MANUF)
#to create a table of TYPE to MANUF

