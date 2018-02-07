#Richard Magnotti

baseball = read.table("baseball.txt",header=T) 

summary(baseball)
str(baseball)

hist(baseball$bat_ave)
table(baseball$team)

boxplot.stats(baseball$bat_ave)
#outliers include:
# [1] 0.111 0.455 0.077 0.100 0.105 0.077 0.083 0.091 0.091 0.100 0.429 0.400 0.600 0.600 0.500 0.667 0.667
#[18] 0.667 0.500 0.500 1.000

baseball.sub1 = baseball[c(3,5:19)]
plot(baseball.sub1)
#for one example of each:
#positive correlation: hits and runs
#negative correlation: age and caught_stealing
#no correlation: hit and bat_ave

cor(baseball.sub1)
#yes, these results agree with my correlation assumptions 
#hits and runs: 0.97070423
#age and caught_stealing: -0.01318307
#hit and bat_ave: 0.12534959 (which obviously isn't 0, but it's far form negative and far from strong positive)

znorm = scale(baseball.sub1)
summary(znorm)

shapiro.test(baseball.sub1$bat_ave)
#with a p-value less than 2.2e-16, this means we can REJECT the null hypothesis (the data is normal)
#therefore we conclude that bat_ave is not normalized

baseball.sample.1 = baseball[sample(1:nrow(baseball), 20, replace=F),]
baseball.sample.2 = baseball[sample(1:nrow(baseball), 20, replace=F),]

t.test(baseball.sample.1$bat_ave)
#95% confidence interval of: 0.2322355 0.2947645 
t.test(baseball.sample.2$bat_ave)
#95% confidence interval of: 0.2302091 0.3571909

t.test(baseball.sample.1$bat_ave,baseball.sample.2$bat_ave)
#the true difference in means is not equal to zero!
#therefore there IS a difference in means
#that difference of means is equal to .0302 (which, relatively, is not too significant, but is worth noting)

lm1 = lm(bat_ave ~ hits + on_base_pct, data = baseball.sub1)
summary(lm1)
#beta values: 
#hits = -1.144e-04
#on_base_pct = 9.301e-01
#intercept = -2.878e-02
#residual standard error = 0.04045 on 275 degrees of freedom
#Multiple R-squared = 0.8101
#Adjusted R-squared = 0.8088 

batAve.predict = predict(lm1, newdata=data.frame(hits=160,on_base_pct=.250),interval="confidence")
batAve.predict

