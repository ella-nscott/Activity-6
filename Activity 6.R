#install.packages(c("dplyr", "ggplot2", "olsrr", "PerformanceAnalytics"))
library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)

ghg <- read.csv("/cloud/project/Deemer_GHG_Data.csv")

#log transform data
#note: add 1 to some variables because can't take log of 0
ghg$log.ch4 <- log(ghg$ch4+1)
ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP+1)
ghg$log.precip <- log(ghg$precipitation)

#controling for categorical binary variables
unique(ghg$Region) #prints out unique objects in vector

#binary variable for boreal region
ghg$BorealV <- ifelse(ghg$Region == "Boreal",1,0)

#binary variable for tropical region
ghg$TropicalV <- ifelse(ghg$Region == "Tropical",1,0)

#binary variable for alpine region
ghg$AlpineV <- ifelse(ghg$Alpine == "yes",1,0)

#binary variable for known hydropower
ghg$HydroV <- ifelse(ghg$hydropower == "yes",1,0)

#multiple regression 
mod.full <- lm(log.ch4 ~ airTemp+
                 log.age+mean.depth+
                 log.DIP+
                 log.precip+ BorealV, data=ghg)
summary(mod.full)

#checking assumptions
#isolate standardized results and the fitted values 
res.full <- rstandard(mod.full)
fit.full <- fitted.values(mod.full)

#normality
#qq plot
qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)

#shapiro-wilks test: good for low number of data
#null hypothesis: data normally distributed, hypothesis: data not normally distributed
#if p value > 0.05: accept null hypothesis, if < 0.05 reject null hypothesis
shapiro.test(res.full)

#residuals
plot(fit.full, res.full, pch=19, col="grey50")
abline(h=0)

#multicolinarity
#isolate continuous model variables into data frame:

reg.data <- data.frame(ghg$airTemp,
                       ghg$log.age, ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip)
#correlation matrix
chart.Correlation(reg.data, histogram=TRUE, pch=19)

#model selection
#run stepwise
full.step <- ols_step_forward_aic(mod.full)

#view table
full.step

#check full model
full.step$model

#plot AIC over time
plot(full.step)

#note: lower AIC indicates better fit, 
#looking at how AIC changes with each variable

#predictions
#prediction with interval for predicting a point
predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="prediction")
#look at prediction with 95% confidence interval of the mean
predict.lm(mod.full, data.frame(airTemp=20,log.age=log(2),
                                mean.depth=15,log.DIP=3,
                                log.precip=6, BorealV=0),
           interval="confidence")
