#tutorial ch 8
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

#prompt 1 (tutorial ch 9)
ETdat <- read.csv("/cloud/project/ETdata.csv")

unique(ETdat$crop)

#install.packages(c("lubridate", "forecast"))
library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)

#average fields (monthly) for almonds
almond <- ETdat %>%
  filter(crop == "Almonds") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))

#plot data
ggplot(almond, aes(x=ymd(date), y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthly evapotranspiration (in)")

#create almond ET time series
almond_ts <- ts(almond$ET.in,
                start = c(2016,1),
                frequency = 12)
#decompose time series
almond_dec <- decompose(almond_ts)

#plot decomposition
plot(almond_dec)

#find autocorrelation factor
acf(na.omit(almond_ts), 
    lag.max = 24)

#partial acf
pacf.plot <- pacf(na.omit(almond_ts))

#1st order ar
almond_y <- na.omit(almond_ts)
model1 <- arima(almond_y,
                order=c(1,0,0))
model1

#4th order ar
model4 <- arima(almond_y,
                order = c(4,0,0))
model4

#calculate fit
AR_fit1 <- almond_y - residuals(model1)
AR_fit4 <- almond_y - residuals(model4)

#plot data
plot(almond_y)

#plot fit
points(AR_fit1, type = "l", col= "tomato3", lty=2, lwd=2)
points(AR_fit4, type = "l", col= "darkgoldenrod4", lty=2, lwd=2)
legend("topleft", c("data", "AR1", "AR4"),
       lty=c(1,2,2), lwd=c(1,2,2),
       col=c("black", "tomato3", "darkgoldenrod4"),
       bty="n")

#ar4 model forecast
newAlmond <- forecast(model4)
newAlmond

#make dataframe for plotting
newAlmondF <- data.frame(newAlmond)

#set up dates
years <- c(rep(2021,4), rep(2022,12), rep(2023,8))
month <- c(seq(9,12), seq(1,12), seq(1,8))
newAlmondF$dateF <- ymd(paste(years,"/", month, "/",1))

#make a plot with data and predictions including a prediction interval
ggplot()+
  geom_line(data = almond, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(almond$date[1]), newAlmondF$dateF[24])+
  geom_line(data = newAlmondF, aes(x=dateF, y=Point.Forecast),
            col="red")+
  geom_ribbon(data=newAlmondF,
              aes(x=dateF, ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

#Homework 6
#Question 1: transform CO2 data and design a regression analysis
#transform data using equation
ghg$co2.tr <- 1/(ghg$co2 + 1000)
plot(ghg$co2.tr)
#more potential variables
ghg$log.chl <- log(ghg$chlorophyll.a+1)
ghg$log.sa <- log(ghg$surface.area+1)
ghg$log.vol <- log(ghg$volume+1)
ghg$log.runoff <- log(ghg$runoff+1)

#binary variables already created in tutorial
#run multiple regression
#create model object
mod.co2 <- lm(co2.tr ~ airTemp+log.age+log.precip+
                log.DIP+BorealV+
                mean.depth+log.sa, data=ghg)
summary(mod.co2)

#checking assumptions
res.co2 <- rstandard(mod.co2)
fit.co2 <- fitted.values(mod.co2)
#normality
# qq plot
qqnorm(res.co2, pch=19, col="grey50")
qqline(res.co2)
# shapiro-wilks test
shapiro.test(res.co2)

#plot residuals
plot(fit.co2,res.co2, pch=19, col="grey50")
abline(h=0)

#isolate continuous variables:
reg.data.co2 <- data.frame(ghg$airTemp, ghg$log.age,
                       ghg$mean.depth, ghg$log.precip,
                       ghg$log.DIP, ghg$log.sa)
                         
# make a correlation matrix 
chart.Correlation(reg.data.co2, histogram=TRUE, pch=19)

#model selection
#stepwise model
# run stepwise
full.step.co2 <- ols_step_forward_aic(mod.co2)
full.step.co2 

full.step.co2$model

plot(full.step.co2)


#Question 2: Decompose the evapotranspiration time series for almonds, 
#pistachios, fallow/idle fields, corn, and table grapes. 

#pistachios
pistachio <- ETdat %>%
  filter(crop == "Pistachios") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))

#create pistachio time series
pistachio_ts <- ts(pistachio$ET.in,
                start = c(2016,1),
                frequency = 12)
#pistachio decomposition
pistachio_dec <- decompose(pistachio_ts)
#plot
plot(pistachio_dec)

#fallow/idle fields
fallow <- ETdat %>%
  filter(crop == "Fallow/Idle Cropland") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))

#fallow/idle fields time series
fallow_ts <- ts(fallow$ET.in,
                   start = c(2016,1),
                   frequency = 12)
#fallow/idle fields decomposition
fallow_dec <- decompose(fallow_ts)
#plot
plot(fallow_dec)

#corn
corn <- ETdat %>%
  filter(crop == "Corn") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))

#corn time series
corn_ts <- ts(corn$ET.in,
                   start = c(2016,1),
                   frequency = 12)
#corn decomposition
corn_dec <- decompose(corn_ts)
#plot
plot(corn_dec)

#table grapes
grapes <- ETdat %>%
  filter(crop == "Grapes (Table/Raisin)") %>%
  group_by(date) %>%
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE))

#grapes time series
grapes_ts <- ts(grapes$ET.in,
                   start = c(2016,1),
                   frequency = 12)
#grapes decomposition
grapes_dec <- decompose(grapes_ts)
#plot
plot(grapes_dec)

#Question 3: Design an autoregressive model for pistachios and 
#fallow/idle fields. Forecast future evapotranspiration for each field Make a 
#plot that includes historical and forecasted evapotranspiration for the crops

#pistachios
#partial ar to determine optimal order for model
pacf.plot <- pacf(na.omit(pistachio_ts))

#first order ar
pistachio_y <- na.omit(pistachio_ts)
p.model1 <- arima(pistachio_y , 
                order = c(1,0,0)) 
p.model1

#second order ar
p.model2 <- arima(pistachio_y,
                  order = c(2,0,0))

#third order ar
p.model3 <- arima(pistachio_y,
                 order = c(3,0,0))
p.model3

#fourth order ar
p.model4 <- arima(pistachio_y,
                  order = c(4,0,0))
p.model4

#fifth order ar
p.model5 <- arima(pistachio_y,
                  order = c(5,0,0))
p.model5

#calculate fit
p_AR_fit1 <- pistachio_y - residuals(p.model1) 
p_AR_fit2 <- pistachio_y - residuals(p.model2)
p_AR_fit3 <- pistachio_y - residuals(p.model3)
p_AR_fit4 <- pistachio_y - residuals(p.model4)
p_AR_fit5 <- pistachio_y - residuals(p.model5)
#plot data
plot(pistachio_y)
# plot fit
points(p_AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(p_AR_fit2, type = "l", col = "purple", lty = 2, lwd=2)
points(p_AR_fit3, type = "l", col = "lightblue", lty = 2, lwd=2)
points(p_AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
points(p_AR_fit5, type = "l", col = "lightgreen", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR3","AR4", "AR5"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black","tomato3","lightblue","darkgoldenrod4", "lightgreen"),
       bty="n")
#model 5 appears to be best fit and has lowest aic
#forecast
newPistachio <- forecast(p.model5)

#create dataframe
newPistachioF <- data.frame(newPistachio)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newPistachioF$dateF <- ymd(paste(years,"/",month,"/",1))

#plot
ggplot() +
  geom_line(data = pistachio, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(pistachio$date[1]),newPistachioF$dateF[24])+  
  geom_line(data = newPistachioF, aes(x = dateF, y = Point.Forecast),
            col="red") +  
  geom_ribbon(data=newPistachioF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ 
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

#fallow/idle fields
#partial ar to determine optimal order for model
pacf.plot <- pacf(na.omit(fallow_ts))

#first order ar
fallow_y <- na.omit(fallow_ts)
f.model1 <- arima(fallow_y , 
                  order = c(1,0,0)) 
f.model1
#third order ar
f.model3 <- arima(fallow_y , 
                  order = c(3,0,0)) 
f.model3
#fourth order ar
f.model4 <- arima(fallow_y,
                  order = c(4,0,0))
f.model4

#calculate fit
f_AR_fit1 <- fallow_y - residuals(f.model1) 
f_AR_fit3 <- fallow_y - residuals(f.model3)
f_AR_fit4 <- fallow_y - residuals(f.model4)

#plot data
plot(fallow_y)

#plot fit
points(f_AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(f_AR_fit3, type = "l", col = "purple", lty = 2, lwd=2)
points(f_AR_fit4, type = "l", col = "lightblue", lty = 2, lwd=2)

#fourth order appears to be best fit
#forecast
newFallow <- forecast(f.model4)

#create dataframe
newFallowF <- data.frame(newFallow)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newFallowF$dateF <- ymd(paste(years,"/",month,"/",1))

#plot
ggplot() +
  geom_line(data = fallow, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(fallow$date[1]),newFallowF$dateF[24])+  
  geom_line(data = newFallowF, aes(x = dateF, y = Point.Forecast),
            col="red") +  
  geom_ribbon(data=newFallowF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ 
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")