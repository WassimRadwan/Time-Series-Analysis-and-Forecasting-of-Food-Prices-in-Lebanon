#Required Libraries
library(fpp2) #includes the forecast package
library(urca)
library(tseries)

#Imported the dataset via From Text (readr)

#Renamed the dataset from wfp_food_prices_lbn to foodprice
df = South_Sour_Sour_Sugar_white_KG_LBP$usdprice

#Print the dataset
df

#Convert the dataset into a time series
data= ts(df, frequency= 12, start=c(2013,1), end= c(2021, 12))

#Observe the time series
data
autoplot(data)

#Descriptive Statistics
summary(data) #Summary statistics
sd(data) #Standard deviation
length(data)# Length of Dataset, 12 x 8 = 108 records

#Split the time series into a training set and test set:

#Train set
train<- window(data, frequency = 12, end = c(2020,12)) 
length(train) #Training set has 96 records

#Test set
test<- window(data, frequency = 12,  start= c(2021, 1))
length(test) #Test set has 12 records

####Benchmarking models:####

#1. Average Method
meanforecast=meanf(train, h=12)
autoplot(meanforecast)  

#2. Naive Method
naiveforecast=naive(train, h=12)
autoplot(naiveforecast)  

#3. Seasonal Naive Method
snaiveforecast=snaive(train, h=12)
autoplot(snaiveforecast)  

#4. Random Walk Forecast Method
randomwalkforecast=rwf(train, drift=TRUE, h=12)
autoplot(randomwalkforecast)  

rmse_mean=accuracy(meanforecast$mean, test)[2]  
rmse_naive=accuracy(naiveforecast$mean, test)[2]  
rmse_snaive=accuracy(snaiveforecast$mean, test)[2]  
rmse_randomwalk=accuracy(randomwalkforecast$mean, test)[2]  

#Tabulating the RMSEs of the benchmarking methods
benchmarking_models <-
  c("meanforecast","naiveforecast","snaiveforecast",
    "randomwalkforecast")

rmse_benchmarks <-c(rmse_mean, rmse_naive, rmse_snaive,
                    rmse_randomwalk)

all_benchmarking_models = data.frame(benchmarking_models, rmse_benchmarks)

#The best benchmarking model, i.e., the one with the lowest RMSE:
all_benchmarking_models$benchmarking_models[which.min(all_benchmarking_models$rmse_benchmarks)]

autoplot(train) +
  autolayer(meanforecast,series="meanf",PI=FALSE)	+  autolayer(naiveforecast,series="naive",PI=FALSE)	+
  autolayer(snaiveforecast,series="snaive",PI=FALSE)	+  autolayer(randomwalkforecast,series="rwf",PI=FALSE)	+
  guides(colour=guide_legend(title="Forecast")) + ggtitle("Forecasts of Different Benchmarking Models")

#The Average method has the lowest RMSE among the benchmarking methods, hence, it is the best method 
#in this case

####Plots####

autoplot(train)+
  ggtitle("Price of Sugar in Lebanon ($/KG)") + xlab("Year") +
  ylab("$/KG") #From 2013 to 2019, the price has been relatively stable. However, we observe a rapidly increasing trend 
#during 2020 followed by a sharp decrease shortly after, followed by a slight increase. Thus, a changing trend is 
#observed. The data shows barely any signs of seasonality

ggseasonplot(train, polar= TRUE) 
gglagplot(train) 
ggseasonplot(train) 
ggtsdisplay(train)
autoplot(mstl(train)) 

#All the plots indicate that there is no seasonality in the data. However,the data shows signs of changing trends

####Differencing and Transformation####

#KPSS test to check whether the data is stationary or not
summary(ur.kpss(train))

#data is non-stationary since the value of the test-statistic (0.8834) is greater than the 1% critical 
#value (0.739). Hence, we must perform differencing

dt = diff(train) #differencing the data 
ndiffs(dt)
summary(ur.kpss(dt)) 

ggtsdisplay(dt) 

#data is now stationary

#To find the appropriate lambda value
#(lam=BoxCox.lambda(train)) #Returns -0.9999242

lam = 0 #Log transformation or Natural Logarithm, forces forecasts and prediction intervals to be positive, and, in
#this case, is a better alternative than the lambda value returned automatically via BoxCox.lambda()

#Transform the training set using Box-Cox transformation , i.e., stabilize the variance
transformed_train=BoxCox(train,lam)

autoplot(transformed_train)+
  ggtitle("Price of Sugar in Lebanon ($/KG)") + xlab("Year") +
  ylab("$/KG")

#KPSS test to check whether the data is stationary or not
summary(ur.kpss(transformed_train)) 
#data is non-stationary since the value of the test-statistic (1.1483) is greater than the 1% critical 
#value (0.739). Hence, we must perform differencing

#Another method to check if the training set requires differencing

ndiffs(transformed_train) # as there is trend we are trying to get rid of it
#as expected we received 1

nsdiffs(transformed_train) # as there is no seasonality we expect 0
#Hence, we must perform first differencing in order to render the data stationary.
#No seasonal differencing is required 

#Differencing the transformed training set
train_diff=diff(transformed_train)

#Checking for differences
ndiffs(train_diff)
#The training data has been differenced since ndiffs() returned an output of 0

summary(ur.kpss(train_diff)) #the test statistic is equal to 0.5152 and it is less 
#than all the critical values, meaning, we accept H0 and conclude that the data 
#is stationary and non-seasonal

#Check the ACF and PACF of the transform differenced training set to determine potential ARIMA Models
ggtsdisplay(train_diff) #Based on  the ACF and PACF plots, potential ARIMA models are ARIMA(p,d,0) and ARIMA(0,d,q)

####Checking the performance of different ARIMA Models####

#If Confint contains a 0, we conclude that the model is not good

fit1transform= Arima(train, order= c(1,1,0),lambda = lam)
confint(fit1transform) #Does not include a 0, model is good

(AICc1transform = fit1transform$aicc) #Extract the AICc value of this ARIMA model.

#Note: AICc can be negative

fit2transform= Arima(train, order= c(0,1,1),lambda = lam)
confint(fit2transform) #Does not include a 0, model is good

(AICc2transform = fit2transform$aicc) #Extract the AICc value of this ARIMA model.


fit3transform= Arima(train, order= c(1,1,1),lambda = lam)
confint(fit3transform)

(AICc3transform = fit3transform$aicc) #Extract the AICc value of this ARIMA model.

fit4transform= Arima(train, order= c(1,1,2),lambda = lam)
confint(fit4transform) 

(AICc4transform = fit4transform$aicc) #Extract the AICc value of this ARIMA model.

fit5transform= Arima(train, order= c(2,1,1),lambda = lam)
confint(fit5transform) 

(AICc5transform = fit5transform$aicc) #Extract the AICc value of this ARIMA model.

fit6transform= Arima(train, order= c(0,1,2),lambda = lam)
confint(fit6transform) #includes a 0 this means this model is not suitable for use

(AICc6transform = fit6transform$aicc) #Extract the AICc value of this ARIMA model.

fit7transform= Arima(train, order= c(2,1,0),lambda = lam)
confint(fit7transform) 

(AICc7transform = fit7transform$aicc) #Extract the AICc value of this ARIMA model.

#fit8transform= Arima(train, order= c(1,0,1),lambda = lam)
#confint(fit8transform) 

#(AICc8transform = fit8transform$aicc) #Extract the AICc value of this ARIMA model.

#fit8 returned an error in this case, so it is ignored

#Use automated algorithm, i.e., auto.arima(), to find the best ARIMA Model for the time series
fit9transform= auto.arima(train, approximation=F, stepwise=F, lambda = lam) #ARIMA (1,1,2)
fit9transform
confint(fit9transform) 

(AICc9transform = fit9transform$aicc) #Extract the AICc value of this ARIMA model.

# Computing the RMSE and compare through the various models:

rmse1<-accuracy(forecast(fit1transform,h=12)$mean,test)[2]
rmse2<-accuracy(forecast(fit2transform,h=12)$mean,test)[2]
rmse3<-accuracy(forecast(fit3transform,h=12)$mean,test)[2]
rmse4<-accuracy(forecast(fit4transform,h=12)$mean,test)[2]
rmse5<-accuracy(forecast(fit5transform,h=12)$mean,test)[2]
rmse6<-accuracy(forecast(fit6transform,h=12)$mean,test)[2]
rmse7<-accuracy(forecast(fit7transform,h=12)$mean,test)[2]
#rmse8<-accuracy(forecast(fit8transform,h=12)$mean,test)[2]
rmse9<-accuracy(forecast(fit9transform,h=12)$mean,test)[2]

# Collecting P-Values to see if they are white noise

p1 <- checkresiduals(fit1transform)$p.value
p2 <- checkresiduals(fit2transform)$p.value
p3 <- checkresiduals(fit3transform)$p.value
p4 <- checkresiduals(fit4transform)$p.value
p5 <- checkresiduals(fit5transform)$p.value
p6 <- checkresiduals(fit6transform)$p.value
p7 <- checkresiduals(fit7transform)$p.value
#p8 <- checkresiduals(fit8transform)$p.value 
p9 <- checkresiduals(fit9transform)$p.value 

# Making a dataframe to capture all scores and their respective models

models <-
  c("ARIMA(1,1,0)","ARIMA(0,1,1)","ARIMA(1,1,1)",
    "ARIMA(1,1,2)","ARIMA(2,1,1)","ARIMA(0,1,2)",
    "ARIMA(2,1,0)","AUTO.Arima(1,1,2)")

aics <-c(AICc1transform, AICc2transform, AICc3transform,
         AICc4transform, AICc5transform, AICc6transform,
         AICc7transform,AICc9transform)

rmses <- c(rmse1,rmse2, rmse3, rmse4, rmse5, rmse6, rmse7,rmse9)

pvalues <-c(p1,p2,p3,p4,p5,p6,p7,p9) 

all_models_arima = data.frame(models, aics,rmses,pvalues)

all_models_arima

# Return the model that had the lowest AICc value:
all_models_arima$models[which.min(all_models_arima$aics)] #ARIMA (1,1,2)

# Making a for loop where if p-value is greater than 0.05 then the models are white noise or else not
all_models_arima["White Noise"] = ifelse(all_models_arima$pvalues > 0.05, "Yes","No")

all_models_arima["White Noise"]

#Working with the best ARIMA model, i.e. ARIMA(1,1,2)

fit4transform # ARIMA(1,1,2)

summary(fit4transform)

forecasts.arima_transformed <- forecast(fit4transform, h=12) #Forecasting the next 12 months

checkresiduals(fit4transform) #The time plot shows that the variance is constant (or homoscedasticity) aside from a 
#few outliers. The histogram shows that the mean of residuals is 0 and they are normally distributed. Lastly, the ACF
#plot shows that there are no spikes outside the bounds, indicating that the residuals are White Noise.

autoplot(forecasts.arima_transformed)+ 
  ggtitle("Forecasts from the ARIMA(1,1,2) (Transformed)") + xlab("Year") +
  ylab("") #Plotting our forecast /  

accuracy(forecasts.arima_transformed, test) #Checking the RMSE of the model

autoplot(fit4transform) ## checking the invertibility and stationarity conditions are satisfied. In this case, all red
# dots are within each circle, thus the fitted model is both stationary and invertible

#Auto.Arima without Stepwise and Approximation
boxcox_arima <- auto.arima(train, lambda = lam) #ARIMA(1,1,2)  AICc= -0.41

boxcox_arima

aic_boxcox_arima <- boxcox_arima$aicc

summary(boxcox_arima)

forecasts.arima.box <- forecast(boxcox_arima, h=12) #Forecasting the next 12 months

checkresiduals(forecasts.arima.box) #Normally distributed with a mean = 0, homoscedasticity, and it appears to 
#be white noise

autoplot(forecasts.arima.box)+
  ggtitle("Forecasts from Auto.Arima(1,1,2) (Transformed w/o Stepwise and Approximation") + xlab("Year") +
  ylab("") #Plotting our forecast

accuracy(forecasts.arima.box, test)

####Checking the performance of different ARIMA models WITHOUT Box-Cox TRANSFORMATION####

#Experimenting with Different ARIMA Models
fit1= Arima(train, order= c(1,1,0))
confint(fit1)

(AICc1 = fit1$aicc) #Extract the AICc value of this ARIMA model.

fit2= Arima(train, order= c(0,1,1))
confint(fit2) 

(AICc2 = fit2$aicc) #Extract the AICc value of this ARIMA model.

fit3= Arima(train, order= c(1,1,1))
confint(fit3)

(AICc3 = fit3$aicc) #Extract the AICc value of this ARIMA model.

#fit4= Arima(train, order= c(1,1,2))
#confint(fit4) 

#(AICc4 = fit4$aicc) #Extract the AICc value of this ARIMA model.

#fit4 returned an error. Hence, it is ignored

fit5= Arima(train, order= c(2,1,1))
confint(fit5) 

(AICc5 = fit5$aicc) #Extract the AICc value of this ARIMA model.

fit6= Arima(train, order= c(0,1,2))
confint(fit6) 

(AICc6 = fit6$aicc) #Extract the AICc value of this ARIMA model.

fit7= Arima(train, order= c(2,1,0))
confint(fit7) 

(AICc7 = fit7$aicc) #Extract the AICc value of this ARIMA model.

#fit8= Arima(train, order= c(1,0,1))
#confint(fit8) 

#(AICc8 = fit8$aicc) #Extract the AICc value of this ARIMA model.

# fit8 returned an error. Hence, it is ignored.

#Use automated algorithm, i.e., auto.arima(), to find the best ARIMA Model for the time series
fit9= auto.arima(train, trace=TRUE, test="kpss", ic="aic", 
                 approximation=F, stepwise=F) # Best model: ARIMA (1,1,2) with drift
confint(fit9)

(AICc9 = fit9$aicc) #Extract the AICc value of this ARIMA model.

#Best model: ARIMA(1,1,2) with drift (fit9)

# Computing the RMSE and compare through the various models
rmse1_2<-accuracy(forecast(fit1,h=12)$mean,test)[2]
rmse2_2<-accuracy(forecast(fit2,h=12)$mean,test)[2]
rmse3_2<-accuracy(forecast(fit3,h=12)$mean,test)[2]
#rmse4_2<-accuracy(forecast(fit4,h=12)$mean,test)[2]
rmse5_2<-accuracy(forecast(fit5,h=12)$mean,test)[2]
rmse6_2<-accuracy(forecast(fit6,h=12)$mean,test)[2]
rmse7_2<-accuracy(forecast(fit7,h=12)$mean,test)[2]
#rmse8_2<-accuracy(forecast(fit8,h=12)$mean,test)[2]
rmse9_2<-accuracy(forecast(fit9,h=12)$mean,test)[2]

# Collecting P-Values to see if they are white noise
p1_2 <- checkresiduals(fit1)$p.value
p2_2 <- checkresiduals(fit2)$p.value
p3_2 <- checkresiduals(fit3)$p.value
#p4_2 <- checkresiduals(fit4)$p.value
p5_2 <- checkresiduals(fit5)$p.value
p6_2 <- checkresiduals(fit6)$p.value
p7_2 <- checkresiduals(fit7)$p.value
#p8_2 <- checkresiduals(fit8)$p.value 
p9_2 <- checkresiduals(fit9)$p.value 

# Making a dataframe to capture all scores of each model
models_2 <-
  c("ARIMA(1,1,0)","ARIMA(0,1,1)","ARIMA(1,1,1)","ARIMA(2,1,1)","ARIMA(0,1,2)",
    "ARIMA(2,1,0)","AUTO.Arima(1,1,2) with drift")

aics_2 <-c(AICc1, AICc2, AICc3, AICc5, AICc6,
           AICc7,AICc9)

rmses_2 <- c(rmse1_2,rmse2_2, rmse3_2,rmse5_2,
             rmse6_2, rmse7_2,rmse9_2)

pvalues_2 <-c(p1_2,p2_2,p3_2,p5_2, p6_2, p7_2,p9_2)

all_models_arima_2 = data.frame(models_2, aics_2,rmses_2,pvalues_2)

all_models_arima_2

# Return the model that had the lowest AICc value:
all_models_arima_2$models_2[which.min(all_models_arima_2$aics_2)] #ARIMA(1,1,2) with drift

# Making a for loop where if p-value is greater than 0.05 then the models are white noise or else not
all_models_arima_2["White Noise"] = ifelse(all_models_arima_2$pvalues > 0.05, "Yes","No")

all_models_arima_2["White Noise"]

#Working with the best ARIMA model, i.e., ARIMA(1,1,2) with drift

fit.arima <- fit9 

summary(fit.arima)

forecasts.arima <- forecast(fit9, h=12)

checkresiduals(fit9) #Variance is constant except for a few outliers (past 2020), Histogram shows that residuals are 
#normally distributed, and the ACF plot displays no autocorrelations,i.e., all spikes are within
#the blue-dashed bounds. Thus, the residuals are White Noise. The variance is constant (Homoscedasticity)

autoplot(forecasts.arima)+
  ggtitle("Forecasts from Arima(1,1,2) with drift") + xlab("Year") +
  ylab("")

accuracy(forecasts.arima, test)

autoplot(fit.arima) #the fitted model is proven to be both stationary and invertible.

#Auto.Arima without Stepwise and Approximation
arima_1 <- auto.arima(train) #ARIMA(1,1,2)with drift 

arima_1

aic_1_arima <- arima_1$aicc

summary(arima_1)

forecasts.arima_1 <- forecast(arima_1, h=12) #Forecasting the next 12 months

checkresiduals(forecasts.arima_1) #Similar analysis to the previous one

autoplot(forecasts.arima.box)+
  ggtitle("Forecasts from Auto.Arima(1,1,2) with drift") + xlab("Year") +
  ylab("") #Plotting our forecast

accuracy(forecasts.arima.box, test)

#### ETS vs. Holt's vs. Holt's-Winter ####

#Simple Exponential Smoothing method:

fc <- ses(data, h=12)
fc[['model']] #AICc = 679.8811

# Accuracy of one-step-ahead training errors
accuracy(fc)

#Rounded accuracy 
round(accuracy(fc),2)

autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("$/KG") + xlab("Year") #Plotting the forecast


#Holt's method:

fc2 <- holt(train, h=12)
fc2[['model']] #AICc = 560.4550

#Holt's method (Damped):

fc3 <- holt(train, damped=TRUE, phi = 0.9, h=12) #damping parameter phi is set to be 0.90
fc3[['model']] #AICc = 563.7557

fc4 <- hw(train,seasonal="additive")
fc5 <- hw(train,seasonal="multiplicative")

fc4[["model"]] #AICc = 586.7083 
fc5[["model"]] #AICc = 1304.327

autoplot(train) +
  autolayer(fc, series="Simple Exponential Smoothing Forecasts", PI=FALSE) +
  autolayer(fc2, series="Holt's forecasts", PI=FALSE)+
  autolayer(fc3, series="Holt's (Damped) forecasts", PI=FALSE)+
  autolayer(fc4, series="HW additive forecasts", PI=FALSE) +
  autolayer(fc5, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("$/KG") +
  ggtitle("Price of Sugar") +
  guides(colour=guide_legend(title="Forecast")) #Plotting the forecasts 

#Holt's method returned the lowest (or best) AICc 

summary(fc3)

forecasts.fc3 <- forecast(fc3, h=12)

checkresiduals(fc3) # Variance is constant in the time plot (except for a few outliers), Histogram shows signs of a 
#normal distribution and the mean is close to 0. The residuals are White Noise (according to the ACF plot)

autoplot(fc3) +
  xlab("Year") + ylab("") #Plotting the forecasts of the time series

accuracy(forecasts.fc3, test)