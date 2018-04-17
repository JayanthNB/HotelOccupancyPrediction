######################Forecasting Hotel Occupancy Rates for the Year 2018##########################
#install.packages("forecast")
library(forecast)

Time_series <- Time_series[1:42, ]

vec_FL <- Time_series$`Finger Lakes`
vec_TI <- Time_series$`Thousand Islands`
vec_AD <- Time_series$Adirondacks

#Training sets
ts_FL <- ts(vec_FL, start=c(2014, 1), end=c(2017, 06), frequency = 12)
ts_TI <- ts(vec_TI, start=c(2014, 1), end=c(2017, 06), frequency = 12)
ts_AD <- ts(vec_AD, start=c(2014, 1), end=c(2017, 06), frequency = 12)

plot(ts_FL)
plot(ts_TI)
plot(ts_AD)

#Training the model: Finger lakes
fit_FL <- HoltWinters(ts_FL)
plot(fit_FL)

#Forecasting: Finger Lakes
forecaseFL_18 <- forecast(fit_FL, 18)

#Training the model: Thousand Islands
fit_TI <- HoltWinters(ts_TI)
plot(fit_TI)

#Forecasting: Thousand Islands
forecaseTI_18 <- forecast(fit_TI, 18)

#Training the model: Adirondack
fit_AD <- HoltWinters(ts_AD)
plot(fit_AD)

#Forecasting: Adirondack
forecaseAD_18 <- forecast(fit_AD, 18)

###########################################Predictive modeling#####################################
##############################################Adirondack###########################################
#Weather and GT data vs Occupancy data
AD <- Time_Series_Adirondack[13:36 ,3:ncol(Time_Series_Adirondack)]
AD_y <- Time_Series_Adirondack[37:48 ,3:ncol(Time_Series_Adirondack)]

AD$MonthYr <- factor(AD$MonthYr)
AD$Month <- factor(AD$Month)

AD_y$MonthYr <- factor(AD_y$MonthYr)
AD_y$Month <- factor(AD_y$Month)

library(randomForest)
regressor_AD = randomForest(x = AD[-1],
                         y = AD$Occupancy_Rate,
                         ntree = 500, importance = TRUE)

# Predicting a new result with Random Forest Regression
y_pred_AD = predict(regressor_AD, AD_y[-1])

var_AD<-importance(regressor_AD)

varImpPlot(regressor_AD)

plot(regressor_AD)

#CTREE
ctree_AD <- ctree(AD$Occupancy_Rate~., data = AD)
plot(ctree_AD)

#####################################################Finger Lakes##########################################################
#Weather and GT data vs Occupancy data
FL <- Time_Series_FingerLakes[13:36 ,3:ncol(Time_Series_FingerLakes)]
FL_y <- Time_Series_FingerLakes[37:48 ,3:ncol(Time_Series_FingerLakes)]

FL$MonthYr <- factor(FL$MonthYr)
FL$Month <- factor(FL$Month)

FL_y$MonthYr <- factor(FL_y$MonthYr)
FL_y$Month <- factor(FL_y$Month)

library(randomForest)
regressor_FL = randomForest(x = FL[-1],
                            y = FL$Occupancy_Rate,
                            ntree = 500, importance = TRUE)

# Predicting a new result with Random Forest Regression
y_pred_FL = predict(regressor_FL, FL_y[-1])

var_FL<-importance(regressor_FL)

varImpPlot(regressor_FL)

plot(regressor_FL)

###################################################Thousand Islands########################################################
#Weather and GT data vs Occupancy data
TI <- Time_Series_ThousandIslands[13:36 ,3:ncol(Time_Series_ThousandIslands)]
TI_y <- Time_Series_ThousandIslands[37:48 ,3:ncol(Time_Series_ThousandIslands)]

TI$MonthYr <- factor(TI$MonthYr)
TI$Month <- factor(TI$Month)

TI_y$MonthYr <- factor(TI_y$MonthYr)
TI_y$Month <- factor(TI_y$Month)

library(randomForest)
regressor_TI = randomForest(x = TI[-1],
                            y = TI$Occupancy_Rate,
                            ntree = 500, importance = TRUE)

# Predicting a new result with Random Forest Regression
y_pred_TI = predict(regressor_TI, TI_y[-1])

var_TI<-importance(regressor_TI)

varImpPlot(regressor_TI)

plot(regressor_TI)