### This script will be used for building and evaluating models of Snow Water Equivalent (SWE) at SNOTEL sites
## data to be used was cleaned in SNOTELDaymetClean.R
## script by Griffin Shelor

##### Loading Packages, setting up script to use parallel package, reading in data #####
library(pacman)
p_load(snotelr, here, tidyverse, randomForest, caret, ranger, rstan, leaflet, shinydashboard, plotly, DT, daymetr, ncdf4, parallel, isotracer, tidymodels, sp, ModelMetrics, gt, gtExtras, RColorBrewer, ggpubr, webshot2, pals)

## setting script to maximise use of cores
options(mc.cores = detectCores())

SnotelDaymet <- read_csv(here("Project", "Data", "SNOTEL", "SnotelDaymetClean.csv")) |>
  filter(Year >= 2010) |>
  filter(state == "AK") |>
  filter(elev == max(elev) | elev == min(elev) | latitude == min(latitude) | latitude == max(latitude)) |>
  mutate(swe_plus = snow_water_equivalent + 1)

write_csv(SnotelDaymet, here("Project", "Data", "SNOTEL", "SnotelDaymetAKSubset.csv"))
## moving data down a row so that it is easier to work into a model statement and Stan data list
SnotelDaymet$prevday_swe <- -999
SnotelDaymet$prevday_maxtemp <- -999
SnotelDaymet$prevday_mintemp <- -999
SnotelDaymet$prevday_meantemp <- -999
SnotelDaymet$prevday_precip <- -999
SnotelDaymet$prevday_cumulative_precip <- -999

SnotelDaymet950 <- SnotelDaymet |>
  filter(site_id == 950) |>
  arrange(date)
SnotelDaymet966 <- SnotelDaymet |>
  filter(site_id == 966) |>
  arrange(date)
SnotelDaymet1062 <- SnotelDaymet |>
  filter(site_id == 1062) |>
  arrange(date)

for (i in 2:nrow(SnotelDaymet950)){
  SnotelDaymet950$prevday_swe[i] = SnotelDaymet950$snow_water_equivalent[i-1]
  SnotelDaymet950$prevday_maxtemp[i] <- SnotelDaymet950$daymet_maxtemp[i-1]
  SnotelDaymet950$prevday_mintemp[i] <- SnotelDaymet950$daymet_mintemp[i-1]
  SnotelDaymet950$prevday_meantemp[i] <- SnotelDaymet950$daymet_meantemp[i-1]
  SnotelDaymet950$prevday_precip[i] <- SnotelDaymet950$daymet_precip[i-1]
  SnotelDaymet950$prevday_cumulative_precip[i] <- SnotelDaymet950$precipitation_cumulative[i-1]
}
for (i in 2:nrow(SnotelDaymet966)){
  SnotelDaymet966$prevday_swe[i] = SnotelDaymet966$snow_water_equivalent[i-1]
  SnotelDaymet966$prevday_maxtemp[i] <- SnotelDaymet966$daymet_maxtemp[i-1]
  SnotelDaymet966$prevday_mintemp[i] <- SnotelDaymet966$daymet_mintemp[i-1]
  SnotelDaymet966$prevday_meantemp[i] <- SnotelDaymet966$daymet_meantemp[i-1]
  SnotelDaymet966$prevday_precip[i] <- SnotelDaymet966$daymet_precip[i-1]
  SnotelDaymet966$prevday_cumulative_precip[i] <- SnotelDaymet966$precipitation_cumulative[i-1]
}
for (i in 2:nrow(SnotelDaymet1062)){
  SnotelDaymet1062$prevday_swe[i] = SnotelDaymet1062$snow_water_equivalent[i-1]
  SnotelDaymet1062$prevday_maxtemp[i] <- SnotelDaymet1062$daymet_maxtemp[i-1]
  SnotelDaymet1062$prevday_mintemp[i] <- SnotelDaymet1062$daymet_mintemp[i-1]
  SnotelDaymet1062$prevday_meantemp[i] <- SnotelDaymet1062$daymet_meantemp[i-1]
  SnotelDaymet1062$prevday_precip[i] <- SnotelDaymet1062$daymet_precip[i-1]
  SnotelDaymet1062$prevday_cumulative_precip[i] <- SnotelDaymet1062$precipitation_cumulative[i-1]
}
## cutting off top row of each site subset since it does not contain true previous day information
SnotelDaymet950 <- SnotelDaymet950[2:nrow(SnotelDaymet950),]
SnotelDaymet966 <- SnotelDaymet966[2:nrow(SnotelDaymet966),]
SnotelDaymet1062 <- SnotelDaymet1062[2:nrow(SnotelDaymet1062),]

## merging them back together
SnotelDaymet <- rbind(SnotelDaymet950, SnotelDaymet966)
SnotelDaymet <- rbind(SnotelDaymet, SnotelDaymet1062) |>
  arrange(date, site_id)

##### Examining correlation of covariates against dependent var (SWE) #####
cor(SnotelDaymet$latitude, SnotelDaymet$snow_water_equivalent)
cor(SnotelDaymet$elev, SnotelDaymet$snow_water_equivalent)
cor(SnotelDaymet$prevday_maxtemp, SnotelDaymet$snow_water_equivalent)
cor(SnotelDaymet$prevday_mintemp, SnotelDaymet$snow_water_equivalent)
cor(SnotelDaymet$prevday_meantemp, SnotelDaymet$snow_water_equivalent)
cor(SnotelDaymet$prevday_precip, SnotelDaymet$snow_water_equivalent)
cor(SnotelDaymet$prevday_cumulative_precip, SnotelDaymet$snow_water_equivalent)
cor(SnotelDaymet$prevday_swe, SnotelDaymet$snow_water_equivalent)



##### Splitting data into testing, training, and forecast subsets #####
## 2 years of forecast data first
SnotelDaymet_WY2022fcast <- SnotelDaymet |>
  filter(date >= "2021-09-01") |>
  filter(date < "2022-06-01")
# SnotelDaymet_WY2021fcast <- SnotelDaymet |>
#   filter(date >= "2020-09-01") |>
#   filter(date <= "2021-06-01")
# SnotelDaymet_WY2020fcast <- SnotelDaymet |>
#   filter(date >= "2019-09-01") |>
#   filter(date <= "2020-06-01")

## subsetting data used to build models
SnotelDaymet_ModelData <- SnotelDaymet |>
  filter(date < "2021-09-01")

## splitting mode data into testing and training subsets
set.seed(802)
ModelData_split <- initial_split(SnotelDaymet_ModelData, prop = 0.75)
SnotelModel_train <- training(ModelData_split)
SnotelModel_test <- testing(ModelData_split)


##### Random Forest Models #####
##### RF model 1, all of the included covariates #####
set.seed(802)
ranger_mod_v1 <- ranger(formula = snow_water_equivalent ~ prevday_swe + latitude + elev + prevday_cumulative_precip + prevday_maxtemp + prevday_mintemp + prevday_meantemp + prevday_precip, data = SnotelModel_train, num.trees = 5000, mtry = 3, importance = "impurity", quantreg = TRUE)

## variable importance
sort(importance(ranger_mod_v1))

## creating predictions
# setting quantiles for ranger predictions
quants <- c(0.1, 0.5, 0.9)
set.seed(802)
ranger_predv1 <- predict(ranger_mod_v1, data = SnotelModel_test, type = "quantiles", quantiles = quants)

RangerPredictions_v1 <- data.frame(predictions(ranger_predv1))
plot(SnotelModel_test$snow_water_equivalent, col = 'black', ylim = c(min(RangerPredictions_v1[,1]),max(RangerPredictions_v1[,3])))
points(RangerPredictions_v1[,2], col = 'steelblue')
lines(RangerPredictions_v1[,1], col = 'red')
lines(RangerPredictions_v1[,3], col = 'red')

RF_v1_rmse <- rmse(SnotelModel_test$snow_water_equivalent, RangerPredictions_v1[,2])
RF_v1_rmse

set.seed(802)
ranger_predv1_fcast <- predict(ranger_mod_v1, data = SnotelDaymet_WY2022fcast, type = "quantiles", quantiles = quants)
RangerPredictions_v1_fcast <- data.frame(predictions(ranger_predv1_fcast))
plot(SnotelDaymet_WY2022fcast$snow_water_equivalent, col = 'black', ylim = c(min(min(RangerPredictions_v1_fcast[,1]), min(SnotelDaymet_WY2022fcast$snow_water_equivalent)),max(max(RangerPredictions_v1_fcast[,1]), max(SnotelDaymet_WY2022fcast$snow_water_equivalent))))
points(RangerPredictions_v1_fcast[,2], col = 'steelblue')
lines(RangerPredictions_v1_fcast[,1], col = 'red')
lines(RangerPredictions_v1_fcast[,3], col = 'red')


## plotting forecasts
RFModel1_FcastPlot <- ggplot(data = SnotelDaymet_WY2022fcast, aes(x = date)) +
  theme_bw() +
  geom_line(aes(y = RangerPredictions_v1_fcast[,2]), colour = 'dodgerblue2', linewidth = 1) +
  geom_line(aes(y = RangerPredictions_v1_fcast[,3]), linetype = 2, colour = 'black', linewidth = 1) +
  geom_line(aes(y = RangerPredictions_v1_fcast[,1]), linetype = 2, colour = 'black', linewidth = 1) +
  geom_point(y = SnotelDaymet_WY2022fcast$snow_water_equivalent, colour = 'red') +
  xlab("Date") +
  ylab("SWE (mm)") +
  ylim(min(min(SnotelDaymet_WY2022fcast$snow_water_equivalent),min(RangerPredictions_v1_fcast[,1])), max(max(SnotelDaymet_WY2022fcast$snow_water_equivalent),max(RangerPredictions_v1_fcast[,3]))) +
  #scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016,2017,2018)) +
  ggtitle("Forecasting SWE Using Random Forest Model with Autoregressive Term", subtitle = "Covariates: Previous SWE, Cumulative Precip, Max Temp, Mean Temp, Min Temp, Daily Precip Latitude, Elevation") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(size = 7, hjust = 0.5), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12))
RFModel1_FcastPlot
ggsave(here("Project", "Outputs", "RFv1ARForecast.png"))


## checking coverage
count = 0
for (i in 1:nrow(SnotelDaymet_WY2022fcast)){
  if (SnotelDaymet_WY2022fcast[i,"snow_water_equivalent"] < RangerPredictions_v1_fcast[i,3] & SnotelDaymet_WY2022fcast[i, "snow_water_equivalent"] > RangerPredictions_v1_fcast[i,1]){
    count <- count + 1
  } else{
    count <- count
  }
  coverage_RF_v1_fcast <- count / nrow(SnotelDaymet_WY2022fcast)
}
coverage_RF_v1_fcast

RF_v1_rmse_fcast <- rmse(SnotelDaymet_WY2022fcast$snow_water_equivalent, RangerPredictions_v1[,2])
RF_v1_rmse_fcast


#### Random Forest Model 2 #####
## removed latitude and previous day's daily precip
set.seed(802)
ranger_mod_v2 <- ranger(formula = snow_water_equivalent ~ prevday_swe + elev + prevday_cumulative_precip + prevday_maxtemp + prevday_mintemp + prevday_meantemp, data = SnotelModel_train, num.trees = 5000, mtry = 3, importance = "impurity", quantreg = TRUE)

## variable importance
sort(importance(ranger_mod_v2))

## creating predictions
# setting quantiles for ranger predictions
quants <- c(0.1, 0.5, 0.9)
set.seed(802)
ranger_predv2 <- predict(ranger_mod_v2, data = SnotelModel_test, type = "quantiles", quantiles = quants)

RangerPredictions_v2 <- data.frame(predictions(ranger_predv2))
plot(SnotelModel_test$snow_water_equivalent, col = 'black', ylim = c(min(min(RangerPredictions_v2[,1]), min(SnotelDaymet_WY2022fcast$snow_water_equivalent)),max(max(RangerPredictions_v2[,1]), max(SnotelDaymet_WY2022fcast$snow_water_equivalent))))
points(RangerPredictions_v2[,2], col = 'steelblue')
lines(RangerPredictions_v2[,1], col = 'red')
lines(RangerPredictions_v2[,3], col = 'red')

RF_v2_rmse <- rmse(SnotelModel_test$snow_water_equivalent, RangerPredictions_v2[,2])
RF_v2_rmse

set.seed(802)
ranger_predv2_fcast <- predict(ranger_mod_v2, data = SnotelDaymet_WY2022fcast, type = "quantiles", quantiles = quants)
RangerPredictions_v2_fcast <- data.frame(predictions(ranger_predv2_fcast))
plot(SnotelDaymet_WY2022fcast$snow_water_equivalent, col = 'black', ylim = c(min(min(RangerPredictions_v2_fcast[,1]), min(SnotelDaymet_WY2022fcast$snow_water_equivalent)),max(max(RangerPredictions_v2_fcast[,1]), max(SnotelDaymet_WY2022fcast$snow_water_equivalent))))
points(RangerPredictions_v2_fcast[,2], col = 'steelblue')
lines(RangerPredictions_v2_fcast[,1], col = 'red')
lines(RangerPredictions_v2_fcast[,3], col = 'red')

RFModel2_FcastPlot <- ggplot(data = SnotelDaymet_WY2022fcast, aes(x = date)) +
  theme_bw() +
  geom_line(aes(y = RangerPredictions_v2_fcast[,2]), colour = 'dodgerblue2', linewidth = 1) +
  geom_line(aes(y = RangerPredictions_v2_fcast[,3]), linetype = 2, colour = 'black', linewidth = 1) +
  geom_line(aes(y = RangerPredictions_v2_fcast[,1]), linetype = 2, colour = 'black', linewidth = 1) +
  geom_point(y = SnotelDaymet_WY2022fcast$snow_water_equivalent, colour = 'red') +
  xlab("Date") +
  ylab("SWE (mm)") +
  ylim(min(min(SnotelDaymet_WY2022fcast$snow_water_equivalent),min(RangerPredictions_v2_fcast[,1])), max(max(SnotelDaymet_WY2022fcast$snow_water_equivalent),max(RangerPredictions_v2_fcast[,3]))) +
  #scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016,2017,2018)) +
  ggtitle("Forecasting SWE Using Random Forest Model with Autoregressive Term", subtitle = "Covariates: Previous SWE, Cumulative Precip, Max Temp, Mean Temp, Min Temp, Elevation") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(size = 7, hjust = 0.5), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12))
RFModel2_FcastPlot
ggsave(here("Project", "Outputs", "RFv2ARForecast.png"))


## checking coverage
count = 0
for (i in 1:nrow(SnotelDaymet_WY2022fcast)){
  if (SnotelDaymet_WY2022fcast[i,"snow_water_equivalent"] < RangerPredictions_v2_fcast[i,3] & SnotelDaymet_WY2022fcast[i, "snow_water_equivalent"] > RangerPredictions_v2_fcast[i,1]){
    count <- count + 1
  } else{
    count <- count
  }
  coverage_RF_v2_fcast <- count / nrow(SnotelDaymet_WY2022fcast)
}
coverage_RF_v2_fcast

RF_v2_rmse_fcast <- rmse(SnotelDaymet_WY2022fcast$snow_water_equivalent, RangerPredictions_v2[,2])
RF_v2_rmse_fcast



##### Random Forest Model 3 #####
set.seed(802)
ranger_mod_v3 <- ranger(formula = snow_water_equivalent ~ elev + prevday_cumulative_precip + prevday_maxtemp + prevday_mintemp + prevday_meantemp, data = SnotelModel_train, num.trees = 5000, mtry = 3, importance = "impurity", quantreg = TRUE)

## variable importance
sort(importance(ranger_mod_v3))

## creating predictions
# setting quantiles for ranger predictions
quants <- c(0.1, 0.5, 0.9)
set.seed(802)
ranger_predv3 <- predict(ranger_mod_v3, data = SnotelModel_test, type = "quantiles", quantiles = quants)

RangerPredictions_v3 <- data.frame(predictions(ranger_predv3))
plot(SnotelModel_test$snow_water_equivalent, col = 'black', ylim = c(min(min(RangerPredictions_v3[,1]), min(SnotelDaymet_WY2022fcast$snow_water_equivalent)),max(max(RangerPredictions_v3[,1]), max(SnotelDaymet_WY2022fcast$snow_water_equivalent))))
points(RangerPredictions_v3[,2], col = 'steelblue')
lines(RangerPredictions_v3[,1], col = 'red')
lines(RangerPredictions_v3[,3], col = 'red')

RF_v3_rmse <- rmse(SnotelModel_test$snow_water_equivalent, RangerPredictions_v3[,2])
RF_v3_rmse

set.seed(802)
ranger_predv3_fcast <- predict(ranger_mod_v3, data = SnotelDaymet_WY2022fcast, type = "quantiles", quantiles = quants)
RangerPredictions_v3_fcast <- data.frame(predictions(ranger_predv3_fcast))

## plotting forecasts
# plot(SnotelDaymet_WY2022fcast$snow_water_equivalent, col = 'black', ylim = c(min(min(RangerPredictions_v3_fcast[,1]), min(SnotelDaymet_WY2022fcast$snow_water_equivalent)),max(max(RangerPredictions_v3_fcast[,1]), max(SnotelDaymet_WY2022fcast$snow_water_equivalent))))
# points(RangerPredictions_v3_fcast[,2], col = 'steelblue')
# lines(RangerPredictions_v3_fcast[,1], col = 'red')
# lines(RangerPredictions_v3_fcast[,3], col = 'red')

## plotting forecasts
RFModel3_FcastPlot <- ggplot(data = SnotelDaymet_WY2022fcast, aes(x = date)) +
  theme_bw() +
  geom_line(aes(y = RangerPredictions_v3_fcast[,2]), colour = 'dodgerblue2', linewidth = 1) +
  geom_line(aes(y = RangerPredictions_v3_fcast[,3]), linetype = 2, colour = 'black', linewidth = 1) +
  geom_line(aes(y = RangerPredictions_v3_fcast[,1]), linetype = 2, colour = 'black', linewidth = 1) +
  geom_point(y = SnotelDaymet_WY2022fcast$snow_water_equivalent, colour = 'red') +
  xlab("Date") +
  ylab("SWE (mm)") +
  ylim(min(min(SnotelDaymet_WY2022fcast$snow_water_equivalent),min(RangerPredictions_v3_fcast[,1])), max(max(SnotelDaymet_WY2022fcast$snow_water_equivalent),max(RangerPredictions_v3_fcast[,3]))) +
  #scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016,2017,2018)) +
  ggtitle("Forecasting SWE Using Random Forest Model Without Autoregressive Term", subtitle = "Covariates: Cumulative Precip, Max Temp, Mean Temp, Min Temp, Elevation") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(size = 8, hjust = 0.5), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12))
RFModel3_FcastPlot
ggsave(here("Project", "Outputs", "RFv3ARForecast.png"))


## checking coverage
count = 0
for (i in 1:nrow(SnotelDaymet_WY2022fcast)){
  if (SnotelDaymet_WY2022fcast[i,"snow_water_equivalent"] < RangerPredictions_v3_fcast[i,3] & SnotelDaymet_WY2022fcast[i, "snow_water_equivalent"] > RangerPredictions_v3_fcast[i,1]){
    count <- count + 1
  } else{
    count <- count
  }
  coverage_RF_v3_fcast <- count / nrow(SnotelDaymet_WY2022fcast)
}
coverage_RF_v3_fcast

RF_v3_rmse_fcast <- rmse(SnotelDaymet_WY2022fcast$snow_water_equivalent, RangerPredictions_v3[,2])
RF_v3_rmse_fcast




##### Stan Models #####
##### Stan Model 3 (models 1 and 2 are running on other laptop) #####
## making list of data to declare what goes into stan model
model3_datalist <- list(N = nrow(SnotelModel_train), y = SnotelModel_train$snow_water_equivalent, x1 = SnotelModel_train$elev, x2 = SnotelModel_train$prevday_cumulative_precip, x3 = SnotelModel_train$prevday_maxtemp, x4 = SnotelModel_train$prevday_mintemp, x5 = SnotelModel_train$prevday_meantemp, x6 = SnotelModel_train$prevday_precip, x7 = SnotelModel_train$latitude, x8 = SnotelModel_train$prevday_swe)

## fitting stan model
set.seed(802)
options(mc.cores = parallel::detectCores())
model3_fit <- stan(file=here("Project", "Scripts", "ProjectModel3.stan"),data = model3_datalist, chains = 3, iter = 20000, warmup = 3000)
model3_fit


## Extracting Parameters
model3_pars <- rstan::extract(model3_fit, c("b0","b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "sigma"))

## making predictions
## adding in process uncertainty
Pred_out_SWE <- matrix(NA, length(model3_pars$b0), nrow(SnotelModel_test))
Pred_out_mean_SWE <- matrix(NA, length(model3_pars$b0), nrow(SnotelModel_test))

## process error
set.seed(802)
for (p in 1:length(model3_pars$b0)){
  swe_init <- SnotelModel_test$snow_water_equivalent[1]
  for(t in 1:nrow(SnotelModel_test)){
    swe_val <- rnorm(1, mean = model3_pars$b0[p] + model3_pars$b1[p] * SnotelModel_test$elev[t] + model3_pars$b2[p] * SnotelModel_test$prevday_cumulative_precip[t] + model3_pars$b3[p] * SnotelModel_test$prevday_maxtemp[t] + model3_pars$b4[p] * SnotelModel_test$prevday_mintemp[t] + model3_pars$b5[p] * SnotelModel_test$prevday_meantemp[t] + model3_pars$b6[p] * SnotelModel_test$prevday_precip[t] + model3_pars$b7[p] * SnotelModel_test$latitude[t] + model3_pars$b8[p] * SnotelModel_test$prevday_swe[t], sd = model3_pars$sigma[p])
    Pred_out_SWE[p,t] <- swe_val
  }
}


## generating forecasts
MeanPred_mod3 <- apply(Pred_out_SWE,2,mean)
Upper_mod3 <- apply(Pred_out_SWE,2,quantile, prob=.9)
Lower_mod3 <- apply(Pred_out_SWE,2,quantile, prob=.1)

## plotting forecasts against data
plot(MeanPred_mod3, type='l', ylim = c(min(Lower_mod3),max(Upper_mod3)), main = "Applying Model on Randomly Split Test Data, Stan Model 3", ylab = "SWE (mm)")
lines(Upper_mod3,lty=2)
lines(Lower_mod3,lty=2)
points(SnotelModel_test$snow_water_equivalent,col='steelblue')

## checking coverage
count = 0
for (i in 1:nrow(SnotelModel_test)){
  if (SnotelModel_test[i,"snow_water_equivalent"] < Upper_mod3[i] & SnotelModel_test[i, "snow_water_equivalent"] > Lower_mod3[i]){
    count <- count + 1
  } else{
    count <- count
  }
  coverage_mod3 <- count / nrow(SnotelModel_test)
}
coverage_mod3

## RMSE
Mod3RMSE <- rmse(SnotelModel_test$snow_water_equivalent, MeanPred_mod3)
Mod3RMSE


### Taking Stan Model 3, making applying to data to be forecasted
## making predictions
## adding in process uncertainty
Pred_out_SWE_fcast <- matrix(NA, length(model3_pars$b0), nrow(SnotelDaymet_WY2022fcast))
Pred_out_mean_SWE_fcast <- matrix(NA, length(model3_pars$b0), nrow(SnotelDaymet_WY2022fcast))


## Parameter error
for (p in 1:length(model3_pars$b0)){
  for (t in 1:nrow(SnotelDaymet_WY2022fcast)){
    mean_SWE <- model3_pars$b0[p] + model3_pars$b1[p] * SnotelDaymet_WY2022fcast$elev[t] + model3_pars$b2[p] * SnotelDaymet_WY2022fcast$prevday_cumulative_precip[t] + model3_pars$b3[p] * SnotelDaymet_WY2022fcast$prevday_maxtemp[t] + model3_pars$b4[p] * SnotelDaymet_WY2022fcast$prevday_mintemp[t] + model3_pars$b5[p] * SnotelDaymet_WY2022fcast$prevday_meantemp[t] + model3_pars$b6[p] * SnotelDaymet_WY2022fcast$prevday_precip[t] + model3_pars$b7[p] * SnotelDaymet_WY2022fcast$latitude[t] + model3_pars$b8[p] * SnotelDaymet_WY2022fcast$prevday_swe[t]
    Pred_out_mean_SWE_fcast[p,t] <- mean_SWE
  }
}



## generating forecasts
MeanPred_mod3param_fcast <- apply(Pred_out_mean_SWE_fcast,2,mean)
Upper_mod3param_fcast <- apply(Pred_out_mean_SWE_fcast,2,quantile, prob=.9)
Lower_mod3param_fcast <- apply(Pred_out_mean_SWE_fcast,2,quantile, prob=.1)

## RMSE
rmse(SnotelDaymet_WY2022fcast$snow_water_equivalent, MeanPred_mod3param_fcast)

## process error
set.seed(802)
for (p in 1:length(model3_pars$b0)){
  swe_init <- SnotelDaymet_WY2022fcast$snow_water_equivalent[1]
  for(t in 1:nrow(SnotelDaymet_WY2022fcast)){
    swe_val <- rnorm(1, mean = model3_pars$b0[p] + model3_pars$b1[p] * SnotelDaymet_WY2022fcast$elev[t] + model3_pars$b2[p] * SnotelDaymet_WY2022fcast$prevday_cumulative_precip[t] + model3_pars$b3[p] * SnotelDaymet_WY2022fcast$prevday_maxtemp[t] + model3_pars$b4[p] * SnotelDaymet_WY2022fcast$prevday_mintemp[t] + model3_pars$b5[p] * SnotelDaymet_WY2022fcast$prevday_meantemp[t] + model3_pars$b6[p] * SnotelDaymet_WY2022fcast$prevday_precip[t] + model3_pars$b7[p] * SnotelDaymet_WY2022fcast$latitude[t] + model3_pars$b8[p] * SnotelDaymet_WY2022fcast$prevday_swe[t], sd = model3_pars$sigma[p])
    Pred_out_SWE_fcast[p,t] <- swe_val
  }
}




## generating forecasts
MeanPred_mod3_fcast <- apply(Pred_out_SWE_fcast,2,mean)
Upper_mod3_fcast <- apply(Pred_out_SWE_fcast,2,quantile, prob=.9)
Lower_mod3_fcast <- apply(Pred_out_SWE_fcast,2,quantile, prob=.1)

## plotting forecasts against data
plot(MeanPred_mod3_fcast, type='l', ylim = c(min(min(Lower_mod3_fcast), min(SnotelDaymet_WY2022fcast$snow_water_equivalent)),max(max(Upper_mod3_fcast), max(SnotelDaymet_WY2022fcast$snow_water_equivalent))), main = "Forecasting SWE from Sept 2021 - May 2022", ylab = "SWE (mm)")
lines(Upper_mod3_fcast,lty=2)
lines(Lower_mod3_fcast,lty=2)
points(SnotelDaymet_WY2022fcast$snow_water_equivalent,col='steelblue')

## checking coverage
count = 0
for (i in 1:nrow(SnotelDaymet_WY2022fcast)){
  if (SnotelDaymet_WY2022fcast[i,"snow_water_equivalent"] < Upper_mod3_fcast[i] & SnotelDaymet_WY2022fcast[i, "snow_water_equivalent"] > Lower_mod3_fcast[i]){
    count <- count + 1
  } else{
    count <- count
  }
  coverage_mod3_fcast <- count / nrow(SnotelDaymet_WY2022fcast)
}
coverage_mod3_fcast

## RMSE
rmse(SnotelDaymet_WY2022fcast$snow_water_equivalent, MeanPred_mod3_fcast)


## plotting forecasts
StanModel3_FcastPlot <- ggplot(data = SnotelDaymet_WY2022fcast, aes(x = date)) +
  theme_bw() +
  geom_line(aes(y = MeanPred_mod3_fcast), colour = 'dodgerblue2', linewidth = 1) +
  geom_line(aes(y = Upper_mod3_fcast), linetype = 2, colour = 'black', linewidth = 1) +
  geom_line(aes(y = Lower_mod3_fcast), linetype = 2, colour = 'black', linewidth = 1) +
  geom_point(y = SnotelDaymet_WY2022fcast$snow_water_equivalent, colour = 'red') +
  xlab("Date") +
  ylab("SWE (mm)") +
  #scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016,2017,2018)) +
  ggtitle("Forecasting SWE Using Autoregressive Model and Normal Distribution", subtitle = "Covariates: Previous SWE, Cumulative Precip, Max Temp, Mean Temp, Min Temp, Latitude, Elevation") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(size = 7, hjust = 0.5), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12))
StanModel3_FcastPlot
ggsave(here("Project", "Outputs", "StanModel3ARForecast.png"))




##### Making Table of Coverage and RMSE values #####
## reprinting values here for convenience
RF_v1_rmse_fcast <- rmse(SnotelDaymet_WY2022fcast$snow_water_equivalent, RangerPredictions_v1[,2])
coverage_RF_v1_fcast
RF_v1_rmse_fcast

RF_v2_rmse_fcast <- rmse(SnotelDaymet_WY2022fcast$snow_water_equivalent, RangerPredictions_v2[,2])
coverage_RF_v2_fcast
RF_v2_rmse_fcast

RF_v3_rmse_fcast <- rmse(SnotelDaymet_WY2022fcast$snow_water_equivalent, RangerPredictions_v3[,2])
coverage_RF_v3_fcast
RF_v3_rmse_fcast

Mod3RMSE_fcast <- rmse(SnotelDaymet_WY2022fcast$snow_water_equivalent, MeanPred_mod3_fcast)
coverage_mod3_fcast
Mod3RMSE_fcast

## creating dataframe of metrics
ModelTypeVar <- c("RF", "RF", "RF", "Stan", "Stan", "Stan", "Stan")
covariate_var <- c("prev SWE, prev cumulative precip, prev max temp, prev mean temp, prev min temp, prev precip, latitude, elevation", "prev SWE, prev cumulative precip, prev max temp, prev mean temp, prev min temp, elevation", "prev cumulative precip, prev max Temp, prev mean Temp, prev min Temp, Elevation", "Elevation, prev cumulative precip, prev max temp, prev min temp, prev mean temp, latitude", "Elevation, Previous SWE,  prev cumulative precipitation, prev max temp, prev min temp, prev mean temp, prev precip, latitude", "Elevation, prev SWE, prev min temp, prev mean temp, prev cumulative precip", "Elevation, Cumulative precip, prev max temp, prev min temp, prev mean temp, prev precip, latitude")
DistributionVar <- c("N/A", "N/A", "N/A", "Lognormal", "Normal", "Lognormal", "Normal")
ARVar <- c("Yes", "Yes", "No", "No", "Yes", "Yes", "No")
coverage_var <- c(coverage_RF_v1_fcast, coverage_RF_v2_fcast, coverage_RF_v3_fcast, 0.8157895, coverage_mod3_fcast, 0.4561404, 0.4868421)
RMSE_var <- c(RF_v1_rmse_fcast, RF_v2_rmse_fcast, RF_v3_rmse_fcast, 740.6208, Mod3RMSE_fcast, 7024.379, 203.5199)

ModelMetricsdf <- data.frame(ModelTypeVar, covariate_var, DistributionVar, ARVar, coverage_var, RMSE_var)

## Creating table
ModelMetricsTable <- ModelMetricsdf |>
  gt() |> # use 'gt' to make an awesome table...
  gt_theme_538() |>
  tab_header(
    title = "Models Used to Forecast SWE", # ...with this title
    )  |>  # and this subtitle
  ## tab_style(style = cell_fill("bisque"),
  ##           locations = cells_body()) |>  # add fill color to table
  fmt_number( # A column (numeric data)
    columns = c(coverage_var), # What column variable? FinalVoATop25$VoA_Rating
    decimals = 3 # With four decimal places
  ) |> 
  fmt_number( # Another column (also numeric data)
    columns = c(RMSE_var), # What column variable? FinalVoATop25$VoA_Ranking
    decimals = 3 # I want this column to have zero decimal places
  ) |>
  data_color( # Update cell colors, testing different color palettes
    columns = c(coverage_var),
    fn = scales::col_numeric( # <- bc it's numeric
      palette = brewer.pal(7, 'RdYlGn'), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = FALSE
    )
  ) |>
  data_color( # Update cell colors, testing different color palettes
    columns = c(RMSE_var),
    fn = scales::col_numeric( # <- bc it's numeric
      palette = brewer.pal(7, 'RdYlGn'), # A color scheme (gradient)
      domain = c(), # Column scale endpoints
      reverse = TRUE
    )
  ) |>  
  cols_label(ModelTypeVar = "Model Type", covariate_var = "Covariates", DistributionVar = "Distribution", ARVar = "Autoregressive", coverage_var = "Coverage", RMSE_var = "RMSE") |> # Update labels
  cols_move_to_end(columns = "RMSE_var") # |>
  # cols_hide() |>
  # tab_footnote(
  #   footnote = ""
  # )
ModelMetricsTable
ModelMetricsTable |>
  gtsave(
    "ModelMetricsTable.png", expand = 5,
    path = here("Project", "Outputs")
  )
