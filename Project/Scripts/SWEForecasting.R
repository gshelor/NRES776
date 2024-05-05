### This script will be used for building and evaluating models of Snow Water Equivalent (SWE) at SNOTEL sites
## data to be used was cleaned in SNOTELDaymetClean.R
## script by Griffin Shelor

##### Loading Packages, setting up script to use parallel package, reading in data #####
library(pacman)
p_load(snotelr, here, tidyverse, randomForest, caret, ranger, rstan, leaflet, shinydashboard, plotly, DT, daymetr, ncdf4, parallel, isotracer, tidymodels)

## setting script to maximise use of cores
options(mc.cores = detectCores())

SnotelDaymet <- read_csv(here("Project", "Data", "SNOTEL", "SnotelDaymetClean.csv")) |>
  filter(Year >= 1990)


##### Examining correlation of covariates against dependent var (SWE) #####
cor(SnotelDaymet$latitude, SnotelDaymet$snow_water_equivalent)
cor(SnotelDaymet$elev, SnotelDaymet$snow_water_equivalent)
cor(SnotelDaymet$daymet_maxtemp, SnotelDaymet$snow_water_equivalent)
cor(SnotelDaymet$daymet_mintemp, SnotelDaymet$snow_water_equivalent)
cor(SnotelDaymet$daymet_meantemp, SnotelDaymet$snow_water_equivalent)
cor(SnotelDaymet$daymet_precip, SnotelDaymet$snow_water_equivalent)
cor(SnotelDaymet$precipitation_cumulative, SnotelDaymet$snow_water_equivalent)



##### Splitting data into testing, training, and forecast subsets #####
## 2 years of forecast data first
SnotelDaymet_WY2022fcast <- SnotelDaymet |>
  filter(date >= "2021-09-01") |>
  filter(date <= "2022-06-01")
SnotelDaymet_WY2021fcast <- SnotelDaymet |>
  filter(date >= "2020-09-01") |>
  filter(date <= "2021-06-01")
SnotelDaymet_WY2020fcast <- SnotelDaymet |>
  filter(date >= "2019-09-01") |>
  filter(date <= "2020-06-01")

## subsetting data used to build models
SnotelDaymet_ModelData <- SnotelDaymet |>
  filter(date < "2019-09-01")

## splitting mode data into testing and training subsets
set.seed(802)
ModelData_split <- initial_split(SnotelDaymet_ModelData, prop = 0.75)
SnotelModel_train <- training(ModelData_split)
SnotelModel_test <- testing(ModelData_split)


##### Random Forest Models #####
## model 1, all of the included covariates
# set.seed(802)
# ranger_mod_v1 <- ranger(formula = snow_water_equivalent ~ latitude + elev + precipitation_cumulative + daymet_maxtemp + daymet_mintemp + daymet_meantemp + daymet_precip, data = SnotelModel_train, num.trees = 10000, mtry = 3, importance = "impurity", quantreg = TRUE)
#  
# ## variable importance
# sort(importance(ranger_mod_v1))




##### Stan Models #####
## making list of data to declare what goes into stan model
model1_datalist <- list(N = nrow(SnotelModel_train), S = unique(SnotelDaymet$site_id), y = SnotelModel_train$snow_water_equivalent, x1 = SnotelModel_train$elev, x2 = SnotelModel_train$precipitation_cumulative, x3 = SnotelModel_train$daymet_maxtemp, x4 = SnotelModel_train$daymet_mintemp, x5 = SnotelModel_train$daymet_meantemp, x6 = SnotelModel_train$daymet_precip, x7 = SnotelModel_train$latitude)

## fitting stan model
set.seed(802)
options(mc.cores = parallel::detectCores())
model1_fit <- stan(file=here("Project", "Scripts", "ProjectModel1.stan"),data = model1_datalist, chains = 3, iter = 50000, warmup = 10000)
model1_fit


## Extracting Parameters
model1_pars <- rstan::extract(model1_fit, c("b0","b1", "b2", "b3", "b4", "b5", "b6", "b7", "eta", "sigma"))

## making predictions
## adding in process uncertainty
Pred_out_SWE <- matrix(NA, length(model1_pars$b0), nrow(SnotelModel_test))
Pred_out_mean_SWE <- matrix(NA, length(model1_pars$b0), nrow(SnotelModel_test))

## process error
set.seed(802)
for (p in 1:length(model1_pars$b0)){
  swe_init <- SnotelModel_test$lbs_per_acre[1]
  for(t in 1:nrow(SnotelModel_test)){
    swe_val <- rlnorm(1, meanlog = model1_pars$b0[p] + model1_pars$b1[p] * SnotelModel_test$elev[t] + model1_pars$b2[p] * SnotelModel_test$precipitation_cumulative[t] + model1_pars$b3[p] * SnotelModel_test$daymet_maxtemp[t] + model1_pars$b4[p] * SnotelModel_test$daymet_mintemp[t] + model1_pars$b5[p] * SnotelModel_test$daymet_meantemp[t] + model1_pars$b6[p] * SnotelModel_test$daymet_precip[t] + model1_pars$b7[p] * SnotelModel_test$latitude[t] + model1_pars$eta[p], sdlog = model1_pars$sigma[p])
    Pred_out_SWE[p,t] <- swe_val
  }
}


## generating forecasts
MeanPred_mod1 <- apply(Pred_out_SWE,2,mean)
Upper_mod1 <- apply(Pred_out_SWE,2,quantile, prob=.9)
Lower_mod1 <- apply(Pred_out_SWE,2,quantile, prob=.1)

## plotting forecasts against data
plot(MeanPred_mod1, type='l', ylim = c(0,4000), main = "Applying Final Model on Randomly Split Test Data, Stan Model 1", ylab = "SWE (mm)")
lines(Upper_mod1,lty=2)
lines(Lower_mod1,lty=2)
points(SnotelModel_test$snow_water_equivalent,col='steelblue')

## checking coverage
count = 0
for (i in 1:nrow(SnotelModel_test)){
  if (SnotelModel_test[i,"lbs_per_acre"] < Upper_mod1[i] & SnotelModel_test[i, "lbs_per_acre"] > Lower_mod1[i]){
    count <- count + 1
  } else{
    count <- count
  }
  coverage_mod1 <- count / nrow(SnotelModel_test)
}
coverage_mod1

## RMSE
rmse(SnotelModel_test$snow_water_equivalent, MeanPred_mod1)

