## loading packages
library(pacman)
p_load(tidyverse, here, rstan, shinystan, ranger, tidymodels, ModelMetrics)
here()
## reading in data
forage <- read_csv(here("ForecastChallenge", "Challenge2", "ForageData.csv"))
## changing name of year column in forage to align with "Forage Year" aggregation of weather covariates later on
colnames(forage) <- c("ForageYear", "lbs_per_acre")
weather <- read_csv(here("ForecastChallenge", "Challenge2", "WeatherDataPrism.csv"))
colnames(weather) <- c("Date", "ppt_in", "tmin_degf", "tmean_degf")

##### Tidying Data, Pivoting weather data, Converting to Forage Year #####
## splitting up date column in weather to be separate year and month columns
weather <- weather |>
  separate(Date, into = c("WeatherYear", "Month"), sep = "-")
weather$WeatherYear <- as.numeric(weather$WeatherYear)
weather$Month <- as.numeric(weather$Month)

## adding column to represent month of Water Year
weather <- weather |>
  mutate(WY_Month = case_when(Month == 10 ~ 1,
                              Month == 11 ~ 2,
                              Month == 12 ~ 3,
                              Month == 1 ~ 4,
                              Month == 2 ~ 5,
                              Month == 3 ~ 6,
                              Month == 4 ~ 7,
                              Month == 5 ~ 8,
                              Month == 6 ~ 9,
                              Month == 7 ~ 10,
                              Month == 8 ~ 11,
                              TRUE ~ 12), 
         WaterYear = case_when(Month < 10 ~ WeatherYear,
                               TRUE ~ WeatherYear + 1),.before = 3)

## summarising each year by median, min, and max of each covariate for the year
# weather_yr <- weather |>
#   group_by(WeatherYear) |>
#   summarise(WaterYear = WaterYear,
#             ppt_in_med = median(ppt_in),
#             ppt_in_mean = mean(ppt_in),
#             ppt_in_sum = sum(ppt_in),
#             ppt_in_min = min(ppt_in),
#             ppt_in_max = max(ppt_in),
#             tmin_degf_med = median(tmin_degf),
#             tmin_degf_mean = mean(tmin_degf),
#             tmin_degf_sum = sum(tmin_degf),
#             tmin_degf_min = min(tmin_degf),
#             tmin_degf_max = max(tmin_degf),
#             tmean_degf_med = median(tmean_degf),
#             tmean_degf_mean = mean(tmean_degf),
#             tmean_degf_sum = sum(tmean_degf),
#             tmean_degf_min = min(tmean_degf),
#             tmean_degf_max = max(tmean_degf))

# plot(weather_yr$WaterYear, weather_yr$ppt_in_med, type = 'l',ylim = c(0,12))
# lines(weather_yr$WaterYear, weather_yr$ppt_in_min, col = 'blue')
# lines(weather_yr$WaterYear, weather_yr$ppt_in_max, col = 'red')

## separating weather data by whether it is in the growing season (Oct, Nov, Dec, Jan, Feb, Mar, Apr) or not
## this is no longer necessary since I am pivoting the weather table to make each month its own column, and each row a different year, rather than a different month in a different year
# weather_growing <- weather |>
#   filter(WY_Month <= 7)
# weather_nongrowing <- weather |>
#   filter(WY_Month > 7)


## pivoting table so each month is its own column, and the weather dataframe matches the yearly alignment of the forage df
weather_pivot <- weather |>
  pivot_wider(
    id_cols = WeatherYear,
    names_from = Month,
    values_from = c(ppt_in, tmin_degf, tmean_degf)
  )

## adding columns to include january from next year, will set values in for loop later
weather_pivot <- weather_pivot |>
  mutate(ppt_in_13 = 0,
         .after = 13) |>
  mutate(tmin_degf_13 = 0,
         .after = 26) |>
  mutate(tmean_degf_13 = 0)

## adding a "Forage Year" column to represent which year the weather variables will be forecasting forage for
weather_pivot <- weather_pivot |>
  mutate(ForageYear = WeatherYear + 1, .before = 2)

## setting value of "13th month" to be January from the "Forage Year" so the most recent January can be included in forecasts for a given year, rather than just previous year's (Weather Year) data
for (i in 1:nrow(weather_pivot) - 1){
  weather_pivot$ppt_in_13[i] <- weather_pivot$ppt_in_1[i+1]
  weather_pivot$tmin_degf_13[i] <- weather_pivot$tmin_degf_1[i + 1]
  weather_pivot$tmean_degf_13[i] <- weather_pivot$tmean_degf_1[i + 1]
}

##### Combining Months with relatively high correlation into one column each, for each variable #####
## creating a sum column for precipitation because I suspect total precipitation over that time period will be meaningful in a way that summed temp wouldn't be
weather_pivot <- weather_pivot |>
  mutate(ppt_in_1013_sum = ppt_in_10 + ppt_in_11 + ppt_in_12 + ppt_in_13,
         ppt_in_1013_mean = (ppt_in_10 + ppt_in_11 + ppt_in_12 + ppt_in_13) / 4,
         ppt_in_913_sum = ppt_in_9 + ppt_in_10 + ppt_in_11 + ppt_in_12 + ppt_in_13,
         tmin_degf_1113_mean = (tmin_degf_11 + tmin_degf_12 + tmin_degf_13) / 3,
         ## adding 10-13 mean for tmin because the precipitation showed relatively decent correlation back to october, same for tmean even though those variables did not show correlation above 0.2 for as many months
         tmin_degf_1013_mean = (tmin_degf_10 + tmin_degf_11 + tmin_degf_12 + tmin_degf_13) / 4,
         ## creating tmin index colums (dividing it by 40 to get a ratio for how high or low the monthly value is above a threshold derived from the report Bob gave us in the assignment)
         tmin_degf_11_index = tmin_degf_11 / 40,
         tmin_degf_12_index = tmin_degf_12 / 40,
         tmin_degf_13_index = tmin_degf_13 / 40,
         ## only 13 showed a correlation that wasn't very low
         tmean_degf_1013_mean = (tmean_degf_10 + tmean_degf_11 + tmean_degf_12 + tmean_degf_13) / 4) |>
  mutate(tmin_degf_1113_mean_index = tmin_degf_1113_mean / 40)

## cutting off last row
weather_pivot_pre2011 <- weather_pivot |>
  filter(ForageYear < 2011)

## creating dataframe for actual forecast
forage_cast <- weather_pivot |>
  filter(ForageYear > 2010 & ForageYear <= 2018)

## list of dataframes to merge with forage
covariate_df_list <- list(forage, weather_pivot_pre2011)
forage_wx <- covariate_df_list |>
  reduce(full_join, by = "ForageYear") |>
  arrange(ForageYear) #|>
  #drop_na()

##### Plots and Correlations of Covariates vs Forage #####
## plots and cor functions with low correlation commented out
## Precipitation
# plot(forage_wx$ppt_in_13, forage_wx$lbs_per_acre)
cor(forage_wx$ppt_in_13, forage_wx$lbs_per_acre)
# plot(forage_wx$ppt_in_12, forage_wx$lbs_per_acre)
cor(forage_wx$ppt_in_12, forage_wx$lbs_per_acre)
# plot(forage_wx$ppt_in_11, forage_wx$lbs_per_acre)
cor(forage_wx$ppt_in_11, forage_wx$lbs_per_acre)
# plot(forage_wx$ppt_in_10, forage_wx$lbs_per_acre)
cor(forage_wx$ppt_in_10, forage_wx$lbs_per_acre)
# plot(forage_wx$ppt_in_9, forage_wx$lbs_per_acre)
# cor(forage_wx$ppt_in_9, forage_wx$lbs_per_acre)
# plot(forage_wx$ppt_in_8, forage_wx$lbs_per_acre)
# cor(forage_wx$ppt_in_8, forage_wx$lbs_per_acre)
# plot(forage_wx$ppt_in_7, forage_wx$lbs_per_acre)
# cor(forage_wx$ppt_in_7, forage_wx$lbs_per_acre)
# plot(forage_wx$ppt_in_6, forage_wx$lbs_per_acre)
# cor(forage_wx$ppt_in_6, forage_wx$lbs_per_acre)
# plot(forage_wx$ppt_in_5, forage_wx$lbs_per_acre)
# cor(forage_wx$ppt_in_5, forage_wx$lbs_per_acre)
# plot(forage_wx$ppt_in_4, forage_wx$lbs_per_acre)
# cor(forage_wx$ppt_in_4, forage_wx$lbs_per_acre)
# plot(forage_wx$ppt_in_3, forage_wx$lbs_per_acre)
# cor(forage_wx$ppt_in_3, forage_wx$lbs_per_acre)
# plot(forage_wx$ppt_in_2, forage_wx$lbs_per_acre)
# cor(forage_wx$ppt_in_2, forage_wx$lbs_per_acre)
# plot(forage_wx$ppt_in_1, forage_wx$lbs_per_acre)
# cor(forage_wx$ppt_in_1, forage_wx$lbs_per_acre)

## Minimum Temperature
# plot(forage_wx$tmin_degf_13, forage_wx$lbs_per_acre)
cor(forage_wx$tmin_degf_13, forage_wx$lbs_per_acre)
# plot(forage_wx$tmin_degf_12, forage_wx$lbs_per_acre)
cor(forage_wx$tmin_degf_12, forage_wx$lbs_per_acre)
# plot(forage_wx$tmin_degf_11, forage_wx$lbs_per_acre)
cor(forage_wx$tmin_degf_11, forage_wx$lbs_per_acre)
# plot(forage_wx$tmin_degf_10, forage_wx$lbs_per_acre)
# cor(forage_wx$tmin_degf_10, forage_wx$lbs_per_acre)
# plot(forage_wx$tmin_degf_9, forage_wx$lbs_per_acre)
# cor(forage_wx$tmin_degf_9, forage_wx$lbs_per_acre)
# plot(forage_wx$tmin_degf_8, forage_wx$lbs_per_acre)
# cor(forage_wx$tmin_degf_8, forage_wx$lbs_per_acre)
# plot(forage_wx$tmin_degf_7, forage_wx$lbs_per_acre)
# cor(forage_wx$tmin_degf_7, forage_wx$lbs_per_acre)
# plot(forage_wx$tmin_degf_6, forage_wx$lbs_per_acre)
# cor(forage_wx$tmin_degf_6, forage_wx$lbs_per_acre)
# plot(forage_wx$tmin_degf_5, forage_wx$lbs_per_acre)
# cor(forage_wx$tmin_degf_5, forage_wx$lbs_per_acre)
# plot(forage_wx$tmin_degf_4, forage_wx$lbs_per_acre)
# cor(forage_wx$tmin_degf_4, forage_wx$lbs_per_acre)
# plot(forage_wx$tmin_degf_3, forage_wx$lbs_per_acre)
# cor(forage_wx$tmin_degf_3, forage_wx$lbs_per_acre)
# plot(forage_wx$tmin_degf_2, forage_wx$lbs_per_acre)
# cor(forage_wx$tmin_degf_2, forage_wx$lbs_per_acre)
# plot(forage_wx$tmin_degf_1, forage_wx$lbs_per_acre)
# cor(forage_wx$tmin_degf_1, forage_wx$lbs_per_acre)

## Mean Temp
# plot(forage_wx$tmean_degf_13, forage_wx$lbs_per_acre)
cor(forage_wx$tmean_degf_13, forage_wx$lbs_per_acre)
# plot(forage_wx$tmean_degf_12, forage_wx$lbs_per_acre)
# cor(forage_wx$tmean_degf_12, forage_wx$lbs_per_acre)
# plot(forage_wx$tmean_degf_11, forage_wx$lbs_per_acre)
# cor(forage_wx$tmean_degf_11, forage_wx$lbs_per_acre)
# plot(forage_wx$tmean_degf_10, forage_wx$lbs_per_acre)
# cor(forage_wx$tmean_degf_10, forage_wx$lbs_per_acre)
# plot(forage_wx$tmean_degf_9, forage_wx$lbs_per_acre)
# cor(forage_wx$tmean_degf_9, forage_wx$lbs_per_acre)
# plot(forage_wx$tmean_degf_8, forage_wx$lbs_per_acre)
# cor(forage_wx$tmean_degf_8, forage_wx$lbs_per_acre)
# plot(forage_wx$tmean_degf_7, forage_wx$lbs_per_acre)
# cor(forage_wx$tmean_degf_7, forage_wx$lbs_per_acre)
# plot(forage_wx$tmean_degf_6, forage_wx$lbs_per_acre)
# cor(forage_wx$tmean_degf_6, forage_wx$lbs_per_acre)
# plot(forage_wx$tmean_degf_5, forage_wx$lbs_per_acre)
# cor(forage_wx$tmean_degf_5, forage_wx$lbs_per_acre)
# plot(forage_wx$tmean_degf_4, forage_wx$lbs_per_acre)
# cor(forage_wx$tmean_degf_4, forage_wx$lbs_per_acre)
# plot(forage_wx$tmean_degf_3, forage_wx$lbs_per_acre)
# cor(forage_wx$tmean_degf_3, forage_wx$lbs_per_acre)
# plot(forage_wx$tmean_degf_2, forage_wx$lbs_per_acre)
# cor(forage_wx$tmean_degf_2, forage_wx$lbs_per_acre)
# plot(forage_wx$tmean_degf_1, forage_wx$lbs_per_acre)
# cor(forage_wx$tmean_degf_1, forage_wx$lbs_per_acre)

##### Splitting Data into training and testing datasets #####
## creating random seeds for testing consistency of model evaluation
set.seed(802)
# seed1 <- sample(1:1000,1)
# seed1
seed2 <- sample(1:1000,1)
seed2
seed3 <- sample(1:1000,1)
seed3
seed4 <- sample(1:1000,1)
seed4
seed5 <- sample(1:1000,1)
seed5

## when I manually chose seeds before, I chose this one and for some reason RMSE was really good on the test dataset after?
seed6 <- 805


## main split (split 1)
set.seed(802)
data_split <- initial_split(forage_wx, prop = 2/3)
forage_train <- training(data_split)
forage_test <- testing(data_split)

## test split 2
set.seed(seed2)
test2_split <- initial_split(forage_wx, prop = 2/3)
test2_train <- training(test2_split)
test2_test <- testing(test2_split)

## test split 3
set.seed(seed3)
test3_split <- initial_split(forage_wx, prop = 2/3)
test3_train <- training(test3_split)
test3_test <- testing(test3_split)

## test split 4
set.seed(seed4)
test4_split <- initial_split(forage_wx, prop = 2/3)
test4_train <- training(test4_split)
test4_test <- testing(test4_split)

## test split 5
set.seed(seed5)
test5_split <- initial_split(forage_wx, prop = 2/3)
test5_train <- training(test5_split)
test5_test <- testing(test5_split)

## test split 6
set.seed(seed6)
test6_split <- initial_split(forage_wx, prop = 2/3)
test6_train <- training(test6_split)
test6_test <- testing(test6_split)

##### Random Forest Modeling #####
## building models
## model 1, all of the individual columns with good-ish correlation, no aggregations
# set.seed(802)
# ranger_mod_v1 <- ranger(formula = lbs_per_acre ~ ppt_in_10 + ppt_in_11 + ppt_in_12 + ppt_in_13 + tmin_degf_11 + tmin_degf_12 + tmin_degf_13 + tmean_degf_13, data = forage_train, num.trees = 10000, mtry = 3, importance = "impurity", quantreg = TRUE)
# 
# # variable importance
# sort(importance(ranger_mod_v1))
# 
# ## model 2, summed precip, individual temps
# set.seed(802)
# ranger_mod_v2 <- ranger(formula = lbs_per_acre ~ ppt_in_1013_sum + tmin_degf_11 + tmin_degf_12 + tmin_degf_13 + tmean_degf_13, data = forage_train, num.trees = 10000, mtry = 3, importance = "impurity", quantreg = TRUE)
# 
# # variable importance
# sort(importance(ranger_mod_v2))
# 
# ## model 3, summed precip, mean tmin, individual tmean
# set.seed(802)
# ranger_mod_v3 <- ranger(formula = lbs_per_acre ~ ppt_in_1013_sum + tmin_degf_1113_mean + tmean_degf_13, data = forage_train, num.trees = 10000, mtry = 3, importance = "impurity", quantreg = TRUE)
# 
# # variable importance
# sort(importance(ranger_mod_v3))
# 
# ## model 4, summed precip, mean tmin, mean tmean
# set.seed(802)
# ranger_mod_v4 <- ranger(formula = lbs_per_acre ~ ppt_in_1013_sum + tmin_degf_1113_mean + tmean_degf_1013_mean, data = forage_train, num.trees = 10000, mtry = 3, importance = "impurity", quantreg = TRUE)
# 
# # variable importance
# sort(importance(ranger_mod_v4))
# 
# ## model 5, individual precip, mean tmin, individual tmean
# set.seed(802)
# ranger_mod_v5 <- ranger(formula = lbs_per_acre ~ ppt_in_10 + ppt_in_11 + ppt_in_12 + ppt_in_13 + tmin_degf_1113_mean + tmean_degf_13, data = forage_train, num.trees = 10000, mtry = 3, importance = "impurity", quantreg = TRUE)
# # variable importance
# sort(importance(ranger_mod_v5))
# 
# ## model 6, individual precip, mean tmin, mean tmean
# set.seed(802)
# ranger_mod_v6 <- ranger(formula = lbs_per_acre ~ ppt_in_10 + ppt_in_11 + ppt_in_12 + ppt_in_13 + tmin_degf_1113_mean + tmean_degf_1013_mean, data = forage_train, num.trees = 10000, mtry = 3, importance = "impurity", quantreg = TRUE)
# # variable importance
# sort(importance(ranger_mod_v6))
# 
# ## model 7, summed precip, individual tmin, mean tmean
# set.seed(802)
# ranger_mod_v7 <- ranger(formula = lbs_per_acre ~ ppt_in_1013_sum + tmin_degf_11 + tmin_degf_12 + tmin_degf_13 + tmean_degf_1013_mean, data = forage_train, num.trees = 10000, mtry = 3, importance = "impurity", quantreg = TRUE)
# # variable importance
# sort(importance(ranger_mod_v7))

## variables which stick out as consistently being important:
# ppt sum, recent tmin months, tmin mean

##### Calling Stan Models #####

#### Stan Model 1 ####
## making list of data to declare what goes into stan model
model1_datalist <- list(N = nrow(forage_train), y = forage_train$lbs_per_acre, x1 = forage_train$ppt_in_10, x2 = forage_train$ppt_in_11, x3 = forage_train$ppt_in_12, x4 = forage_train$ppt_in_13, x5 = forage_train$tmin_degf_11, x6 = forage_train$tmin_degf_12, x7 = forage_train$tmin_degf_13, x8 = forage_train$tmean_degf_13)

## fitting stan model
set.seed(802)
options(mc.cores = parallel::detectCores())
model1_fit <- stan(file=here("ForecastChallenge", "Challenge2", "FC2Model1.stan"),data = model1_datalist, chains = 3, iter = 20000, warmup = 3000)
model1_fit


## Extracting Parameters
model1_pars <- rstan::extract(model1_fit, c("b0","b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "sigma"))

## making predictions
## adding in process uncertainty
Pred_out_forage <- matrix(NA, length(model1_pars$b0), nrow(forage_test))
Pred_out_mean_forage <- matrix(NA, length(model1_pars$b0), nrow(forage_test))

## process error
set.seed(802)
for (p in 1:length(model1_pars$b0)){
  forage_lbs <- forage_test$lbs_per_acre[1]
  for(t in 1:nrow(forage_test)){
    forage_lbs <- rnorm(1, mean = model1_pars$b0[p] + model1_pars$b1[p] * forage_test$ppt_in_10[t] + model1_pars$b2[p] * forage_test$ppt_in_11[t] + model1_pars$b3[p] * forage_test$ppt_in_12[t] + model1_pars$b4[p] * forage_test$ppt_in_13[t] + model1_pars$b5[p] * forage_test$tmin_degf_11[t] + model1_pars$b6[p] * forage_test$tmin_degf_12[t] + model1_pars$b7[p] * forage_test$tmin_degf_13[t] + model1_pars$b8[p] * forage_test$tmean_degf_13[t], sd=model1_pars$sigma[p])
    Pred_out_forage[p,t] <- forage_lbs
  }
}


## generating forecasts
MeanPred_mod1 <- apply(Pred_out_forage,2,mean)
Upper_mod1 <- apply(Pred_out_forage,2,quantile, prob=.9)
Lower_mod1 <- apply(Pred_out_forage,2,quantile, prob=.1)

## plotting forecasts against data
plot(MeanPred_mod1, type='l', ylim = c(0,5000), main = "Applying Final Model on Randomly Split Test Data, Stan Model 1", ylab = "Forage Production (lbs/acre)")
lines(Upper_mod1,lty=2)
lines(Lower_mod1,lty=2)
points(forage_test$lbs_per_acre,col='steelblue')

## checking coverage
count = 0
for (i in 1:nrow(forage_test)){
  if (forage_test[i,"lbs_per_acre"] < Upper_mod1[i] & forage_test[i, "lbs_per_acre"] > Lower_mod1[i]){
    count <- count + 1
  } else{
    count <- count
  }
  coverage_mod1 <- count / nrow(forage_test)
}
coverage_mod1

## RMSE
rmse(forage_test$lbs_per_acre, MeanPred_mod1)



#### Stan Model 2 ####
## making list of data to declare what goes into stan model
# model2_datalist <- list(N = nrow(forage_train), y = forage_train$lbs_per_acre, x1 = forage_train$ppt_in_1013_sum, x5 = forage_train$tmin_degf_11, x6 = forage_train$tmin_degf_12, x7 = forage_train$tmin_degf_13, x8 = forage_train$tmean_degf_13)
# 
# ## fitting stan model
# set.seed(802)
# options(mc.cores = parallel::detectCores())
# model2_fit <- stan(file=here("ForecastChallenge", "Challenge2", "FC2Model2.stan"),data = model2_datalist, chains = 3, iter = 10000, warmup = 3000)
# model2_fit
# 
# 
# ## Extracting Parameters
# model2_pars <- rstan::extract(model2_fit, c("b0","b1", "b5", "b6", "b7", "b8", "sigma"))
# 
# ## making predictions
# ## adding in process uncertainty
# Pred_out_forage <- matrix(NA, length(model2_pars$b0), nrow(forage_test))
# Pred_out_mean_forage <- matrix(NA, length(model2_pars$b0), nrow(forage_test))
# 
# ## process error
# set.seed(802)
# for (p in 1:length(model2_pars$b0)){
#   forage_lbs <- forage_test$lbs_per_acre[1]
#   for(t in 1:nrow(forage_test)){
#     forage_lbs <- rnorm(1, mean = model2_pars$b0[p] + model2_pars$b1[p] * forage_test$ppt_in_1013_sum[t] + model2_pars$b5[p] * forage_test$tmin_degf_11[t] + model2_pars$b6[p] * forage_test$tmin_degf_12[t] + model2_pars$b7[p] * forage_test$tmin_degf_13[t] + model2_pars$b8[p] * forage_test$tmean_degf_13[t], sd=model2_pars$sigma[p])
#     Pred_out_forage[p,t] <- forage_lbs
#   }
# }
# 
# 
# ## generating forecasts
# MeanPred_mod2 <- apply(Pred_out_forage,2,mean)
# Upper_mod2 <- apply(Pred_out_forage,2,quantile, prob=.9)
# Lower_mod2 <- apply(Pred_out_forage,2,quantile, prob=.1)
# 
# ## plotting forecasts against data
# plot(MeanPred_mod2, type='l', ylim = c(0,5000), main = "Applying Final Model on Randomly Split Test Data, Stan Model 2", ylab = "Forage Production (lbs/acre)")
# lines(Upper_mod2,lty=2)
# lines(Lower_mod2,lty=2)
# points(forage_test$lbs_per_acre,col='steelblue')
# 
# ## checking coverage
# count = 0
# for (i in 1:nrow(forage_test)){
#   if (forage_test[i,"lbs_per_acre"] < Upper_mod2[i] & forage_test[i, "lbs_per_acre"] > Lower_mod2[i]){
#     count <- count + 1
#   } else{
#     count <- count
#   }
#   coverage_mod2 <- count / nrow(forage_test)
# }
# coverage_mod2
# 
# ## RMSE
# rmse(forage_test$lbs_per_acre, MeanPred_mod2)



#### Stan Model 5 ####
## making list of data to declare what goes into stan model
model5_datalist <- list(N = nrow(forage_train), y = forage_train$lbs_per_acre, x1 = forage_train$ppt_in_1013_sum, x5 = forage_train$tmin_degf_11, x6 = forage_train$tmin_degf_12, x7 = forage_train$tmin_degf_13)

## fitting stan model
set.seed(802)
options(mc.cores = parallel::detectCores())
model5_fit <- stan(file=here("ForecastChallenge", "Challenge2", "FC2Model5.stan"),data = model5_datalist, chains = 3, iter = 10000, warmup = 3000)
model5_fit


## Extracting Parameters
model5_pars <- rstan::extract(model5_fit, c("b0","b1", "b5", "b6", "b7", "sigma"))

## making predictions
## adding in process uncertainty
Pred_out_forage <- matrix(NA, length(model5_pars$b0), nrow(forage_test))
Pred_out_mean_forage <- matrix(NA, length(model5_pars$b0), nrow(forage_test))

## process error
set.seed(802)
for (p in 1:length(model5_pars$b0)){
  forage_lbs <- forage_test$lbs_per_acre[1]
  for(t in 1:nrow(forage_test)){
    forage_lbs <- rnorm(1, mean = model5_pars$b0[p] + model5_pars$b1[p] * forage_test$ppt_in_1013_sum[t] + model5_pars$b5[p] * forage_test$tmin_degf_11[t] + model5_pars$b6[p] * forage_test$tmin_degf_12[t] + model5_pars$b7[p] * forage_test$tmin_degf_13[t], sd=model5_pars$sigma[p])
    Pred_out_forage[p,t] <- forage_lbs
  }
}


## generating forecasts
MeanPred_mod5 <- apply(Pred_out_forage,2,mean)
Upper_mod5 <- apply(Pred_out_forage,2,quantile, prob=.9)
Lower_mod5 <- apply(Pred_out_forage,2,quantile, prob=.1)

## plotting forecasts against data
plot(MeanPred_mod5, type='l', ylim = c(0,5000), main = "Applying Final Model on Randomly Split Test Data, Stan Model 5", ylab = "Forage Production (lbs/acre)")
lines(Upper_mod5,lty=2)
lines(Lower_mod5,lty=2)
points(forage_test$lbs_per_acre,col='steelblue')

## checking coverage
count = 0
for (i in 1:nrow(forage_test)){
  if (forage_test[i,"lbs_per_acre"] < Upper_mod5[i] & forage_test[i, "lbs_per_acre"] > Lower_mod5[i]){
    count <- count + 1
  } else{
    count <- count
  }
  coverage_mod5 <- count / nrow(forage_test)
}
coverage_mod5

## RMSE
rmse(forage_test$lbs_per_acre, MeanPred_mod5)




#### Stan Model 7 ####
## making list of data to declare what goes into stan model
model7_datalist <- list(N = nrow(forage_train), y = forage_train$lbs_per_acre, x1 = forage_train$ppt_in_1013_sum, x5 = forage_train$tmin_degf_1113_mean)

## fitting stan model
set.seed(802)
options(mc.cores = parallel::detectCores())
model7_fit <- stan(file=here("ForecastChallenge", "Challenge2", "FC2Model7.stan"),data = model7_datalist, chains = 3, iter = 10000, warmup = 3000)
model7_fit


## Extracting Parameters
model7_pars <- rstan::extract(model7_fit, c("b0","b1", "b5", "sigma"))

## making predictions
## adding in process uncertainty
Pred_out_forage <- matrix(NA, length(model7_pars$b0), nrow(forage_test))
Pred_out_mean_forage <- matrix(NA, length(model7_pars$b0), nrow(forage_test))

## process error
set.seed(802)
for (p in 1:length(model7_pars$b0)){
  forage_lbs <- forage_test$lbs_per_acre[1]
  for(t in 1:nrow(forage_test)){
    forage_lbs <- rnorm(1, mean = model7_pars$b0[p] + model7_pars$b1[p] * forage_test$ppt_in_1013_sum[t] + model7_pars$b5[p] * forage_test$tmin_degf_1113_mean[t], sd=model7_pars$sigma[p])
    Pred_out_forage[p,t] <- forage_lbs
  }
}


## generating forecasts
MeanPred_mod7 <- apply(Pred_out_forage,2,mean)
Upper_mod7 <- apply(Pred_out_forage,2,quantile, prob=.9)
Lower_mod7 <- apply(Pred_out_forage,2,quantile, prob=.1)

## plotting forecasts against data
plot(MeanPred_mod7, type='l', ylim = c(0,5000), main = "Applying Final Model on Randomly Split Test Data, Stan Model 7", ylab = "Forage Production (lbs/acre)")
lines(Upper_mod7,lty=2)
lines(Lower_mod7,lty=2)
points(forage_test$lbs_per_acre,col='steelblue')

## checking coverage
count = 0
for (i in 1:nrow(forage_test)){
  if (forage_test[i,"lbs_per_acre"] < Upper_mod7[i] & forage_test[i, "lbs_per_acre"] > Lower_mod7[i]){
    count <- count + 1
  } else{
    count <- count
  }
  coverage_mod7 <- count / nrow(forage_test)
}
coverage_mod7

## RMSE
rmse(forage_test$lbs_per_acre, MeanPred_mod7)


#### Stan Model 8 ####
## making list of data to declare what goes into stan model
# model8_datalist <- list(N = nrow(forage_train), y = forage_train$lbs_per_acre, x1 = forage_train$ppt_in_10, x2 = forage_train$ppt_in_11, x3 = forage_train$ppt_in_12, x4 = forage_train$ppt_in_13, x5 = forage_train$tmin_degf_1113_mean)
# 
# ## fitting stan model
# set.seed(802)
# options(mc.cores = parallel::detectCores())
# model8_fit <- stan(file=here("ForecastChallenge", "Challenge2", "FC2model8.stan"),data = model8_datalist, chains = 3, iter = 10000, warmup = 3000)
# model8_fit
# 
# 
# ## Extracting Parameters
# model8_pars <- rstan::extract(model8_fit, c("b0","b1", "b2", "b3", "b4", "b5", "sigma"))
# 
# ## making predictions
# ## adding in process uncertainty
# Pred_out_forage <- matrix(NA, length(model8_pars$b0), nrow(forage_test))
# Pred_out_mean_forage <- matrix(NA, length(model8_pars$b0), nrow(forage_test))
# 
# ## process error
# set.seed(802)
# for (p in 1:length(model8_pars$b0)){
#   forage_lbs <- forage_test$lbs_per_acre[1]
#   for(t in 1:nrow(forage_test)){
#     forage_lbs <- rnorm(1, mean = model8_pars$b0[p] + model8_pars$b1[p] * forage_test$ppt_in_10[t] + model8_pars$b2[p] * forage_test$ppt_in_11[t] + model8_pars$b3[p] * forage_test$ppt_in_12[t] + model8_pars$b4[p] * forage_test$ppt_in_13[t] + model8_pars$b5[p] * forage_test$tmin_degf_1113_mean[t], sd=model8_pars$sigma[p])
#     Pred_out_forage[p,t] <- forage_lbs
#   }
# }
# 
# 
# ## generating forecasts
# MeanPred_mod8 <- apply(Pred_out_forage,2,mean)
# Upper_mod8 <- apply(Pred_out_forage,2,quantile, prob=.9)
# Lower_mod8 <- apply(Pred_out_forage,2,quantile, prob=.1)
# 
# ## plotting forecasts against data
# plot(MeanPred_mod8, type='l', ylim = c(0,5000), main = "Applying Final Model on Randomly Split Test Data, Stan Model 8", ylab = "Forage Production (lbs/acre)")
# lines(Upper_mod8,lty=2)
# lines(Lower_mod8,lty=2)
# points(forage_test$lbs_per_acre,col='steelblue')
# 
# ## checking coverage
# count = 0
# for (i in 1:nrow(forage_test)){
#   if (forage_test[i,"lbs_per_acre"] < Upper_mod8[i] & forage_test[i, "lbs_per_acre"] > Lower_mod8[i]){
#     count <- count + 1
#   } else{
#     count <- count
#   }
#   coverage_mod8 <- count / nrow(forage_test)
# }
# coverage_mod8
# 
# ## RMSE
# rmse(forage_test$lbs_per_acre, MeanPred_mod8)

#### Stan Model 9 ####
## making list of data to declare what goes into stan model
# model9_datalist <- list(N = nrow(forage_train), y = forage_train$lbs_per_acre, x1 = forage_train$ppt_in_1013_sum, x5 = forage_train$tmin_degf_11_index, x6 = forage_train$tmin_degf_12_index, x7 = forage_train$tmin_degf_13_index)
# 
# ## fitting stan model
# set.seed(802)
# options(mc.cores = parallel::detectCores())
# model9_fit <- stan(file=here("ForecastChallenge", "Challenge2", "FC2Model9.stan"),data = model9_datalist, chains = 3, iter = 10000, warmup = 3000)
# model9_fit
# 
# 
# ## Extracting Parameters
# model9_pars <- rstan::extract(model9_fit, c("b0","b1", "b5", "b6", "b7", "sigma"))
# 
# ## making predictions
# ## adding in process uncertainty
# Pred_out_forage <- matrix(NA, length(model9_pars$b0), nrow(forage_test))
# Pred_out_mean_forage <- matrix(NA, length(model9_pars$b0), nrow(forage_test))
# 
# ## process error
# set.seed(802)
# for (p in 1:length(model9_pars$b0)){
#   forage_lbs <- forage_test$lbs_per_acre[1]
#   for(t in 1:nrow(forage_test)){
#     forage_lbs <- rnorm(1, mean = model9_pars$b0[p] + model9_pars$b1[p] * forage_test$ppt_in_1013_sum[t] + model9_pars$b5[p] * forage_test$tmin_degf_11_index[t] + model9_pars$b6[p] * forage_test$tmin_degf_12_index[t] + model9_pars$b7[p] * forage_test$tmin_degf_13_index[t], sd=model9_pars$sigma[p])
#     Pred_out_forage[p,t] <- forage_lbs
#   }
# }
# 
# 
# ## generating forecasts
# MeanPred_mod9 <- apply(Pred_out_forage,2,mean)
# Upper_mod9 <- apply(Pred_out_forage,2,quantile, prob=.9)
# Lower_mod9 <- apply(Pred_out_forage,2,quantile, prob=.1)
# 
# ## plotting forecasts against data
# plot(MeanPred_mod9, type='l', ylim = c(0,5000), main = "Applying Final Model on Randomly Split Test Data, Stan Model 9", ylab = "Forage Production (lbs/acre)")
# lines(Upper_mod9,lty=2)
# lines(Lower_mod9,lty=2)
# points(forage_test$lbs_per_acre,col='steelblue')
# 
# ## checking coverage
# count = 0
# for (i in 1:nrow(forage_test)){
#   if (forage_test[i,"lbs_per_acre"] < Upper_mod9[i] & forage_test[i, "lbs_per_acre"] > Lower_mod9[i]){
#     count <- count + 1
#   } else{
#     count <- count
#   }
#   coverage_mod9 <- count / nrow(forage_test)
# }
# coverage_mod9
# 
# ## RMSE
# rmse(forage_test$lbs_per_acre, MeanPred_mod9)




#### Stan Model 10 ####
## making list of data to declare what goes into stan model
# model10_datalist <- list(N = nrow(forage_train), y = forage_train$lbs_per_acre, x1 = forage_train$ppt_in_1013_sum, x5 = forage_train$tmin_degf_1113_mean_index)
# 
# ## fitting stan model
# set.seed(802)
# options(mc.cores = parallel::detectCores())
# model10_fit <- stan(file=here("ForecastChallenge", "Challenge2", "FC2Model10.stan"),data = model10_datalist, chains = 3, iter = 10000, warmup = 3000)
# model10_fit
# 
# 
# ## Extracting Parameters
# model10_pars <- rstan::extract(model10_fit, c("b0","b1", "b5", "sigma"))
# 
# ## making predictions
# ## adding in process uncertainty
# Pred_out_forage <- matrix(NA, length(model10_pars$b0), nrow(forage_test))
# Pred_out_mean_forage <- matrix(NA, length(model10_pars$b0), nrow(forage_test))
# 
# ## process error
# set.seed(802)
# for (p in 1:length(model10_pars$b0)){
#   forage_lbs <- forage_test$lbs_per_acre[1]
#   for(t in 1:nrow(forage_test)){
#     forage_lbs <- rnorm(1, mean = model10_pars$b0[p] + model10_pars$b1[p] * forage_test$ppt_in_1013_sum[t] + model10_pars$b5[p] * forage_test$tmin_degf_1113_mean_index[t], sd=model10_pars$sigma[p])
#     Pred_out_forage[p,t] <- forage_lbs
#   }
# }
# 
# 
# ## generating forecasts
# MeanPred_mod10 <- apply(Pred_out_forage,2,mean)
# Upper_mod10 <- apply(Pred_out_forage,2,quantile, prob=.9)
# Lower_mod10 <- apply(Pred_out_forage,2,quantile, prob=.1)
# 
# ## plotting forecasts against data
# plot(MeanPred_mod10, type='l', ylim = c(0,5000), main = "Applying Final Model on Randomly Split Test Data, Stan Model 10", ylab = "Forage Production (lbs/acre)")
# lines(Upper_mod10,lty=2)
# lines(Lower_mod10,lty=2)
# points(forage_test$lbs_per_acre,col='steelblue')
# 
# ## checking coverage
# count = 0
# for (i in 1:nrow(forage_test)){
#   if (forage_test[i,"lbs_per_acre"] < Upper_mod10[i] & forage_test[i, "lbs_per_acre"] > Lower_mod10[i]){
#     count <- count + 1
#   } else{
#     count <- count
#   }
#   coverage_mod10 <- count / nrow(forage_test)
# }
# coverage_mod10
# 
# ## RMSE
# rmse(forage_test$lbs_per_acre, MeanPred_mod10)


#### Stan models 11-14 (rerunning model 5 on different splits) ####
## model 11
## making list of data to declare what goes into stan model
# model11_datalist <- list(N = nrow(test2_train), y = test2_train$lbs_per_acre, x1 = test2_train$ppt_in_1013_sum, x5 = test2_train$tmin_degf_11, x6 = test2_train$tmin_degf_12, x7 = test2_train$tmin_degf_13)
# 
# ## fitting stan model
# set.seed(802)
# options(mc.cores = parallel::detectCores())
# model11_fit <- stan(file=here("ForecastChallenge", "Challenge2", "FC2Model5.stan"),data = model11_datalist, chains = 3, iter = 10000, warmup = 3000)
# model11_fit
# 
# 
# ## Extracting Parameters
# model11_pars <- rstan::extract(model11_fit, c("b0","b1", "b5", "b6", "b7", "sigma"))
# 
# ## making predictions
# ## adding in process uncertainty
# Pred_out_forage <- matrix(NA, length(model11_pars$b0), nrow(test2_test))
# Pred_out_mean_forage <- matrix(NA, length(model11_pars$b0), nrow(test2_test))
# 
# ## process error
# set.seed(802)
# for (p in 1:length(model11_pars$b0)){
#   forage_lbs <- test2_test$lbs_per_acre[1]
#   for(t in 1:nrow(test2_test)){
#     forage_lbs <- rnorm(1, mean = model11_pars$b0[p] + model11_pars$b1[p] * test2_test$ppt_in_1013_sum[t] + model11_pars$b5[p] * test2_test$tmin_degf_11[t] + model11_pars$b6[p] * test2_test$tmin_degf_12[t] + model11_pars$b7[p] * test2_test$tmin_degf_13[t], sd=model11_pars$sigma[p])
#     Pred_out_forage[p,t] <- forage_lbs
#   }
# }
# 
# 
# ## generating forecasts
# MeanPred_mod11 <- apply(Pred_out_forage,2,mean)
# Upper_mod11 <- apply(Pred_out_forage,2,quantile, prob=.9)
# Lower_mod11 <- apply(Pred_out_forage,2,quantile, prob=.1)
# 
# ## plotting forecasts against data
# plot(MeanPred_mod11, type='l', ylim = c(0,5000), main = "Applying Final Model on Randomly Split Test Data, Stan Model 11", ylab = "Forage Production (lbs/acre)")
# lines(Upper_mod11,lty=2)
# lines(Lower_mod11,lty=2)
# points(test2_test$lbs_per_acre,col='steelblue')
# 
# ## checking coverage
# count = 0
# for (i in 1:nrow(test2_test)){
#   if (test2_test[i,"lbs_per_acre"] < Upper_mod11[i] & test2_test[i, "lbs_per_acre"] > Lower_mod11[i]){
#     count <- count + 1
#   } else{
#     count <- count
#   }
#   coverage_mod11 <- count / nrow(test2_test)
# }
# coverage_mod11
# 
# ## RMSE
# rmse(test2_test$lbs_per_acre, MeanPred_mod11)







## model 12
## making list of data to declare what goes into stan model
# model12_datalist <- list(N = nrow(test3_train), y = test3_train$lbs_per_acre, x1 = test3_train$ppt_in_1013_sum, x5 = test3_train$tmin_degf_11, x6 = test3_train$tmin_degf_12, x7 = test3_train$tmin_degf_13)
# 
# ## fitting stan model
# set.seed(802)
# options(mc.cores = parallel::detectCores())
# model12_fit <- stan(file=here("ForecastChallenge", "Challenge2", "FC2Model5.stan"),data = model12_datalist, chains = 3, iter = 10000, warmup = 3000)
# model12_fit
# 
# 
# ## Extracting Parameters
# model12_pars <- rstan::extract(model12_fit, c("b0","b1", "b5", "b6", "b7", "sigma"))
# 
# ## making predictions
# ## adding in process uncertainty
# Pred_out_forage <- matrix(NA, length(model12_pars$b0), nrow(test3_test))
# Pred_out_mean_forage <- matrix(NA, length(model12_pars$b0), nrow(test3_test))
# 
# ## process error
# set.seed(802)
# for (p in 1:length(model12_pars$b0)){
#   forage_lbs <- test3_test$lbs_per_acre[1]
#   for(t in 1:nrow(test3_test)){
#     forage_lbs <- rnorm(1, mean = model12_pars$b0[p] + model12_pars$b1[p] * test3_test$ppt_in_1013_sum[t] + model12_pars$b5[p] * test3_test$tmin_degf_11[t] + model12_pars$b6[p] * test3_test$tmin_degf_12[t] + model12_pars$b7[p] * test3_test$tmin_degf_13[t], sd=model12_pars$sigma[p])
#     Pred_out_forage[p,t] <- forage_lbs
#   }
# }
# 
# 
# ## generating forecasts
# MeanPred_mod12 <- apply(Pred_out_forage,2,mean)
# Upper_mod12 <- apply(Pred_out_forage,2,quantile, prob=.9)
# Lower_mod12 <- apply(Pred_out_forage,2,quantile, prob=.1)
# 
# ## plotting forecasts against data
# plot(MeanPred_mod12, type='l', ylim = c(0,5000), main = "Applying Final Model on Randomly Split Test Data, Stan Model 12", ylab = "Forage Production (lbs/acre)")
# lines(Upper_mod12,lty=2)
# lines(Lower_mod12,lty=2)
# points(test3_test$lbs_per_acre,col='steelblue')
# 
# ## checking coverage
# count = 0
# for (i in 1:nrow(test3_test)){
#   if (test3_test[i,"lbs_per_acre"] < Upper_mod12[i] & test3_test[i, "lbs_per_acre"] > Lower_mod12[i]){
#     count <- count + 1
#   } else{
#     count <- count
#   }
#   coverage_mod12 <- count / nrow(test3_test)
# }
# coverage_mod12
# 
# ## RMSE
# rmse(test3_test$lbs_per_acre, MeanPred_mod12)



## model 13
## making list of data to declare what goes into stan model
# model13_datalist <- list(N = nrow(test4_train), y = test4_train$lbs_per_acre, x1 = test4_train$ppt_in_1013_sum, x5 = test4_train$tmin_degf_11, x6 = test4_train$tmin_degf_12, x7 = test4_train$tmin_degf_13)
# 
# ## fitting stan model
# set.seed(802)
# options(mc.cores = parallel::detectCores())
# model13_fit <- stan(file=here("ForecastChallenge", "Challenge2", "FC2Model5.stan"),data = model13_datalist, chains = 3, iter = 10000, warmup = 3000)
# model13_fit
# 
# 
# ## Extracting Parameters
# model13_pars <- rstan::extract(model13_fit, c("b0","b1", "b5", "b6", "b7", "sigma"))
# 
# ## making predictions
# ## adding in process uncertainty
# Pred_out_forage <- matrix(NA, length(model13_pars$b0), nrow(test4_test))
# Pred_out_mean_forage <- matrix(NA, length(model13_pars$b0), nrow(test4_test))
# 
# ## process error
# set.seed(802)
# for (p in 1:length(model13_pars$b0)){
#   forage_lbs <- test4_test$lbs_per_acre[1]
#   for(t in 1:nrow(test4_test)){
#     forage_lbs <- rnorm(1, mean = model13_pars$b0[p] + model13_pars$b1[p] * test4_test$ppt_in_1013_sum[t] + model13_pars$b5[p] * test4_test$tmin_degf_11[t] + model13_pars$b6[p] * test4_test$tmin_degf_12[t] + model13_pars$b7[p] * test4_test$tmin_degf_13[t], sd=model13_pars$sigma[p])
#     Pred_out_forage[p,t] <- forage_lbs
#   }
# }
# 
# 
# ## generating forecasts
# MeanPred_mod13 <- apply(Pred_out_forage,2,mean)
# Upper_mod13 <- apply(Pred_out_forage,2,quantile, prob=.9)
# Lower_mod13 <- apply(Pred_out_forage,2,quantile, prob=.1)
# 
# ## plotting forecasts against data
# plot(MeanPred_mod13, type='l', ylim = c(0,5000), main = "Applying Final Model on Randomly Split Test Data, Stan Model 13", ylab = "Forage Production (lbs/acre)")
# lines(Upper_mod13,lty=2)
# lines(Lower_mod13,lty=2)
# points(test4_test$lbs_per_acre,col='steelblue')
# 
# ## checking coverage
# count = 0
# for (i in 1:nrow(test4_test)){
#   if (test4_test[i,"lbs_per_acre"] < Upper_mod13[i] & test4_test[i, "lbs_per_acre"] > Lower_mod13[i]){
#     count <- count + 1
#   } else{
#     count <- count
#   }
#   coverage_mod13 <- count / nrow(test4_test)
# }
# coverage_mod13
# 
# ## RMSE
# rmse(test4_test$lbs_per_acre, MeanPred_mod13)



## model 14
## making list of data to declare what goes into stan model
# model14_datalist <- list(N = nrow(test5_train), y = test5_train$lbs_per_acre, x1 = test5_train$ppt_in_1013_sum, x5 = test5_train$tmin_degf_11, x6 = test5_train$tmin_degf_12, x7 = test5_train$tmin_degf_13)
# 
# ## fitting stan model
# set.seed(802)
# options(mc.cores = parallel::detectCores())
# model14_fit <- stan(file=here("ForecastChallenge", "Challenge2", "FC2Model5.stan"),data = model14_datalist, chains = 3, iter = 10000, warmup = 3000)
# model14_fit
# 
# 
# ## Extracting Parameters
# model14_pars <- rstan::extract(model14_fit, c("b0","b1", "b5", "b6", "b7", "sigma"))
# 
# ## making predictions
# ## adding in process uncertainty
# Pred_out_forage <- matrix(NA, length(model14_pars$b0), nrow(test5_test))
# Pred_out_mean_forage <- matrix(NA, length(model14_pars$b0), nrow(test5_test))
# 
# ## process error
# set.seed(802)
# for (p in 1:length(model14_pars$b0)){
#   forage_lbs <- test5_test$lbs_per_acre[1]
#   for(t in 1:nrow(test5_test)){
#     forage_lbs <- rnorm(1, mean = model14_pars$b0[p] + model14_pars$b1[p] * test5_test$ppt_in_1013_sum[t] + model14_pars$b5[p] * test5_test$tmin_degf_11[t] + model14_pars$b6[p] * test5_test$tmin_degf_12[t] + model14_pars$b7[p] * test5_test$tmin_degf_13[t], sd=model14_pars$sigma[p])
#     Pred_out_forage[p,t] <- forage_lbs
#   }
# }
# 
# 
# ## generating forecasts
# MeanPred_mod14 <- apply(Pred_out_forage,2,mean)
# Upper_mod14 <- apply(Pred_out_forage,2,quantile, prob=.9)
# Lower_mod14 <- apply(Pred_out_forage,2,quantile, prob=.1)
# 
# ## plotting forecasts against data
# plot(MeanPred_mod14, type='l', ylim = c(0,5000), main = "Applying Final Model on Randomly Split Test Data, Stan Model 14", ylab = "Forage Production (lbs/acre)")
# lines(Upper_mod14,lty=2)
# lines(Lower_mod14,lty=2)
# points(test5_test$lbs_per_acre,col='steelblue')
# 
# ## checking coverage
# count = 0
# for (i in 1:nrow(test5_test)){
#   if (test5_test[i,"lbs_per_acre"] < Upper_mod14[i] & test5_test[i, "lbs_per_acre"] > Lower_mod14[i]){
#     count <- count + 1
#   } else{
#     count <- count
#   }
#   coverage_mod14 <- count / nrow(test5_test)
# }
# coverage_mod14
# 
# ## RMSE
# rmse(test5_test$lbs_per_acre, MeanPred_mod14)



#### Stan Model 19 ####
## making list of data to declare what goes into stan model
model19_datalist <- list(N = nrow(test6_train), y = test6_train$lbs_per_acre, x1 = test6_train$ppt_in_1013_sum, x5 = test6_train$tmin_degf_11, x6 = test6_train$tmin_degf_12, x7 = test6_train$tmin_degf_13)

## fitting stan model
set.seed(802)
options(mc.cores = parallel::detectCores())
model19_fit <- stan(file=here("ForecastChallenge", "Challenge2", "FC2Model5.stan"),data = model19_datalist, chains = 3, iter = 10000, warmup = 3000)
model19_fit


## Extracting Parameters
model19_pars <- rstan::extract(model19_fit, c("b0","b1", "b5", "b6", "b7", "sigma"))

## making predictions
## adding in process uncertainty
Pred_out_forage <- matrix(NA, length(model19_pars$b0), nrow(test6_test))
Pred_out_mean_forage <- matrix(NA, length(model19_pars$b0), nrow(test6_test))

## process error
set.seed(802)
for (p in 1:length(model19_pars$b0)){
  forage_lbs <- test6_test$lbs_per_acre[1]
  for(t in 1:nrow(test6_test)){
    forage_lbs <- rnorm(1, mean = model19_pars$b0[p] + model19_pars$b1[p] * test6_test$ppt_in_1013_sum[t] + model19_pars$b5[p] * test6_test$tmin_degf_11[t] + model19_pars$b6[p] * test6_test$tmin_degf_12[t] + model19_pars$b7[p] * test6_test$tmin_degf_13[t], sd=model19_pars$sigma[p])
    Pred_out_forage[p,t] <- forage_lbs
  }
}


## generating forecasts
MeanPred_mod19 <- apply(Pred_out_forage,2,mean)
Upper_mod19 <- apply(Pred_out_forage,2,quantile, prob=.9)
Lower_mod19 <- apply(Pred_out_forage,2,quantile, prob=.1)

## comparing model 5 and model 19, looking at what plot looks like with the widest credible interval
Upper_mod5_19_max <- c()
Lower_mod5_19_min <- c()
for (x in 1:length(Upper_mod5)){
  Upper_mod5_19_max[x] <- max(Upper_mod5[x], Upper_mod19[x])
  Lower_mod5_19_min[x] <- min(Lower_mod5[x], Lower_mod19[x])
}

## plotting forecasts against data, trying to compare model 5 and model 19 (same models but run on different splits)
plot(MeanPred_mod19, type='l', ylim = c(0,5000), main = "Applying Final Model on Randomly Split Test Data", sub = "Stan Model 19 plus lines for Mod5 (different splits on each model, red = mod5, darkgreen = max of upper & min of lower)", ylab = "Forage Production (lbs/acre)")
## 
lines(Upper_mod19,lty=2)
lines(Lower_mod19,lty=2)
lines(Upper_mod5, lty=3, col = 'red')
lines(Lower_mod5, lty=3, col = 'red')
lines(Upper_mod5_19_max, lty=4, col = 'darkgreen')
lines(Lower_mod5_19_min, lty=4, col = 'darkgreen')
points(forage_test$lbs_per_acre, col = 'red')
points(test6_test$lbs_per_acre,col='steelblue')

## plotting forecasts against data, model 5 along with max of upper_mod5 and upper_mod19 and min of lower_mod5 and lower_mod19, and points of real test data from each model's different split
plot(MeanPred_mod19, type='l', ylim = c(0,5000), main = "Applying Final Model on Randomly Split Test Data", sub = "Stan Model 19 plus lines for Mod5 (different splits on each model, red = mod5, darkgreen = max of upper & min of lower)", ylab = "Forage Production (lbs/acre)")
lines(MeanPred_mod5, col = 'red')
#lines(Upper_mod19,lty=2)
#lines(Lower_mod19,lty=2)
#lines(Upper_mod5, lty=3, col = 'red')
#lines(Lower_mod5, lty=3, col = 'red')
lines(Upper_mod5_19_max, lty=4, col = 'darkgreen')
lines(Lower_mod5_19_min, lty=4, col = 'darkgreen')
points(forage_test$lbs_per_acre, col = 'red')
points(test6_test$lbs_per_acre,col='steelblue')

## checking coverage
count = 0
for (i in 1:nrow(test6_test)){
  if (test6_test[i,"lbs_per_acre"] < Upper_mod19[i] & test6_test[i, "lbs_per_acre"] > Lower_mod19[i]){
    count <- count + 1
  } else{
    count <- count
  }
  coverage_mod19 <- count / nrow(test6_test)
}
coverage_mod19

## RMSE
rmse(test6_test$lbs_per_acre, MeanPred_mod19)

##### COMPARING MODEL 5 and MODEL 19 #####
## filtering out years in split 1's test that were in split 6's training and vice versa
`%nin%` = Negate(`%in%`)
forage_testunique <- forage_test |>
  filter(ForageYear %nin% test6_train$ForageYear)
test6_testunique <- test6_test |>
  filter(ForageYear %nin% forage_train$ForageYear)

## predicting test data for model 19 using model 5
## adding in process uncertainty
Pred_out_forage <- matrix(NA, length(model5_pars$b0), nrow(test6_testunique))
Pred_out_mean_forage <- matrix(NA, length(model5_pars$b0), nrow(test6_testunique))

## process error
set.seed(802)
for (p in 1:length(model5_pars$b0)){
  forage_lbs <- test6_testunique$lbs_per_acre[1]
  for(t in 1:nrow(test6_testunique)){
    forage_lbs <- rnorm(1, mean = model5_pars$b0[p] + model5_pars$b1[p] * test6_testunique$ppt_in_1013_sum[t] + model5_pars$b5[p] * test6_testunique$tmin_degf_11[t] + model5_pars$b6[p] * test6_testunique$tmin_degf_12[t] + model5_pars$b7[p] * test6_testunique$tmin_degf_13[t], sd=model5_pars$sigma[p])
    Pred_out_forage[p,t] <- forage_lbs
  }
}

## generating forecasts for model 19 data using model 5
MeanPred_mod5_pred19 <- apply(Pred_out_forage,2,mean)
Upper_mod5_pred19 <- apply(Pred_out_forage,2,quantile, prob=.9)
Lower_mod5_pred19 <- apply(Pred_out_forage,2,quantile, prob=.1)

## predicting test data for model 5 using model 19
## adding in process uncertainty
Pred_out_forage <- matrix(NA, length(model19_pars$b0), nrow(forage_testunique))
Pred_out_mean_forage <- matrix(NA, length(model19_pars$b0), nrow(forage_testunique))

## process error
set.seed(802)
for (p in 1:length(model19_pars$b0)){
  forage_lbs <- forage_testunique$lbs_per_acre[1]
  for(t in 1:nrow(forage_testunique)){
    forage_lbs <- rnorm(1, mean = model19_pars$b0[p] + model19_pars$b1[p] * forage_testunique$ppt_in_1013_sum[t] + model19_pars$b5[p] * forage_testunique$tmin_degf_11[t] + model19_pars$b6[p] * forage_testunique$tmin_degf_12[t] + model19_pars$b7[p] * forage_testunique$tmin_degf_13[t], sd=model19_pars$sigma[p])
    Pred_out_forage[p,t] <- forage_lbs
  }
}

## generating forecasts for model 19 data using model 5
MeanPred_mod19_pred5 <- apply(Pred_out_forage,2,mean)
Upper_mod19_pred5 <- apply(Pred_out_forage,2,quantile, prob=.9)
Lower_mod19_pred5 <- apply(Pred_out_forage,2,quantile, prob=.1)


## comparing model 5 and model 19, looking at what plot looks like with the widest credible interval
# Upper_mod5_19_max <- c()
# Lower_mod5_19_min <- c()
# for (x in 1:length(Upper_mod5)){
#   Upper_mod5_19_max[x] <- max(Upper_mod5[x], Upper_mod19[x])
#   Lower_mod5_19_min[x] <- min(Lower_mod5[x], Lower_mod19[x])
# }

## plotting forecasts against data, model 5 along with max of upper_mod5 and upper_mod19 and min of lower_mod5 and lower_mod19, and points of real test data from each model's different split
# plot(MeanPred_mod19, type='l', ylim = c(0,5000), main = "Applying Final Model on Randomly Split Test Data", sub = "Stan Model 19 plus lines for Mod5 (different splits on each model, red = mod5, darkgreen = max of upper & min of lower)", ylab = "Forage Production (lbs/acre)")
# lines(MeanPred_mod5, col = 'red')
#lines(Upper_mod19,lty=2)
#lines(Lower_mod19,lty=2)
#lines(Upper_mod5, lty=3, col = 'red')
#lines(Lower_mod5, lty=3, col = 'red')
# lines(Upper_mod5_19_max, lty=4, col = 'darkgreen')
# lines(Lower_mod5_19_min, lty=4, col = 'darkgreen')
# points(forage_test$lbs_per_acre, col = 'red')
# points(test6_test$lbs_per_acre,col='steelblue')


## plotting model 5 predictions of model 19 test data
plot(MeanPred_mod5_pred19, type='l', ylim = c(0,5000), main = "Applying Final Model on Randomly Split Test Data", sub = "Stan Model 5 prediction of Mod19 data", ylab = "Forage Production (lbs/acre)")
# lines(MeanPred_mod5, col = 'red')
# lines(Upper_mod19,lty=2)
# lines(Lower_mod19,lty=2)
# lines(Upper_mod5, lty=3, col = 'red')
# lines(Lower_mod5, lty=3, col = 'red')
lines(Upper_mod5_pred19, lty=4, col = 'darkgreen')
lines(Lower_mod5_pred19, lty=4, col = 'darkgreen')
# points(forage_test$lbs_per_acre, col = 'red')
points(test6_test$lbs_per_acre,col='steelblue')

## plotting model 19 predictions of model 5 test data
plot(MeanPred_mod19_pred5, type='l', ylim = c(0,5000), main = "Applying Final Model on Randomly Split Test Data", sub = "Stan Model 19 prediction of Mod5 data", ylab = "Forage Production (lbs/acre)")
# lines(MeanPred_mod5, col = 'red')
# lines(Upper_mod19,lty=2)
# lines(Lower_mod19,lty=2)
# lines(Upper_mod5, lty=3, col = 'red')
# lines(Lower_mod5, lty=3, col = 'red')
lines(Upper_mod19_pred5, lty=4, col = 'darkgreen')
lines(Lower_mod19_pred5, lty=4, col = 'darkgreen')
points(forage_test$lbs_per_acre, col = 'red')
# points(test6_test$lbs_per_acre,col='steelblue')

## RMSE
mod5_pred19_rmse <- rmse(test6_testunique$lbs_per_acre, MeanPred_mod5_pred19)
mod5_pred19_rmse
mod19_pred5_rmse <- rmse(forage_testunique$lbs_per_acre, MeanPred_mod19_pred5)
mod19_pred5_rmse

#### Stan Model 20 ####
## making list of data to declare what goes into stan model
# model20_datalist <- list(N = nrow(test6_train), y = test6_train$lbs_per_acre, x1 = test6_train$ppt_in_1013_sum, x5 = test6_train$tmin_degf_1113_mean)
# 
# ## fitting stan model
# set.seed(802)
# options(mc.cores = parallel::detectCores())
# model20_fit <- stan(file=here("ForecastChallenge", "Challenge2", "FC2Model7.stan"),data = model20_datalist, chains = 3, iter = 10000, warmup = 3000)
# model20_fit
# 
# 
# ## Extracting Parameters
# model20_pars <- rstan::extract(model20_fit, c("b0","b1", "b5", "sigma"))
# 
# ## making predictions
# ## adding in process uncertainty
# Pred_out_forage <- matrix(NA, length(model20_pars$b0), nrow(test6_test))
# Pred_out_mean_forage <- matrix(NA, length(model20_pars$b0), nrow(test6_test))
# 
# ## process error
# set.seed(802)
# for (p in 1:length(model20_pars$b0)){
#   forage_lbs <- test6_test$lbs_per_acre[1]
#   for(t in 1:nrow(test6_test)){
#     forage_lbs <- rnorm(1, mean = model20_pars$b0[p] + model20_pars$b1[p] * test6_test$ppt_in_1013_sum[t] + model20_pars$b5[p] * test6_test$tmin_degf_1113_mean[t], sd=model20_pars$sigma[p])
#     Pred_out_forage[p,t] <- forage_lbs
#   }
# }
# 
# 
# ## generating forecasts
# MeanPred_mod20 <- apply(Pred_out_forage,2,mean)
# Upper_mod20 <- apply(Pred_out_forage,2,quantile, prob=.9)
# Lower_mod20 <- apply(Pred_out_forage,2,quantile, prob=.1)
# 
# ## plotting forecasts against data
# plot(MeanPred_mod20, type='l', ylim = c(0,5000), main = "Applying Final Model on Randomly Split Test Data, Stan Model 20", ylab = "Forage Production (lbs/acre)")
# lines(Upper_mod20,lty=2)
# lines(Lower_mod20,lty=2)
# points(test6_test$lbs_per_acre,col='steelblue')
# 
# ## checking coverage
# count = 0
# for (i in 1:nrow(test6_test)){
#   if (test6_test[i,"lbs_per_acre"] < Upper_mod20[i] & test6_test[i, "lbs_per_acre"] > Lower_mod20[i]){
#     count <- count + 1
#   } else{
#     count <- count
#   }
#   coverage_mod20 <- count / nrow(test6_test)
# }
# coverage_mod20
# 
# ## RMSE
# rmse(test6_test$lbs_per_acre, MeanPred_mod20)



##### coverage and RMSE stats #####
coverage_mod1
# coverage_mod2
# coverage_mod3
# coverage_mod4
coverage_mod5
# coverage_mod6
coverage_mod7
# coverage_mod8
# coverage_mod9
# coverage_mod10
# coverage_mod11
# coverage_mod12
# coverage_mod13
# coverage_mod14
# coverage_mod15
# coverage_mod16
# coverage_mod17
# coverage_mod18
coverage_mod19
# coverage_mod20

rmse(forage_test$lbs_per_acre, MeanPred_mod1)
# rmse(forage_test$lbs_per_acre, MeanPred_mod2)
# rmse(forage_test$lbs_per_acre, MeanPred_mod3)
# rmse(forage_test$lbs_per_acre, MeanPred_mod4)
rmse(forage_test$lbs_per_acre, MeanPred_mod5)
# rmse(forage_test$lbs_per_acre, MeanPred_mod6)
rmse(forage_test$lbs_per_acre, MeanPred_mod7)
# rmse(forage_test$lbs_per_acre, MeanPred_mod8)
# rmse(forage_test$lbs_per_acre, MeanPred_mod9)
# rmse(forage_test$lbs_per_acre, MeanPred_mod10)
# rmse(test2_test$lbs_per_acre, MeanPred_mod11)
# rmse(test3_test$lbs_per_acre, MeanPred_mod12)
# rmse(test4_test$lbs_per_acre, MeanPred_mod13)
# rmse(test5_test$lbs_per_acre, MeanPred_mod14)
# rmse(test2_test$lbs_per_acre, MeanPred_mod15)
# rmse(test2_test$lbs_per_acre, MeanPred_mod16)
# rmse(test2_test$lbs_per_acre, MeanPred_mod17)
# rmse(test2_test$lbs_per_acre, MeanPred_mod18)
rmse(test6_test$lbs_per_acre, MeanPred_mod19)
# rmse(test6_test$lbs_per_acre, MeanPred_mod20)

##### looking at similarities in variables between different splits and the weather data for the forecast period #####
# hist(forage_test$ppt_in_1013_sum)
# hist(test2_test$ppt_in_1013_sum)
# hist(test3_test$ppt_in_1013_sum)
# hist(test4_test$ppt_in_1013_sum)
# hist(test5_test$ppt_in_1013_sum)
# hist(forage_cast$ppt_in_1013_sum)
# 
# mean(forage_test$ppt_in_1013_sum)
# mean(test2_test$ppt_in_1013_sum)
# mean(test3_test$ppt_in_1013_sum)
# mean(test4_test$ppt_in_1013_sum)
# mean(test5_test$ppt_in_1013_sum)
# mean(forage_cast$ppt_in_1013_sum)
# 
# sd(forage_test$ppt_in_1013_sum)
# sd(test2_test$ppt_in_1013_sum)
# sd(test3_test$ppt_in_1013_sum)
# sd(test4_test$ppt_in_1013_sum)
# sd(test5_test$ppt_in_1013_sum)
# sd(forage_cast$ppt_in_1013_sum)

## checking ranges between min and max of different models, as well as between max and min of same index, and min between max of same index
# model 5
# max(Upper_mod5) - min(Lower_mod5)
# Lower_mod5_minindex <- which.max(Upper_mod5)
# max(Upper_mod5) - Lower_mod5[Lower_mod5_minindex]
# Upper_mod5_maxindex <- which.min(Lower_mod5)
# Upper_mod5[Upper_mod5_maxindex] - min(Lower_mod5)

# model 19
# max(Upper_mod19) - min(Lower_mod19)
# Lower_mod19_minindex <- which.max(Upper_mod19)
# max(Upper_mod19) - Lower_mod19[Lower_mod19_minindex]
# Upper_mod19_maxindex <- which.min(Lower_mod19)
# Upper_mod19[Upper_mod19_maxindex] - min(Lower_mod19)

##### Section for making real projections #####
## Model 5 identified as best model, will be official FC2 submission
## also will create forecasts for Model 19, Model 1, and Mean of Models 5 and 19
## Model 5 first
## making predictions
## adding in process uncertainty
Pred_out_forage <- matrix(NA, length(model5_pars$b0), nrow(forage_cast))
Pred_out_mean_forage <- matrix(NA, length(model5_pars$b0), nrow(forage_cast))

## process error
set.seed(802)
for (p in 1:length(model5_pars$b0)){
  forage_lbs <- forage_cast$lbs_per_acre[1]
  for(t in 1:nrow(forage_cast)){
    forage_lbs <- rnorm(1, mean = model5_pars$b0[p] + model5_pars$b1[p] * forage_cast$ppt_in_1013_sum[t] + model5_pars$b5[p] * forage_cast$tmin_degf_11[t] + model5_pars$b6[p] * forage_cast$tmin_degf_12[t] + model5_pars$b7[p] * forage_cast$tmin_degf_13[t], sd=model5_pars$sigma[p])
    Pred_out_forage[p,t] <- forage_lbs
  }
}


## generating forecasts
MeanPred_mod5_fcast <- apply(Pred_out_forage,2,mean)
Upper_mod5_fcast <- apply(Pred_out_forage,2,quantile, prob=.9)
Lower_mod5_fcast <- apply(Pred_out_forage,2,quantile, prob=.1)

## plotting forecasts against data
plot(MeanPred_mod5_fcast, type='l', ylim = c(0,5000), main = "Applying Final Model on Unknown Years", ylab = "Forage Production (lbs/acre)")
lines(Upper_mod5_fcast,lty=2)
lines(Lower_mod5_fcast,lty=2)

## Adding Prediction Columns to forage Data frame with only forecasted dates included
MelissaSydneyGriffin <- cbind(forage_cast, MeanPred_mod5_fcast)
MelissaSydneyGriffin <- cbind(MelissaSydneyGriffin, Lower_mod5_fcast)
MelissaSydneyGriffin <- cbind(MelissaSydneyGriffin, Upper_mod5_fcast)

## selecting only relevant columns
MelissaSydneyGriffin <- MelissaSydneyGriffin |>
  select(ForageYear, MeanPred_mod5_fcast, Lower_mod5_fcast, Upper_mod5_fcast)
colnames(MelissaSydneyGriffin) <- c("Year", "Forecast", "LowerPI", "UpperPI")

## plotting forecasts
Model5_FcastPlot <- ggplot(data = MelissaSydneyGriffin, aes(x = Year)) +
  theme_bw() +
  geom_line(aes(y = Forecast), colour = 'dodgerblue2', linewidth = 1) +
  geom_line(aes(y = LowerPI), linetype = 2, colour = 'dodgerblue2', linewidth = 1) +
  geom_line(aes(y = UpperPI), linetype = 2, colour = 'dodgerblue2', linewidth = 1) +
  xlab("Year") +
  ylab("Forage Production (lbs/acre)") +
  scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016,2017,2018)) +
  ggtitle("Forage Production Forecast Using Our Final Model") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12))
Model5_FcastPlot
ggsave(here("ForecastChallenge", "Challenge2", "OfficialForecast.png"))

## writing csv
write.csv(MelissaSydneyGriffin, here("ForecastChallenge", "Challenge2", "MelissaSydneyGriffin.csv"))



## Model 19
## making predictions
## adding in process uncertainty
Pred_out_forage <- matrix(NA, length(model19_pars$b0), nrow(forage_cast))
Pred_out_mean_forage <- matrix(NA, length(model19_pars$b0), nrow(forage_cast))

## process error
set.seed(802)
for (p in 1:length(model19_pars$b0)){
  forage_lbs <- forage_cast$lbs_per_acre[1]
  for(t in 1:nrow(forage_cast)){
    forage_lbs <- rnorm(1, mean = model19_pars$b0[p] + model19_pars$b1[p] * forage_cast$ppt_in_1013_sum[t] + model19_pars$b5[p] * forage_cast$tmin_degf_11[t] + model19_pars$b6[p] * forage_cast$tmin_degf_12[t] + model19_pars$b7[p] * forage_cast$tmin_degf_13[t], sd=model19_pars$sigma[p])
    Pred_out_forage[p,t] <- forage_lbs
  }
}


## generating forecasts
MeanPred_mod19_fcast <- apply(Pred_out_forage,2,mean)
Upper_mod19_fcast <- apply(Pred_out_forage,2,quantile, prob=.9)
Lower_mod19_fcast <- apply(Pred_out_forage,2,quantile, prob=.1)

## Adding Prediction Columns to forage Data frame with only forecasted dates included
CuriositySubmission1 <- cbind(forage_cast, MeanPred_mod19_fcast)
CuriositySubmission1 <- cbind(CuriositySubmission1, Lower_mod19_fcast)
CuriositySubmission1 <- cbind(CuriositySubmission1, Upper_mod19_fcast)

## selecting only relevant columns
CuriositySubmission1 <- CuriositySubmission1 |>
  select(ForageYear, MeanPred_mod19_fcast, Lower_mod19_fcast, Upper_mod19_fcast)
colnames(CuriositySubmission1) <- c("Year", "Forecast", "LowerPI", "UpperPI")

## plotting forecasts
Model19_FcastPlot <- ggplot(data = CuriositySubmission1, aes(x = Year)) +
  theme_bw() +
  geom_line(aes(y = Forecast), colour = 'dodgerblue2', linewidth = 1) +
  geom_line(aes(y = LowerPI), linetype = 2, colour = 'dodgerblue2', linewidth = 1) +
  geom_line(aes(y = UpperPI), linetype = 2, colour = 'dodgerblue2', linewidth = 1) +
  xlab("Year") +
  ylab("Forage Production (lbs/acre)") +
  scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016,2017,2018)) +
  ggtitle("Forage Production Forecast Using Model 19") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12))
Model19_FcastPlot

## writing csv
write.csv(CuriositySubmission1, here("ForecastChallenge", "Challenge2", "CuriositySubmission1.csv"))

### Mean of Model 5 and Model 19
MeanPred_mod519_meanfcast <- (MeanPred_mod5_fcast + MeanPred_mod19_fcast) / 2
Upper_mod519_meanfcast <- (Upper_mod5_fcast + Upper_mod19_fcast) / 2
Lower_mod519_meanfcast <- (Lower_mod5_fcast + Lower_mod19_fcast) / 2

## Adding Prediction Columns to forage Data frame with only forecasted dates included
CuriositySubmission2 <- cbind(forage_cast, MeanPred_mod519_meanfcast)
CuriositySubmission2 <- cbind(CuriositySubmission2, Lower_mod519_meanfcast)
CuriositySubmission2 <- cbind(CuriositySubmission2, Upper_mod519_meanfcast)

## selecting only relevant columns
CuriositySubmission2 <- CuriositySubmission2 |>
  select(ForageYear, MeanPred_mod519_meanfcast, Lower_mod519_meanfcast, Upper_mod519_meanfcast)
colnames(CuriositySubmission2) <- c("Year", "Forecast", "LowerPI", "UpperPI")

## plotting forecasts
Model519_FcastPlot <- ggplot(data = CuriositySubmission2, aes(x = Year)) +
  theme_bw() +
  geom_line(aes(y = Forecast), colour = 'dodgerblue2', linewidth = 1) +
  geom_line(aes(y = LowerPI), linetype = 2, colour = 'dodgerblue2', linewidth = 1) +
  geom_line(aes(y = UpperPI), linetype = 2, colour = 'dodgerblue2', linewidth = 1) +
  xlab("Year") +
  ylab("Forage Production (lbs/acre)") +
  scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016,2017,2018)) +
  ggtitle("Forage Production Forecast Using a Mean of Models 5 and 19") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12))
Model519_FcastPlot

## writing csv
write.csv(CuriositySubmission2, here("ForecastChallenge", "Challenge2", "CuriositySubmission2.csv"))


## Model 1
## making predictions
## adding in process uncertainty
Pred_out_forage <- matrix(NA, length(model1_pars$b0), nrow(forage_cast))
Pred_out_mean_forage <- matrix(NA, length(model1_pars$b0), nrow(forage_cast))

## process error
set.seed(802)
for (p in 1:length(model1_pars$b0)){
  forage_lbs <- forage_cast$lbs_per_acre[1]
  for(t in 1:nrow(forage_cast)){
    forage_lbs <- rnorm(1, mean = model1_pars$b0[p] + model1_pars$b1[p] * forage_cast$ppt_in_10[t] + model1_pars$b2[p] * forage_cast$ppt_in_11[t] + model1_pars$b3[p] * forage_cast$ppt_in_12[t] + model1_pars$b4[p] * forage_cast$ppt_in_13[t] + model1_pars$b5[p] * forage_cast$tmin_degf_11[t] + model1_pars$b6[p] * forage_cast$tmin_degf_12[t] + model1_pars$b7[p] * forage_cast$tmin_degf_13[t] + model1_pars$b8[p] * forage_cast$tmean_degf_13[t], sd=model1_pars$sigma[p])
    Pred_out_forage[p,t] <- forage_lbs
  }
}


## generating forecasts
MeanPred_mod1_fcast <- apply(Pred_out_forage,2,mean)
Upper_mod1_fcast <- apply(Pred_out_forage,2,quantile, prob=.9)
Lower_mod1_fcast <- apply(Pred_out_forage,2,quantile, prob=.1)

## Adding Prediction Columns to forage Data frame with only forecasted dates included
CuriositySubmission3 <- cbind(forage_cast, MeanPred_mod1_fcast)
CuriositySubmission3 <- cbind(CuriositySubmission3, Lower_mod1_fcast)
CuriositySubmission3 <- cbind(CuriositySubmission3, Upper_mod1_fcast)

## selecting only relevant columns
CuriositySubmission3 <- CuriositySubmission3 |>
  select(ForageYear, MeanPred_mod1_fcast, Lower_mod1_fcast, Upper_mod1_fcast)
colnames(CuriositySubmission3) <- c("Year", "Forecast", "LowerPI", "UpperPI")

## plotting forecasts
Model1_FcastPlot <- ggplot(data = CuriositySubmission3, aes(x = Year)) +
  theme_bw() +
  geom_line(aes(y = Forecast), colour = 'dodgerblue2', linewidth = 1) +
  geom_line(aes(y = LowerPI), linetype = 2, colour = 'dodgerblue2', linewidth = 1) +
  geom_line(aes(y = UpperPI), linetype = 2, colour = 'dodgerblue2', linewidth = 1) +
  xlab("Year") +
  ylab("Forage Production (lbs/acre)") +
  scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016,2017,2018)) +
  ggtitle("Forage Production Forecast Using Model 1") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12))
Model1_FcastPlot

## writing csv
write.csv(CuriositySubmission3, here("ForecastChallenge", "Challenge2", "CuriositySubmission3.csv"))
