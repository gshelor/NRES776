## loading packages
library(pacman)
p_load(tidyverse, here, rstan)
here()
## reading in data
forage <- read_csv(here("ForecastChallenge", "Challenge2", "ForageData.csv"))
weather <- read_csv(here("ForecastChallenge", "Challenge2", "WeatherDataPrism.csv"))
colnames(weather) <- c("Date", "ppt_in", "tmin_degf", "tmean_degf")

## splitting up date column in weather to be separate year and month columns
weather <- weather |>
  separate(Date, into = c("Year", "Month"), sep = "-")
weather$Year <- as.numeric(weather$Year)
weather$Month <- as.numeric(weather$Month)

## summarising each year by median, min, and max of each covariate for the year
weather_yr <- weather |>
  group_by(Year) |>
  summarise(ppt_in_med = median(ppt_in),
            ppt_in_min = min(ppt_in),
            ppt_in_max = max(ppt_in),
            tmin_degf_med = median(tmin_degf),
            tmin_degf_min = min(tmin_degf),
            tmin_degf_max = max(tmin_degf),
            tmean_degf_med = median(tmean_degf),
            tmean_degf_min = min(tmean_degf),
            tmean_degf_max = max(tmean_degf))

plot(weather_yr$Year, weather_yr$ppt_in_med, type = 'l',ylim = c(0,12))
lines(weather_yr$ppt_in_min, col = 'blue')
lines(weather_yr$ppt_in_max, col = 'red')

## separating weather data by whether it is in the growing season (Oct, Nov, Dec, Jan, Feb, Mar, Apr) or not
weather_growing <- weather |>
  filter(Month >= 10 | Month <= 4)
weather_nongrowing <- weather |>
  filter(Month < 10 & Month > 4)

## summarising each year by median, min, and max of each covariate for the growing months of the year
weather_growing_agg <- weather_growing |>
  group_by(Year) |>
  summarise(ppt_in_med_grow = median(ppt_in),
            ppt_in_min_grow = min(ppt_in),
            ppt_in_max_grow = max(ppt_in),
            tmin_degf_med_grow = median(tmin_degf),
            tmin_degf_min_grow = min(tmin_degf),
            tmin_degf_max_grow = max(tmin_degf),
            tmean_degf_med_grow = median(tmean_degf),
            tmean_degf_min_grow = min(tmean_degf),
            tmean_degf_max_grow = max(tmean_degf))

## summarising each year by median, min, and max of each covariate for the non-growing months of the year
weather_nongrowing_agg <- weather_nongrowing |>
  group_by(Year) |>
  summarise(ppt_in_med_nongrow = median(ppt_in),
            ppt_in_min_nongrow = min(ppt_in),
            ppt_in_max_nongrow = max(ppt_in),
            tmin_degf_med_nongrow = median(tmin_degf),
            tmin_degf_min_nongrow = min(tmin_degf),
            tmin_degf_max_nongrow = max(tmin_degf),
            tmean_degf_med_nongrow = median(tmean_degf),
            tmean_degf_min_nongrow = min(tmean_degf),
            tmean_degf_max_nongrow = max(tmean_degf))

