library(rstan)
library(shinystan)
library(here)

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))##set working directory to current file 
here()

head(cars)
plot(cars$speed,cars$dist)


##### A list is needed to declare the data that goes into the model. 
##### The things listed here should match the data block
modeldata <- list(N=dim(cars)[1], y=cars$dist, x=cars$speed)

### Stan fit the model using a monte carlo algorithm. You can essentially think of this as a sophisticated guess and check.
### The end result is a vector of parameters that make up the posterior distribution.
### This code runs the stan model. It will first compile the code and then run through all of the chains sequentially
### There are a number of parameters you can adjust, chain lengths, number of chains. 
fit1 <- stan(file=here("UNR-EcoForecast-main", "Lectures", "StanExample.stan"),data=modeldata, chains=3,iter=2000, warmup=1000)
fit1

### The extract function can used to extract the parameter value draw from the chains. 
### The parameters you want to extract need to be specified. 
pars <- rstan::extract(fit1, c('b0','b1','sigma'))

# Notice how long the parameter is # of chains*(Total iterations-warmup)
length(pars$b0)

### Plots of the posterior 
par(mfrow=c(2,2))
hist(pars$b0)
hist(pars$b1)
hist(pars$sigma)


## visualizations to check convergence. Have chains converged? 
launch_shinystan(fit1)


b0 <- pars$b0
b1 <- pars$b1
y_out <- matrix(NA, nrow = nrow(b0), ncol = nrow(cars))
for (p in 1:length(pars$b0)){
  y = b0[p] + b1[p] * cars$speed
  y_out[p,] <- y
}

par(mfrow = c(1,1))
matplot(t(y_out), type = 'l')

mean_dist <- apply(y_out, 2,mean)
mean_dist
lines(mean_dist, lwd=6)


UL_dist <- apply(y_out, 2,quantile, prob = 0.975)
lines(UL_dist, lwd=6)

LL_dist <- apply(y_out, 2,quantile, prob = 0.025)
lines(LL_dist, lwd=6)