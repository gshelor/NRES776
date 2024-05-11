//
// This Stan program defines a model with a
// vector of values 'y' modeled as lognormally distributed
// with mean 'mu' and standard deviation 'sigma'.
//


// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y; // SWE values
  vector[N] x1; // elevation
  vector[N] x2; // cumulative precipitation
  vector[N] x3; // temp min
  vector[N] x4; // temp mean
}


// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real b0; // intercept
  real b1; // elevation
  real b2; // cumulative precip
  real b3; // temp min
  real b4; // temp mean
  //real mu; // product of deterministic function, used in moment matching to determine shape and rate
  real <lower=0> shape; // parameter in gamma dist
  real <lower=0> rate; // parameter in gamma dist
  real<lower=0> sigma;
}


// The model to be estimated.
model {
  sigma ~ normal(0,100);
  b0 ~ normal(0,10);
  b1 ~ normal(0,10);
  b2 ~ normal(0,10);
  b3 ~ normal(0,10);
  b4 ~ normal(0,10);
  mu = b0 + b1*x1 + b2*x2 + b3*x3 + b4*x4;
  shape = mu^2 / sigma^2;
  rate = mu / sigma^2;
  y ~ gamma(shape, rate);
}

