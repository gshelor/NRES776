//
// This Stan program defines a model with a
// vector of values 'y' modeled as lognormally distributed
// with mean 'mu' and standard deviation 'sigma'.
//


// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  int sitevec; // unique site IDs
  vector[N] y; // SWE values, plus 1
  vector[N] x1; // elevation
  vector[N] x2; // cumulative precipitation
  vector[N] x3; // temp max
  vector[N] x4; // temp min
  vector[N] x5; // temp mean
  vector[N] x6; // latitude
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real b0; // intercept
  real b1; // elevation
  real b2; // cumulative precip
  real b3; // temp max
  real b4; // temp min
  real b5; // temp mean
  real b6; // latitude
  real eta; // accounting for variance between sites
  real tau; // error for site-specific variance
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  eta ~ normal(0, tau);
  y ~ lognormal(b0 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6 + eta[sitevec], sigma);
}

