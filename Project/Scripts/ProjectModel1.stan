//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//


// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y; // SWE values
  vector[N] x1; // previous SWE value at the site
  vector[N] x2; // elevation
  vector[N] x3; // cumulative precipitation
  vector[N] x4; // temp max
  vector[N] x5; // temp min
  vector[N] x6; // temp mean
  vector[N] x7; // daily precipitation
  vector[N] x8; // latitude
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real mu;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  y ~ normal(mu, sigma);
}

