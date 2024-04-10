//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y; // forage (lbs/acre)
  vector[N] x1; // ppt_in_1013_sum
  vector[N] x5; // tmin_degf_11
  vector[N] x6; // tmin_degf_12
  vector[N] x7; // tmin_degf_13
  vector[N] x8; // tmean_degf_13
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real b0;
  real b1;
  real b5;
  real b6;
  real b7;
  real b8;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  y ~ normal(b0 + b1*x1 + b5*x5 + b6*x6 + b7*x7 + b8*x8, sigma);
}

