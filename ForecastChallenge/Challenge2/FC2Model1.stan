//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y;
  vector[N] x1; // ppt_in_10
  vector[N] x2; // ppt_in_11
  vector[N] x3; // ppt_in_12
  vector[N] x4; // ppt_in_13
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
  real b2;
  real b3;
  real b4;
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
  y ~ normal(b0 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6 + b7*x7 + b8*x8, sigma);
}

