//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y; //NDVI
  vector[N] x1; //NDVI
  vector[N] x2; //rain as a covariate
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real b0; // intercept
  real b1; // NDVI parameter
  real b2; // rain parameter
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  // y ~ normal(b0 + b1*y + b2*x, sigma);
  // testing use of for loop in stan script
  for (i in 1:N){
    y[i] ~ normal(b0 + b1*x1[i] + b2*x2[i], sigma);
  }
}
