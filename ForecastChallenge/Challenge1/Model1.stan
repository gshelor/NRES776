//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//



// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y; // Soil Moisture, response variable
  vector[N] y1; // Soil moisture
  vector[N] x1; // AirTempC_Max
  vector[N] x2; // AirTempC_Min
  vector[N] x3; // 
  
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real b0; // intercept
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  y ~ normal(mu, sigma);
}

