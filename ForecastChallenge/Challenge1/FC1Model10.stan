//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//



// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y; // Soil Moisture, response variable
  vector[N] x4; // AirTempC_Avg
  vector[N] x5; // Solar_Incoming_Total
  vector[N] x6; // Ppt_mm_Tot
  vector[N] x14; // SoilTemp_20cm_Avg
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real b0; // intercept
  real<lower=0> sigma;
  real b4;
  real b5;
  real b6;
  real b14;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed
// and standard deviation 'sigma'.
model {
  y ~ normal(b0 + b4*x4 + b5*x5 + b6*x6 + b14*x14, sigma);
}

