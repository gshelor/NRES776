//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//



// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y; // Soil Moisture, response variable
  vector[N] x1; // Soil moisture
  vector[N] x2; // AirTempC_Max
  vector[N] x3; // AirTempC_Min
  vector[N] x4; // AirTempC_Avg
  vector[N] x5; // Solar_Incoming_Total
  vector[N] x6; // Ppt_mm_Tot
  vector[N] x7; // Relative_Humidity_Max
  vector[N] x8; // Relative_Humidity_Min
  vector[N] x9; // Wind_speed_Max
  vector[N] x10; // WndSpeed_Avg
  vector[N] x11; // WinDir_Avg
  vector[N] x12; // WinDir_Std_Dev
  vector[N] x13; // SoilTemp_5cm_Avg
  vector[N] x14; // SoilTemp_20cm_Avg
  vector[N] x15; // Dewpoint_Avg
  vector[N] x16; // SoilTemp_5cm_Max
  vector[N] x17; // SoilTemp_5cm_Min
  vector[N] x18; // SoilTemp_20cm_Max
  vector[N] x19; // SoilTemp_20cm_Min
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real b0; // intercept
  real<lower=0> sigma;
  real b1;
  real b2;
  real b3;
  real b4;
  real b5;
  real b6;
  real b7;
  real b8;
  real b9;
  real b10;
  real b11;
  real b12;
  real b13;
  real b14;
  real b15;
  real b16;
  real b17;
  real b18;
  real b19;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed
// and standard deviation 'sigma'.
model {
  y ~ normal(b0 + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + b6*x6 + b7*x7 + b8*x8 + b9*x9 + b10*x10 + b11*x11 + b12*x12 + b13*x13 + b14*x14 + b15*x15 + b16*x16 + b17*x17 + b18*x18 + b19*x19, sigma);
}

