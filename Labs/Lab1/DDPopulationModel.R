## Lab 1 DD population model
## Griffin Shelor

## assigning values to r and K, creating time vector, creating N vector
r <- 0.2
K <- 500
time_vector <- seq(1:100)
N <- rep(0, 100)
N[1] <- 3

## for loop
for (i in 2:length(time_vector)) {
  N[i] = N[i - 1] + r * N[i - 1] * (1 - N[i - 1]/K)
}

## plotting population over time
plot(x = time_vector, y = N)


## trying a function
logistic <- function(n, R, k) {
  new_n <- n + R * n * (1 - n / k)
  return(new_n)
}

## for loop with function representing logistic growth equation
for (i in 2:length(time_vector)) {
  N[i] <- logistic(n = N[i - 1], R = r, k = K)
}
plot(x = time_vector, y = N, main = "Functionalized Model")
