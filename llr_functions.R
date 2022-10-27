### Statistical Computing - Assignment 5 
### Luis Navarro 
### Script llr_functions.R

library(plyr)

### Function LLR 
llr = function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

### Function F Hat
compute_f_hat = function(z, x, y, omega) {
  Wz = make_weight_matrix(x, z, omega)
  X = make_predictor_matrix(x)
  f_hat = c(1, z) %*% solve(t(X) %*% Wz %*% X) %*% t(X) %*% Wz %*% y
  return(f_hat)
}

#### My versions of make_weight_matrix and make_predictor_matrix
### Auxiliary Functions 
### Weight Function 
weight_function <- function(r) {
  if(abs(r) < 1) {
    w <- (1-(abs(r))^3)^3 
  } else {
    w <-0
  }
  return(w)
}

### R function 
r_function <- function(x,z,omega){
  r <- abs(x-z)/omega
  return(r)
}


### Make Weight Matrix 
make_weight_matrix <- function(x,z,omega){
  ### Define an Empty Matrix 
  size <- length(x)
  weights <- matrix(data=0, nrow = size, ncol = size)
  ### Compute Weights 
  r_vector <- r_function(x,z,omega)
  weight_diag <- laply(r_vector,weight_function)
  for(i in seq(1:size)){
    weights[i,i] = weight_diag[i]
  }
  ### Return Weights
  return(weights)
}

#### make_predictor_matrix function 
make_predictor_matrix <- function(x){
  x <- as.matrix(x)
  size <- length(x)
  ones <- as.matrix(rep(1,size))
  x_pred <- as.matrix(cbind(ones,x))
  return(x_pred)
}




