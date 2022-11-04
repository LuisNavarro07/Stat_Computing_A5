### Statistical Computing - Assignment 5 
### Luis Navarro 
### Script llr_functions.R ### Speed test 1

library(plyr)

### Function LLR 
llr_3 = function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat_3, x, y, omega)
  return(fits)
}


### Function F Hat
compute_f_hat_3 = function(z, x, y, omega) {
  ### Change this line --> Wz = make_weight_matrix(x, z, omega)
  ### For this line --> Wz = diag(make_weight_matrix(x, z, omega))
  Wz = diag(make_weight_matrix(x, z, omega))
  X = make_predictor_matrix(x)
  c <- matrix_product(x,y,Wz)
  ### Change this line --> f_hat = c(1, z) %*% solve(inner_mat(x,Wz)) %*% t(X) %*% Wz %*% y
  ### For this lines --> f_hat = c[1] + c[2]*z
  f_hat = c[1] + c[2]*z
  return(f_hat)
}

### Change to the f_hat line of code using apply 
### First thing to note is that t(X) %*% Wz %*% X is always a 2x2 matrix, where the entries of the matrix are easy to compute
matrix_product <- function(x,y,Wz){
  ### Define all the multiplications in vectorized form. In this case all should be nX1 vectors 
  objects <- list(Wz,Wz*x,Wz*x*x,Wz*y,Wz*x*y)
  sums <- as.double(lapply(objects,sum))
  inv_fac <- 1/((sums[1]*sums[3])-(sums[2]^2))
  inv_val <- sums[1:3]*inv_fac
  #
  c1 <- sums[4]*inv_val[3] - sums[5]*inv_val[2]
  c2 <- -sums[4]*inv_val[2] + sums[5]*inv_val[1]
  
  c <- c(c1,c2)
  return(c)
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




