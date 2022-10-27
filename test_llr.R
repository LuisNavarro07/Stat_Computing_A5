### Statistical Computing - Assignment 5 
### Luis Navarro 
### Script test_llr.R
library(tinytest)
library(testthat)
library(matrixcalc)
context("Check local linear regression function")
setwd("C:/Users/luise/OneDrive - Indiana University/Statistical Computing/Assignments/A5_Git")
source("llr_functions.R")

n = 15
## a very simple regression model
x = rnorm(n)
y = rnorm(x + rnorm(n))
z = seq(-1, 1, length.out = 100)

### I don't know why the testthat functions do not work in my computer it keeps appearing this error 
### Error: 'ansi_hyperlink_types' is not an exported object from 'namespace:cli' 
### I think it is because the cli package I have is an

### I will do the test in a simpler way  
isTRUE(length(llr(x, y, z, omega = 1)) == length(z))
expect_equal(length(llr(x, y, z, omega = 1)), length(z))
### this is the code that does not runs in my computer although I don't know why. Expect_equal also shows no output 
test_that("llr output has correct length", {
  expect_equal(length(llr(x, y, z, omega = 1)), length(z))
})

#### Matrix is Diagonal, All elements are positive, and weights are correct 
## test1: is a matrix f nxn
dim(make_weight_matrix(x,z,omega=1))
### test2: is a diagonal matrix 
is.diagonal.matrix(make_weight_matrix(x,z,omega=1))
### test 3 all elements are positive
laply(diag(make_weight_matrix(x,z,omega=1)),function(x){if(x >= 0){TRUE} else{FALSE}})
### test4: weights are correct 
### the make weight matrix function takes z and omega as fixed scalars. X is the vector. The weights are computed using the weight and r functions
### For the case when z = 0 and  omega = 1. Then W(r) = W(abs(x))
### This is how the graph should look 
### https://www.wolframalpha.com/input?i=y+%3D+%281-abs%28r%29%5E3%29%5E3+%3B+r%3D%28-1%2C1%29 
### This should show a diagonal matrix with positive weights 
make_weight_matrix(x=seq(from=-1,to=1,by=0.25),z=0,omega=1)
plot(seq(from=-1,to=1,by=0.25),diag(make_weight_matrix(x=seq(from=-1,to=1,by=0.25),z=0,omega=1)),type="l")
### Looks to be working fine, let's increase the number of obs so it gets smoother 
plot(seq(from=-1,to=1,by=0.001),diag(make_weight_matrix(x=seq(from=-1,to=1,by=0.001),z=0,omega=1)),type="l")

### tests with expect_equal 
expect_equal(is.diagonal.matrix(make_weight_matrix(x,z,omega=1)), TRUE)
expect_equal(laply(diag(make_weight_matrix(x,z,omega=1)),function(x){if(x >= 0){TRUE} else{FALSE}}),
             rep(TRUE,length(x)))


test_that("make_weight_matrix works on simple cases", {
  ## check that the output is a diagonal matrix, that all the elements are positive, that the weights are correct in simple cases where you know what the output shuold be
  expect_equal(is.diagonal.matrix(make_weight_matrix(x,z,omega=1)), TRUE)
  expect_equal(laply(diag(make_weight_matrix(x,z,omega=1)),function(x){if(x >= 0){TRUE} else{FALSE}}),
               rep(TRUE,length(x)))
})


#################################################################################

### test first column equals ones
laply(make_predictor_matrix(x)[,1] ,function(x){if(x == 1){TRUE} else{FALSE}})
expect_equal(laply(make_predictor_matrix(x)[,1] ,function(x){if(x == 1){TRUE} else{FALSE}}),
             rep(TRUE,length(x)))

test_that("make_predictor_matrix works on simple cases", {
  ## write tests to check that the dimensions are correct, the first column is all 1's, etc.
  expect_equal(laply(make_predictor_matrix(x)[,1] ,function(x){if(x == 1){TRUE} else{FALSE}}),
               rep(TRUE,length(x)))
})

##############################################################################

library(reshape2)
library(reshape)
#### Evaluate the Function 
data(french_fries)
french_fries = french_fries[complete.cases(french_fries),]
z = seq(0, 15, length.out = 100)
fits = llr(z = z, x = french_fries$potato, y = french_fries$buttery, omega = 1)
plot(z, fits, type ="l")


### Different Values of Omega 

