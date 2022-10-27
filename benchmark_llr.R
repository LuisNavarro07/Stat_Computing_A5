### Statistical Computing - Assignment 5 
### Luis Navarro 
### Script benchmark_llr.R
rm(list = ls()) 
library(microbenchmark)

setwd("C:/Users/luise/OneDrive - Indiana University/Statistical Computing/Assignments/A5_Git")
source("llr_functions.R")
source("llr_functions_1.R")
source("llr_functions_2.R")

n = 15
## a very simple regression model
x = rnorm(n)
y = rnorm(x + rnorm(n))
z = seq(-1, 1, length.out = 100)
omega = 1

microbenchmark(
  llr(x,y,z,omega),
  llr_1(x,y,z,omega),
  llr_2(x,y,z,omega),times = 100
)