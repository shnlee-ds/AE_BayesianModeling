rm(list=ls())
library(microbenchmark)
library(Rcpp)

setwd("/Users/shnlee/Desktop/RA2019/R_Cpp_simulation")
sourceCpp("getL_Andrew.cpp")

# Although the "cut-off" approach enables us to run the function faster, 
# it makes a quite big difference in terms of the simulation results. (especially when y[i] is very large)
# So I added an additional term to adjust for this kind of error using below formula.
# "Integrate r / (r + x) dx, (r is given constant)" = r*(log(r+x)) + C

####### Speed comparison (r = 1.0)
set.seed(111)
y1 = rpois(1, 1000000) # Input case 1: Single (Very large) Y
y2 = rpois(1000,50000) # Input case 2: Multiple Y's


# Sepeed test with rpois(1, 1000000)
microbenchmark(
  LsumC(y1,1), # original getL
  LsumC_new(y1,1,0.0), LsumC_new(y1,1,0.001), LsumC_new(y1,1,0.005), LsumC_new(y1,1,0.01)) # new getL

# Speed test with rpois(1000,50000)
microbenchmark(
  LsumC(y2,1), # original getL
  LsumC_new(y2,1,0.0), LsumC_new(y2,1,0.001), LsumC_new(y2,1,0.005), LsumC_new(y2,1,0.01), #new getL
  times = 10)

######## Simulation result comparison
LsumC_y1 = c(); LsumC_y2 = c(); LsumC_new_y1_0.001 = c(); LsumC_new_y2_0.001 = c()
LsumC_new_y1_0.005 = c(); LsumC_new_y2_0.005 = c(); LsumC_new_y1_0.01 = c(); LsumC_new_y2_0.01 = c()

n.trials = 30
for(i in 1:n.trials){
  ##LsumC: Original version
  LsumC_y1 = c(LsumC_y1, LsumC(y1,1)) 
  LsumC_y2 = c(LsumC_y2, LsumC(y2,1))
  
  ##LsumC_new: Add an error term with the cut-off approach
  LsumC_new_y1_0.001 = c(LsumC_new_y1_0.001, LsumC_new(y1,1,0.001))
  LsumC_new_y2_0.001 = c(LsumC_new_y2_0.001, LsumC_new(y2,1,0.001))
  
  LsumC_new_y1_0.005 = c(LsumC_new_y1_0.005, LsumC_new(y1,1,0.005))
  LsumC_new_y2_0.005 = c(LsumC_new_y2_0.005, LsumC_new(y2,1,0.005))
  
  LsumC_new_y1_0.01 = c(LsumC_new_y1_0.01, LsumC_new(y1,1,0.01))
  LsumC_new_y2_0.01 = c(LsumC_new_y2_0.01, LsumC_new(y2,1,0.01))
}

mean(LsumC_y1); mean(LsumC_new_y1_0.001); mean(LsumC_new_y1_0.005); mean(LsumC_new_y1_0.01) 
mean(LsumC_y2); mean(LsumC_new_y2_0.001); mean(LsumC_new_y2_0.005); mean(LsumC_new_y2_0.01)
