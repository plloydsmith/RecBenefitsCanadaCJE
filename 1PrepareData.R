# Use this code to run all preparation scripts.

rm(list=ls(all=TRUE))
ls()

#------------------------------------#
# Load Packages
#------------------------------------#
library(pacman)
p_load(tidyverse, mice, fastDummies, furrr, micemd)

source("data/code/1CleanData.R")

source("data/code/2CreateSocio.R")

source("data/code/3CreateTravelCosts.R")


set.seed(12345)
n_impute <- 200
maxit <- 10
n_impute2 <- 1
seed <- TRUE
n_sample <- NA # set to NA to use full sample
km_hr <- 60 
yearly_hours <- 2040
vehicle_cost <- (0.65 + 0.58 + 0.49) / 3  # https://caa.ca/docs/eng/CAA_Driving_Costs_English.pdf
opp_time_lo <- 1/3
opp_time_hi <- 1
adj_lo <- 1
adj_hi <- 1.5

# used in imputation
socio_vars <-c("income", "indig", "imigrant", "ageindex", "male", 
			   "urban", "university", "college", "cabin", "num_overnt_trips")

# Note: this imputation script takes a long time to run 
# (~48 hours on a desktop computer with 32 GB RAM)

source("data/code/4SimulateTravelCosts.R")

source("data/code/5CreateAverageTravelCosts.R")
