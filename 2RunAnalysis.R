# Use this code to run all analysis scripts.

rm(list=ls(all=TRUE))
ls()

#------------------------------------#
# Load Packages
#------------------------------------#

library(pacman)

p_load(tidyverse, fastDummies, rstan, rmdcev, furrr)

set.seed(12345)

n_sample <- 24000
n_costs_sim <- 200
n_workers = 6
n_classes = 1

# File names
file_name_national_single = "output/mdcev_national_single.RData"
file_name_national_single_w = "output/mdcev_national_single_w.RData"
file_name_provincial_single = "output/mdcev_provincial_single.RData"

file_name_national_estimation = "output/mdcev_national_estimation.RData"
file_name_national_estimation_w = "output/mdcev_national_estimation_w.RData"
file_name_national_welfare = "output/mdcev_national_welfare.RData"
file_name_national_welfare_w = "output/mdcev_national_welfare_w.RData"
file_name_provincial_estimation = "output/mdcev_provincial_estimation.RData"
file_name_provincial_welfare = "output/mdcev_provincial_welfare.RData"
file_name_provincial_estimation_w = "output/mdcev_provincial_estimation_w.RData"
file_name_provincial_welfare_w = "output/mdcev_provincial_welfare_w.RData"

source('code/helper_modified.R')


df_mdcev <- read_csv("data/clean/AverageTravelCost.csv") %>%
	arrange(id, activity) %>%
	select(id, activity, price, quant, income, province, weight) 

df_mdcev <- as.data.frame(df_mdcev)

activity_names <- unique(df_mdcev$activity)

mdcev_formula <- formula(paste("~ ", 
							   paste(activity_names[-1], collapse=" + "), -1))

mdcev_formula <- ~ 0

# Estimate national and provincial models no simulation
start.time <- Sys.time()
source("code/EstimateMDCEV_single.R", echo=TRUE)
end.time <- Sys.time()
time.taken1 <- end.time - start.time
time.taken1

# Modelling

# Load cost simulation data
load("data/clean/TravelCostsSimulation.Rdata")

costs_sim <- costs_sim[1:n_costs_sim]

gc()
start_time <- Sys.time()
source("code/EstimateMDCEV_national.R", echo=TRUE)
end.time <- Sys.time()
time.taken2 <- end.time - start.time
time.taken2

start.time <- Sys.time()
source("code/EstimateMDCEV_provincial.R", echo=TRUE)
end.time <- Sys.time()
time.taken3 <- end.time - start.time
time.taken3


# Simulate nationral/provincial welfare


nerrs = 50
nalts <- length(activity_names)
npols <- nalts + 1

# Closing each site individual than all together

policy_names <- c(activity_names, "all")
price_p <- cbind(0,diag(nalts)*10000000)
price_p <- rbind(price_p, c(0,rep(100000000,nalts)))# add all closures to price_list

start.time <- Sys.time()
source("code/WelfareMDCEV_national.R")
end.time <- Sys.time()
time.taken4 <- end.time - start.time
time.taken4

start.time <- Sys.time()
source("code/WelfareMDCEV_provincial.R")
end.time <- Sys.time()
time.taken5 <- end.time - start.time
time.taken5

start.time <- Sys.time()
source("code/AnalyzeProvWelfare.R")
source("code/AnalyzeWelfare.R")
end.time <- Sys.time()
time.taken6 <- end.time - start.time
time.taken6


# Clean results and put in table
source("code/CreateNames.R")
source("code/CreateEstimationTables.R")
source("code/CreateWelfareTables.R")
source("code/CreateAppendixTables.R")
