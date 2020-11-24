# Filename:           SimulateTravelCosts.R
# Created:            01/26/2016
# Description: Import travel cost information and impute travel costs
#
# NOTES:
#
#-----------------------------------------------------------------------------

library(randtoolbox)



halton_draws <- halton(n_impute, dim = 2)

opp_time_list <- as.list(opp_time_lo + halton_draws[,2]*(opp_time_hi - opp_time_lo))

adj_list <- as.list(adj_lo + halton_draws[,1]*(adj_hi - adj_lo))

# Bring in helper functions
source("data/code/helperfunctions.R")

#-----------------------------------#
# Import data
#------------------------------------#
all_nontravel <- read_csv("data/clean/NonTravelCosts.csv") %>%
	select(-count) %>%
	spread(type, costs)


df_socio <- read_csv("data/clean/SocioImpute.csv") %>%
	arrange(id)

df_days <- read_csv("data/clean/DaysClean.csv")

df_data <- read_csv("data/clean/DataBeforeImputation.csv")  %>%
	arrange(id) %>% 
	left_join(df_socio, by = "id") %>%
	mutate(income = income / 100000) %>%
	left_join(df_days, by = c("id", "activity", "days"))

df_n_people <- df_data %>%
	select(id, activity, n_people)



if(!is.na(n_sample)){
	df_data <- df_data %>%
		distinct(id) %>%
		sample_n(., n_sample ) %>%
		left_join(df_data, by = "id") 
}

summary(df_data)
df_costs_trans <- df_data %>%
	select(id, activity, cost_trans, cost_accom)

# Prepare data for imputation for non-zero days
impute.data <- df_data  %>%
	filter(days > 0) %>%
	select(id, province, activity, cost_trans, cost_accom, day_close, day_far, all_of(socio_vars)) %>%
	rename(cost_trans_impute = cost_trans,
		   cost_accom_impute = cost_accom)

#impute.data <- dummy_cols(impute.data, select_columns = c("province"))

impute.data <- impute.data %>%
	group_split(activity)

start_time_nonzero <- Sys.time()

df_impute_nonzeros <- vector("list", length = length(impute.data))

for(i in 1:length(impute.data)){
	start_time <- Sys.time()
	
	ini <- mice(impute.data[[i]], maxit=0, print=F)
	pred <- ini$pred
	pred[, "id"] <- 0
	
	# Impute Multivariate Imputation by Chained Equations
	tempData <- mice.par(impute.data[[i]], pred = pred, m = n_impute, 
						 maxit=maxit, meth='pmm',seed=seed, nnodes = 6)
	
	
	df_impute_nonzeros[[i]] <- complete(tempData,"long") %>%
	select(id, province, activity, cost_trans_impute, cost_accom_impute, .imp, all_of(socio_vars)) %>%
	rename(imputation = .imp) %>%
	group_split(imputation)
	end_time <- Sys.time()
	
	cat(paste(i,": ",round(end_time - start_time,1), "  ")); flush.console()
}
end_time_nonzero <- Sys.time()
sim_time_nonzero <- end_time_nonzero - start_time_nonzero
sim_time_nonzero

# Prepare data for imputation for zero days
impute.data <- df_data %>% 
	filter(days == 0) %>%
	select(id, province, activity, cost_trans, cost_accom, days, all_of(socio_vars)) %>%
	rename(cost_trans_impute = cost_trans,
		   cost_accom_impute = cost_accom) 

impute.data <- impute.data %>%
	group_split(activity)

plan(multiprocess, workers = 6)

start_time_zero <- Sys.time()

costs_sim <- vector("list", length = length(impute.data))

for(i in 1:length(impute.data)){
start_time <- Sys.time()
	
	costs_sim[[i]] <- future_map2(df_impute_nonzeros[[i]], adj_list, ~ImputeMissingData(impute.data = impute.data[[i]], 
												   other.data = .x, 
												   delta = .y,
												   maxit = maxit, 
												   n_impute = n_impute),
					 .progress = TRUE,
					 .options = future_options(seed = seed))



	costs_sim[[i]] <- map(costs_sim[[i]], function(x){
		out <- x %>%
			select(id, province, activity, cost_trans_impute, cost_accom_impute, income, adjust)
	return(out)
})
end_time <- Sys.time()
cat(paste(i,": ",round(end_time - start_time,1), "  ")); flush.console()

}
end_time_zero <- Sys.time()
sim_time_zero <- end_time_zero - start_time_zero
sim_time_zero
closeAllConnections()

rm(df_impute_nonzeros)

# Convert to a list of activities
start_time <- Sys.time()
costs_sim <- map(costs_sim, 
					   ~map(., function(x){
	x <- x %>%
		select(id, activity, province, income, cost_trans_impute, cost_accom_impute, adjust) %>%
		mutate(income = income * 100000) %>%
		left_join(all_nontravel, by = c("activity", "province")) %>%
		left_join(df_n_people, by = c("id", "activity"))
	return(x)
	})
)
end_time <- Sys.time()
end_time - start_time


start_time <- Sys.time()
costs_sim <- map(costs_sim, 
					   ~map2_dfr(., opp_time_list, 
					   		  ~CalcTimeCosts(data = .x, opp_time = .y))
					   )
end_time <- Sys.time()
end_time - start_time

costs_sim <- map(costs_sim, function(x){
					   	x <- x %>%
					   		group_by(adjust) %>%
					   		mutate(imp_id = cur_group_id()) %>%
					   		ungroup()
					   	return(x)
					   })

df_sim_info <-	costs_sim[[1]] %>%
	distinct(imp_id, adjust, opp_time)

write_csv(df_sim_info, "data/clean/SimulationInfo.csv")

costs_sim <- map(costs_sim, function(x){
	   	x <- x %>%
	   		select(id, activity, imp_id, costs, cost_trans_impute, cost_accom_impute, cost_time) %>%
	   		arrange(id, activity)
	   	return(x)
	   })

costs_sim <- as_tibble(do.call(rbind, costs_sim)) %>%
	group_split(imp_id)

# Extract main cost variable and save for analysis
costs_sim <- map(costs_sim, function(x){
	x <- x %>%
		select(id, imp_id, activity, costs)
	return(x)
})

save(costs_sim, file = "data/clean/TravelCostsSimulation.Rdata")

rm(list=ls())