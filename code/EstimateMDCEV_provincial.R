
df_mdcev_temp <- df_mdcev %>%
	select(-price)

# Estimate Models
plan(multiprocess, workers = n_workers)


load(file_name_provincial_single)


# BC
#load("output/old/mdcev_provincial_bc.RData")

#init <- GetInitialValues(parms)
df_mdcev_temp <- df_mdcev_temp %>%
	named_group_split(province)


df_parms <- vector("list", length = length(df_mdcev_temp))
names(df_parms) <- names(df_mdcev_temp)

for (i in 1:length(df_mdcev_temp)){
	init = output_prov[[i]]$stan_fit$par
	
	costs_temp <- future_map(costs_sim, function(x, data = df_mdcev_temp[[i]]){
		out <- data %>%
			select(id, activity) %>%
			left_join(x, by = c("id", "activity"))
		return(out)
	} )
	
	n_sample_temp <- length(unique(df_mdcev_temp[[i]]$id))
	
	par_est <- future_map(costs_temp, ~suppressMessages(FitMDCEV_Sims(formula = mdcev_formula, 
													data = df_mdcev_temp[[i]],
													costs_sim = .x,
													n_sample = n_sample_temp,
													init = init,
													weights = "no")),
						  .progress = TRUE,
						  future_options(seed = NA))
	
	id_sample <- par_est %>%
		map("id_sample")
	
	par_est <- map(par_est,~ discard(.x, names(.x) == "id_sample"))
	
	parms <- par_est %>%
		map(as.data.frame) %>%
		bind_rows()

	df_parms[[i]] <- list(parms = parms,
						  id_sample = id_sample)
}	
closeAllConnections()

save(df_parms, file = file_name_provincial_estimation)

plan(multiprocess, workers = n_workers)

for (i in 1:length(df_mdcev_temp)){
	init = output_prov[[i]]$stan_fit$par
	
	costs_temp <- future_map(costs_sim, function(x, data = df_mdcev_temp[[i]]){
		out <- data %>%
			select(id, activity) %>%
			left_join(x, by = c("id", "activity"))
		return(out)
	} )
	
	n_sample_temp <- length(unique(df_mdcev_temp[[i]]$id))
	
	par_est <- future_map(costs_temp, ~suppressMessages(FitMDCEV_Sims(formula = mdcev_formula, 
																	  data = df_mdcev_temp[[i]],
																	  costs_sim = .x,
																	  n_sample = n_sample_temp,
																	  init = init,
																	  weights = "yes")),
						  .progress = TRUE,
						  future_options(seed = NA))
	
	id_sample <- par_est %>%
		map("id_sample")
	
	par_est <- map(par_est,~ discard(.x, names(.x) == "id_sample"))
	
	parms <- par_est %>%
		map(as.data.frame) %>%
		bind_rows()
	
	df_parms[[i]] <- list(parms = parms,
						  id_sample = id_sample)
}	
closeAllConnections()

save(df_parms, file = file_name_provincial_estimation_w)

rm(df_mdcev_temp, df_parms, output_prov, df_parms, par_est, cost_temp, init, id_sample, n_sample_temp)
