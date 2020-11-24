
df_mdcev_temp <- df_mdcev %>%
	arrange(id, activity) %>%
	select(id, activity, quant, income, province, weight)


# Subset cost data for sampled people only
plan(multiprocess, workers = n_workers)

start_time <- Sys.time()

df_id_welfare <- future_map_dfr(costs_sim, function(.x, data = df_mdcev_temp){
	
	out <- KeepExpendBelowIncome(costs_sim = .x, data = data) %>%
		distinct(id, imp_id, province)})

end_time <- Sys.time()
end_time - start_time

closeAllConnections()

policies <- list(price_p = price_p, dat_psi_p = NULL, dat_phi_p = NULL, price_change_only = TRUE)

policies$price_p <- CreateListsRow(price_p)

mle_options <- list(fixed_scale1 = 0,
					model = "gamma",
					n_classes = 1,
					trunc_data = 0,
					psi_ascs = 1,
					gamma_ascs = 1,
					seed = "123",
					jacobian_analytical_grad = 1,
					max_iterations = 2000,
					print_iterations = 1,
					hessian = TRUE,
					n_draws = 1,
					keep_loglik = 0,
					prior_psi_sd = 10,
					prior_hi_sd = 10,
					prior_gamma_sd = 10,
					prior_alpha_shape =1,
					prior_scale_sd = 1,
					prior_delta_sd = 10,
					gamma_nonrandom = 1,
					alpha_nonrandom = 1)

df_mdcev_temp <- df_mdcev_temp %>%
	named_group_split(province)

df_id_welfare <- df_id_welfare %>%
	group_split(province, .keep = FALSE)

df_id_welfare <- map(df_id_welfare, function(x){
	x <- x %>%
 group_split(imp_id)
})

# Organize parameter estimates

load(file_name_provincial_estimation)

df_parms <- map(df_parms, "parms")

df_wtp <- vector("list", length = length(df_parms))
names(df_wtp) <- names(df_parms)

for (i in 1:length(df_parms)){

parms <- df_parms[[i]]  %>%
		arrange(imp_id)   %>%
	mutate(sim_id = seq(1:n_costs_sim)) %>%
	pivot_longer(-c("sim_id", "imp_id"), names_to = "parms", values_to = "value") %>% 
	select(-imp_id) %>%
		group_split(sim_id)

names(parms) <- rep("parms", length(parms))

plan(multiprocess, workers = n_workers)

	costs_temp <- future_map(costs_sim, function(x, data = df_mdcev_temp[[i]]){
		out <- data %>%
			select(id, activity) %>%
			left_join(x, by = c("id", "activity"))
		return(out)
	} )
	
	mdcev_out <- future_map2(costs_temp, parms, 
							 ~SimWelfareBootstrap(costs_sim = .x, 
							 					 parms = .y, nerrs = nerrs, 
							 					 mdcev_formula = mdcev_formula,
							 					 data = df_mdcev_temp[[i]], 
							 					 mle_options = mle_options, 
							 					 policies = policies),
							 .progress = TRUE,
							 future_options(seed = NA))
	
	end_time <- Sys.time()
	end_time - start_time
	
	df_wtp[[i]] <- map2_dfr(df_id_welfare[[i]], mdcev_out, 
							~CombineWelfareID(id = .x, sim_out = .y, 
											  policy_names = policy_names))

closeAllConnections()
}


save(df_wtp, file = file_name_provincial_welfare)


load(file_name_provincial_estimation_w)

df_parms <- map(df_parms, "parms")

df_wtp <- vector("list", length = length(df_parms))
names(df_wtp) <- names(df_parms)

for (i in 1:length(df_parms)){
	
	parms <- df_parms[[i]]  %>%
		arrange(imp_id)   %>%
		mutate(sim_id = seq(1:n_costs_sim)) %>%
		pivot_longer(-c("sim_id", "imp_id"), names_to = "parms", values_to = "value") %>% 
		select(-imp_id) %>%
		group_split(sim_id)
	
	names(parms) <- rep("parms", length(parms))
	
	plan(multiprocess, workers = n_workers)
	
	costs_temp <- future_map(costs_sim, function(x, data = df_mdcev_temp[[i]]){
		out <- data %>%
			select(id, activity) %>%
			left_join(x, by = c("id", "activity"))
		return(out)
	} )
	
	mdcev_out <- future_map2(costs_temp, parms, 
							 ~SimWelfareBootstrap(costs_sim = .x, 
							 					 parms = .y, nerrs = nerrs, 
							 					 mdcev_formula = mdcev_formula,
							 					 data = df_mdcev_temp[[i]], 
							 					 mle_options = mle_options, 
							 					 policies = policies),
							 .progress = TRUE,
							 future_options(seed = NA))
	
	end_time <- Sys.time()
	end_time - start_time
	
	df_wtp[[i]] <- map2_dfr(df_id_welfare[[i]], mdcev_out, 
							~CombineWelfareID(id = .x, sim_out = .y, 
											  policy_names = policy_names))
	
	closeAllConnections()
}


save(df_wtp, file = file_name_provincial_welfare_w)


rm(df_mdcev_temp, df_id_welfare, policies, costs_temp, mle_options, parms, mdcev_out, df_wtp)
