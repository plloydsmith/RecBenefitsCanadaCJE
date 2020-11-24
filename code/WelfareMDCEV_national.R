
df_mdcev_temp <- df_mdcev %>%
	arrange(id, activity) %>%
	select(id, activity, quant, income, province, weight)


# Subset cost data for sampled people only
plan(multiprocess, workers = n_workers)

start_time <- Sys.time()

df_id_welfare <- future_map(costs_sim, function(.x, data = df_mdcev_temp){
	
	out <- KeepExpendBelowIncome(costs_sim = .x, data = data) %>%
		distinct(id, imp_id)})

end_time <- Sys.time()
end_time - start_time

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

# Organize parameter estimates

load(file_name_national_estimation)

parms_all <- map(mdcev_sims$par_est, `[`, c("scale", "alpha", "psi", "gamma"))

parms_all <- parms_all %>%
	map(as.data.frame) %>%
	bind_rows() %>%
	mutate(sim_id = seq(1:n_costs_sim)) %>%
	pivot_longer(-c("sim_id"), names_to = "parms", values_to = "value") %>%
		group_split(sim_id)

	mdcev_out <- future_map2(costs_sim, parms_all, 
							 ~SimWelfareBootstrap(costs_sim = .x, 
							 					 parms = .y, nerrs = nerrs, 
							 					 mdcev_formula = mdcev_formula,
							 					 data = df_mdcev_temp, 
							 					 mle_options = mle_options, 
							 					 policies = policies),
							 .progress = TRUE,
							 future_options(seed = NA))
	
	
	df_wtp <- map2_dfr(df_id_welfare, mdcev_out, 
							~CombineWelfareID(id = .x, sim_out = .y, 
											  policy_names = policy_names))


save(df_wtp, file = file_name_national_welfare)


load(file_name_national_estimation_w)

parms_all <- map(mdcev_sims$par_est, `[`, c("scale", "alpha", "psi", "gamma"))

parms_all <- parms_all %>%
	map(as.data.frame) %>%
	bind_rows() %>%
	mutate(sim_id = seq(1:n_costs_sim)) %>%
	pivot_longer(-c("sim_id"), names_to = "parms", values_to = "value") %>%
	group_split(sim_id)

mdcev_out <- future_map2(costs_sim, parms_all, 
						 ~SimWelfareBootstrap(costs_sim = .x, 
						 					 parms = .y, nerrs = nerrs, 
						 					 mdcev_formula = mdcev_formula,
						 					 data = df_mdcev_temp, 
						 					 mle_options = mle_options, 
						 					 policies = policies),
						 .progress = TRUE,
						 future_options(seed = NA))


df_wtp <- map2_dfr(df_id_welfare, mdcev_out, 
				   ~CombineWelfareID(id = .x, sim_out = .y, 
				   				  policy_names = policy_names))


closeAllConnections()

save(df_wtp, file = file_name_national_welfare_w)

rm(df_mdcev_temp, df_id_welfare, policies, mle_options, parms_all, df_wtp)
