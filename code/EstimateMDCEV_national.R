
df_mdcev_temp <- df_mdcev %>%
	select(-price)

# Get starting values from previous model run
load(file_name_national_single)

init = output$stan_fit$par
#load("output/mdcev_national_sims.RData")

#init <- GetInitialValues(parms)

# Estimate Models
plan(multiprocess, workers = n_workers)
start_time <- Sys.time()

par_est <- future_map(costs_sim, ~FitMDCEV_Sims(formula = mdcev_formula, 
																 data = df_mdcev_temp,
																 costs_sim = .x,
																 n_sample = n_sample,
																 init = init,
																 weights = "no"),
					  .progress = TRUE,
					  future_options(seed = NA))

end_time <- Sys.time()
est_time <- end_time - start_time
est_time

mdcev_sims <- list()
mdcev_sims$par_est <- par_est
mdcev_sims$est_time <- est_time

save(mdcev_sims, file = file_name_national_estimation)

# Estimate Models
load(file_name_national_single_w)

init = output$stan_fit$par
start_time <- Sys.time()

par_est <- future_map(costs_sim, ~suppressMessages(FitMDCEV_Sims(formula = mdcev_formula, 
																  data = df_mdcev_temp,
																  costs_sim = .x,
																  n_sample = n_sample,
																  init = init,
																  weights = "yes")),
					  .progress = TRUE,
					  future_options(seed = NA))

end_time <- Sys.time()
est_time <- end_time - start_time
est_time
closeAllConnections()

mdcev_sims <- list()
mdcev_sims$par_est <- par_est
mdcev_sims$est_time <- est_time

save(mdcev_sims, file = file_name_national_estimation_w)

rm(df_mdcev_temp, init, output)
