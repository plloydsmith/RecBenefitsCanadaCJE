



named_group_split <- function(.tbl, ...) {
	# https://github.com/tidyverse/dplyr/issues/4223
	grouped <- group_by(.tbl, ...)
	names <- rlang::eval_bare(rlang::expr(paste(!!!group_keys(grouped), sep = " / ")))
	
	grouped %>% 
		group_split() %>% 
		rlang::set_names(names)
}


CreatePsiASCs <- function(df){
	df <- dummy_cols(df, select_columns = c("activity"))
	col_old <- colnames(df)
	col_new <- gsub(pattern = "activity_",replacement = "", x  = col_old)		
	colnames(df) <- col_new
	return(df)
}


FitMDCEV_Sims <- function(data, costs_sim, formula, init,
						  n_sample, weights, n_classes = 1, fixed_scale1 = 0){
	
	imp_id <- unique(costs_sim$imp_id)
	
	data_temp <- KeepExpendBelowIncome(data = data, 
										costs_sim = costs_sim) 
	
	if (weights == "no"){
		df_id <- as_tibble(sample(unique(data_temp$id), 
							   size = n_sample, replace = T))
	} else if (weights == "yes"){
		df_weight <- data_temp %>%
			distinct(id, weight)
		df_id <- as_tibble(sample(unique(data_temp$id), 
							   size = n_sample, replace = T, prob = df_weight$weight))
	}	
	
	data_temp <- df_id %>%
		mutate(unique_id = seq(n())) %>%
		rename(id = value) %>%	
		left_join(data_temp, by = "id") %>%
		select(-id) %>%
		rename(id = unique_id) %>%
		arrange(id, activity)
	
	# data_temp <- CreatePsiASCs(data_temp)
	
	data_temp <- mdcev.data(data = data_temp, 
							 id.var = "id", 
							 alt.var = "activity", 
							 choice = "quant" )
	
	mdcev_est <- mdcev(formula = formula,
						data = data_temp,
						model = "gamma",
						initial.parameters = init,
						print_iterations = F,
						n_classes = n_classes,
						algorithm = "MLE")
	
	out <- mdcev_est[["stan_fit"]][["par"]]
	out$theta <- NULL
	out$class_probabilities <- mdcev_est[["class_probabilities"]]
	out$n_individuals <- mdcev_est[["n_individuals"]]
	out$imp_id <- imp_id
	out$id_sample <- df_id$value	

	return(out)
}



KeepExpendBelowIncome <- function(data, costs_sim){
	
	df_costs <- left_join(costs_sim, data, by = c("id", "activity"))
	
	keep_expend <- df_costs %>%
		mutate(spend = quant * costs) %>%
		group_by(id, income) %>%
		summarise(quant = sum(quant),
				  spend = sum(spend), .groups = 'drop') %>%
		mutate(num_exp = income - spend) %>%		  
		filter(num_exp > 0) %>%
		ungroup(.) %>%
		select(id) %>%
		as.matrix()
	
	df_clean <- df_costs %>%
		filter(id %in% keep_expend) %>%
		mutate(good = as.numeric(as.factor(activity))) %>%
		rename(price = costs) %>%
		arrange(id, activity, good)
	
	return(df_clean)
}

ReturnOutliers <- function(data, costs_sim){
	
	df_costs <- left_join(costs_sim, data, by = c("id", "activity"))
	
	keep_expend <- df_costs %>%
		mutate(spend = days * costs) %>%
		group_by(id, income) %>%
		summarise(days = sum(days),
				  spend = sum(spend), .groups = 'drop') %>%
		mutate(num_exp = income - spend) %>%		  
		filter(num_exp > 0) %>%
		ungroup(.) %>%
		select(id) %>%
		as.matrix()
	
	df_outlier <- df_costs %>%
		distinct(id) %>%
		filter(!(id %in% keep_expend))
	
	return(df_outlier)
}


MdcevSelection <- function(model){

	tmpH <- floor(model$time.taken/60^2)
	tmpM <- floor((model$time.taken-tmpH*60^2)/60)
	tmpS <- round(model$time.taken-tmpH*60^2-tmpM*60,2)
	timeTaken <- paste(formatC(tmpH,width=2,format='d',flag=0),
					   formatC(tmpM,width=2,format='d',flag=0),
					   tmpS,sep=':')
	aic3 = 3*model$parms_info[["n_vars"]][["n_parms_total"]] - 2*model$log.likelihood 
	caic = log(model$n_individuals+1)*model$parms_info[["n_vars"]][["n_parms_total"]]- 2*model$log.likelihood
	
	
	output <- as_tibble(cbind(model = model$model,
						classes = model$n_classes,
						parameters = model$parms_info[["n_vars"]][["n_parms_total"]],
						loglike = model$log.likelihood,
						aic = model$aic,
						aic3 = aic3,
						bic = model$bic,
						caic = caic,
					time = timeTaken))
	return(output)
}

SimWelfareBootstrap <- function(costs_sim, data, mdcev_formula, parms, 
								nerrs, mle_options, policies, cond_err = 1){
	
	df_mdcev_sims <- KeepExpendBelowIncome(data = data, 
									  costs_sim = costs_sim) 
	
	#df_mdcev_sims <- CreatePsiASCs(df_mdcev_sims)
	
	df_mdcev_sims <- suppressMessages(mdcev.data(df_mdcev_sims, 
						   id.var = "id",
						   alt.var = "activity", 
						   choice = "quant"))
	
	alt_names <-  as_vector(unique(attr(df_mdcev_sims, "index")["alt"]))
	random_parameters = "fixed"
	algorithm = "MLE"
	
	stan_data <- rmdcev:::processMDCEVdata(mdcev_formula, df_mdcev_sims, model_options = mle_options)
	
	parms_info <- rmdcev:::CreateParmInfo(stan_data, alt_names, algorithm, random_parameters)

	stan_est <- list(stan_data = stan_data,
					 parms_info = parms_info,
					 n_draws = 1,
					 n_classes = 1,
					 random_parameters = random_parameters,
					 algorithm = algorithm)
	
	sim_welfare <- rmdcev:::ProcessSimulationData(est_sim = parms, object = stan_est, 
												  policies, nsims = 1)
	df_common <- sim_welfare
	df_common$df_indiv <- NULL
	
	sim_options <- list(n_classes = 1,
						model_num = stan_data$model_num,
						price_change_only = policies$price_change_only)

	wtp_cond <- suppressMessages(mdcev.sim(sim_welfare$df_indiv, 
						df_common = df_common,
						sim_options = sim_options,
						cond_err = cond_err, tol = 1e-10, nerrs = nerrs, sim_type = "welfare"))
	
	return(wtp_cond)
}


GetInitialValues <- function(parms){ 
	
	parms <- as.data.frame(colMeans(parms) ) %>%
		rownames_to_column(var = "parms") 
	
	colnames(parms) <- c("parms", "value")
	
	psi <- parms %>%
		filter(grepl('psi', parms))
	
	gamma <- parms %>%
		filter(grepl('gamma', parms))
	
	alpha <- parms %>%
		filter(grepl('alpha', parms))
	
	scale <- parms %>%
		filter(grepl('scale', parms))
	
	init = list(psi = array(psi$value, dim = c(1, length(psi$value))), 
				gamma = array(gamma$value, dim = c(1, length(gamma$value))), 
				alpha = array(alpha$value, dim = c(1, 1)), 
				scale = array(scale$value, dim = c(1)))
	return(init)
}

CombineWelfareID <- function(id, sim_out, policy_names){
#	id <- df_id_welfare[[2]][[12]]
#	sim_out <- mdcev_out[[12]]
		out_temp <- cbind(id, do.call(rbind, sim_out))
		names(out_temp) <- c("id", "imp_id", policy_names)
		out_temp <- as_tibble(out_temp) %>%
			pivot_longer(-c(id, imp_id), names_to = "activity", values_to = "wtp")
		return(out_temp)
}
