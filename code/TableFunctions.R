CreateEstimateTableWide <- function(parms, activity){
	
	est_export <- parms %>%
		gather(parm, value)  %>%
		group_by(parm) %>%
		summarise(mean = round(mean(value), 2),
				  sd = sd(value),
				  zstat = mean / sd) %>%
		print(n=200)
	
	est_export <- est_export %>%
		mutate(good = parse_number(parm),
			   parm = gsub('[[:digit:]]+', '', parm)) %>%
		#   	separate(parms, c("parm", "good"),
		#   			 sep = "([\\.])") %>%
		filter(parm != "imp_id") %>%
		mutate(good = as.numeric(good),
			   good = ifelse(parm == "psi", good + 1, good),
			   parm = ifelse(parm == "n_individuals", 
			   			  "N", ifelse(parm == "sum_log_lik",
			   			  			"LL", parm))) %>%
		left_join(activity, by = "good") %>%
		select(-good)
	
	est_parm_rest <- est_export %>%
		filter(parm == "alpha" | parm == "scale" ) %>%
		mutate_each(~(round(., 3)), -parm, -activity) %>% 
		mutate(sd = paste0("(", sprintf("%.2f", sd), ")")) %>%
		select(-activity, -zstat) %>%
		rename(activity = parm,
			   psi_mean = mean,
			   psi_sd = sd) %>%
		mutate(gamma_mean = "-",
			   gamma_sd = "-",
			   psi_mean = as.character(sprintf("%.2f", psi_mean))) 
	
	est_rest <- est_export %>%
		filter(parm == "LL" | parm == "N" ) %>%
		mutate_each(~(round(., 0)), -parm, -activity) %>% 
		mutate(sd = ifelse(parm == "N", "-",
						   paste0("(", sprintf("%.0f", sd), ")"))) %>%
		select(-activity, -zstat) %>%
		rename(activity = parm,
			   psi_mean = mean,
			   psi_sd = sd) %>%
		mutate(gamma_mean = "-",
			   gamma_sd = "-",
			   psi_mean = as.character(psi_mean))
	
	est_export <- est_export %>%
		select(-zstat) %>%
		filter(grepl('psi', parm) | parm == "gamma" ) 
	
	main_table <- est_export %>%
		mutate_each(~(round(., 3)), -parm, -activity) %>% 
		mutate(sd = paste0("(", sprintf("%.2f", sd), ")"),
			   mean = as.character(sprintf("%.2f", mean))) %>%
		gather(key, value, mean:sd) %>%
		mutate(term = paste(parm, key, sep = '_')) %>%
		select(-parm, -key) %>%
		spread(term, value) %>%
		arrange(activity) %>%
		mutate(activity = as.character(activity)) %>%
		select(activity, psi_mean, psi_sd, gamma_mean, gamma_sd) %>%
		bind_rows(est_parm_rest, est_rest)
	
	main_table <- main_table[,-1]
	
	return(main_table)
}


CreateEstimateTableLong <- function(parms, activity){
	
	est_export <- parms %>%
		gather(parms, value)  %>%
		group_by(parms) %>%
		summarise(mean = round(mean(value), 2),
				  sd = sd(value),
				  zstat = mean / sd) %>%
		print(n=200)
	
	psi.0 <- tibble(parms = "psi.0",
					mean = 0.00,
					sd = 0.00,
					zstat = 0.00)
	
	est_export <- est_export %>%
		bind_rows(psi.0) %>%
		separate(parms, c("parm", "good"),
				 sep = "([\\.])") %>%
		filter(parm != "imp_id") %>%
		mutate(good = as.numeric(good),
			   good = ifelse(parm == "psi", good + 1, good),
			   parm = ifelse(parm == "n_individuals", 
			   			  "N", ifelse(parm == "sum_log_lik",
			   			  			"LL", parm))) %>%
		left_join(activity, by = "good") %>%
		select(-good)
	
	est_parm_rest <- est_export %>%
		filter(parm == "alpha" | parm == "scale" ) %>%
		mutate_each(~(round(., 2)), -parm, -activity) %>% 
		mutate(sd = paste0("(", sprintf("%.2f", sd), ")")) %>%
		select(-activity, -zstat) %>%
		rename(activity = parm) %>%
		mutate(mean = as.character(sprintf("%.2f", mean))) %>%
		mutate(activity= case_when(activity == "alpha" ~ "Satiation parameter ($\\alpha$)",
								   activity == "scale" ~ "Scale parameter ($\\sigma$)",
								   TRUE ~ activity))
	
	est_rest <- est_export %>%
		filter(parm == "LL" | parm == "N" ) %>%
		mutate_each(~(round(., 0)), -parm, -activity) %>% 
		mutate(sd = ifelse(parm == "N", "-",
						   paste0("(", sprintf("%.0f", sd), ")"))) %>%
		select(-activity, -zstat) %>%
		rename(activity = parm) %>%
		mutate(mean = as.character(mean))
	
	est_export <- est_export %>%
		select(-zstat) %>%
		filter(grepl('psi', parm) | parm == "gamma" ) 
	
	main_table <- est_export %>%
		mutate_each(~(round(., 2)), -parm, -activity) %>% 
		mutate(sd = paste0("(", sprintf("%.2f", sd), ")"),
			   mean = as.character(sprintf("%.2f", mean)),
			   sd = ifelse(parm == "psi" & activity == "Birding", "(fixed)", sd)) %>%
		mutate(activity = as.character(activity)) %>%
		arrange(desc(parm), activity) %>%
		bind_rows(est_parm_rest, est_rest) %>%
		select(activity, mean, sd)
	
	#	names <- c(activity$activity, activity$activity, 
	#							  "Satiation parameter ($\\alpha$)", 
	#							  "Scale parameter ($\\sigma$)", "N observations", "Log-likelihood")
	#	main_table <- cbind(names, main_table)
	
	return(main_table)
}
