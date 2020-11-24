
df_mdcev_temp <- mdcev.data(data = df_mdcev, 
							id.var = "id", 
							alt.var = "activity", 
							choice = "quant" )

init = list(alpha = 0.5,
			scale = 1,
			gamma = rep(1,length(activity_names)))

output <- mdcev(formula = mdcev_formula,
					   data = df_mdcev_temp,
						initial.parameters = init,
					   model = "gamma",
					   algorithm = "MLE")

summary(output)

save(output, file = file_name_national_single)

weights <- df_mdcev_temp %>%
	distinct(id, weight)

output_w <- mdcev(formula = mdcev_formula,
				data = df_mdcev_temp,
				initial.parameters = output$stan_fit$par,
				model = "gamma",
				weights= weights$weight,
				algorithm = "MLE")

summary(output_w)

save(output_w, file = file_name_national_single_w)

# Estimate Provincial Models
plan(multiprocess, workers = n_workers)

df_mdcev_temp <- df_mdcev_temp %>%
	named_group_split(province)

output_prov <- vector("list", length = length(df_mdcev_temp))
names(output_prov) <- names(df_mdcev_temp)

for (i in 1:length(df_mdcev_temp)){

	df_mdcev_prov <- mdcev.data(data = df_mdcev_temp[[i]], 
								id.var = "id", 
								alt.var = "activity", 
								choice = "quant" )
	
	output_prov[[i]] <- mdcev(formula = mdcev_formula, 
					 data = df_mdcev_prov,
					initial.parameters = output$stan_fit$par,
					model = "gamma",
					algorithm = "MLE")
}	

save(output_prov, file = file_name_provincial_single)

rm(df_mdcev_temp, init, weights, output, output_w, output_prov, df_mdcev_prov)
