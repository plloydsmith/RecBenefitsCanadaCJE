
df_id <- df_mdcev %>%
	distinct(id, income, province, weight) 

df_weights <- df_id %>%
	select(id, weight) %>%
	mutate(weight = weight / mean(weight))

load(file_name_national_welfare)

wtp <- df_wtp   %>%
	mutate(weighted = "no") 

load(file_name_national_welfare_w)

wtp2 <- df_wtp   %>%
	mutate(weighted = "yes")

rm(df_wtp, costs_sim)
gc()

wtp <- bind_rows(wtp, wtp2) 
rm(wtp2)
gc()

wtp <- wtp %>%
	group_split(activity)

df_wtp <- vector("list", length = length(wtp))
names(df_wtp) <- c("all", activity_names)

for (i in 1:length(df_wtp)){
	
	wtp_i <- wtp[[i]] %>%
		left_join(df_id, by = "id") %>%
#		mutate(wtp = ifelse(wtp < income*-2, income*-2, wtp)) %>%
		select(-weight) %>%
		left_join(df_weights, by = "id") %>%
		mutate(weight = ifelse(weighted == "no", 1, weight))
	
	df_mean <- wtp_i  %>%
		group_by(weighted) %>%
		summarise(wtp_m = round(mean(wtp*weight),0))
	
	df_sd <- wtp_i %>%
		group_by(imp_id, weighted) %>%
		summarise(wtp = mean(wtp*weight)) %>%
		ungroup(.) %>%
		group_by(weighted) %>%
		summarise(wtp_sd = round(sd(wtp), 0),
				  wtp_hi = round(quantile(wtp, .975),0),
				  wtp_lo = round(quantile(wtp, .025),0))
	out <- full_join(df_mean, df_sd)
	df_wtp[[i]] <- out
}

df <- bind_rows(df_wtp, .id = "activity")

write_csv(df_wtp, "temp/wtp_national.csv")


df_outlier_summary <- wtp %>%
	filter(wtp)
	mutate(trim = ifelse(wtp < income*-2, 1, 0)) %>%
	group_by(activity) %>%
	summarise(num_trim = sum(trim),
			  per_trim = round(num_trim / n()*100,2))

df_outlier_summary

	
# Normalize weights


# WtP per participant
df_days <- df_mdcev %>%
	select(id, activity, quant) %>%
	group_split(activity)

df_wtp <- vector("list", length = length(df_days))
names(df_wtp) <- c(activity_names)

for (i in 1:length(df_wtp)){
	
	df_days_i <- df_days[[i]] 
	
	wtp_i <- wtp[[i+1]] %>%
		left_join(df_id, by = "id") %>%
		#		mutate(wtp = ifelse(wtp < income*-2, income*-2, wtp)) %>%
		select(-weight) %>%
		left_join(df_weights, by = "id") %>%
		mutate(weight = ifelse(weighted == "no", 1, weight)) %>%
		left_join(df_days_i, by = c("id", "activity")) %>%
		filter(quant > 0)
	
	df_mean <- wtp_i  %>%
		group_by(weighted) %>%
		summarise(wtp_m = round(mean(wtp*weight),0))
	
	df_sd <- wtp_i %>%
		group_by(imp_id, weighted) %>%
		summarise(wtp = mean(wtp*weight)) %>%
		ungroup(.) %>%
		group_by(weighted) %>%
		summarise(wtp_sd = round(sd(wtp), 0),
				  wtp_hi = round(quantile(wtp, .975),0),
				  wtp_lo = round(quantile(wtp, .025),0))
	out <- full_join(df_mean, df_sd)
	df_wtp[[i]] <- out
}
df <- bind_rows(df_wtp, .id = "activity")

write_csv(df, "temp/wtp_national_participant.csv")

gc()

# WtP per day
df_days <- df_mdcev %>%
	select(id, activity, quant) %>%
	group_split(activity)

df_wtp <- vector("list", length = length(df_days))
names(df_wtp) <- c(activity_names)

for (i in 1:length(df_wtp)){

	df_days_i <- df_days[[i]] 
	
	wtp_i <- wtp[[i+1]] %>%
		left_join(df_id, by = "id") %>%
		#		mutate(wtp = ifelse(wtp < income*-2, income*-2, wtp)) %>%
		select(-weight) %>%
		left_join(df_weights, by = "id") %>%
		mutate(weight = ifelse(weighted == "no", 1, weight)) %>%
		left_join(df_days_i, by = c("id", "activity"))
	
	df_mean <- wtp_i  %>%
		filter(quant > 0) %>%
		group_by(weighted) %>%
		summarise(participants = n() / max(imp_id),
				  mean_wtp = mean(wtp*weight),
				  mean_days = round(mean(quant*weight), 1),
				  wtp_day = round(mean_wtp / mean(quant*weight),0))

	mean_days_i <- wtp_i %>%
		filter(quant > 0) %>%
	 	group_by(weighted) %>%
	 	summarise(mean_days = mean(quant*weight))
	 	
	df_sd <- wtp_i %>%
		filter(quant > 0) %>%
		group_by(imp_id, weighted) %>%
		summarise(wtp = mean(wtp*weight)) %>%
		ungroup(.) %>%
		full_join(mean_days_i) %>%
		group_by(weighted) %>%
		summarise(wtp_sd = round(sd(wtp / mean_days), 0),
				  wtp_hi = round(quantile(wtp / mean_days, .975),0),
				  wtp_lo = round(quantile(wtp / mean_days, .025),0))
	out <- full_join(df_mean, df_sd)
	df_wtp[[i]] <- out
}
df <- bind_rows(df_wtp, .id = "activity")

write_csv(df, "temp/wtp_national_per_day.csv")

