
df_id <- df_mdcev %>%
	distinct(id, income, province, weight) 

load(file_name_provincial_welfare_w)

wtp <- as_tibble(do.call(bind_rows, df_wtp)) %>%
	left_join(df_id, by = "id") %>%
	group_by(activity, province) %>%
	mutate(weight = weight / mean(weight)) %>%
	ungroup(.)

rm(df_wtp)

df_mean <- wtp  %>%
	group_by(activity, province) %>%
	summarise(wtp_m = round(mean(wtp*weight),0))

df_sd <- wtp %>%
	group_by(imp_id, activity, province) %>%
	summarise(wtp = mean(wtp*weight)) %>%
	ungroup(.) %>%
	group_by(activity, province) %>%
	summarise(wtp_sd = round(sd(wtp), 0),
			  wtp_hi = round(quantile(wtp, .975),0),
			  wtp_lo = round(quantile(wtp, .025),0))	

df <- full_join(df_mean, df_sd) %>%
	ungroup(.)

write_csv(df, "temp/wtp_prov_w.csv")

