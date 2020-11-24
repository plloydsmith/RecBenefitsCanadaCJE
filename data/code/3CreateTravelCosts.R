# Filename:           CreateTravelCosts.R
# Created:            01/26/2018
# Description: Import activity data and create travel costs for simulation
#
# NOTES:
#
#-----------------------------------------------------------------------------

# Assumptions
n_people_cutoff <- 5
max_days_per_activity <- 365
names_grouped_activities <- c("hiking","cycling","camping","water_nonmotor","ski_down", "ski_cross","golf")
names_hunt_others <- c("hunt_small", "hunt_other", "trap")

#------------------------------------#
# Import data
#------------------------------------#
df_all <- read_csv("data/clean/CleanData.csv")  %>%
	arrange(id)

df_socio <- read_csv("data/clean/SocioImpute.csv") %>%
	arrange(id)

df_province <- df_socio %>%
	select(id, province)

df_region <- df_socio %>%
	select(id, region)

df_prov_region <- df_socio %>%
	distinct(region, province)

names_province <- unique(df_province$province) 


# Combine hunting and trapping for small animals
df_hunt_other <- df_all %>%
	filter(activity %in% names_hunt_others) %>%
#	mutate(activity = ifelse(activity == "hunt_small" | activity == "hunt_other" | activity == "trap",
#							 "hunt_other", activity)) %>%
	mutate(activity = "hunt_other") %>%
	group_by(id, activity, type) %>%
	summarise(value = sum(value, na.rm = TRUE)) %>%
	ungroup(.)

df_all <- df_all %>%
	filter(!(activity %in% names_hunt_others)) %>%
	bind_rows(df_hunt_other) 

# Clean up days
df_all <- df_all %>%
	pivot_wider(names_from = "type", values_from = "value") %>%
	left_join(df_province, by = "id") %>%
	mutate(day_close = ifelse(is.na(day_close), 0, day_close),
		   day_far = ifelse(is.na(day_far), 0, day_far),
		   days = day_close + day_far,
		   days = ifelse(days > max_days_per_activity, max_days_per_activity, days)) 

df_adjust <- df_all %>%
	group_by(id) %>%
	summarise(tot_days = sum(days),
			  adj_days = ifelse(tot_days > max_days_per_activity, max_days_per_activity / tot_days, 1))

df_all <- left_join(df_all, df_adjust, by = c("id")) %>%
	mutate(days = days * adj_days,
		   day_close = day_close *adj_days,
		   day_far = day_far *adj_days) %>%
	select(-tot_days, -adj_days)


# Save days
df_days <- df_all %>%
	distinct(id, activity, day_close, day_far, days)

write_csv(df_days, "data/clean/DaysClean.csv")

rm(df_adjust, df_hunt_other, df_days)

# Clean up missing n_people
df_n_people <- df_all %>%
	select(id, activity, province, n_people) 

df_n_people_grouped <- df_n_people %>%
	filter(activity %in% names_grouped_activities) %>%
	group_by(id) %>%
	mutate(n_people = mean(n_people, na.rm = TRUE))

df_n_people <- df_n_people %>%
	filter(!(activity %in% names_grouped_activities)) %>%
	bind_rows(df_n_people_grouped) %>%
	mutate(n_people = as.numeric(n_people),
		   n_people = case_when(n_people == 0 ~ 1, 
		   					 n_people > n_people_cutoff ~ n_people_cutoff, 
		   					 TRUE ~ n_people)) %>%
	group_by(activity, province) %>%
	mutate(n_people = replace(n_people, is.na(n_people), mean(n_people, na.rm=TRUE)),
		   n_people = ifelse(is.nan(n_people), NA, n_people)) %>%
	ungroup(.) 


df_all <- df_all %>%
	select(-n_people) %>%
	left_join(df_n_people, by = c("id", "activity", "province")) 

rm(df_n_people, df_n_people_grouped)			  

# Summary
#----------------------------------------------

df_all%>%
	mutate(days200 = ifelse(day_far > 200, 1, 0),
		   part = ifelse(days>0, 1, 0)) %>%
	group_by(activity, province) %>%
	summarise(days = mean(days),
			  days_close = mean(day_close, na.rm = T),
			  day_far = mean(day_far, na.rm = T),
			  days200 = sum(days200),
			  part = mean(part))

# Group travels costs by close/far
df_all %>%
	filter(!is.na(n_people)) %>%
	filter(activity != "hiking") %>%
	mutate(close = ifelse(day_far == 0 & day_close > 0, cost_trans / day_close, NA),
		   far = ifelse(day_far > 0 & day_close == 0, cost_trans / day_far, NA),
		   ov100 = ifelse(close > 100, 1, 0)) %>%
#	filter(ov100 == 0) %>%
#	ggplot(aes(x = far)) + 
#	geom_histogram(binwidth = 10)
	group_by(activity) %>%
	summarise(#count = sum(ov100, na.rm = TRUE),
			  m_close = mean(close, na.rm = TRUE),
			  sd_close = sd(close, na.rm = TRUE),
			  max_close = max(close, na.rm = TRUE),
			  m_far = mean(far, na.rm = TRUE),
			  sd_far = sd(far, na.rm = TRUE))

df_all %>%
	group_by(province, activity) %>%
	summarise(days = mean(days, na.rm = TRUE),
			  costs_trans = mean(cost_trans, na.rm = TRUE),
			  costs_equip = mean(cost_equip, na.rm = TRUE),
			  count = n()) %>%
	print(n=300)




# Aggregate trans and accom

df_all <- df_all %>%
	select(id, activity, days, n_people, cost_trans, cost_accom, cost_equip)



# Create separate costs for grouped activities
#----------------------------------------------

df_grouped <- df_all %>%
	filter(activity %in% names_grouped_activities) 

grouped_total_costs <- df_grouped %>%
	filter(activity == "hiking") %>%
	select(id, cost_equip, cost_trans, cost_accom, n_people) %>%
	pivot_longer(c(cost_equip, cost_trans, cost_accom), names_to = "type", values_to = "costs_total") %>%
	mutate(costs_per_person = costs_total / n_people)

# Create data frame for folks with single activity
grouped_single <- df_grouped %>%
	select(id, activity, days) %>%
	filter(days != 0) %>%
	group_by(id) %>%
	mutate(qnum = n()) %>%
	filter(qnum == 1) %>%
	left_join(grouped_total_costs, by = "id") %>%
	mutate(costs_per_person_day = ifelse(days > 0 & costs_per_person > 0, costs_per_person / days, NA)) %>%
	select(id, activity, type, costs_per_person_day) %>%
	ungroup(.) %>%
	left_join(df_region, by = "id")

# Provincial specific for Hiking and Golf
# National specific for others
grouped_list <- grouped_single %>%
	distinct(activity, region, type)

grouped_single_mean <- grouped_single %>%
	filter(!is.na(costs_per_person_day)) %>%
	group_by(activity, region, type) %>%
	summarise(count = n(),
			  costs_per_person_day = mean(costs_per_person_day)) %>%
	ungroup(.)

grouped_single_mean_national <- grouped_single_mean %>%
	filter(activity != "hiking", activity != "golf") %>%
	group_by(activity, type) %>%
	summarise(costs_national = weighted.mean(costs_per_person_day, count),
			  count_national = sum(count)) %>%
	ungroup(.)

grouped_single_mean <- grouped_list %>%
	left_join(grouped_single_mean, by = c("activity", "region", "type")) %>%
	mutate(costs_per_person_day = ifelse(activity == "hiking" | activity == "golf", costs_per_person_day, NA),
		   count = ifelse(activity == "hiking" | activity == "golf", count, NA)) %>%
	left_join(grouped_single_mean_national, by = c("activity", "type")) %>%
	rowwise(.) %>%
	mutate(costs_per_person_day = sum(costs_national, costs_per_person_day, na.rm = T),
		   count = sum(count, count_national, na.rm = T)) %>%
	select(-costs_national, -count_national) %>%
	ungroup(.) %>%
	select(-count) %>%
	pivot_wider(names_from = "type", values_from = "costs_per_person_day") #%>%
#	rename(ec_mean = cost_equip_per)
	#	filter(!((region == "prairies" | region == "north") & activity == "ski_down")) %>%
#	bind_rows(bc_skicross, north_skidown)

# replace BC costs with prairies
#bc_skicross <- grouped_single_mean %>%
#	filter(region == "prairies" & activity == "ski_cross") %>%
#	mutate(region = "BC")

# collapse north down_ski costs in with prairies
#north_skidown <- grouped_single_mean %>%
#	filter((region == "prairies" | region == "north") & activity == "ski_down") %>%
#	ungroup(.) %>%
#	summarise(ec_mean = weighted.mean(ec_mean, count),
#			  cost_trans_per = weighted.mean(cost_trans_per, count),
#			  count = sum(count),) %>%
#	mutate(region = "prairies",
#		   activity = "ski_down")

#north_skidown <- north_skidown %>%
#	mutate(region = "north") %>%
#	bind_rows(north_skidown)


#grouped_single_mean <- grouped_single_mean %>%
#	filter(!(region == "BC" & activity == "ski_cross")) %>%
#	filter(!((region == "prairies" | region == "north") & activity == "ski_down")) %>%
#	bind_rows(bc_skicross, north_skidown)


grouped_costs <- df_grouped %>%
	select(id, activity, days) %>%
	left_join(df_region, by = "id") %>%
	left_join(df_province, by = "id") %>%
	left_join(grouped_single_mean, by = c("activity", "region")) %>%
	select(id, province, activity, days, cost_equip, cost_accom, cost_trans) %>%
	pivot_longer(cost_equip:cost_trans, names_to = "type", values_to = "costs_per_person_day") %>%
	mutate(costs_per_person_all = days * costs_per_person_day)

grouped_costs_total <- grouped_costs %>%
	group_by(id, type) %>%
	summarise(costs_per_person_all_total = sum(costs_per_person_all)) %>%	
	left_join(grouped_total_costs, by = c("id", "type")) %>%
	mutate(adj_factor = ifelse(costs_per_person_all_total > 0, costs_per_person / costs_per_person_all_total, 1))

grouped_costs <- grouped_costs %>%
	left_join(grouped_costs_total, by = c("id", "type")) %>%
	mutate(costs_adj = ifelse(days > 0 & costs_total > 0, costs_per_person_all / days  * adj_factor, NA)) 

grouped_costs_prov_average <- grouped_costs %>%
	filter(!is.na(costs_adj)) %>%
	filter(type != "cost_trans") %>%
	group_by(activity, type, province) %>%
	summarise(count = n(),
			  costs = mean(costs_adj)) 

df_grouped_final <- grouped_costs %>%
	select(id, activity, days, n_people, type, costs_adj) %>%
	left_join(grouped_single, by = c("id", "activity", "type")) %>%
	mutate(costs = ifelse(is.na(costs_per_person_day), costs_adj, costs_per_person_day)) %>%
	select(id, activity, days, n_people, type, costs) %>%
	pivot_wider(names_from = "type", values_from = "costs") 

rm(grouped_list, grouped_single, 
   grouped_single_mean_national, grouped_costs)



# Start on ungrouped data
df_all <- df_all %>%
	filter(!activity %in% names_grouped_activities) %>%
	pivot_longer(c(cost_trans, cost_accom, cost_equip), 
				 names_to = "type", 
				 values_to = "costs") %>%
	mutate(costs = ifelse(days > 0, (costs / n_people) / days, NA)) %>%
	pivot_wider(names_from = "type", values_from = "costs") %>%
	bind_rows(df_grouped_final) %>%
	arrange(id, activity) %>%
	left_join(df_province, by = "id")

# Create average equipment and accomodation costs for non-grouped activities
all_equip_accom <- df_all %>%
	select(-cost_trans) %>%
	pivot_longer(c(cost_accom, cost_equip), 
				 names_to = "type", 
				 values_to = "costs") %>%
	filter(!is.na(costs))  %>%
	group_by(activity, province, type) %>%
	summarise(costs = mean(costs, na.rm=TRUE),
			  count = n())

# collapse north down_ski costs in with prairies
martimes_waterfowl <- all_equip_accom %>%
	filter((province == "NB" | province == "PE") & activity == "hunt_waterfowl") %>%
	ungroup(.) %>%
	group_by(activity, type) %>%
	summarise(costs = weighted.mean(costs, count),
			  count = sum(count)) %>%
	mutate(province = "NB")

martimes_waterfowl <- martimes_waterfowl %>%
	mutate(province = "PE") %>%
	bind_rows(martimes_waterfowl)
	
all_equip_accom <- all_equip_accom %>%
	filter(!((province == "NB" | province == "PE") & activity == "hunt_waterfowl")) %>%
	bind_rows(martimes_waterfowl)

write_csv(all_equip_accom, "data/clean/NonTravelCosts.csv")


df_data <- df_all %>%
	select(id, activity, days, n_people, cost_trans, cost_accom) %>%
	mutate(cost_trans = ifelse(days == 0 | cost_trans == 0, 
							   NA, cost_trans))

write_csv(df_data, "data/clean/DataBeforeImputation.csv")

rm(list=ls())