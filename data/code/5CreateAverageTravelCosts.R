# Filename:           CreateKTData.R
# Created:            01/26/2018
# Description: Convert to KT model data form
#
# NOTES:
#
#-----------------------------------------------------------------------------
rm(list=ls(all=TRUE))
ls()

#------------------------------------#
# Load Packages
#------------------------------------#
library(reshape2)

#------------------------------------#
# Import data
#------------------------------------#

df_data <- read_csv("data/clean/DataBeforeImputation.csv") %>%
	select(-cost_trans, -cost_accom)

df_socio <- read_csv("data/clean/SocioImpute.csv") %>%
	arrange(id)

load("data/clean/TravelCostsSimulation.Rdata")

costs_sim <- map(costs_sim, function(x){
	x <- x %>%
	select(id, activity, costs)
	return(x)
	})

df_costs <- do.call(rbind, costs_sim) %>%
	group_by(id, activity) %>%
	summarise(costs = mean(costs))

df_costs <-	left_join(df_costs, df_data, by = c("id", "activity")) %>%
	left_join(df_socio, by = "id")

keep_expend <- df_costs %>%
	mutate(spend = days * costs) %>%
	group_by(id, income) %>%
	summarise(days = sum(days),
			  spend = sum(spend)) %>%
	mutate(num_exp = income - spend) %>%		  
	filter(num_exp > 0) %>%
	ungroup(.) %>%
	select(id) %>%
	as.matrix()

df_mdcev <- df_costs %>%
	filter(id %in% keep_expend) %>%
	mutate(good = as.numeric(as.factor(activity))) %>%
	rename(quant = days,
		   price = costs) %>%
	arrange(id, activity)
	
summary(df_mdcev)

write_csv(df_mdcev, "data/clean/AverageTravelCost.csv")

rm(list=ls())