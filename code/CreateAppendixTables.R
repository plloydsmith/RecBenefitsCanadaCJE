
# This scripts creates
# Figure 1
# Table B-1
# Table B-2

#-----------------------------------------------------------------------------
rm(list=ls(all=TRUE))
ls()

#------------------------------------#
# Load Packages
#------------------------------------#
library(tidyverse)
library(xtable)

# Import Data ------------------------------------#

activity <- read_csv("temp/activity_names.csv")


df_data <- read_csv("Data/clean/AverageTravelCost.csv") %>%
	select(id, activity, good, quant, price, income, province, weight) %>%
	mutate(province = as.factor(province),
		   province = factor(province, levels(province)[c(2,1,11,3,8,10,4,9,6,5,12,7)]))

df_data <- read_csv("Data/clean/AverageTravelCost.csv") %>%
	select(id, activity, good, quant, price, income, province, weight) %>%
	mutate(province = as.factor(province),
		   province = factor(province, levels(province)[c(2,1,11,3,8,10,4,9,6,5,12,7)]))

df_graph <- df_data %>%
	mutate(days = ifelse(quant > 0, 1, 0)) %>%
	group_by(id) %>%
	summarise(n_act = sum(days))%>%
	ungroup(.) %>%
	group_by(n_act) %>%
	summarise(count = n()) %>%
	mutate(ratio = round(count / sum(count)*100,1))

p1 <- ggplot(df_graph, aes(x=n_act,y=ratio, label = ratio)) +
	geom_bar(stat='identity') +
	geom_text(vjust=-.5) +
	xlab("Number of different recreational activities") + 
	ylab("Proportion of people (%)") +
	ylim(0,17) +
	scale_x_continuous(labels = c(0:17), breaks = c(0:17), limits=c(-0.5, 17.5)) +
	theme_bw()

p1

# Figure 1
ggsave("temp/multiple_activities.png", 
	   plot=p1, width =10, height =5)


df_days <- df_data %>%
	full_join(activity) %>%
	group_by(activity_full, province) %>%
	mutate(weight = weight / mean(weight)) %>%
	summarise(days = mean(quant*weight)) %>%
	rename(Activity = activity_full) %>%
	pivot_wider(names_from = province, values_from = days)


df_days_nat <- df_data %>%
	full_join(activity) %>%
	group_by(activity_full) %>%
	summarise(days = mean(quant*weight)) %>%
	rename(Activity = activity_full,
		   CA = days)

df_days <- left_join(df_days, df_days_nat, by = "Activity")

addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c()



table_export <- xtable(df_days, type = "latex")
align(table_export) <- "llrrrrrrrrrrrr|r"
digits(table_export) <- 1

# Table B-1
print(table_export, 
	  include.rownames = FALSE, 
	  sanitize.rownames.function = function(x) {x},
	  hline.after = c(0,17), 
	  booktabs = TRUE,
	  file = "temp/days_prov.tex")



# Average costs

df_days <- df_data %>%
	full_join(activity) %>%
	group_by(activity_full, province) %>%
	mutate(weight = weight / mean(weight)) %>%
	summarise(days = mean(quant*weight)) %>%
	rename(Activity = activity_full) %>%
	pivot_wider(names_from = province, values_from = days)

df_costs <- df_data %>%
	full_join(activity) %>%
	group_by(activity_full, province) %>%
	mutate(weight = weight / mean(weight)) %>%
	summarise(costs = round(mean(price*weight), 1)) %>%
	rename(Activity = activity_full) %>%
	pivot_wider(names_from = province, values_from = costs)


df_costs_nat <- df_data %>%
	full_join(activity) %>%
	group_by(activity_full) %>%
	summarise(costs = round(mean(price*weight), 1)) %>%
	rename(Activity = activity_full,
		   CA = costs)

df_costs <- left_join(df_costs, df_costs_nat, by = "Activity")

addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c()



table_export <- xtable(df_costs, type = "latex")
align(table_export) <- "llrrrrrrrrrrrr|r"
digits(table_export) <- 1

# Table B-2
print(table_export, 
	  include.rownames = FALSE, 
	  #	  add.to.row = addtorow, 
	  #	  include.colnames = FALSE, 
	  sanitize.rownames.function = function(x) {x},
	  hline.after = c(0,17), 
	  booktabs = TRUE,
	  file = "temp/costs_prov.tex")

