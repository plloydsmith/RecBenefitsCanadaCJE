
# This scripts creates
# Table 1
# Table 3
# Table 4
# Figure 2

rm(list=ls(all=TRUE))
ls()

# Load Packages ------------------------------------#
#library(pacman)

library(tidyverse)
library(xtable)
library(scales)

# Import Data ------------------------------------#

activity <- read_csv("temp/activity_names.csv")

text_size <- 32

df <- read_csv("temp/wtp_national.csv") 


df_pop <- read_csv("Data/clean/population.csv") %>%
	select(-name) %>%
	pivot_longer(-province, names_to = "year", values_to = "population") %>%
	mutate(year = as.numeric(gsub("[^0-9]", "", year))) %>%
	filter(year == 2012) %>%
	select(-year) %>%
	filter(province != "CA", province != "NU") %>%
	mutate(population = round(population / 1000000,2),
		   province = as.factor(province),
		   province = factor(province, levels(province)[c(2,1,11,3,8,10,4,9,6,5,12,7)]))



df_activity_table <- activity %>%
	select(activity, activity_full)


addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- c("Activity & Full activity description \\\\\n")


table_export <- xtable(df_activity_table, type = "latex")
align(table_export) <- "lll"
digits(table_export) <- 0


#-----------------------------------------------
# Table 1
#-----------------------------------------------

print(table_export, 
	  include.rownames = FALSE, 
	  add.to.row = addtorow, 
	  include.colnames = FALSE, 
	  sanitize.rownames.function = function(x) {x},
	  hline.after = c(0, 17), 
	  booktabs = TRUE,
	  file = "temp/activity_table.tex")


df_weight <- df %>%
	filter(weighted == "yes") %>%
	left_join(activity, by = "activity") %>%
	select(activity_full, wtp_m, wtp_sd, wtp_hi, wtp_lo) %>%
	arrange(activity_full) %>%
	mutate(activity_full = ifelse(is.na(activity_full), "All", activity_full)) %>%
	mutate_each(~(.*-1), -activity_full, -wtp_sd) %>%
	mutate_each(~(dollar_format()(c(.))), -activity_full)


# National model total
sum <- df %>%
	filter(weighted == "yes") %>%
	left_join(activity, by = "activity") %>%
	select(activity_full, wtp_m, wtp_sd, wtp_hi, wtp_lo) %>%
	arrange(activity_full) %>%
	mutate(activity_full = ifelse(is.na(activity_full), "All", activity_full)) %>%
	filter(activity_full == "All") %>%
	mutate_each(~(.*-1), -activity_full, -wtp_sd)

df_pop %>%
	summarise(pop = sum(population) *sum$wtp_m)

df %>%
	filter(weighted == "yes") %>%
	left_join(activity, by = "activity") %>%
	select(activity_full, wtp_m, wtp_sd, wtp_hi, wtp_lo) %>%
	arrange(activity_full) %>%
	mutate(activity_full = ifelse(is.na(activity_full), "All", activity_full)) %>%
	filter(activity_full != "All") %>%
	mutate_each(~(.*-1), -activity_full, -wtp_sd) %>%
	summarise(sum = sum(as.numeric(wtp_m)))

df_part <- read_csv("temp/wtp_national_participant.csv") %>%
	filter(weighted == "yes") %>%
	left_join(activity, by = "activity") %>%
	select(activity_full, wtp_m, wtp_sd, wtp_hi, wtp_lo) %>%
	arrange(activity_full) %>%
	mutate(activity_full = ifelse(is.na(activity_full), "All", activity_full)) %>%
	mutate_each(~(.*-1), -activity_full, -wtp_sd) %>%
	mutate_each(~(dollar_format()(c(.))), -activity_full)

# Combined

df_all <- tibble(activity_full = "All",
				 wtp_m = "-",
				 wtp_sd = "-",
				 wtp_hi = "-", 
				 wtp_lo = "-")

df_part <- df_part %>%
	select(-wtp_sd) %>%
	bind_rows(df_all)

df_combined <- df_weight %>%
	select(-wtp_sd) %>%
	left_join(df_part, by = "activity_full")

addtorow <- list()
addtorow$pos <- list(0, 0, 0)
addtorow$command <- c("& \\multicolumn{3}{c}{Per person} & \\multicolumn{3}{c}{Per participant} \\\\\n",
					  " \\hline\n",
					  "Activity & mean & low & high & mean & low & high \\\\\n")


table_export <- xtable(df_combined, type = "latex")
align(table_export) <- "llrrrrrrr"
digits(table_export) <- 0

#-----------------------------------------------
# Table 3
#-----------------------------------------------

print(table_export, 
	  include.rownames = FALSE, 
	  add.to.row = addtorow, 
	  include.colnames = FALSE, 
	  sanitize.rownames.function = function(x) {x},
	  hline.after = c(0, 17), 
	  booktabs = TRUE,
	  file = "temp/welfare_national_w_combined.tex")

df_prov_w <- read_csv("temp/wtp_prov_w.csv") %>%
	left_join(activity, by = "activity") %>%
	select(province, activity_full, wtp_m, wtp_sd, wtp_hi, wtp_lo) %>%
	arrange(activity_full) %>%
	mutate(activity_full = ifelse(is.na(activity_full), "All", activity_full))  %>%
	mutate(province = as.factor(province),
		   province = factor(province, levels(province)[c(2,1,11,3,8,10,4,9,6,5,12,7)])) %>%
	arrange(province, activity_full)

df_weight_w <- df_prov_w %>%
	left_join(df_pop, by = "province") %>%
	mutate_each(~(.*population), -province, -population, -activity_full) %>% 
	select(province, activity_full, population, wtp_m, wtp_sd, wtp_hi, wtp_lo) %>%
	arrange(activity_full) %>%
	mutate(activity_full = ifelse(is.na(activity_full), "All", activity_full))

welfare_total_w <- df_weight_w %>%
	filter(activity_full == "All") %>%
	select(-activity_full, -population) %>%
	mutate_each(~(.*-1), -province, -wtp_sd) %>%
	mutate_each(~(dollar_format()(c(round(.,0)))), -province)

df_weight_w %>%
	filter(activity_full == "All") %>%
	select(-activity_full,-province) %>%
	colSums()

welfare_per <- df_prov_w %>%
	filter(activity_full == "All") %>%
	select(-activity_full) %>%
	mutate_each(~(.*-1), -province, -wtp_sd) %>%
	mutate_each(~(dollar_format()(c(.))), -province)


# Combined
welfare_total_w <- welfare_total_w %>%
	select(-wtp_sd)

df_combined <- welfare_per %>%
	select(-wtp_sd) %>%
	left_join(df_pop, by = "province") %>%
	left_join(welfare_total_w, by = "province")

addtorow <- list()
addtorow$pos <- list(0, 0,0)
addtorow$command <- c("& \\multicolumn{3}{c}{Per person} & Population & \\multicolumn{3}{c}{Aggregate value (\\$ millions)} \\\\\n",
					  " \\hline\n",
					  "Province & mean & low & high &  (millions) & mean & low & high \\\\\n")


table_export <- xtable(df_combined, type = "latex", digits = c(0, 0, 0, 0, 0, 2, 0, 0, 0) )
align(table_export) <- "llrrrrrrr"
#digits(table_export) <- 0


#-----------------------------------------------
# Table 4
#-----------------------------------------------

print(table_export, 
	  include.rownames = FALSE, 
	  add.to.row = addtorow, 
	  include.colnames = FALSE, 
	  sanitize.rownames.function = function(x) {x},
	  hline.after = c(0, 12), 
	  booktabs = TRUE,
	  file = "temp/welfare_provincial_combined.tex")


df_sum <- df_weight %>%
	filter(activity_full != "All") %>%
	group_by(province) %>%
	summarise(wtp_m = sum(wtp_m))


df_weight <- df_weight %>%
	mutate_each(~(.*-1), -activity_full, -wtp_sd, -province) %>%
	mutate_each(~(dollar_format()(c(.))), -activity_full, -province) %>%
	arrange(province, activity_full)
	


df_wtp_day <- read_csv("temp/wtp_national_per_day.csv") %>%
	mutate(wtp_day = wtp_day *-1,
		   wtp_hi = wtp_hi *-1,
		   wtp_lo = wtp_lo *-1) %>%
	filter(weighted == "yes") %>%
	left_join(activity, by = "activity")



#-----------------------------------------------
# Figure 2
#-----------------------------------------------

df_wtp_day %>%
	mutate(activity_full = fct_reorder(as.factor(activity_full), wtp_day)) %>%
	ggplot(aes(y = wtp_day, x = activity_full)) +
#	geom_segment( aes(xend=activity_full, yend=0)) +
	#	geom_point(size=5) +
	geom_linerange(aes(ymin = wtp_lo, ymax = wtp_hi)) +
	geom_label(aes(label=wtp_day), size = 12) +
	#	geom_point(data=df_us_wtp, aes(y = wtp_day, x = activity), colour="blue", size=6) +
	coord_flip() +
	theme_bw() + 
	scale_y_continuous(labels = scales::dollar, limits = c(0,100)) +
	ylab("Average benefits per day") +
	xlab("Recreation activity") +
	theme(legend.title = element_text(size = text_size),
		  legend.text = element_text(size = text_size),
		  axis.text.x =	element_text(size = text_size, colour = "black"),
		  axis.text.y = element_text(size = text_size, colour = "black"),
		  axis.ticks = element_blank(),
		  axis.title  = element_text(size = text_size))

ggsave("temp/wtp_day.png", width =20, height =14 )
