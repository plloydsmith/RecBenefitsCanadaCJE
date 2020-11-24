
# This scripts creates
# Table 2
# Table B-3

rm(list=ls(all=TRUE))
ls()

# Load Packages ------------------------------------#
#library(pacman)

library(tidyverse)
library(xtable)

source("code/TableFunctions.R")
# Import Data ------------------------------------#

activity <- read_csv("temp/activity_names.csv") %>%
	mutate(good = good) %>%
	select(good, activity_new) %>%
	rename(activity = activity_new)

addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c("Activity-specific & \\multicolumn{2}{c}{$\\psi_k$} & \\multicolumn{2}{c}{$\\gamma_k$} \\\\\n",
					  "parameters & mean & sd & mean & sd \\\\\n")


load("output/mdcev_national_estimation_w.RData")


parms <- map(mdcev_sims$par_est, function(x){
	x$id_sample <- NULL
	return(unlist(x))})

parms <- bind_rows(lapply(parms, as.data.frame.list))

table_ests <- CreateEstimateTableWide(parms, activity) 

parm_names <- tibble(parm_names = c(activity$activity, 
						  "Satiation parameter ($\\alpha_1$)", 
						  "Scale parameter ($\\sigma$)", "N observations", "Log-likelihood"))

table_ests <- bind_cols(parm_names, table_ests) 


table_export <- xtable(table_ests, 
					   include.rownames = T, type = "latex")
align(table_export) <- "lrrrrr"

#-----------------------------------------------
# Table 2
#-----------------------------------------------

print(xtable(table_ests), 
	  include.rownames = FALSE, 
	  add.to.row = addtorow, 
	  include.colnames = FALSE, 
		sanitize.rownames.function = function(x) {x},
	  hline.after = c(0, 17, 19), 
	  booktabs = TRUE,
	  file = "temp/mdcev_national_w.tex")


load("output/mdcev_provincial_estimation_w.RData")


addtorow <- list()
addtorow$pos <- list(0, 0, 0, 0, 0, 17, 17)
addtorow$command <- c("& \\multicolumn{2}{c}{BC} & \\multicolumn{2}{c}{Alberta} & \\multicolumn{2}{c}{Yukon} & \\multicolumn{2}{c}{NWT} \\\\\n",
						" \\hline\n",
					  "& mean & sd & mean & sd & mean & sd & mean & sd \\\\\n",
					  " \\hline\n",
					  "\\multicolumn{9}{l}{\\textbf{Marginal utility parameters ($Q_k$)}} \\\\\n",
					  " \\hline\n",
					  "\\multicolumn{9}{l}{\\textbf{Translation parameters ($\\gamma_k$)}} \\\\\n")

parms <- df_parms[["BC"]][["parms"]]

table_ests <- CreateEstimateTableLong(parms, activity) 

parms <- df_parms[["AB"]][["parms"]]

table_ests2 <- CreateEstimateTableLong(parms, activity) %>%
	select(-activity)

parms <- df_parms[["YT"]][["parms"]]

table_ests3 <- CreateEstimateTableLong(parms, activity) %>%
	select(-activity)

parms <- df_parms[["NT"]][["parms"]]

table_ests4 <- CreateEstimateTableLong(parms, activity) %>%
	select(-activity)

table_est <- cbind(table_ests, table_ests2, table_ests3, table_ests4)

table_export <- xtable(table_est, type = "latex")
align(table_export) <- "llrlrlrlrl"

#-----------------------------------------------
# Table B-3 (1/3)
#-----------------------------------------------

print(table_export, 
	  	  include.rownames = FALSE, 
	  hline.after = c(34,36), 
	  add.to.row = addtorow, 
	  include.colnames = FALSE, 
	  sanitize.rownames.function = function(x) {x},
	  booktabs = TRUE,
	  file = "temp/mdcev_prov1.tex")


addtorow$command <- c("& \\multicolumn{2}{c}{Saskatchewan} & \\multicolumn{2}{c}{Manitoba} & \\multicolumn{2}{c}{Ontario} & \\multicolumn{2}{c}{Quebec} \\\\\n",
					  " \\hline\n",
					  "& mean & sd & mean & sd & mean & sd & mean & sd \\\\\n",
					  " \\hline\n",
					  "\\multicolumn{9}{l}{\\textbf{Marginal utility parameters ($\\psi_k$)}} \\\\\n",
					  " \\hline\n",
					  "\\multicolumn{9}{l}{\\textbf{Translation parameters ($\\gamma_k$)}} \\\\\n")

parms <- df_parms[["SK"]][["parms"]]

table_ests <- CreateEstimateTableLong(parms, activity) 

parms <- df_parms[["MB"]][["parms"]]

table_ests2 <- CreateEstimateTableLong(parms, activity) %>%
	select(-activity)

parms <- df_parms[["ON"]][["parms"]]

table_ests3 <- CreateEstimateTableLong(parms, activity) %>%
	select(-activity)

parms <- df_parms[["QC"]][["parms"]]

table_ests4 <- CreateEstimateTableLong(parms, activity) %>%
	select(-activity)

table_est <- cbind(table_ests, table_ests2, table_ests3, table_ests4)

table_export <- xtable(table_est, type = "latex")
align(table_export) <- "llrlrlrlrl"

#-----------------------------------------------
# Table B-3 (2/3)
#-----------------------------------------------

print(table_export, 
	  include.rownames = FALSE, 
	  hline.after = c(34,36), 
	  add.to.row = addtorow, 
	  include.colnames = FALSE, 
	  sanitize.rownames.function = function(x) {x},
	  booktabs = TRUE,
	  file = "temp/mdcev_prov2.tex")


addtorow$command <- c("& \\multicolumn{2}{c}{New Brunswick} & \\multicolumn{2}{c}{PEI} & \\multicolumn{2}{c}{Nova Scotia} & \\multicolumn{2}{c}{NFLD \\& Labrador} \\\\\n",
					  " \\hline\n",
					  "& mean & sd & mean & sd & mean & sd & mean & sd \\\\\n",
					  " \\hline\n",
					  "\\multicolumn{9}{l}{\\textbf{Marginal utility parameters ($\\psi_k$)}} \\\\\n",
					  " \\hline\n",
					  "\\multicolumn{9}{l}{\\textbf{Translation parameters ($\\gamma_k$)}} \\\\\n")

parms <- df_parms[["NB"]][["parms"]]

table_ests <- CreateEstimateTableLong(parms, activity) 

parms <- df_parms[["PE"]][["parms"]]


table_ests2 <- CreateEstimateTableLong(parms, activity) %>%
	select(-activity)

parms <- df_parms[["NS"]][["parms"]]

table_ests3 <- CreateEstimateTableLong(parms, activity) %>%
	select(-activity)

parms <- df_parms[["NL"]][["parms"]]

table_ests4 <- CreateEstimateTableLong(parms, activity) %>%
	select(-activity)

table_est <- cbind(table_ests, table_ests2, table_ests3, table_ests4)

table_export <- xtable(table_est, type = "latex")
align(table_export) <- "llrlrlrlrl"

#-----------------------------------------------
# Table B-3 (3/3)
#-----------------------------------------------

print(table_export, 
	  include.rownames = FALSE, 
	  hline.after = c(34,36), 
	  add.to.row = addtorow, 
	  include.colnames = FALSE, 
	  sanitize.rownames.function = function(x) {x},
	  booktabs = TRUE,
	  file = "temp/mdcev_prov3.tex")

