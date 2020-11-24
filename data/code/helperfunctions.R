# Some helper functions for creating data

ComputeTimeCosts <- function(cost_trans, income, opp_time, km_hr, vehicle_cost){
	x <- (cost_trans / vehicle_cost / km_hr) *  (opp_time * income / yearly_hours)
	return(x)
}

CalcTimeCosts <- function(data, opp_time){
	data <- data %>%
		group_by(activity, province) %>%
		mutate(cost_time = ComputeTimeCosts(cost_trans_impute *n_people, # multiply by number of people to get total costs
			   							 income, opp_time, km_hr, vehicle_cost),
			   costs = cost_equip + cost_accom_impute + cost_trans_impute + cost_time,
			   opp_time = opp_time) %>%
		ungroup(.) %>%
		select(id, activity, adjust, costs, cost_equip, cost_accom_impute, cost_trans_impute, cost_time, opp_time)
	
	return(data)
}

ImputeMissingData <- function(impute.data, other.data, delta,
							  maxit, n_impute){

	#other.data$imputation <- NULL
	impute.data <- impute.data %>%
		bind_rows(other.data) %>%
		arrange(id, activity)
	
#	impute.data <- dummy_cols(impute.data, select_columns = c("province"))
	
	ini <- mice(impute.data, maxit=0, print=F)
	
	pred <- ini$pred
	pred[, "id"] <- 0
	
	post <- ini$post
	
	cmd <- paste("imp[[j]][,i] <- imp[[j]][,i] *", delta)
	post["cost_trans_impute"] <- cmd
	post["cost_accom_impute"] <- cmd
	
	imp <- mice(impute.data, post = post, pred = pred, 
				seed = seed, method= 'pmm',
				m = 1, maxit = maxit, print = F)
	imp.all <- complete(imp,"long")
	imp.all$adjust <- delta
	
	imp.all <- imp.all %>%
		select(id, province, activity, cost_trans_impute, cost_accom_impute, days, income, adjust)
	
	return(imp.all)
}
