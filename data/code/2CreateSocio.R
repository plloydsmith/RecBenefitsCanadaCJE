#-----------------------------------------------------------------------------
# Filename:           CreateSocio.R
# Created:            14/04/2018
#
# NOTES: Create respondent characteristic information used for imputation.
#
#-----------------------------------------------------------------------------

#------------------------------------#
# Import data
#------------------------------------#
df <- read_csv("data/raw/Raw2012Data.csv") 

table(df$Q30_Notrips)
summary(df$Q30_Overnt)
table(df$Q30_Overnt_Codes)
table(df$Q30_Sameday)
table(df$Q30_Sameday_Codes)
table(df$Q33)

summary(df$Q33)
max_donations <- 12000
socio <- df  %>%
	transmute(province = Wt_Province,
			  id = PUMFID,
			  indig = as.numeric(case_when(Q51 == 1 ~ "1", 
			  				  Q51 == 2 ~ "0", 
			  				  TRUE ~ "NA")),
		   imigrant = as.numeric(case_when(Q52 == 1 ~ "1", 
		   					 Q52 == 2 ~ "0", 
		   					 TRUE ~ "NA")),
			  income = if_else(Q54 == 1, 25000, 
					if_else(Q54 == 2, (49999-25000)/2+25000, 
					if_else(Q54 == 3, (74999-50000)/2+50000, 
					if_else(Q54 == 4, (99999-75000)/2+75000, 
					if_else(Q54 == 5, 150000, NULL))))),
		   participate_hunt = as.numeric(case_when(Q26a == 1 ~ "1", 
		   							 Q26a == 2 ~ "0", 
		   							 TRUE ~ "NA")),
			  age = as.numeric(ifelse(AgeClass < 77, AgeClass, "NA")),
			  male = as.numeric(case_when(Q46 == 1 ~ "1", 
			  							Q46 == 2 ~ "0", 
			  							TRUE ~ "NA")),
		urban = ifelse(Census_Urban == 1, 1, 0),
			  education = as.numeric(ifelse(Q53 < 77, Q53, "NA")),
		web_sample = ifelse(Sam_From == "Web", 1, 0),
		cabin = as.numeric(case_when(Q33 == 1 ~ "1", 
					   Q33 == 2 ~ "0", 
					   TRUE ~ "NA")),
		num_overnt_trips = Q30_Overnt,
		donations = ifelse(Q35 < max_donations, Q35, max_donations),
		weight_address = Address_Analysis_Weight / mean(Address_Analysis_Weight, na.rm = TRUE),
		weight = Combined_Analysis_Weight/ mean(Combined_Analysis_Weight))
#	dummy_cols(select_columns = "Province") #%>%
#	region = case_when(Province == "BC" ~ "BC",
#					   Province == "AB" ~ "AB",
#					   Province == "NB" | Province == "NL" | 
#					   Province == "NS" | Province == "PE" ~ "MT")) %>%


socio_vars <-c("province", "income", "indig", "imigrant", "age", "male", 
			   "urban", "education", "donations", "cabin", 
			   "num_overnt_trips", "web_sample")


summary(socio)

# Prepare data for imputation
impute.data <- socio %>% 
	select(all_of(socio_vars))

other.data <- socio %>% 
	select(!all_of(socio_vars))

# Impute Multivariate Imputation by Chained Equations
tempData <- mice(impute.data, m=5, maxit=5, meth='pmm',seed=500)
impute.data <- complete(tempData,1)

socio.impute <- cbind(other.data, impute.data)

summary(socio.impute)
table(socio.impute$income)

socio.impute %>%
	group_by(province) %>%
	summarise(count = n(),
			  weight  = mean(weight))

socio.impute <- socio.impute %>%
	mutate(university = ifelse(education > 4, 1, 0),
		   college = ifelse(education == 4, 1, 0),
		   ageindex  = age / mean(age),
		   donate = ifelse(donations > 0, 1, 0)) %>%
	select(-participate_hunt, -weight_address, -donations, -donate)  %>%
	mutate(region = case_when(province == "AB" | province == "SK" | province == "MB" ~ "prairies",
							  province == "NT" | province == "YT"~ "north",
							  province == "NB" | province == "NS" | province == "PE" | province == "NL" ~ "maritimes",
							  TRUE ~ province))
		   

write_csv(socio.impute, "data/clean/SocioImpute.csv")
write_csv(socio, "data/clean/Socio.csv")

rm(list=ls())