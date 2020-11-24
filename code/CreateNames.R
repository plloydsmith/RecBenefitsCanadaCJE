
activity <- read_csv("data/clean/AverageTravelCost.csv") %>%
	distinct(good, activity) %>%
	mutate(good = as.character(good),
		   activity_full = case_when(activity == "birding" ~ "Birding",
		   						 activity == "camping" ~ "Camping",
		   						 activity == "cycling" ~ "Cycling",
		   						 activity == "fish" ~ "Fishing",
		   						 activity == "garden" ~ "Gardening",
		   						 activity == "golf" ~ "Golfing",
		   						 activity == "hiking" ~ "Hiking",
		   						 activity == "hunt_birds" ~ "Hunting birds",
		   						 activity == "hunt_large" ~ "Hunting large game",
		   						 activity == "hunt_other" ~ "Hunting other",
		   						 activity == "hunt_waterfowl" ~ "Hunting waterfowl",
		   						 activity == "land_motor" ~ "Motorized land vehicles",
		   						 activity == "photo" ~ "Photography/filming",
		   						 activity == "ski_cross" ~ "Cross-country skiing",
		   						 activity == "ski_down" ~ "Alpine skiing/snowboarding",
		   						 activity == "water_motor" ~ "Motorized boating",
		   						 activity == "water_nonmotor" ~ "Beach/non-motorized boating",
		   						 TRUE ~ activity )) %>%
	arrange(activity_full)


write_csv(activity, "temp/activity_names.csv")