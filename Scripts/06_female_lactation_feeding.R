#load packages
source("Scripts/00-packages.R")

#read in data --------------------------------------------------------
feeding <- read.csv("Input/feeding_distances_all.csv")
tree_cones <- read.csv("Input/tree_cones.csv")
# mushrooms <- read.csv("Input/mushrooms.csv")
#
# #filter for within territory
# feeding_within_territory <- feeding %>%
#   filter(within_territory == TRUE)

# remove years following a cone crop failure ------------------------
failed_years <- tree_cones %>%
  filter(cone_index < 1) %>%
  pull(year)

years_to_remove <- failed_years + 1

# female feeding during lactation -----------------------
feeding_lactation <- feeding %>%
  filter(sex == "F", repro_stage == "lactation",
         !year %in% years_to_remove)
  
length(unique(feeding_lactation$squirrel_id))

length(unique(feeding_lactation$year))
unique(feeding_lactation$year)

#save
write.csv(feeding_lactation, "Input/female_lactation_feeding.csv", row.names = FALSE)

