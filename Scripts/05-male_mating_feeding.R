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

# investigate sample sizes ------------------------------------------------
feeding_numbers <- feeding %>%
  filter(repro_stage == "mating", sex == "M") %>%
  group_by(year) %>%
  summarise(total_events = n(), .groups = "drop")

# keep only feeding obs before April 30th ---------------------------
mating_lac <- read.csv("Input/reproductive_windows.csv")

# identify years when mating ends on/before April 30th and years that extend beyond April 30th
## we will cut off feeding obs beyond April 30th for those years
mating_lac <- mating_lac %>%
  mutate(
    mating_end = as.Date(mating_end),
    april_30 = ymd(paste0(year, "-04-30")), # create a column for April 30th on each row
    cutoff = pmin(mating_end, april_30))  # if mating_end is after Apr 30, Apr 30 is cutoff, if mating_end is before April 30th, that date is the cutoff instead

# join with feeding table
feeding_mating <- feeding %>%
  filter(sex == "M") %>%
  mutate(date = as.Date(date)) %>%
  left_join(mating_lac %>% dplyr::select(year, cutoff), by = "year") %>%
  filter(date <= cutoff)

# remove years following a cone crop failure ------------------------
failed_years <- tree_cones %>%
  filter(cone_index < 0.6) %>%
  pull(year)

years_to_remove <- failed_years + 1

# male feeding during mating -----------------------
feeding_mating <- feeding_mating %>%
  filter(!year %in% years_to_remove) %>%
  dplyr::select(-cutoff)  #remove cutoff column

length(unique(feeding_mating$squirrel_id))

length(unique(feeding_mating$year))
unique(feeding_mating$year)

#save
write.csv(feeding_mating, "Input/male_mating_feeding.csv", row.names = FALSE)
