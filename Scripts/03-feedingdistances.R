#load packages
source("Scripts/00-packages.R")

#read in data
feeding <- read.csv("Input/all_feeding_census.csv")

#locx's to numeric and remove NAs
feeding <- feeding %>%
  mutate(
    locx_census_numeric = loc_to_numeric(locx_census), 
    locx_obs_numeric = loc_to_numeric(locx_obs)) %>%
  na.omit()

#fix one weird grid entry
feeding$grid[feeding$squirrel_id == 20942] <- "SU"

# create midden buffers to represent territory sizes and middens "islands" --------------------------
#define the buffer sizes in "grid logic"
buffer_radius <- 52 / 30 #52m buffer in grid terms - max territory size of 0.86ha according to LaMontagne
midden_radius <- 5 / 30 #5m buffer in grid terms

#add buffer to midden locs = territory
feeding <- feeding %>%
  mutate(buffer_radius = buffer_radius)

#add buffer to midden locs = midden island
feeding <- feeding %>%
  mutate(midden_radius = midden_radius)

#plot with a panel for each grid
census_mids_buff <- ggplot(feeding, aes(x = locx_census_numeric, y = as.numeric(locy_census))) +
  #plot midden locations, colored by grid
  geom_point(aes(color = grid), size = 1, show.legend = FALSE) +
  #add the 52m buffer (territory size)
  geom_circle(
    aes(
      x0 = locx_census_numeric, 
      y0 = as.numeric(locy_census),
      r = buffer_radius),
    color = "red", fill = NA, alpha = 0.5) +
  #add the 5m buffer (midden "island")
  geom_circle(
    aes(
      x0 = locx_census_numeric, 
      y0 = as.numeric(locy_census),
      r = midden_radius),
    color = "blue", fill = NA, alpha = 0.5) +
  #facet by grid
  facet_wrap(~ grid, scales = "free") + 
  #add labels and titles
  labs(
    title = "Middens and their territory radius by Grid",
    x = "locx",
    y = "locy") +
  # Apply minimal theme
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5))

census_mids_buff

#save
ggsave("Output/census_mids_buff.jpeg", plot = census_mids_buff, width = 8, height = 6)

# distance feeding obs to midden ------------------------------------------
buffer_radius_meters <- 52 #52m radius
midden_radius_meters <- 5 #5m midden "island"

#calculate Euclidean distance and determine if within buffer
feeding_distances <- feeding %>%
  filter(food_type == "capital") %>% #filter for only old/cached cone feeding events
  mutate(
    #calculate Euclidean distance (in meters)
    distance_to_midden = sqrt(
      ((locx_obs_numeric - locx_census_numeric) * 30)^2 + #convert x-difference to meters
        ((as.numeric(locy_obs) - as.numeric(locy_census)) * 30)^2 #convert y-difference to meters
    ),
    #check if the observation is within the buffer radius and within the midden "island"
    within_territory = distance_to_midden <= buffer_radius_meters,
    within_midden = distance_to_midden <= midden_radius_meters) %>%
  na.omit()

#save
write.csv(feeding_distances, "Input/feeding_distances.csv", row.names = FALSE)

# descriptive statistics --------------------------------------------------
##calculate summary statistics of distance_to_midden by sex
feeding_distances %>%
  group_by(sex) %>%
  summarize(
    mean_distance = mean(distance_to_midden, na.rm = TRUE),
    median_distance = median(distance_to_midden, na.rm = TRUE),
    max_distance = max(distance_to_midden, na.rm = TRUE))

summary(feeding_distances$distance_to_midden)

#let's do that again but only for feeding events happening within territory (to exclude crazy distances likely caused by observer error)
feeding_within_territory <- feeding_distances %>%
  filter(within_territory == TRUE)

feeding_summary <- feeding_within_territory %>%
  mutate(
    location = case_when(
      within_midden ~ "on_midden",    #feeding event within midden radius
      within_territory ~ "off_midden" #feeding event within territory but outside midden radius
    )) %>%
  group_by(sex, location, snow, year_type) %>% #group by sex and location (on/off midden)
  summarize(
    mean_distance = mean(distance_to_midden, na.rm = TRUE),
    median_distance = median(distance_to_midden, na.rm = TRUE),
    max_distance = max(distance_to_midden, na.rm = TRUE),
    sd_distance = sd(distance_to_midden, na.rm = TRUE),
    n = n(),
    .groups = "drop")

feeding_summary

write.csv(feeding_summary, "Output/feeding_summary.csv", row.names = FALSE)
