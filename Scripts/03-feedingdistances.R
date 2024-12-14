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

# create midden buffers to represent territory sizes --------------------------
#define the buffer size in "grid logic"
buffer_radius <- 52 / 30 #52m buffer in grid terms - max territory size of 0.86ha according to LaMontagne

#add buffer to midden locs = territory
feeding <- feeding %>%
  mutate(buffer_radius = buffer_radius)

#plot with a panel for each grid
census_mids_buff <- ggplot(feeding, aes(x = locx_census_numeric, y = as.numeric(locy_census))) +
  geom_point(aes(color = grid), size = 2, show.legend = FALSE) +
  geom_circle(
    aes(
      x0 = locx_census_numeric, 
      y0 = as.numeric(locy_census),
      r = buffer_radius),
    color = "red", fill = NA, alpha = 0.5) +
  facet_wrap(~ grid, scales = "free") + 
  labs(
    title = "Middens with 52m Buffers by Grid",
    x = "locx",
    y = "locy") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5))

census_mids_buff

ggsave("Output/census_mids_buff.jpeg", plot = census_mids_buff, width = 8, height = 6)

# distance feeding obs to midden ------------------------------------------
buffer_radius_meters <- 52 #52m radius

#calculate Euclidean distance and determine if within buffer
feeding_distances <- feeding %>%
  mutate(
    #calculate Euclidean distance (in meters)
    distance_to_midden = sqrt(
      ((locx_obs_numeric - locx_census_numeric) * 30)^2 + #convert x-difference to meters
        ((as.numeric(locy_obs) - as.numeric(locy_census)) * 30)^2 #convert y-difference to meters
    ),
    #check if the observation is within the buffer radius
    within_territory = distance_to_midden <= buffer_radius_meters) %>%
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
  group_by(sex) %>%
  summarize(
    mean_distance = mean(distance_to_midden, na.rm = TRUE),
    median_distance = median(distance_to_midden, na.rm = TRUE),
    max_distance = max(distance_to_midden, na.rm = TRUE),
    sd_distance = sd(distance_to_midden, na.rm = TRUE),
    n = n()
  )

feeding_summary

write.csv(feeding_summary, "Output/feeding_summary.csv", row.names = FALSE)
