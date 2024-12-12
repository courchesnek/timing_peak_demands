#load packages
source("Scripts/00-packages.R")

#read in data
feeding <- read.csv("Input/feeding_census.csv")

#locx's to numeric
feeding <- feeding %>%
  mutate(
    locx_census_numeric = loc_to_numeric(locx_census), 
    locx_obs_numeric = loc_to_numeric(locx_obs))


# create midden buffers to represent territory sizes --------------------------
#define the buffer size in "grid logic"
buffer_radius <- 20 / 30 #20m buffer in grid terms

#add buffer to midden locs = territory
feeding <- feeding %>%
  mutate(buffer_radius = buffer_radius)

#plot to see if it worked
census_mids_buff <- ggplot(feeding, aes(x = locx_census_numeric, y = as.numeric(locy_census))) +
  geom_point(color = "blue", size = 3) +
  geom_circle(
    aes(
      x0 = locx_census_numeric, 
      y0 = as.numeric(locy_census),
      r = buffer_radius
    ),
    color = "red", fill = NA
  ) + # Draw circular buffers
  labs(
    x = "locx",
    y = "locy",
  ) +
  theme_minimal()

census_mids_buff

ggsave("Output/census_mids_buff.jpeg", plot = census_mids_buff, width = 8, height = 6)



# distance feeding obs to midden ------------------------------------------
buffer_radius_meters <- 20 #20m radius

#calculate Euclidean distance and determine if within buffer
feeding <- feeding %>%
  mutate(
    #calculate Euclidean distance (in meters)
    distance_to_midden = sqrt(
      ((locx_obs_numeric - locx_census_numeric) * 30)^2 + #convert x-difference to meters
        ((as.numeric(locy_obs) - as.numeric(locy_census)) * 30)^2 #convert y-difference to meters
    ),
    #check if the observation is within the buffer radius
    within_territory = distance_to_midden <= buffer_radius_meters)

#remove NAs
feeding <- na.omit(feeding)

# descriptive statistics --------------------------------------------------
##calculate summary statistics of distance_to_midden by sex
feeding %>%
  group_by(sex) %>%
  summarize(
    mean_distance = mean(distance_to_midden, na.rm = TRUE),
    median_distance = median(distance_to_midden, na.rm = TRUE),
    max_distance = max(distance_to_midden, na.rm = TRUE))

#let's do that again but only for feeding events happening within territory (to exclude crazy distances likely caused by observer error)
feeding_within_territory <- feeding %>%
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

write.csv(feeding_summary, "Output/feeding_summary.csv", row.names = FALSE)

# stats: compare distance to midden between sexes -------------------------
##check distribution of data
ggplot(feeding_within_territory, aes(x = distance_to_midden)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Distance to Midden",
    x = "Distance to Midden (m)",
    y = "Frequency"
  ) +
  theme_minimal()

set.seed(123) # Set seed for reproducibility
subset_data <- feeding_within_territory %>%
  sample_n(5000)

shapiro.test(subset_data$distance_to_midden) #p-value <0.05 so data is NOT normally distributed

#let's do a Wilcoxon rank-sum test - comparing two groups (males and females) with continuous, non-normally distributed data
wilcox_test <- wilcox.test(distance_to_midden ~ sex, data = feeding_within_territory)
wilcox_test

#calculate effect size of the relationship
cliffs_delta <- cliff.delta(distance_to_midden ~ sex, data = feeding_within_territory)
cliffs_delta

#plot
ggplot(feeding_within_territory, aes(x = sex, y = distance_to_midden, fill = sex)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Feeding Distance to Midden by Sex",
    x = "Sex",
    y = "Distance to Midden (m)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(feeding_within_territory, aes(x = sex, y = distance_to_midden, fill = sex)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) + #add boxplot inside the violin
  labs(
    title = "Feeding Distance to Midden by Sex",
    x = "Sex",
    y = "Distance to Midden (m)") +
  scale_fill_manual(values = c("pink", "blue")) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#plot all middens and all feeding obs - suuuuper messy? --------------------------------------------------------
#all feeding
ggplot() +
  geom_circle(
    data = feeding,
    aes(
      x0 = locx_census_numeric,
      y0 = as.numeric(locy_census),
      r = buffer_radius),
    color = "red", fill = NA, alpha = 0.5) +
  geom_point(
    data = feeding,
    aes(x = locx_census_numeric, y = as.numeric(locy_census)),
    color = "blue", size = 3, alpha = 0.8) +
  geom_point(
    data = feeding,
    aes(x = locx_obs_numeric, y = as.numeric(locy_obs), color = sex),
    size = 1, alpha = 0.5) +
  labs(
    x = "locx (numeric)",
    y = "locy",
    color = "Sex of Feeding Event") +
  scale_color_manual(values = c("pink", "blue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#within_territory feeding
ggplot() +
  geom_circle(
    data = feeding_within_territory,
    aes(
      x0 = locx_census_numeric,
      y0 = as.numeric(locy_census),
      r = buffer_radius
    ),
    color = "red", fill = NA, alpha = 0.5
  ) +
  geom_point(
    data = feeding_within_territory,
    aes(x = locx_census_numeric, y = as.numeric(locy_census)),
    color = "blue", size = 3, alpha = 0.3
  ) +
  geom_point(
    data = feeding_within_territory,
    aes(x = locx_obs_numeric, y = as.numeric(locy_obs), color = sex),
    size = 1.5, alpha = 0.8) +
  labs(
    x = "locx (numeric)",
    y = "locy",
    color = "Sex of Feeding Event") +
  scale_color_manual(values = c("pink", "blue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))





















