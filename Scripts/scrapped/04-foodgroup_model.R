#load packages
source("Scripts/00-packages.R")

#read in data --------------------------------------------------------
feeding <- read.csv("Input/feeding_distances_all.csv")

# define DEE values (kJ/day) --------------------------------------------------------------
DEE_values <- data.frame(
  sex = c("M", "M", "M", "F", "F", "F"),
  season = c("winter", "mating", "non-breeding", "winter", "lactation", "non-breeding"),
  DEE_kJ_day = c(207, 407, 318, 207, 470, 318)) #assuming winter and non-breeding are the same between sexes

# align feeding repro_stage with DEE seasons ------------------------------
feeding <- feeding %>%
  mutate(
    date = as.Date(date),  #ensure the `date` column is in Date format
    season = case_when(
      #assign 'winter' if it's non-breeding before May 1
      repro_stage == "non-breeding" & format(date, "%m-%d") < "05-01" ~ "winter",
      #keep other values as they are from repro_stage
      TRUE ~ repro_stage))

# distribution of feeding events? -----------------------------------------
feeding_distribution <- feeding %>%
  group_by(sex, season) %>%
  summarise(total_events = n(), .groups = "drop")

ggplot(feeding_distribution, aes(x = season, y = total_events, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Feeding Events by Season and Sex",
       x = "Season",
       y = "Number of Feeding Events") +
  theme_minimal()

individual_representation <- feeding %>%
  group_by(sex, season) %>%
  summarise(
    unique_individuals = n_distinct(squirrel_id),
    total_events = n()) %>%
  ungroup()

ggplot(individual_representation, aes(x = season, y = unique_individuals, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Squirrel_IDs in Feeding Observations by Season and Sex",
       x = "Season",
       y = "Number of Individual Squirrels") +
  theme_minimal()

#unique individuals is pretty similar.... 


# create 3 food groups: on-mid/off-mid cones and other ----------------
feed <- feeding %>%
  mutate(food_group = case_when(
    detail == 2 & within_midden == TRUE  ~ "on_midden_cones",
    detail == 2 & within_midden == FALSE ~ "off_midden_cones",
                                   TRUE  ~ "other")) %>%
  dplyr::select(-food_type, -repro_stage, -year, -buffer_radius, -midden_radius, -locx_obs, -locy_obs, -locx_census, -locy_census, -locx_census_numeric, -locx_obs_numeric, -distance_to_midden)

#three-level factor for feeding, and make sure sex and season are factors
feed <- feed %>%
  mutate(
    food_group = factor(food_group, levels = c("on_midden_cones", "off_midden_cones", "other")),
    sex = factor(sex, levels = c("F", "M")),
    season = factor(season, levels = c("winter", "mating", "lactation", "non-breeding")))

# model -------------------------------------------------------------------
#fit a multinomial model with a random intercept for individual squirrels:
model_multinom <- mblogit(
  food_group ~ sex * season, 
  data = feed,
  random =  ~ 1 | squirrel_id)

summary(model_multinom)


# generate predictions and plot -------------------------------------------
emm <- emmeans(model_multinom, ~ sex * season, type = "response")
emm_df <- as.data.frame(emm)






