#load packages
source("Scripts/00-packages.R")

# read in data ----------------------------
feeding <- read.csv("Input/all_feeding_census.csv")
midden_cones <- read.csv("Input/midden_cones.csv")

feeding <- feeding %>%
  mutate(date = as.Date(date)) %>%
  na.omit()

#filter out unnecessary columns
feeding <- feeding %>%
  dplyr::select(-locx_obs, -locy_obs, -snow, -locx_census, -locy_census)

#how many ind squirrels?
length(unique(feeding$squirrel_id))

# align feeding repro_stage with DEE seasons ------------------------------
feeding <- feeding %>%
  mutate(
    date = as.Date(date),  #ensure the `date` column is in Date format
    season = case_when(
      #assign 'winter' if it's non-breeding before May 1
      repro_stage == "non-breeding" & format(date, "%m-%d") < "05-01" ~ "winter",
      #keep other values as they are from repro_stage
      TRUE ~ repro_stage))


# adjust levels and data classes ---------------------------------------------
feeding <- feeding %>%
  mutate(food_type = factor(food_type),
         season = factor(season),
         sex = factor(sex),
         squirrel_id = factor(squirrel_id))

midden_cones <- midden_cones %>%
  mutate(squirrel_id = factor(squirrel_id))

feeding <- feeding %>%
  mutate(season = relevel(factor(season), ref = "non-breeding"),
         year = as.numeric(year)) %>%
  rename(feeding_year = year)

# join feeding and cone data ----------------------------------------------
#need to make sure cones from year 1 are being compared to feeding in year +1 (ex: 2022 cones comapred to 2023 feeding)
midden_cones_adjusted <- midden_cones %>%
  dplyr::rename(
    cache_size_new_prev_year = cache_size_new,
    total_cones_prev_year = total_cones,
    log_cache_size_new_prev_year = log_cache_size_new,
    log_total_cones_prev_year = log_total_cones) %>%
  mutate(year = as.numeric(year),
         following_year = year + 1)

#join feeding and cones based on year +1 from cones
feeding_cache <- left_join(feeding, midden_cones_adjusted %>%
                 dplyr::select(squirrel_id, following_year, 
                 cache_size_new_prev_year, total_cones_prev_year, 
                 log_cache_size_new_prev_year, log_total_cones_prev_year),
                 by = c("squirrel_id", "feeding_year" = "following_year")) %>%
                 na.omit()

# minimum number of feeding events per squirrel? --------------------------
#set minimum yearly feeding events to at least 30 per squirrel
feeding_counts <- feeding_cache %>%
  group_by(squirrel_id, feeding_year) %>%
  mutate(yearly_feeding_events = n()) %>%
  ungroup() %>%
  filter(yearly_feeding_events >= 30) #want to minimize error to be reasonable while maintaining total sample size

#group by squirrel_id, feeding_year and season
feeding_counts <- feeding_counts %>%
  group_by(squirrel_id, sex, feeding_year, season, log_cache_size_new_prev_year, log_total_cones_prev_year) %>%
  summarise(
    count_capital = sum(food_type == "capital"), 
    count_income = sum(food_type == "income"),
    total_count = count_capital + count_income, 
    .groups = "drop")

#filter for at least 30 events by squirrel * year * season combo
# feeding_counts <- feeding_counts %>%
#   filter(total_count >= 30)

# proportions -------------------------------------------------------------
proportion_capital <- feeding_counts %>%
  mutate(proportion_capital = count_capital / total_count) %>%
  dplyr::select(-count_income)

# model -------------------------------------------------------------------
#adjust proportions to avoid exact 0s and 1s
proportion_capital$proportion_capital <- pmax(pmin(proportion_capital$proportion_capital, 1 - 1e-5), 1e-5)

#beta regression for continuous response constrained between 0 and 1 (ideal for proportions)
model <- glmmTMB(proportion_capital ~ log_cache_size_new_prev_year * sex + 
                 log_total_cones_prev_year + sex * season + (1 | squirrel_id), 
                 family = beta_family(link = "logit"),
                 data = proportion_capital) 

summary(model)

#check residuals
residuals_model <- simulateResiduals(model)
plot(residuals_model)




