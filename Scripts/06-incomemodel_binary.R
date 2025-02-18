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

#summarise data ###########################################################

# part 1: compare proportion feeding on capital  ------------------------------
#create a binary response variable: capital = 1, income = 0
feeding_cache_binomial <- feeding_cache %>%
  mutate(food_type_bin = ifelse(food_type == "capital", 1, 0))  #capital = 1, income = 0

feeding_cache_binomial$food_type_bin <- factor(feeding_cache_binomial$food_type_bin, levels = c(0, 1))
feeding_cache_binomial$food_type_bin <- relevel(feeding_cache_binomial$food_type_bin, ref = "1")  #make capital the reference

#group by squirrel_id and count the number of feeding events per squirrel
feeding_counts <- feeding_cache %>%
  group_by(squirrel_id, sex, feeding_year, season) %>%
  summarise(feeding_events = n())

##add cache size to counts
feeding_counts <- feeding_counts %>%
  left_join(
    feeding_cache %>%
      dplyr::select(squirrel_id, sex, feeding_year, log_cache_size_new_prev_year) %>%
      distinct(),
    by = c("squirrel_id", "sex", "feeding_year"))

##some squirrels don't have enough obs - what should the threshold be?
subset(feeding_counts, feeding_events >= 1 & feeding_events <= 29) %>% nrow() #1331 squirrels with <30 feeding events in a year

#create a column to count feeding events by squirrel and by year
feeding_cache_binomial <- feeding_cache_binomial %>%
  group_by(squirrel_id, feeding_year) %>%
  mutate(feeding_events = n()) %>%
  ungroup()

#filter for a minimum of 30 feeding events 
feeding_cache_binomial_30 <- feeding_cache_binomial %>%
  group_by(squirrel_id, feeding_year) %>%
  mutate(feeding_events = n()) %>%
  ungroup() %>%
  filter(feeding_events >= 30) #want to minimize error to be reasonable while maintaining total sample size

#fit a GLMM model to predict capital feeding vs income feeding based on cones cached and sex
model <- glmer(food_type_bin ~ log_cache_size_new_prev_year * sex + log_total_cones_prev_year + sex * season + (1 | squirrel_id),
               family = binomial(link = "logit"),
               data = feeding_cache_binomial_30,
               control = glmerControl(optimizer = "bobyqa", 
               optCtrl = list(maxfun = 1000000)))  #increase maxfun (number of iterations) to fix convergence issues

#model reference categories?
contrasts(feeding_cache_binomial$food_type_bin) #capital is reference category
contrasts(feeding_cache_binomial$sex) #female is reference category
contrasts(feeding_cache_binomial$season) #non-breeding is reference category

model_summary <- summary(model)

#extract the model output into a tidy format
model_output <- tidy(model)

model_comparisons <- model_output %>%
  filter(term %in% c("log_cache_size_new_prev_year", 
                     "seasonlactation", 
                     "seasonmating", 
                     "seasonwinter",
                     "log_cache_size_new_prev_year:sexM",
                     "sexM:seasonlactation", 
                     "sexM:seasonmating", 
                     "sexM:seasonwinter")) %>%
  dplyr::select(-group, -effect)

#calculate the estimates and standard errors for the comparisons
model_comparisons <- model_comparisons %>%
  mutate(
    #for comparison of sex-specific effects with standard errors
    lower = estimate - std.error,
    upper = estimate + std.error)

#create a function to calculate if the confidence intervals overlap
compare_intervals <- function(main_effect, interaction_effect) {
  main_lower <- filter(model_comparisons, term == main_effect)$lower
  main_upper <- filter(model_comparisons, term == main_effect)$upper
  interaction_lower <- filter(model_comparisons, term == interaction_effect)$lower
  interaction_upper <- filter(model_comparisons, term == interaction_effect)$upper
  
  #check if the intervals overlap
  overlap <- !(interaction_upper < main_lower | interaction_lower > main_upper)
  return(overlap)
}

comparisons <- tibble(
  Comparison = c("log_cache_size_new_prev_year vs log_cache_size_new_prev_year:sexM", 
                 "seasonlactation vs sexM:seasonlactation", 
                 "seasonmating vs sexM:seasonmating", 
                 "seasonwinter vs sexM:seasonwinter"),
  Overlap = c(
    compare_intervals("log_cache_size_new_prev_year", "log_cache_size_new_prev_year:sexM"),
    compare_intervals("seasonlactation", "sexM:seasonlactation"),
    compare_intervals("seasonmating", "sexM:seasonmating"),
    compare_intervals("seasonwinter", "sexM:seasonwinter")))


# plots -------------------------------------------------------------------
















# part 2: compare feeding rates -------------------------------------------
#calculate feeding rates
feeding_rate <- feeding_cache_binomial_30 %>%
  mutate(feeding_date = as.Date(date)) %>%
  group_by(squirrel_id, feeding_year, sex, season, log_cache_size_new_prev_year, log_total_cones_prev_year, food_type) %>%
  summarise(
    #count the number of feeding events for the respective food type (capital or income)
    feeding_events = n(),
    #count the number of distinct feeding days for each food type
    feeding_days = n_distinct(feeding_date)) %>%
  ungroup() %>%
  #calculate feeding rate as the number of events divided by the number of feeding days
  mutate(
    feeding_rate = ifelse(feeding_days == 0, 0, feeding_events / feeding_days))

write.csv(feeding_rate, "Output/feeding_rate.csv", row.names = FALSE)

observed_variance <- var(feeding_rate$feeding_rate)
mean_feeding_rate <- mean(feeding_rate$feeding_rate)
dispersion <- observed_variance / mean_feeding_rate
# >1 indicates overdispersion - need to use negative binomial

#fit a negative binomial for feeding rate
model_feedingrate <- lmer(
  feeding_rate ~ food_type * season * sex + log_cache_size_new_prev_year * sex 
  + sex * season + log_total_cones_prev_year + (1 | squirrel_id),  #random effect for squirrel_id
  data = feeding_rate,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000)))

summary(model_feedingrate)

#model reference categories?
contrasts(feeding_rate$food_type) #capital is reference category
contrasts(feeding_rate$sex) #female is reference category
contrasts(feeding_rate$season) #non-breeding is reference category

#comparisons between groups - do CIs overlap?
model_feedingrate_output <- tidy(model_feedingrate) %>%
  dplyr::select(-group, -effect)

model_comparison <- model_feedingrate_output %>%
  mutate(
    #for comparison of sex-specific effects with standard errors
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error)

# Function to compare intervals
compare_interval <- function(main_effect, interaction_effect, model_comparison) {
  # Check if the terms exist in the model
  if(!(main_effect %in% model_comparison$term) | !(interaction_effect %in% model_comparison$term)) {
    return(NA)  # If either term doesn't exist, return NA
  }
  
  main_lower <- filter(model_comparison, term == main_effect)$lower
  main_upper <- filter(model_comparison, term == main_effect)$upper
  interaction_lower <- filter(model_comparison, term == interaction_effect)$lower
  interaction_upper <- filter(model_comparison, term == interaction_effect)$upper
  
  # Check if the intervals overlap
  overlap <- !(interaction_upper < main_lower | interaction_lower > main_upper)
  return(overlap)
}

# Create comparisons and check overlap
comparison <- tibble(
  Comparison = c("food_typeincome vs food_typeincome:sexM", 
                 "food_typeincome:seasonlactation vs food_typeincome:seasonlactation:sexM", 
                 "food_typeincome:seasonmating vs food_typeincome:seasonmating:sexM", 
                 "food_typeincome:seasonwinter vs food_typeincome:seasonwinter:sexM"),
  Overlap = c(
    compare_intervals("food_typeincome", "food_typeincome:sexM", model_comparison),
    compare_intervals("food_typeincome:seasonlactation", "food_typeincome:seasonlactation:sexM", model_comparison),
    compare_intervals("food_typeincome:seasonmating", "food_typeincome:seasonmating:sexM", model_comparison),
    compare_intervals("food_typeincome:seasonwinter", "food_typeincome:seasonwinter:sexM", model_comparison))
)












