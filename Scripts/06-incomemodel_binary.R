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

# make food type binary and address sample size issues  ------------------------------
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

#remove unnecessary columns
feeding_cache_binomial_30 <- feeding_cache_binomial_30 %>%
  dplyr::select(-grid, -date, -repro_stage, -year_type, -cache_size_new_prev_year, -total_cones_prev_year,)

#how many squirrels left?
length(unique(feeding_cache_binomial_30$squirrel_id))

#how many years of data?
length(unique(feeding_cache_binomial_30$feeding_year))

# model -------------------------------------------------------------------
#fit a GLMM model to predict capital feeding based on cones cached and sex
model <- glmer(food_type_bin ~ log_cache_size_new_prev_year * sex + log_total_cones_prev_year + sex * season + (1 | squirrel_id),
               family = binomial(link = "logit"),
               data = feeding_cache_binomial_30,
               control = glmerControl(optimizer = "bobyqa", 
               optCtrl = list(maxfun = 1000000)))  #increase maxfun (number of iterations) to fix convergence issues

#model reference categories?
contrasts(feeding_cache_binomial_30$food_type_bin) #capital is reference category - the model is predicting the probability of capital feeding
contrasts(feeding_cache_binomial_30$sex) #female is reference category
contrasts(feeding_cache_binomial_30$season) #winter is reference category

model_summary <- summary(model)

#extract the model output into a tidy format
model_output <- tidy(model)

model_comparisons <- model_output %>%
  filter(term %in% c("log_cache_size_new_prev_year", 
                     "seasonlactation", 
                     "seasonmating", 
                     "seasonnon-breeding",
                     "log_cache_size_new_prev_year:sexM",
                     "sexM:seasonlactation", 
                     "sexM:seasonmating", 
                     "sexM:seasonnon-breeding")) %>%
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
                 "seasonnon-breeding vs sexM:seasonnon-breeding"),
  Overlap = c(
    compare_intervals("log_cache_size_new_prev_year", "log_cache_size_new_prev_year:sexM"),
    compare_intervals("seasonlactation", "sexM:seasonlactation"),
    compare_intervals("seasonmating", "sexM:seasonmating"),
    compare_intervals("seasonnon-breeding", "sexM:seasonnon-breeding")))

# plots -------------------------------------------------------------------
#generate predictions to plot
predictions <- predict(model, type = "response")

#add predictions to data set
feeding_cache_binomial_30$predicted_probability_capital <- predictions

probability_cap_feeding <- ggplot(feeding_cache_binomial_30, aes(x = log_cache_size_new_prev_year, 
                            y = predicted_probability_capital, 
                            color = sex)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", aes(group = sex), 
              se = TRUE, 
              linetype = "solid", 
              linewidth = 1) +
  facet_wrap(~season, scales = "free_x", labeller = as_labeller(c("winter" = "Winter", "mating" = "Mating", "lactation" = "Lactation", "non-breeding" = "Non-Breeding"))) + 
  scale_color_manual(values = c("F" = "#FF99CC", "M" = "#99CCFF"), labels = c("F" = "Female", "M" = "Male")) +
  ylim(0,1) +
  labs(title = "Predicted Probability of Capital Feeding",
       x = "Log Number of New Cones Cached (Previous Year)",
       y = "Predicted Probability of Capital Feeding",
       color = "Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(face = "bold", size = 11),
        strip.text = element_text(face = "bold", size = 11))

probability_cap_feeding

#save
ggsave("Output/probability_capital_feeding.jpeg", plot = probability_cap_feeding, width = 12, height = 6)





