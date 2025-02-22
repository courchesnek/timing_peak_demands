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


# make food type binary and address sample size issues  ------------------------------
#create a binary response variable: capital = 1, income = 0 - keep as numeric
feeding_cache_binomial <- feeding_cache %>%
  mutate(food_type_bin = ifelse(food_type == "capital", 1, 0))  #capital = 1, income = 0

#feeding_cache_binomial$food_type_bin <- factor(feeding_cache_binomial$food_type_bin, levels = c(0, 1))
#feeding_cache_binomial$food_type_bin <- relevel(feeding_cache_binomial$food_type_bin, ref = "1")  #make capital the reference

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
  dplyr::select(-grid, -date, -repro_stage, -cache_size_new_prev_year, -total_cones_prev_year,)

#summarise data ---------------------------------
feeding_summary <- feeding_cache_binomial_30 %>%
  group_by(sex, season, food_type) %>%
  summarise(
    total_observations = n(),
    .groups = "drop") %>%
  pivot_wider(
    names_from = food_type, 
    values_from = total_observations,
    values_fill = list(total_observations = 0))

#save
write.csv(feeding_summary, "Output/capvsinc_summary.csv", row.names = FALSE)

#how many squirrels left?
length(unique(feeding_cache_binomial_30$squirrel_id))

feeding_cache_binomial_30 %>%
  group_by(sex) %>%
  summarise(unique_squirrels = n_distinct(squirrel_id))

#how many years of data?
length(unique(feeding_cache_binomial_30$feeding_year))

# model -------------------------------------------------------------------
##re-level season so winter is the reference category
feeding_cache_binomial_30$season <- relevel(feeding_cache_binomial_30$season, ref = "winter")

#fit a GLMM model to predict capital feeding based on cones cached and sex
final_model <- glmer(food_type_bin ~ log_cache_size_new_prev_year * sex + sex * season + (1 | squirrel_id),
               family = binomial(link = "logit"),
               data = feeding_cache_binomial_30,
               control = glmerControl(optimizer = "bobyqa", 
               optCtrl = list(maxfun = 1000000)))  #increase maxfun (number of iterations) to fix convergence issues

#model reference categories?
contrasts(feeding_cache_binomial_30$sex) #female is reference category
contrasts(feeding_cache_binomial_30$season) #winter is reference category

model_summary <- summary(final_model)

#extract the model output into a tidy format
model_output <- tidy(final_model)

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
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error)

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

#clean up model output to save as csv
model_output <- model_output %>%
  dplyr::select(-effect, -group)

model_output <- model_output[-11, ]

model_output <- model_output %>%
  rename(zvalue = statistic)
  
#save
write.csv(model_output, "Output/income_model_output.csv", row.names = FALSE)

# plots -------------------------------------------------------------------
#generate predictions to plot
# predictions <- predict(final_model, type = "response")
# 
# #add predictions to data set
# feeding_cache_binomial_30$predicted_probability_capital <- predictions
# 
# probability_cap_feeding <- ggplot(feeding_cache_binomial_30, aes(x = log_cache_size_new_prev_year, 
#                             y = predicted_probability_capital, 
#                             color = sex)) +
#   geom_point(alpha = 0.6) +  
#   geom_smooth(method = "lm", aes(group = sex), 
#               se = TRUE, 
#               linetype = "solid", 
#               linewidth = 1) +
#   facet_wrap(~season, scales = "free_x", labeller = as_labeller(c("winter" = "Winter", "mating" = "Mating", "lactation" = "Lactation", "non-breeding" = "Non-Breeding"))) + 
#   scale_color_manual(values = c("F" = "#FF99CC", "M" = "#99CCFF"), labels = c("F" = "Female", "M" = "Male")) +
#   ylim(0,1) +
#   labs(title = "Predicted Probability of Capital Feeding",
#        x = "Log Number of New Cones Cached (Previous Year)",
#        y = "Predicted Probability of Capital Feeding",
#        color = "Sex") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 0, hjust = 1, face = "bold"),
#         plot.title = element_text(hjust = 0.5, face = "bold"),
#         legend.title = element_text(face = "bold", size = 12),
#         legend.text = element_text(face = "bold", size = 11),
#         strip.text = element_text(face = "bold", size = 11))
# 
# probability_cap_feeding
# 
# #save
# ggsave("Output/probability_capital_feeding.jpeg", plot = probability_cap_feeding, width = 12, height = 6)

#generate predictions to plot
# emm <- emmeans(final_model, ~ sex * season * log_cache_size_new_prev_year,
#                at = list(log_cache_size_new_prev_year = seq(min(feeding_cache_binomial_30$log_cache_size_new_prev_year),
#                                                             max(feeding_cache_binomial_30$log_cache_size_new_prev_year),
#                                                             length = 100)),
#                type = "response")
# 
# emm_df <- as.data.frame(emm)
# 
# #change labels
# pred_data <- emm_df %>%
#   rename(
#     predicted_probability = prob,
#     conf.low = asymp.LCL,
#     conf.high = asymp.UCL)
# 
# pred_data <- pred_data %>%
#   mutate(
#     sex = factor(sex, levels = c("F", "M"), labels = c("Females", "Males")),
#     season = factor(season, levels = c("winter", "mating", "lactation", "non-breeding"),
#                     labels = c("Winter", "Mating", "Lactation", "Non-breeding")))
# 
# 
# season_colours <- c("Winter"       = "#6699CC",
#                     "Mating"       = "#663300",
#                     "Lactation"    = "#66CC66",
#                     "Non-breeding" = "#CC0000")
# 
# ggplot(pred_data, aes(x = log_cache_size_new_prev_year, y = predicted_probability, color = season, fill = season)) +
#   geom_ribbon(
#     aes(ymin = conf.low, ymax = conf.high, fill = season, group = season),
#     alpha = 0.1, color = NA) +
#   geom_line(aes(group = season), size = 1) +
#   facet_wrap(~ sex) +
#   scale_color_manual(
#     name = "Season",
#     values = season_colours,
#     breaks = c("Winter", "Mating", "Lactation", "Non-breeding")) +
#   scale_fill_manual(
#     name = "Season",
#     values = season_colours,
#     breaks = c("Winter", "Mating", "Lactation", "Non-breeding")) +
#   labs(
#     title = "Predicted probability of capital feeding by sex and season",
#     x = "Number of new cones cached in the previous year (log-scaled)",
#     y = "Probability of capital feeding") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold"),
#     legend.title = element_text(face = "bold"),
#     legend.text = element_text(face = "bold"))

#generate predictions to plot
#calculate the average log_total_cones_prev_year for each year type
feeding_cache_binomial_30 <- feeding_cache_binomial_30 %>%
  mutate(cache_year = feeding_year - 1)

feeding_cache_binomial_30 <- feeding_cache_binomial_30 %>%
  mutate(cache_year_type = case_when(
    cache_year %in% c(2010, 2014, 2019, 2022) ~ "mast",
    cache_year %in% c(2011, 2015, 2020, 2023) ~ "post-mast",
    TRUE ~ "non-mast"))

average_cones <- feeding_cache_binomial_30 %>%
  group_by(cache_year_type) %>%
  summarize(avg_log_total = mean(log_total_cones_prev_year, na.rm = TRUE))

#generate predictions by year type
#create a prediction grid for log_cache_size_new_prev_year, with sex and cache_year_type as grouping factors.
cache_seq <- seq(
  from = min(feeding_cache_binomial_30$log_cache_size_new_prev_year, na.rm = TRUE),
  to   = max(feeding_cache_binomial_30$log_cache_size_new_prev_year, na.rm = TRUE),
  length.out = 100)

pred_grid <- expand.grid(
  sex = c("F", "M"),
  season = c("winter", "mating", "lactation", "non-breeding"),
  log_cache_size_new_prev_year = cache_seq)

pred_grid <- pred_grid %>%
  mutate(
    squirrel_id = NA)   #population-level predictions

#predict and backtransform
pred_grid$predicted_probability <- predict(
  final_model,
  newdata = pred_grid,
  re.form = NA,
  type = "response") #exclude random effects for population-level predictions

pred_grid <- pred_grid %>%
  mutate(cached_cones = exp(log_cache_size_new_prev_year))

ggplot(pred_grid, aes(x = cached_cones, y = predicted_probability, color = season)) +
  geom_line(size = 1) +
  facet_wrap(~ sex) +
  scale_x_log10() +
  labs(
    title = "Predicted Probability of Capital Feeding by Sex and Season (Non-mast Years)",
    x = "Number of new cones cached (actual count)",
    y = "Probability of capital feeding") +
  theme_minimal()

# variability in cache across year types? --------------------------------
#add year type column to cache data
##define mast years
mast_years <- c(1993, 1998, 2005, 2010, 2014, 2019, 2022)

#add a column for year_type
midden_cones <- midden_cones %>%
  mutate(
    year_type = case_when(
      year %in% mast_years ~ "mast",
      year %in% (mast_years + 1) ~ "post-mast",
      TRUE ~ "non-mast"))

#step 1: aggregate the data by sex and year_type
agg_data <- midden_cones %>%
  group_by(sex, year_type) %>%
  summarize(mean_cache = mean(cache_size_new, na.rm = TRUE)) %>%
  ungroup()

#step 2: calculate the observed variance in mean cache sizes for each sex
female_means <- agg_data$mean_cache[agg_data$sex == "F"]
male_means   <- agg_data$mean_cache[agg_data$sex == "M"]

obs_var_female <- var(female_means)
obs_var_male   <- var(male_means)

#compute the ratio of variances (female/male)
obs_ratio <- obs_var_female / obs_var_male
cat("Observed variance ratio (female/male):", obs_ratio, "\n")

#step 3: permutation test to assess significance
##because data is non-normally distributed and sample size is small (i.e. only 3 groupings for each sex), a permutation test is a robust, non-parametric option
set.seed(123)  #for reproducibility
n_perm <- 10000
perm_ratios <- numeric(n_perm)

combined_means <- agg_data$mean_cache
original_labels <- agg_data$sex

for (i in 1:n_perm) {
  perm_labels <- sample(original_labels)
  perm_female_var <- var(combined_means[perm_labels == "F"])
  perm_male_var   <- var(combined_means[perm_labels == "M"])
  perm_ratios[i] <- perm_female_var / perm_male_var
}

# For a one-tailed test (females are hypothesized to be more variable),
# calculate the p-value as the proportion of permuted ratios that are as high or higher than observed.
p_value <- mean(perm_ratios >= obs_ratio)
cat("Permutation test p-value:", p_value, "\n")
## p-value >0.05 = no true difference in the variabilty of cached cones between females and males across year types.


