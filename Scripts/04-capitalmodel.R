#load packages
source("Scripts/00-packages.R")

#read in data --------------------------------------------------------
feeding <- read.csv("Input/feeding_distances.csv")

#filter for within territory and only capital
feeding_within_territory <- feeding %>%
  filter(within_territory == TRUE,
         food_type == "capital")

#reorder the levels of repro_stage
feeding_within_territory <- feeding_within_territory %>%
  mutate(repro_stage = factor(repro_stage, levels = c("mating", "lactation", "non-breeding")))

#reorder levels of year type
feeding_within_territory <- feeding_within_territory %>%
  mutate(year_type = factor(year_type, levels = c("mast", "post-mast", "non-mast")))

#compare prop of on vs off midden feeding events between the sexes ----------------
feeding_proportions <- feeding_within_territory %>%
  group_by(sex, snow, year_type, repro_stage, within_midden) %>%
  summarise(
    total_events = n(),  #count total feeding events
    .groups = "drop") %>%
  group_by(sex, snow, year_type) %>% 
  mutate(
    proportion = total_events / sum(total_events))

feeding_proportions$snow <- factor(feeding_proportions$snow, levels = c("snow", "no snow"))

#plot proportions
feeding_proportions_plot <- ggplot(feeding_proportions, 
                                   aes(x = year_type, y = proportion, fill = within_midden)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(repro_stage ~ snow + sex) +  #separate by repro stage, snow condition and sex
  scale_fill_manual(values = c("#33CC66", "#996600"), 
                    labels = c("Off Midden", "On Midden")) +
  labs(
    title = "Proportion of Capital Feeding Events On Midden vs Off Midden by Reproductive Stage and Snow Cover",
    x = "Year Type",
    y = "Proportion of Capital Feeding Events",
    fill = "Location") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 10))

feeding_proportions_plot

#save
ggsave("Output/feeding_proportions.jpeg", plot = feeding_proportions_plot, width = 12, height = 6)

#off-midden feeding events only for analysis, since on-midden events overwhelm the data --------
feed_offmid <- feeding_within_territory %>%
  filter(within_midden == FALSE)

#check distribution of data ---------------------------------------------------------
distribution <- ggplot(feed_offmid, aes(x = distance_to_midden)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    x = "Distance to midden (m)",
    y = "Frequency") +
  theme_minimal() +
  xlim(0,125)

distribution

ggsave("Output/distribution_capital.jpeg", plot = distribution, width = 8, height = 6)

##since distance_to_midden is continuous and non-normally distributed, log transform
feed_offmid <- feed_offmid %>%
  mutate(log_distance = log(distance_to_midden + 1)) #add 1 to avoid log(0)

distribution_log <- ggplot(feed_offmid, aes(x = log_distance)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    x = "Distance to midden log-transformed",
    y = "Frequency") +
  theme_minimal()

distribution_log

ggsave("Output/distribution_capital_logged.jpeg", plot = distribution_log, width = 8, height = 6)

# summary statistics ------------------------------------------------------
summary_stats <- feeding_within_territory %>%
  group_by(sex, snow, repro_stage, year_type) %>%
  mutate(
    location = case_when(
      within_midden ~ "on_midden")) %>%
  summarize(
    mean_distance = mean(distance_to_midden, na.rm = TRUE),
    median_distance = median(distance_to_midden, na.rm = TRUE),
    sd_distance = sd(distance_to_midden, na.rm = TRUE),
    max_distance = max(distance_to_midden, na.rm = TRUE),
    total_observations = n(),                            
    observations_on_midden = sum(within_midden, na.rm = TRUE), 
    .groups = "drop")

print(summary_stats)

#save
write.csv(summary_stats, "Output/summary_stats.csv", row.names = FALSE)

#interactions?
ggplot(feed_offmid, aes(x = repro_stage, y = distance_to_midden, color = sex)) +
  geom_boxplot() +
  facet_grid(~ year_type)

#how many squirrels left?
length(unique(feed_offmid$squirrel_id))

feed_offmid %>%
  group_by(sex) %>%
  summarise(unique_squirrels = n_distinct(squirrel_id))

#how many years of data?
length(unique(feed_offmid$year))

#how many grids?
length(unique(feed_offmid$grid))

# model -------------------------------------------------------------------
#ensure year_type is a factor and set the correct order
feed_offmid <- feed_offmid %>%
  mutate(year_type = factor(year_type, levels = c("non-mast", "mast", "post-mast")),
         sex = factor(sex),
         repro_stage = factor(repro_stage, levels = c("non-breeding", "mating", "lactation")),
         snow = factor(snow))

model <- lmer(log_distance ~ sex * repro_stage + year_type + snow + (1 | squirrel_id), 
              data = feed_offmid, REML = FALSE)

model_summary <- summary(model)

#model reference categories?
contrasts(feed_offmid$year_type) #non-mast year is reference category
contrasts(feed_offmid$sex) #female is reference category
contrasts(feed_offmid$repro_stage) #non-breeding is reference category
contrasts(feed_offmid$snow) #no snow is reference category

#save csv
fixed_effects <- as.data.frame(model_summary$coefficients)
fixed_effects$Term <- rownames(fixed_effects)  #add a column for the term names
rownames(fixed_effects) <- NULL  #remove row names for cleaner output
fixed_effects <- fixed_effects[, c("Term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
write.csv(fixed_effects, "Output/model_summary.csv", row.names = FALSE)

#calculate the effect size of the model
r2_values <- r.squaredGLMM(model)
print(r2_values)

#extract results to a table
results <- as.data.frame(summary(model)$coefficients)

#add meaningful column names
colnames(results) <- c("Estimate", "Std. Error", "df", "t value", "Pr(>|t|)")

#round the results for readability
results <- round(results, 3)

#add confidence intervals for the fixed effects
results$Lower_CI <- results$Estimate - 1.96 * results$`Std. Error`
results$Upper_CI <- results$Estimate + 1.96 * results$`Std. Error`

results <- results %>% 
  tibble::rownames_to_column("term")

#print the table
print(results)

# compare model terms -----------------------------------------------------
model_comparisons <- results %>%
  filter(term %in% c("(Intercept)", 
                     "sexM", 
                     "repro_stagemating", 
                     "repro_stagelactation", 
                     "sexM:repro_stagemating", 
                     "sexM:repro_stagelactation"))

#for baseline:
female_baseline <- model_comparisons %>% filter(term == "(Intercept)")
male_baseline <- model_comparisons %>% filter(term == "sexM")
#male baseline = female baseline + sexM
male_baseline_est <- female_baseline$Estimate + male_baseline$Estimate
male_baseline_lower <- female_baseline$Lower_CI + male_baseline$Lower_CI
male_baseline_upper <- female_baseline$Upper_CI + male_baseline$Upper_CI

#for mating:
female_mating <- model_comparisons %>% filter(term == "repro_stagemating")
male_mating_int <- model_comparisons %>% filter(term == "sexM:repro_stagemating")
male_mating_est <- female_mating$Estimate + male_mating_int$Estimate
male_mating_lower <- female_mating$Lower_CI + male_mating_int$Lower_CI
male_mating_upper <- female_mating$Upper_CI + male_mating_int$Upper_CI

#for lactation:
female_lactation <- model_comparisons %>% filter(term == "repro_stagelactation")
male_lactation_int <- model_comparisons %>% filter(term == "sexM:repro_stagelactation")
male_lactation_est <- female_lactation$Estimate + male_lactation_int$Estimate
male_lactation_lower <- female_lactation$Lower_CI + male_lactation_int$Lower_CI
male_lactation_upper <- female_lactation$Upper_CI + male_lactation_int$Upper_CI

#create a summary tibble comparing female vs. male effects
comparisons <- tibble(
  Comparison = c("Baseline: Female vs. Male",
                 "Mating: Female vs. Male",
                 "Lactation: Female vs. Male"),
  Female_Estimate = c(female_baseline$Estimate,
                      female_mating$Estimate,
                      female_lactation$Estimate),
  Female_Lower_CI = c(female_baseline$Lower_CI,
                      female_mating$Lower_CI,
                      female_lactation$Lower_CI),
  Female_Upper_CI = c(female_baseline$Upper_CI,
                      female_mating$Upper_CI,
                      female_lactation$Upper_CI),
  Male_Estimate = c(male_baseline_est,
                    male_mating_est,
                    male_lactation_est),
  Male_Lower_CI = c(male_baseline_lower,
                    male_mating_lower,
                    male_lactation_lower),
  Male_Upper_CI = c(male_baseline_upper,
                    male_mating_upper,
                    male_lactation_upper))

#check for overlap between the female and male 95% CIs
comparisons <- comparisons %>%
  mutate(
    Overlap = if_else((Male_Upper_CI < Female_Lower_CI) | (Male_Lower_CI > Female_Upper_CI),
                      FALSE, TRUE))

print(comparisons)







# plot ---------------------------------------------------------------------
predictions <- ggpredict(model, terms = c("sex [F, M]", "repro_stage [mating, lactation, non-breeding]"))

#convert log_distance back to meters to report results
predictions <- predictions %>%
  rowwise() %>%
  mutate(
    distance_meters = exp(predicted),
    lower_ci_meters = exp(conf.low),
    upper_ci_meters = exp(conf.high)) %>%
  ungroup()

predictions <- predictions %>%
  rename(sex = x, repro_stage = group)

#plot distance in meters - violin plot
feeding_distances_meters <- ggplot(predictions, aes(x = repro_stage, y = distance_meters, color = sex, group = sex)) +
  geom_line(size = 1, position = position_dodge(0.2)) +  
  geom_point(size = 2, position = position_dodge(0.2)) +  
  geom_errorbar(
    aes(ymin = lower_ci_meters, ymax = upper_ci_meters),
    width = 0.2,
    size = 0.8,
    position = position_dodge(0.2)) +
  labs(
    title = "Predicted Feeding Distances by Sex and Reproductive Stage",
    x = "Reproductive Stage",
    y = "Feeding Distance (m)",
    color = "Sex") +
  scale_x_discrete(labels = c("Mating", "Lactating", "Non-breeding")) +
  scale_color_manual(
    values = c("F" = "#FF66FF", "M" = "#0066FF"),
    labels = c("Female", "Male")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12))

feeding_distances_meters

#save
ggsave("Output/feeding_distances_meters.jpeg", plot = feeding_distances_meters, width = 10, height = 6)

# do cached cones correlate with feeding distances? -----------------------
#read in cone data
midden_cones <- read.csv("Input/midden_cones.csv")

#need to join with feeding obs but based on lag (cones cached in one year compared to feeding distances the next year)
midden_cones_adjusted <- midden_cones %>%
  dplyr::rename(
    cache_size_new_prev_year = cache_size_new,
    total_cones_prev_year = total_cones,
    log_cache_size_new_prev_year = log_cache_size_new,
    log_total_cones_prev_year = log_total_cones) %>%
  mutate(year = as.numeric(year),
         following_year = year + 1)

#filter for only positive caching events
positive_caches <- midden_cones_adjusted %>%
  filter(log_cache_size_new_prev_year > 0)

#join feeding obs and cones
feeding_cones <- left_join(feed_offmid, positive_caches %>%
                 dplyr::select(squirrel_id, following_year, 
                               cache_size_new_prev_year, total_cones_prev_year, 
                               log_cache_size_new_prev_year, log_total_cones_prev_year),
                               by = c("squirrel_id", "year" = "following_year")) %>%
                 na.omit()

#fit a linear mixed-effects model
# feeding_cones_model <- lmer(
#   log_distance ~ sex + log_cache_size_new_prev_year + log_total_cones_prev_year + (1 | squirrel_id),
#   data = feeding_cones,
#   REML = FALSE)

feeding_cones_model2 <- lmer(
  log_distance ~ sex * log_cache_size_new_prev_year + log_total_cones_prev_year + repro_stage + snow + (1 | squirrel_id),
  data = feeding_cones,
  REML = FALSE)

#AIC(feeding_cones_model, feeding_cones_model2) #second model is better

#model summary
summary(feeding_cones_model2)

#calculate the average log_total_cones_prev_year for each year type
feeding_cones <- feeding_cones %>%
  mutate(cache_year = year - 1)

feeding_cones <- feeding_cones %>%
  mutate(cache_year_type = case_when(
    cache_year %in% c(2010, 2014, 2019, 2022) ~ "mast",
    cache_year %in% c(2011, 2015, 2020, 2023) ~ "post-mast",
    TRUE ~ "non-mast"))

average_cones <- feeding_cones %>%
  group_by(cache_year_type) %>%
  summarize(avg_log_total = mean(log_total_cones_prev_year, na.rm = TRUE))

#create a prediction grid for log_cache_size_new_prev_year, with sex and cache_year_type as grouping factors.
cache_seq <- seq(
  from = min(feeding_cones$log_cache_size_new_prev_year, na.rm = TRUE),
  to   = max(feeding_cones$log_cache_size_new_prev_year, na.rm = TRUE),
  length.out = 100)

pred_grid <- expand.grid(
  sex = c("F", "M"),
  cache_year_type = unique(feeding_cones$cache_year_type),
  log_cache_size_new_prev_year = cache_seq)

#join the average log_total_cones value for each cache_year_type.
pred_grid <- left_join(pred_grid, average_cones, by = "cache_year_type")

#set typical values for the other predictors.
pred_grid <- pred_grid %>%
  mutate(
    repro_stage = "non-breeding",         
    snow = "no snow",           
    squirrel_id = NA,                   # population-level predictions
    log_total_cones_prev_year = avg_log_total)

#predict on the log scale using your interaction model.
pred_grid$pred_log_distance <- predict(
  feeding_cones_model2,
  newdata = pred_grid,
  re.form = NA)   #exclude random effects for population-level predictions

#back-transform predictions and the cache predictor to actual values.
pred_grid <- pred_grid %>%
  mutate(
    pred_distance = exp(pred_log_distance),
    cache_size = exp(log_cache_size_new_prev_year))

#plot the results, faceting by the cache_year_type.
ggplot(pred_grid, aes(x = cache_size, y = pred_distance, color = sex)) +
  geom_line(size = 1) +
  facet_wrap(~ cache_year_type) +
  scale_x_log10(
    breaks = c(1, 10, 100, 1000, 10000, 100000),
    labels = c("1", "10", "100", "1K", "10K", "100K")) +   # helpful if cache sizes span multiple orders of magnitude
  labs(
    x = "Number of new cones cached",
    y = "Predicted Feeding Distance (m)",
    color = "Sex",
    title = "Predicted Feeding Distance vs. Cones Cached by Year Type"
  ) +
  theme_minimal()







