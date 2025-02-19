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
ggsave("Output/feeding_proportions.jpeg", plot = feeding_proportions_plot, width = 8, height = 6)

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
      within_midden ~ "on_midden",
      within_territory ~ "off_midden")) %>%
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
  mutate(year_type = factor(year_type, levels = c("mast", "post-mast", "non-mast")))

model <- lmer(log_distance ~ sex * repro_stage + year_type + snow + (1 | squirrel_id), 
              data = feed_offmid, REML = FALSE)

model_summary <- summary(model)

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

#add predictor names for clarity
results$Predictor <- rownames(results)

#print the table
print(results)

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

#filter for only positive caching events
positive_caches <- midden_cones %>%
  filter(log_cache_size_new > 0)

#join feeding obs and cones
feeding_cones <- feed_offmid %>%
  inner_join(positive_caches, by = c("squirrel_id", "sex", "grid", "year"), relationship = "many-to-many")

#fit a linear mixed-effects model
feeding_cones_model <- lmer(
  log_distance ~ log_cache_size_new + log_total_cones + (1 | squirrel_id),
  data = feeding_cones,
  REML = FALSE)

#model summary
summary(feeding_cones_model)

#transform the log variables back to their original scale
feeding_cones_transformed <- feeding_cones %>%
  mutate(
    distance_meters = exp(log_distance), 
    cache_size_new = exp(log_cache_size_new),  
    total_cones = exp(log_total_cones))

#plot feeding distance in meters vs. cache size (new cones)
ggplot(feeding_cones_transformed, aes(x = cache_size_new, y = distance_meters, color = sex)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship Between Feeding Distance (m) and Number of New Cones Cached by Sex",
    x = "New Cones Cached",
    y = "Feeding Distance (m)",
    color = "Sex") +
  scale_color_manual(values = c("F" = "#FF66FF", "M" = "#0066FF"), labels = c("Female", "Male")) +
  facet_wrap(~sex, scales = "free_x") +  # Separate by sex
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"))

#let's extend the model by adding other variables
global_model <- lmer(
  log_distance ~ log_cache_size_new * sex + repro_stage + year_type + snow +
    (1 | squirrel_id),
  data = feeding_cones,
  REML = FALSE)















