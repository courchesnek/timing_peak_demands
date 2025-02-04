#load packages
source("Scripts/00-packages.R")

#read in data
feeding <- read.csv("Input/feeding_distances.csv")

#filter for within territory and only capital
feeding_within_territory <- feeding %>%
  filter(within_territory == TRUE,
         food_type == "capital")

#reorder the levels of repro_stage
feeding_within_territory <- feeding_within_territory %>%
  mutate(repro_stage = factor(repro_stage, levels = c("MATING", "LACTATING")))

#reorder levels of year type
feeding_within_territory <- feeding_within_territory %>%
  mutate(year_type = factor(year_type, levels = c("mast", "post-mast", "non-mast")))

#compare prop of on vs off midden feeding events between the sexes
feeding_proportions <- feeding_within_territory %>%
  group_by(sex, snow, year_type, repro_stage, within_midden) %>%
  summarise(
    total_events = n(),  #count total feeding events
    .groups = "drop") %>%
  group_by(sex, snow, year_type) %>% 
  mutate(
    proportion = total_events / sum(total_events))

#plot proportions
feeding_proportions_plot <- ggplot(feeding_proportions, 
  aes(x = year_type, y = proportion, fill = within_midden)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(repro_stage ~ snow + sex) +  #separate by repro stage, snow condition and sex
  scale_fill_manual(values = c("#33CC66", "#996600"), 
                    labels = c("Off Midden", "On Midden")) +
  labs(
    title = "Proportion of Capital Feeding Events On Midden vs Off Midden by Reproductive Stage",
    x = "Year Type",
    y = "Proportion of Capital Feeding Events",
    fill = "Location") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 10))

feeding_proportions_plot

#off-midden feeding events only for analysis, since on-midden events overwhelm the data
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

# model -------------------------------------------------------------------
#ensure year_type is a factor and set the correct order
feed_offmid <- feed_offmid %>%
  mutate(year_type = factor(year_type, levels = c("mast", "post-mast", "non-mast")))

#fit a mixed-effect model
model_snow <- lmer(
  log_distance ~ sex * repro_stage * snow + year_type + (1 | squirrel_id),
  data = feed_offmid,
  na.action = na.omit)

model_year <- lmer(
  log_distance ~ sex * repro_stage * year_type + snow + (1 | squirrel_id), 
  data = feed_offmid,
  na.action = na.omit)

AIC(model_year) #model with year type is better

model_summary <- summary(model_year)

#save csv
fixed_effects <- as.data.frame(model_summary$coefficients)
fixed_effects$Term <- rownames(fixed_effects)  #add a column for the term names
rownames(fixed_effects) <- NULL  #remove row names for cleaner output
fixed_effects <- fixed_effects[, c("Term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
write.csv(fixed_effects, "Output/model_summary.csv", row.names = FALSE)

#calculate the effect size of the model
r2_values <- r.squaredGLMM(model_year)
print(r2_values)

#extract results to a table
results <- as.data.frame(summary(model_year)$coefficients)

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
predictions <- ggpredict(
  model_year,
  terms = c(
    "repro_stage [MATING, LACTATING]", 
    "sex [F, M]", 
    "year_type [mast, post-mast, non-mast]"))

results_predict <- ggplot(predictions, aes(x = x, y = predicted, color = group, group = group)) +
  geom_line(size = 1) +  #line for each group
  geom_point(size = 2, position = position_dodge(0.2)) +  #points for estimates
  facet_wrap(~facet, scales = "free") +  #separate by year_type
  labs(
    title = "Predicted Feeding Distances by Sex, Reproductive Stage, and Year Type",
    x = "Reproductive Stage",
    y = "Log Feeding Distance",
    color = "Sex") +
  theme_minimal()

results_predict

dodge <- position_dodge(0.2)  #define dodge position for consistency

feeding_distances <- ggplot(predictions, aes(x = x, y = predicted, color = group, group = group)) +
  geom_line(size = 1, position = dodge) +  #line for each group
  geom_point(size = 2, position = dodge) +  #points for estimates
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.2,
    position = dodge,
    size = 0.8) +  #confidence intervals
  facet_wrap(~facet, scales = "free", ncol = 3, labeller = labeller(facet = c("mast" = "Mast", "post-mast" = "Post-mast", "non-mast" = "Non-mast"))) +  #separate by year_type
  labs(
    title = "Predicted Feeding Distances by Sex, Reproductive Stage, and Year Type",
    x = "Reproductive Stage",
    y = "Log Feeding Distance",
    color = "Sex") +
  scale_x_discrete(labels=c("Mating", "Lactating")) +
  scale_color_manual(
    values = c("F" = "#FF66FF", "M" = "#0066FF"),
    labels = c("Female", "Male")) + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "dotted", color = "grey80"))

feeding_distances

#save
ggsave("Output/feeding_distances.jpeg", plot = feeding_distances, width = 10, height = 6)

# #jtools effectplot - doesn't match the results from the model output????
# effect_plot <- interact_plot(
#   model = model_year,  #the fitted linear mixed model
#   pred = repro_stage,  #predictor on the x-axis (reproductive stage)
#   modx = year_type,    #moderator (year type)
#   mod2 = sex,          #second moderator (sex)
#   outcome.scale = "response",  #plot on the response scale (log feeding distance)
#   x.label = "Reproductive Stage",
#   y.label = "Predicted Feeding Distance (Log Scale)",
#   main.title = "Interaction Effects of Year Type, Sex, and Reproductive Stage")
# 
# effect_plot

#box-whisker
box_whisker <- ggplot(results, aes(x = reorder(Predictor, Estimate), y = Estimate)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +  #flip the coordinates for horizontal bars
  labs(
    title = "Effect Sizes of Fixed Effects in Feeding Distance Model",
    x = "Predictor",
    y = "Estimate (with 95% CI)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.y = element_text(size = 10))

box_whisker

# convert log_distance back to meters to report results -------------------
predictions <- predictions %>%
  rowwise() %>%
  mutate(
    distance_meters = exp(predicted),
    lower_ci_meters = exp(conf.low),
    upper_ci_meters = exp(conf.high)) %>%
  ungroup()

head(predictions)

#plot distance in meters
dodge <- position_dodge(0.2)  #define dodge position for consistency

feeding_distances_meters <- ggplot(predictions, aes(x = x, y = distance_meters, color = group, group = group)) +
  geom_line(size = 1, position = dodge) +  # Line for each group
  geom_point(size = 2, position = dodge) +  # Points for estimates
  geom_errorbar(
    aes(ymin = lower_ci_meters, ymax = upper_ci_meters),  # Use converted CI in meters
    width = 0.2,
    position = dodge,
    size = 0.8) +  # Confidence intervals
  facet_wrap(~facet, scales = "free", ncol = 3, labeller = labeller(facet = c("mast" = "Mast", "post-mast" = "Post-mast", "non-mast" = "Non-mast"))) +  # Separate by year_type
  labs(
    title = "Predicted Feeding Distances by Sex, Reproductive Stage, and Year Type",
    x = "Reproductive Stage",
    y = "Predicted Feeding Distance (m)",  # Update y-axis label
    color = "Sex") +
  scale_x_discrete(labels = c("Mating", "Lactating")) +
  scale_color_manual(
    values = c("F" = "#FF66FF", "M" = "#0066FF"),
    labels = c("Female", "Male")) + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "dotted", color = "grey80"))

feeding_distances_meters

#save
ggsave("Output/feeding_distances_meters.jpeg", plot = feeding_distances_meters, width = 10, height = 6)






