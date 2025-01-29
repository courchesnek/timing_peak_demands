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
    x = "Distance to Midden (m)",
    y = "Frequency") +
  theme_minimal() +
  xlim(0,125)

distribution

ggsave("Output/distribution_capital.jpeg", plot = distribution, width = 8, height = 6)

# summary statistics ------------------------------------------------------
summary_stats <- feed_offmid %>%
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

#interactions?
ggplot(feed_offmid, aes(x = repro_stage, y = distance_to_midden, color = sex)) +
  geom_boxplot() +
  facet_grid(~ year_type)

# model -------------------------------------------------------------------
##since distance_to_midden is continuous and non-normally distributed, log transform
feed_offmid <- feed_offmid %>%
  mutate(log_distance = log(distance_to_midden + 1)) #add 1 to avoid log(0)

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

AIC(model_year) #model with year type is better - delta AIC of 50.125

summary(model_year)

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

#print the table
print(results)

# plot ---------------------------------------------------------------------
predictions <- ggpredict(model_year, terms = c("repro_stage", "sex", "year_type"))

ggplot(predictions, aes(x = x, y = predicted, color = group, group = group)) +
  geom_line(size = 1) +  #line for each group
  geom_point(size = 2, position = position_dodge(0.2)) +  #points for estimates
  facet_wrap(~facet, scales = "free") +  #separate by year_type
  labs(
    title = "Predicted Feeding Distances by Sex, Reproductive Stage, and Year Type",
    x = "Reproductive Stage",
    y = "Log Feeding Distance",
    color = "Sex") +
  theme_minimal()

#jtools effectplot
effect_plot <- interact_plot(
  model = model_year,  #the fitted linear mixed model
  pred = repro_stage,  #predictor on the x-axis (reproductive stage)
  modx = year_type,    #moderator (year type)
  mod2 = sex,          #second moderator (sex)
  outcome.scale = "response",  #plot on the response scale (log feeding distance)
  x.label = "Reproductive Stage",
  y.label = "Predicted Feeding Distance (Log Scale)",
  main.title = "Interaction Effects of Year Type, Sex, and Reproductive Stage")

effect_plot

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






