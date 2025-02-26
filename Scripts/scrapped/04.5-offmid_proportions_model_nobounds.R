#load packages
source("Scripts/00-packages.R")

#read in data --------------------------------------------------------
feeding <- read.csv("Input/feeding_distances.csv")

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

#filter for only cache feeding
feeding <- feeding %>%
  filter(food_type == "capital")

#compare prop of on vs off midden feeding events between the sexes ----------------
feeding_proportions <- feeding %>%
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
ggsave("Output/feeding_proportions_nobounds.jpeg", plot = feeding_proportions_plot, width = 12, height = 6)

# model binary outcome for feeding off midden --------------------------
feeding <- feeding %>%
  mutate(year_type = factor(year_type, levels = c("non-mast", "mast", "post-mast")),
         sex = factor(sex),
         season = factor(season, levels = c("winter", "non-breeding", "mating", "lactation")))

model <- glmer(as.numeric(!within_midden) ~ sex * season + year_type + (1 | squirrel_id), 
               data = feeding, 
               family = binomial(link = "logit"),
               control = glmerControl(optimizer = "bobyqa", 
                                      optCtrl = list(maxfun = 1000000)))  #increase maxfun (number of iterations) to fix convergence issues)

summary(model)

#model reference categories?
contrasts(feeding$year_type) #non-mast year is reference category
contrasts(feeding$sex) #female is reference category
contrasts(feeding$season) #winter is reference category

#extract model output table -----------------------------------------
model_output <- tidy(model)

model_output <- model_output %>%
  dplyr::select(-effect, -group)

model_output <- model_output[-11, ]

model_output <- model_output %>%
  rename(zvalue = statistic)

# generate predictions and plot -------------------------------------------
#generate predictions for sex and repro_stage while averaging over snow and year_type (control variables)
emm <- emmeans(model, ~ sex * season, type = "response") 
emm_df <- as.data.frame(emm)

#plot
prop_offmid_feeding <- ggplot(emm_df, aes(x = season, y = response, color = sex)) +
  geom_point(position = position_dodge(width = 0.3), size = 3.5) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.3,
                position = position_dodge(width = 0.3)) +
  scale_x_discrete(
    limits = c("winter", "mating", "lactation", "non-breeding"),
    labels = c("winter" = "Winter",
               "mating" = "Mating", 
               "lactation" = "Lactation",
               "non-breeding" = "Non-breeding")) +
  scale_color_manual(
    labels = c("Female", "Male"),
    values = c("F" = "#FF99CC", "M" = "#99CCFF")) + 
  labs(x = "Season",
       y = "Predicted Proportion of Off-midden Feeding",
       color = "Sex",
       title = "Predicted Off-midden Feeding by Sex and Season (no territory buffer)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

prop_offmid_feeding

#save
ggsave("Output/offmid_cones_nobounds.jpeg", plot = prop_offmid_feeding, width = 10, height = 6)

# comparisons -------------------------------------------------------------
#need to use link scale
emm_link <- emmeans(model, ~ sex * season, type = "link")
as.data.frame(emm_link)

contrast_results <- contrast(emm_link, method = list(
  "F_lactation_vs_M_mating"    = c(0, 0, 0, -1, 1, 0, 0, 0),
  "F_mating_vs_M_mating"       = c(0, 0, 1, -1, 0, 0, 0, 0),
  "F_lactation_vs_M_lactation" = c(0, 0, 0, 0, 1, -1, 0, 0),
  "F_mating_vs_M_lactation"    = c(0, 0, 1, 0, 0, -1, 0, 0)))


summary(contrast_results, infer = TRUE)

#back-transform back to response scale - proportions/probabilities
contrast_result_response <- regrid(contrast_results)
summary(contrast_result_response, infer = TRUE)

#observer errors?
distances <- feeding %>%
  group_by(sex) %>%
  filter(distance_to_midden > 52) %>%
  summarise(total = n(),
            over_100m = sum(distance_to_midden > 100),
            over_200m = sum(distance_to_midden > 200),
            over_500m = sum(distance_to_midden > 500))





