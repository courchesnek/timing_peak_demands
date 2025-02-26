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

#filter for within territory and only capital
feeding_within_territory <- feeding %>%
  filter(within_territory == TRUE,
         food_type == "capital")

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


# how many obs in each season? --------------------------------------------
feeding_within_territory %>%
  group_by(season) %>%
  summarise(n_obs = n())

#lactation:1079
#mating:2086
#non-breeding (no snow): 222
#non-breeding (snow, i.e. winter): 990

feeding <- feeding %>%
  mutate(
    season = factor(season, levels = c("winter", "mating", "lactation", "non-breeding")),
    sex = factor(sex, levels = c("M", "F")))

#data summary
feeding_fixed <- feeding %>%
  mutate(off_territory_val = if_else(within_territory == FALSE, 1, 0))

summary_table <- feeding_fixed %>%
  group_by(sex, season) %>%
  summarise(
    total_events      = n(),
    on_midden         = sum(within_midden, na.rm = TRUE),
    off_midden        = sum(!within_midden, na.rm = TRUE),
    within_territory  = sum(within_territory, na.rm = TRUE),
    off_territory     = sum(off_territory_val, na.rm = TRUE)) %>%
  ungroup()

feeding_within_territory %>%
  group_by(sex) %>%
  summarise(
    on_midden    = sum(within_midden, na.rm = TRUE),
    off_midden   = sum(!within_midden, na.rm = TRUE),
    unique_squirrels = n_distinct(squirrel_id))

# model binary outcome for feeding off midden --------------------------
feeding_within_territory <- feeding_within_territory %>%
  mutate(year_type = factor(year_type, levels = c("non-mast", "mast", "post-mast")),
         sex = factor(sex),
         season = factor(season, levels = c("winter", "mating", "lactation", "non-breeding")))

model <- glmer(as.numeric(!within_midden) ~ sex * season + year_type + (1 | squirrel_id), 
               data = feeding_within_territory, 
               family = binomial(link = "logit"),
               control = glmerControl(optimizer = "bobyqa", 
               optCtrl = list(maxfun = 1000000)))  #increase maxfun (number of iterations) to fix convergence issues)

summary(model)

#model reference categories?
contrasts(feeding_within_territory$year_type) #non-mast year is reference category
contrasts(feeding_within_territory$sex) #female is reference category
contrasts(feeding_within_territory$season) #winter is reference category


# compare CI overlaps and extract model output table ----------------------
model_output <- tidy(model)

model_comparisons <- model_output %>%
  filter(term %in% c("(Intercept)",
                     "sexM", 
                     "seasonmating", 
                     "seasonlactation",
                     "seasonnon-breeding",
                     "sexM:seasonmating", 
                     "sexM:seasonlactation",
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
  Comparison = c("(Intercept) vs sexM",
                 "seasonmating vs sexM:seasonmating", 
                 "seasonlactation vs sexM:seasonlactation",
                 "seasonnon-breeding vs sexM:seasonnon-breeding"),
  Overlap = c(
    compare_intervals("(Intercept)", "sexM"),
    compare_intervals("seasonmating", "sexM:seasonmating"),
    compare_intervals("seasonlactation", "sexM:seasonlactation"),
    compare_intervals("seasonnon-breeding", "sexM:seasonnon-breeding")))

#clean up model output to save as csv
model_output <- model_output %>%
  dplyr::select(-effect, -group)

model_output <- model_output[-11, ]

model_output <- model_output %>%
  rename(zvalue = statistic)

#save
write.csv(model_output, "Output/offmid_cones_model_output.csv", row.names = FALSE)

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
    labels = c("winter" = "Non-breeding (Snow)",
               "mating" = "Mating", 
               "lactation" = "Lactation",
               "non-breeding" = "Non-breeding (No Snow)")) +
  scale_color_manual(
    labels = c("Female", "Male"),
    values = c("F" = "#FF99CC", "M" = "#99CCFF")) + 
  labs(x = "Season",
       y = "Proportion of Off-midden Cone Feeding",
       color = "Sex",
       title = "Proportion of Off-midden Cone Feeding by Sex and Season") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

prop_offmid_feeding

#save
ggsave("Output/offmid_cones.jpeg", plot = prop_offmid_feeding, width = 10, height = 6)

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

#get z- and p-values









