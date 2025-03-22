#load packages
source("Scripts/00-packages.R")

#read in data --------------------------------------------------------
feeding <- read.csv("Input/feeding_distances_all.csv")
tree_cones <- read.csv("Input/tree_cones.csv")
mushrooms <- read.csv("Input/mushrooms.csv")

#filter for within territory
feeding_within_territory <- feeding %>%
  filter(within_territory == TRUE)

# investigate sample sizes ------------------------------------------------
feeding_numbers <- feeding_within_territory %>%
  filter(repro_stage == "mating") %>%
  group_by(year, sex) %>%
  summarise(total_events = n(), .groups = "drop")

#keep only years where total_count = 50 for both males and females
# valid_years <- feeding_numbers %>%
#   group_by(year) %>%
#   filter(all(total_events >= 50)) %>%  #keep year if all groups have >=50 events
#   pull(year) %>%
#   unique()
# 
# valid_years

# keep only years with earlier mating ---------------------------
mating_lac <- read.csv("Input/reproductive_windows.csv")

mating_years <- mating_lac %>%
  filter(month(mating_end) < 4 | (month(mating_end) == 4 & day(mating_end) <= 30)) %>%
  pull(year) %>%
  unique()

mating_years

# male on- vs off-midden feeding during mating -----------------------
feeding_mating <- feeding_within_territory %>%
  filter(sex == "M", repro_stage == "mating",
         year %in% mating_years)

length(unique(feeding_mating$squirrel_id))

length(unique(feeding_mating$year))
unique(feeding_mating$year)

#create detailed food groups -------------------------------------------
male_feeding_detailed <- feeding_mating %>%
  mutate(food_group = case_when(
    detail == 2 ~ "cone",
    detail == 4 ~ "mushroom/truffle",
    detail == 3 ~ "spruce_bud",
    TRUE ~ "other")) %>% # everything else = other
  filter(!is.na(food_group)) %>%
  mutate(midden_status = ifelse(within_midden == TRUE, 1, 0))

#ensure food_group is a factor
male_feeding_detailed$food_group <- factor(male_feeding_detailed$food_group,
                                           levels = c("cone", "mushroom/truffle", "spruce_bud", "other"))

# combine cone and mushroom production indices and join to feeding data --------
food_production <- mushrooms %>%
  dplyr::select(-LowerCI, -UpperCI) %>%
  left_join(tree_cones, by = "year") %>%
  mutate(next_year = year + 1) %>%
  rename(mushroom_index_previous = mushroom_index,
         cone_index_previous = cone_index) %>%
  mutate(
    mushroom_index_previous_scaled = as.numeric(scale(mushroom_index_previous)),
    cone_index_previous_scaled = as.numeric(scale(cone_index_previous)))

male_feeding_detailed <- male_feeding_detailed %>%
  left_join(food_production, by = c("year" = "next_year"))

# glmer model -------------------------------------------------------------------
# fit generalized linear mixed effects model with binary response
model <- glmer(midden_status ~ food_group + cone_index_previous_scaled + mushroom_index_previous_scaled + (1 | squirrel_id) + (1 | year),
               data = male_feeding_detailed,
               family = binomial(link = "logit"))

#check residuals
sim_res <- simulateResiduals(model) #remember: with large sample sizes, even very small deviations can become significant
plot(sim_res) 

testOutliers(sim_res) #no significant outliers

#model summary
summary(model)

# generate predictions and plot -------------------------------------------
#step 1: generate predictions across all years - on-midden feeding
pred_on_midden <- as.data.frame(emmeans(model, ~ food_group, type = "response"))

#step 1.5: generate predictions across all years - off-midden feeding
pred_off_midden <- pred_on_midden
pred_off_midden$prob <- 1 - pred_off_midden$prob

#step 2: compute overall proportions within each broad category
on_summary <- male_feeding_detailed %>%
  filter(midden_status == 1) %>%
  group_by(food_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(prop_detail = count / sum(count))

off_summary <- male_feeding_detailed %>%
  filter(midden_status == 0) %>%
  group_by(food_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(prop_detail = count / sum(count))

#step 3: merge summaries with model predictions
#for on-midden events, multiply the detailed proportion by the predicted probability
on_summary <- on_summary %>%
  left_join(pred_on_midden, by = "food_group") %>%
  mutate(final_prop = prop_detail * prob)

#for off-midden events, use (1 - predicted probability)
off_summary <- off_summary %>%
  left_join(pred_off_midden, by = "food_group") %>%
  mutate(final_prop = prop_detail * prob)

#step 5: combine on- and off-midden summaries
final_predicted <- bind_rows(
  on_summary %>% mutate(midden_status = "on"),
  off_summary %>% mutate(midden_status = "off")) %>%
  mutate(
    Overall = "Overall",
    midden_status = factor(midden_status, levels = c("on", "off"))) %>%
  dplyr::select(midden_status, food_group, count, prop_detail, prob, SE, df, asymp.LCL, asymp.UCL, final_prop, Overall)

final_predicted$Overall <- factor(final_predicted$Overall, levels = "Overall")
final_predicted$midden_status <- factor(final_predicted$midden_status, levels = c("on", "off"))

#step 7: plot stacked bar graph
male_mating_model <- ggplot(final_predicted, 
                            aes(x = Overall, y = final_prop, 
                                fill = food_group, pattern = midden_status)) +
  geom_bar_pattern(stat = "identity", position = "stack",
                   color = "black",              
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.02) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
  scale_fill_manual(
    name = "Food Type",
    breaks = c("cone", "mushroom/truffle", "spruce_bud", "other"),
    values = c(
      "cone" = "#E69F00",
      "mushroom/truffle" = "#56B4E9",
      "spruce_bud" = "#359B73",
      "other" = "#F748A5"),
    labels = c(
      "cone" = "Cone", 
      "mushroom/truffle" = "Mushroom/Truffle", 
      "spruce_bud" = "Spruce Bud", 
      "other" = "Other"),
    guide = guide_legend(override.aes = list(pattern = "none"))) +
  scale_pattern_manual(
    name = "Feeding Location",
    breaks = c("on", "off"),
    values = c("on" = "none", "off" = "stripe"),
    labels = c("on" = "On Midden", "off" = "Off Midden")) +
  guides(
    fill = guide_legend(override.aes = list(pattern = "none"), order = 1),
    pattern = guide_legend(override.aes = list(fill = "white"), order = 2)) +
  labs(x = NULL,
       y = "Proportion of Feeding Events",
       title = "Proportion of Food Types Consumed\nby Males During Mating",
       fill = "Food Type") +
  theme_minimal(base_size = 23) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
    panel.grid = element_blank(),
    plot.title = element_text(size = 30, hjust = 0.5, face = "bold", margin = margin(b = 10, unit = "pt")),
    legend.title = element_text(size = 25, face = "bold"),
    legend.text = element_text(size = 20),
    legend.spacing.y = unit(2, "cm"),
    legend.key.height = unit(1.4, "cm"),
    plot.margin = margin(t = 25, r = 20, b = 20, l = 10),
    legend.box.margin = margin(t = 0, r = 10, b = 0, l = -10),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "black"))

male_mating_model

#save
ggsave("Output/male_mating_model.jpeg", plot = male_mating_model, width = 12, height = 7)

#is the model predicted proportion of on-midden cone feeding significantly different from expectation of 90% on-midden cone feeding?
expected_prob_on_midden_cone <- 0.90

#calculate difference between expected and predicted
on_midden_cone <- final_predicted %>%
  filter(food_group == "on_midden_cone") %>%
  mutate(difference = expected_prob_on_midden_cone - final_prop)

#check if the 0.90 is outside the confidence interval for the predicted probability
on_midden_cone <- on_midden_cone %>%
  filter(food_group == "on_midden_cone") %>%
  mutate(is_significant = asymp.LCL > expected_prob_on_midden_cone | asymp.UCL < expected_prob_on_midden_cone) 

# calculate stats for predictions -----------------------------------------
#function to calculate min-max ranges and confidence intervals
calculate_stats <- function(df) {
  df %>%
    mutate(
      #calculate min/max ranges based on SE
      min_range = final_prop - SE,
      max_range = final_prop + SE,
      
      #calculate confidence intervals for final proportions
      lower_ci = final_prop - 1.96 * SE,
      upper_ci = final_prop + 1.96 * SE)}

final_predicted_stats <- calculate_stats(final_predicted)

# pairwise comparison -----------------------------------------------------
#obtain the estimated marginal means on the link (logit) scale for each food group
emm_link <- emmeans(model, ~ food_group, type = "link")

# Test whether the estimates differ significantly from 0 (which corresponds to p = 0.5)
test_results <- test(emm_link, null = 0)
print(test_results)

emm_overall <- emmeans(model, ~ 1, type = "link")
test_overall <- test(emm_overall, null = 0)
print(test_overall)



# data summary ------------------------------------------------------------
male_feeding_summary <- male_feeding_detailed %>%
  count(food_group, name = "sample_size")

#compare prop of on vs off midden feeding events between the sexes ----------------
feeding_proportions <- feeding_within_territory %>%
  group_by(sex, year_type, repro_stage, within_midden) %>%
  summarise(
    total_events = n(),  #count total feeding events
    .groups = "drop") %>%
  group_by(sex, year_type) %>% 
  mutate(
    proportion = total_events / sum(total_events))

#plot proportions
feeding_proportions_plot <- ggplot(feeding_proportions, 
                                   aes(x = year_type, y = proportion, fill = within_midden)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(factor(repro_stage, levels = c("mating", "lactation", "non-breeding")) ~ sex) + 
  scale_fill_manual(values = c("#33CC66", "#996600"), 
                    labels = c("Off Midden", "On Midden")) +
  labs(
    title = "Proportion of Cone Feeding Events On Midden vs Off Midden by Reproductive Stage",
    x = "Year Type",
    y = "Proportion of Cone Feeding Events",
    fill = "Location") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 10))

feeding_proportions_plot

#save
ggsave("Output/feeding_proportions.jpeg", plot = feeding_proportions_plot, width = 12, height = 6)





