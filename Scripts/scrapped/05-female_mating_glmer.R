#load packages
source("Scripts/00-packages.R")

#read in data --------------------------------------------------------
feeding <- read.csv("Input/feeding_distances_all.csv")
tree_cones <- read.csv("Input/tree_cones.csv")
mushrooms <- read.csv("Input/mushrooms.csv")

#filter for within territory
feeding_within_territory <- feeding %>%
  filter(within_territory == TRUE)

# keep only years with earlier mating ---------------------------
mating_lac <- read.csv("Input/reproductive_windows.csv")

mating_years <- mating_lac %>%
  filter(month(mating_end) < 4 | (month(mating_end) == 4 & day(mating_end) <= 30)) %>%
  pull(year) %>%
  unique()

mating_years

# female on- vs off-midden feeding during mating -----------------------
feeding_mating <- feeding_within_territory %>%
  filter(sex == "F", repro_stage == "mating",
         year %in% mating_years)

length(unique(feeding_mating$squirrel_id))

length(unique(feeding_mating$year))
unique(feeding_mating$year)

#create detailed food groups -------------------------------------------
female_feeding_detailed <- feeding_mating %>%
  mutate(food_group = case_when(
    detail == 2 ~ "cone",
    detail == 4 ~ "mushroom/truffle",
    detail == 3 ~ "spruce_bud",
    TRUE ~ "other")) %>% # everything else = other
  filter(!is.na(food_group)) %>%
  mutate(midden_status = ifelse(within_midden == TRUE, 1, 0))

#ensure food_group is a factor
female_feeding_detailed$food_group <- factor(female_feeding_detailed$food_group,
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

female_feeding_detailed <- female_feeding_detailed %>%
  left_join(food_production, by = c("year" = "next_year"))

# glmer model -------------------------------------------------------------------
# fit generalized linear mixed effects model with two-column binary response
model <- glmer(midden_status ~ food_group + cone_index_previous_scaled + mushroom_index_previous_scaled + (1 | squirrel_id) + (1 | year),
               data = female_feeding_detailed,
               family = binomial(link = "logit"))

#check residuals
sim_res <- simulateResiduals(model) #remember: with large sample sizes, even very small deviations can become significant
plot(sim_res) 

testOutliers(sim_res) #no significant outliers

#model summary
summary(model)

# generate predictions and plot --------------------------------------------
##note: model still only predicts for on-midden feeding, but off-midden feeding can be calculated as 1 - on-midden
#step 1: generate model predictions (on‑midden probabilities) by food group
pred_on_midden <- as.data.frame(emmeans(model, ~ food_group, type = "response"))

#create off‑midden predictions as the complement
pred_off_midden <- pred_on_midden
pred_off_midden$prob <- 1 - pred_off_midden$prob

#step 2: compute observed overall proportions (from the aggregated data)
#observed on‑midden counts by food group:
on_summary <- female_feeding_detailed %>%
  filter(midden_status == 1) %>%
  group_by(food_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(prop_detail = count / sum(count))

#observed off‑midden counts by food group:
off_summary <- female_feeding_detailed %>%
  filter(midden_status == 0) %>%
  group_by(food_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(prop_detail = count / sum(count))

#step 3: merge observed summaries with model predictions
on_summary <- on_summary %>%
  left_join(pred_on_midden, by = "food_group") %>%
  mutate(final_prop = prop_detail * prob)

off_summary <- off_summary %>%
  left_join(pred_off_midden, by = "food_group") %>%
  mutate(final_prop = prop_detail * prob)

#step 4: combine on‑ and off‑midden summaries into one data frame
final_predicted <- bind_rows(
  on_summary %>% mutate(midden_status = "on"),
  off_summary %>% mutate(midden_status = "off")) %>%
  mutate(
    Overall = "Overall",
    midden_status = factor(midden_status, levels = c("on", "off"))) %>%
  dplyr::select(midden_status, food_group, count, prop_detail, prob, SE, df, asymp.LCL, asymp.UCL, final_prop, Overall)

final_predicted$Overall <- factor(final_predicted$Overall, levels = "Overall")
final_predicted$midden_status <- factor(final_predicted$midden_status, levels = c("on", "off"))

#step 5: create the stacked bar plot with patterned aesthetics
female_mating_model <- ggplot(final_predicted, 
                            aes(x = Overall, y = final_prop, 
                                fill = food_group, pattern = midden_status)) +
  geom_bar_pattern(stat = "identity", position = "stack",
                   color = "black",              
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.02) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
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
       title = "Proportion of Food Types Consumed\nby Females During Mating",
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

female_mating_model

# Save the plot
ggsave("Output/female_mating_model.jpeg", plot = female_mating_model, width = 12, height = 7)

#compare the predicted on‑midden proportion for cone feeding against an expected 90% ------------------
expected_prob_on_midden_cone <- 0.90

on_midden_cone <- final_predicted %>%
  filter(food_group == "cone", midden_status == "on") %>%
  mutate(difference = expected_prob_on_midden_cone - final_prop,
         is_significant = asymp.LCL > expected_prob_on_midden_cone | asymp.UCL < expected_prob_on_midden_cone)

on_midden_cone

#calculate additional statistics (e.g., confidence intervals) --------------------------
calculate_stats <- function(df) {
  df %>%
    mutate(
      min_range = final_prop - SE,
      max_range = final_prop + SE,
      lower_ci = final_prop - 1.96 * SE,
      upper_ci = final_prop + 1.96 * SE)}

final_predicted_stats <- calculate_stats(final_predicted)

# data summary ------------------------------------------------------------
summary_table <- female_feeding_detailed %>%
  mutate(`Feeding location` = ifelse(midden_status == 1, "On-midden", "Off-midden")) %>%
  group_by(`Feeding location`, food_group) %>%
  summarise(`Sample size (n)` = n(), .groups = "drop") %>%
  rename(`Food type` = food_group) %>%
  dplyr::select(`Feeding location`, `Food type`, `Sample size (n)`)