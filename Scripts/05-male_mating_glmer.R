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

#save
write.csv(male_feeding_detailed, "Output/male_feeding_detailed.csv", row.names = FALSE)

# glmer model -------------------------------------------------------------------
# fit generalized linear mixed effects model with two-column binary response
model <- glmer(midden_status ~ food_group + cone_index_previous_scaled + mushroom_index_previous_scaled + (1 | squirrel_id) + (1 | year),
               data = male_feeding_detailed,
               family = binomial(link = "logit"))

#check residuals
sim_res <- simulateResiduals(model) #remember: with large sample sizes, even very small deviations can become significant
plot(sim_res) 

testOutliers(sim_res) #no significant outliers

#model summary
summary(model)

# generate model-based predictions: probability of a feeding event being within each food type grouping  --------------------------------------------
##note: model still only predicts for on-midden feeding, but off-midden feeding can be calculated as 1 - on-midden
#step 1: generate model predictions (on‑midden probabilities) by food group
pred_on_midden <- as.data.frame(emmeans(model, ~ food_group, type = "response"))

#create off‑midden predictions as the complement derived from on-midden predictions
pred_off_midden <- pred_on_midden %>%
  rename(old_LCL = asymp.LCL, old_UCL = asymp.UCL) %>%
  transmute(
    food_group,
    prob       = 1 - prob,   
    SE,
    df,
    asymp.LCL  = 1 - old_UCL,
    asymp.UCL  = 1 - old_LCL)

#step 2: compute observed overall frequency of each (food_group and location)
#observed on‑midden counts by food group:
on_summary <- male_feeding_detailed %>%
  filter(midden_status == 1) %>%
  group_by(food_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(prop_detail = count / sum(count))

#observed off‑midden counts by food group:
off_summary <- male_feeding_detailed %>%
  filter(midden_status == 0) %>%
  group_by(food_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(prop_detail = count / sum(count))

#step 3: merge observed summaries with model predictions - this gives us the weighted predictions for diet composition
on_summary <- on_summary %>%
  left_join(pred_on_midden, by = "food_group") %>%
  mutate(final_prop = prop_detail * prob,
         CI_lower   = prop_detail * asymp.LCL,
         CI_upper   = prop_detail * asymp.UCL)

off_summary <- off_summary %>%
  left_join(pred_off_midden, by = "food_group") %>%
  mutate(final_prop = prop_detail * prob,
         CI_lower   = prop_detail * asymp.LCL,
         CI_upper   = prop_detail * asymp.UCL)

#step 4: combine on‑ and off‑midden summaries into one data frame
final_predicted <- bind_rows(
  on_summary %>% mutate(midden_status = "on"),
  off_summary %>% mutate(midden_status = "off")) %>%
  mutate(
    Overall = "Overall",
    midden_status = factor(midden_status, levels = c("on", "off"))) %>%
  mutate(
    across(.cols = c(prob, SE, df, CI_lower, CI_upper, final_prop),
           .fns = ~ round(.x, 4))) %>%
  dplyr::select(midden_status, food_group, count, prop_detail, prob, SE, df, CI_lower, CI_upper, final_prop, Overall)

final_predicted$Overall <- factor(final_predicted$Overall, levels = "Overall")
final_predicted$midden_status <- factor(final_predicted$midden_status, levels = c("on", "off"))

#save as csv
write.csv(final_predicted, "Output/final_weighted_predictions_m.csv", row.names = FALSE)

#step 5: create the stacked bar plot with patterned aesthetics - plotting predicted diet compositions
male_mating_model <- ggplot(final_predicted, 
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

# Save the plot
ggsave("Output/male_mating_model.jpeg", plot = male_mating_model, width = 12, height = 7)

# statistical check against predictions: one-sided significant test -----------------------------------
#1) During mating, the proportion of male feeding events on 
#     cached cones (on-midden) will exceed 90%. 
cone_on <- final_predicted %>% 
  filter(food_group == "cone", midden_status == "on")
cone_on

# check if the lower CI is above 0.90
cone_on_meets <- cone_on$asymp.LCL > 0.90
# ANSWER = FALSE - the lower CI is 0.46, which does not meet the 90% threshold

#2) During mating, the combined proportion of male feeding events on mushrooms, 
#     spruce buds and other non-cone foods off-midden will be less than 10%.
non_cones_off <- final_predicted %>%
  filter(food_group %in% c("mushroom/truffle","spruce_bud","other"),
         midden_status == "off")

combined_prop <- sum(non_cones_off$final_prop)
combined_SE   <- sqrt(sum(non_cones_off$SE^2))
combined_LCL  <- combined_prop - 1.96 * combined_SE
combined_UCL  <- combined_prop + 1.96 * combined_SE

tibble(
  combined_prop,
  combined_LCL,
  combined_UCL,
  meets    = combined_UCL < 0.10)
# ANSWER = FALSE - the combined proportion is 0.170 (or 17%), and the combined 
#   upper CI is 0.710, which is well above the 10% threshold

#3) During mating, the proportion of male feeding events on cones off-midden 
#     will be below 5%, reflecting snow-limited access.
cone_off <- final_predicted %>%
  filter(food_group == "cone", midden_status == "off")

# inspect predicted proportion and its 95% CI
cone_off$final_prop   
cone_off$asymp.LCL   
cone_off$asymp.UCL

# test
cone_off$asymp.UCL < 0.05
# ANSWER = FALSE - the upper CI is 0.537, which is well above 0.05

# data summary ------------------------------------------------------------
summary_table <- male_feeding_detailed %>%
  mutate(`Feeding location` = ifelse(midden_status == 1, "On-midden", "Off-midden")) %>%
  group_by(`Feeding location`, food_group) %>%
  summarise(`Sample size (n)` = n(), .groups = "drop") %>%
  rename(`Food type` = food_group) %>%
  dplyr::select(`Feeding location`, `Food type`, `Sample size (n)`)
