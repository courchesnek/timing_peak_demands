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

# female on- vs off-midden feeding during lactation -----------------------
feeding_lactation <- feeding_within_territory %>%
  filter(sex == "F", repro_stage == "lactation",
         year %in% mating_years)

length(unique(feeding_lactation$squirrel_id))

length(unique(feeding_lactation$year))
unique(feeding_lactation$year)

#create detailed food groups -------------------------------------------
female_feeding_detailed <- feeding_lactation %>%
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
sim_res <- simulateResiduals(model, n = 1000) #remember: with large sample sizes, even very small deviations can become significant
plot(sim_res) 

#test for overdispersion
testDispersion(sim_res) #no overdispersion detected

#outlier test with bootstrap
testOutliers(sim_res, type = "bootstrap", n = 1000) #no significant outliers

#model summary
summary(model)

# generate predictions and plot --------------------------------------------
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
write.csv(final_predicted, "Output/final_weighted_predictions_f.csv", row.names = FALSE)

#step 5: create the stacked bar plot with patterned aesthetics
female_lactation_model <- ggplot(final_predicted, 
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
       title = "Proportion of Food Types Consumed\nby Females During Lactation",
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

female_lactation_model

# Save the plot
ggsave("Output/female_lactation_model.jpeg", plot = female_lactation_model, width = 12, height = 7)

# statistical check against predictions: one-sided significant test -----------------------------------
#1) During lactation, the proportion of female feeding events on mushrooms, spruce buds and 
#     other non-cone foods off-midden will exceed 50%.
off_noncones <- final_predicted %>%
  filter(midden_status == "off",
         food_group %in% c("mushroom/truffle","spruce_bud","other")) %>%
  summarize(
    prop        = sum(final_prop),
    # approximate variance from the CIs
    var         = sum(((CI_upper - CI_lower)/(2*1.96))^2)) %>%
  mutate(
    se          = sqrt(var),
    lower_bound = prop - 1.96*se,
    meets_50    = lower_bound > 0.50)
# check if the lower CI is above 0.50
# ANSWER = FALSE - the lower CI is 0.303, which does not meet the 50% threshold

#2) During lactation, the proportion of female feeding events on scattered cones (off-midden) 
#     will exceed that on cached cones (on-midden). 
cone_on  <- final_predicted %>% 
              filter(food_group == "cone",  midden_status == "on")
cone_off <- final_predicted %>% 
              filter(food_group == "cone",  midden_status == "off")

cone_on_upper  <- cone_on  %>% pull(CI_upper)
cone_off_lower <- cone_off %>% pull(CI_lower)

# test
cone_off_lower > cone_on_upper
# ANSWER = FALSE - the off-midden cone CI lower bound does not exceed the on-midden cone CI upper bound

# data summary ------------------------------------------------------------
summary_table <- female_feeding_detailed %>%
  mutate(`Feeding location` = ifelse(midden_status == 1, "On-midden", "Off-midden")) %>%
  group_by(`Feeding location`, food_group) %>%
  summarise(`Sample size (n)` = n(), .groups = "drop") %>%
  rename(`Food type` = food_group) %>%
  dplyr::select(`Feeding location`, `Food type`, `Sample size (n)`)