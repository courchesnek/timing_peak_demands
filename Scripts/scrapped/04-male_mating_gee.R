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

#make within_midden column numeric
feeding_mating$feeding_loc <- as.numeric(feeding_mating$within_midden)

#create detailed food groups -------------------------------------------
male_feeding_detailed <- feeding_mating %>%
  mutate(food_group = case_when(
      detail == 2 ~ "cone",
      detail == 4 ~ "mushroom/truffle",
      detail == 3 ~ "spruce_bud",
      TRUE ~ "other")) %>% # everything else = other
  filter(!is.na(food_group))

male_feeding_detailed$food_group <- factor(male_feeding_detailed$food_group)

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

# GEE model -------------------------------------------------------------------
gee_model <- geeglm(feeding_loc ~ food_group + cone_index_previous_scaled + mushroom_index_previous_scaled, 
                    id = squirrel_id, 
                    data = male_feeding_detailed,
                    family = binomial,
                    corstr = "exchangeable") #accounts for the correlation between repeated measures of the same ind squirrel

summary(gee_model)

#extract model-predicted probability
pred_prob <- mean(predict(gee_model, type = "response"))
cat("Model-predicted proportion of on_midden_cone:", pred_prob, "\n")
#on average, 64.5% of male feeding events during mating involve on-midden cones
#weak-moderate correlation (0.211) between repeated measures within each squirrel - meaning their feeding locations are not spatially independent
##some correlation makes sense since these are on-midden events, which is a small space so these locations are clustered

#test whether on-midden cone feeding (~64.5%) significantly differs from expected range (90-100%)
#prediction: on-midden feeding = 90-100%

#z-test - compare model predicted probabilty against hypothesized 0.90
#extract model's estimated intercept
intercept <- gee_model$coefficients[1]

#convert the log-odds to the predicted probability (proportion)
p_model <- plogis(intercept)

#extract the number of clusters (observations) from the model
n <- 410 #number of distinct groupings of repeated measurements (not the number of unique squirrels)

#define the hypothesized proportion
p_hypothesized <- 0.90

#calculate the Z-score
z_score <- (p_model - p_hypothesized) / sqrt((p_hypothesized * (1 - p_hypothesized)) / n)

#calculate the p-value (two-tailed test)
p_value <- 2 * exp(pnorm(-abs(z_score), log.p = TRUE))

#print the results
cat("Model-predicted proportion:", p_model, "\n")
print(z_score)
print(p_value, digits = 20) #p-value is very low, so we reject the hypothesis that the proportion of on-midden feeding is 90-100%

# generate predictions and plot based on detailed food groups -------------------------------------------
#computer overall proportions for each food type
on_summary <- male_feeding_detailed %>%
  group_by(food_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(prop_detail = count / sum(count))

off_summary <- male_feeding_detailed %>%
  group_by(food_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(prop_detail = count / sum(count))

#merge summaries with GEE model predictions
on_summary <- on_summary %>%
  mutate(prob = pred_prob, 
         final_prop = prop_detail * prob)

off_summary <- off_summary %>%
  mutate(prob = pred_prob, 
         final_prop = prop_detail * (1 - prob))

#combine data
final_predicted <- bind_rows(
  on_summary %>% mutate(food_group = paste0("on_midden_", food_group)),
  off_summary %>% mutate(food_group = paste0("off_midden_", food_group))) %>%
  mutate(
    food_type_simple = case_when(
      food_group %in% c("on_midden_cone", "off_midden_cone") ~ "cone",
      food_group %in% c("on_midden_mushroom/truffle", "off_midden_mushroom/truffle") ~ "mushroom/truffle",
      food_group == "off_midden_spruce_bud" ~ "spruce_bud",
      food_group == "off_midden_other" ~ "other",
      TRUE ~ NA_character_),
    midden_status = if_else(grepl("^on_midden", food_group), "on", "off")) %>%
  mutate(
    Overall = "Overall",
    midden_status = factor(midden_status, levels = c("on", "off"))) %>%
  na.omit()

#stacked bar graph
male_mating_model <- ggplot(final_predicted, 
                            aes(x = Overall, y = final_prop, 
                                fill = food_type_simple, pattern = midden_status)) +
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

# data summary ------------------------------------------------------------
male_feeding_summary <- male_feeding_detailed %>%
  count(food_group, name = "sample_size")

# compare prop of on vs off midden feeding events between the sexes ----------------
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
