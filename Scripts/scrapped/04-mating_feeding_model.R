#load packages
source("Scripts/00-packages.R")

#read in data --------------------------------------------------------
feeding <- read.csv("Input/feeding_distances_all.csv")

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
male_feeding_yearly <- feeding_within_territory %>%
  filter(sex == "M", repro_stage == "mating", 
         year %in% mating_years) %>%
  group_by(year) %>%
  summarise(
    total_events = n(),
    off_midden = sum(within_midden == FALSE),
    on_midden = sum(within_midden == TRUE),
    .groups = "drop") %>%
  mutate(
    prop_off = off_midden / total_events,
    prop_on = on_midden / total_events)

feeding_mating <- feeding_within_territory %>%
  filter(sex == "M", repro_stage == "mating",
         year %in% mating_years)

length(unique(feeding_mating$squirrel_id))

length(unique(feeding_mating$year))
unique(feeding_mating$year)


# model -------------------------------------------------------------------
model <- glm(cbind(on_midden, off_midden) ~ factor(year),
             data = male_feeding_yearly,
             family = binomial)

summary(model)


# generate predictions and plot -------------------------------------------
pred_overall <- as.data.frame(emmeans(model, ~ year, type = "response"))

# Create a detailed food grouping variable
# (Note: adjust the conditions as needed for your system)
male_feeding_detailed <- feeding_mating %>%
  mutate(food_group = case_when(
    #on-midden: Only count if food is capital and the detail indicates cone or mushroom/truffle.
    within_midden == TRUE & detail == 2 ~ "on_midden_cone",
    within_midden == TRUE & detail == 4 ~ "on_midden_mushroom",
    #if on-midden but with any other detail, we ignore (set to NA)
    within_midden == TRUE & !(detail %in% c(2,4)) ~ NA_character_,
    #0ff-midden: All events that are off the midden get classified by detail
    within_midden == FALSE & detail == 2 ~ "off_midden_cone",
    within_midden == FALSE & detail == 4 ~ "off_midden_mushroom",
    within_midden == FALSE & detail == 1 ~ "off_midden_animal_material",
    within_midden == FALSE & detail == 3 ~ "off_midden_spruce_bud",
    within_midden == FALSE ~ "off_midden_other",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(food_group))

#compute proportions within each broad category
on_summary <- male_feeding_detailed %>%
  filter(grepl("on_midden", food_group)) %>%
  group_by(year, food_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(prop_detail = count / sum(count))

off_summary <- male_feeding_detailed %>%
  filter(grepl("off_midden", food_group)) %>%
  group_by(year, food_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(prop_detail = count / sum(count))

#merge with model predictions
on_summary <- on_summary %>%
  left_join(pred_overall, by = "year") %>%
  mutate(final_prop = prop_detail * prob)  #prob is predicted p(on-midden)

off_summary <- off_summary %>%
  left_join(pred_overall, by = "year") %>%
  mutate(final_prop = prop_detail * (1 - prob))

#combine the summaries:
final_predicted <- bind_rows(on_summary, off_summary)

#re-organize food groups
final_predicted <- final_predicted %>%
  mutate(
    food_type_simple = case_when(
      food_group %in% c("on_midden_cone", "off_midden_cone") ~ "cone",
      food_group %in% c("on_midden_mushroom", "off_midden_mushroom") ~ "mushroom/truffle",
      food_group == "off_midden_spruce_bud" ~ "spruce_bud",
      food_group == "off_midden_other" ~ "other",
      TRUE ~ NA_character_),
    midden_status = if_else(grepl("^on_midden", food_group), "on", "off"))

#relevel midden status - on midden on top of stack
final_predicted <- final_predicted %>%
  mutate(midden_status = factor(midden_status, levels = c("on","off")))

#plot stacked bar graph
male_mating_model <- ggplot(final_predicted, aes(x = factor(year), y = final_prop, fill = food_type_simple, pattern = midden_status)) +
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
    guide = guide_legend(
      override.aes = list(pattern = "none"))) +
  scale_pattern_manual(name = "Feeding Location",
                       breaks = c("on", "off"),
                       values = c("on" = "none", "off" = "stripe"),
                       labels = c("on" = "On Midden", "off" = "Off Midden")) +
  guides(
    fill = guide_legend(override.aes = list(pattern = "none"), order = 1),
    pattern = guide_legend(override.aes = list(fill = "white"), order = 2)) +
  labs(x = "Year",
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
    axis.text.x = element_text(hjust = 0.5, color = "black"),
    axis.text.y = element_text(color = "black"))

male_mating_model

#save
ggsave("Output/male_mating_model.jpeg", plot = male_mating_model, width = 12, height = 7)

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



