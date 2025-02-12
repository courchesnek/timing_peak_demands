#load packages
source("Scripts/00-packages.R")

# read in data ----------------------------
feeding <- read.csv("Input/all_feeding_census.csv")

feeding <- feeding %>%
  mutate(date = as.Date(date)) %>%
  na.omit()

#filter out unnecessary columns
feeding <- feeding %>%
  dplyr::select(-locx_obs, -locy_obs, -snow, -locx_census, -locy_census)

#how many ind squirrels?
length(unique(feeding$squirrel_id))

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

# merge feeding and DEE values --------------------------------------------
feeding <- feeding %>%
  left_join(DEE_values, by = c("sex", "season")) %>%
  mutate(
    DEE_kJ_day = case_when(
      #males during lactation: assign "non-breeding" DEE
      sex == "M" & season == "lactation" ~ DEE_values$DEE_kJ_day[DEE_values$sex == "M" & DEE_values$season == "non-breeding"],
      #females during mating before May 1: assign "winter" DEE
      sex == "F" & season == "mating" & format(as.Date(date), "%m-%d") < "05-01" ~ DEE_values$DEE_kJ_day[DEE_values$sex == "F" & DEE_values$season == "winter"],
      #females during mating after May 1: assign "non-breeding" DEE
      sex == "F" & season == "mating" & format(as.Date(date), "%m-%d") >= "05-01" ~ DEE_values$DEE_kJ_day[DEE_values$sex == "F" & DEE_values$season == "non-breeding"],
      #keep existing DEE_kJ_day otherwise
      TRUE ~ DEE_kJ_day))

#fix female winters
feeding <- feeding %>%
  mutate(
    season = case_when(
      sex == "F" & season == "mating" & DEE_kJ_day == 207 ~ "winter", #reassign to winter
      TRUE ~ season))

#make sure everything worked
DEE_summary <- feeding %>%
  group_by(sex, season) %>%
  summarise(
    mean_DEE = mean(DEE_kJ_day, na.rm = TRUE),
    min_DEE = min(DEE_kJ_day, na.rm = TRUE),
    max_DEE = max(DEE_kJ_day, na.rm = TRUE),
    n_events = n())

# calculate proportions ---------------------------------------------------
feeding_proportions <- feeding %>%
  group_by(sex, season, food_type) %>% #group by sex, season, and food type
  summarise(
    total_events = n(),                  #count total feeding events
    DEE_kJ_day = unique(DEE_kJ_day),     #use unique DEE value for the season
    .groups = "drop"
  ) %>%
  group_by(sex, season) %>% #group by sex and season for proportions
  mutate(
    proportion_events = total_events / sum(total_events), #proportion by events
    proportion_DEE = DEE_kJ_day * proportion_events       #how much DEE is coming from that prop food type
  ) %>%
  arrange(sex, season, food_type) %>%
  ungroup()


# plot --------------------------------------------------------------------
#reorder season levels
feeding_proportions <- feeding_proportions %>%
  mutate(
    season = factor(season, levels = c("winter", "mating", "lactation", "non-breeding")),
    food_type = factor(food_type, levels = c("income", "capital")))

energetics <- ggplot(feeding_proportions, aes(x = season, y = proportion_DEE, fill = food_type)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.7) +
  facet_wrap(~sex, labeller = labeller(sex = c("F" = "Female", "M" = "Male"))) +
  labs(
    title = "Proportion of Feeding Events by Sex and Season, Scaled to Daily Energy Expenditure Estimates",
    x = "Season",
    y = "Daily Energy Expenditure (kJ/day)",
    fill = "Food Type") +
  scale_fill_manual(
    values = c("income" = "#99FF66", "capital" = "#FF9933"),
    labels = c("Income Feeding", "Capital Feeding")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"))

energetics

#save
ggsave("Output/feeding_energetics.jpeg", plot = energetics, width = 12, height = 6)

type_proportions <- ggplot(feeding_proportions, aes(x = season, y = proportion_events, fill = food_type)) +
  geom_bar(stat = "identity", position = "stack", color = "black", width = 0.7) +
  facet_wrap(~sex, labeller = labeller(sex = c("F" = "Female", "M" = "Male"))) +
  labs(
    title = "Proportion of Capital and Income Feeding by Sex and Season",
    x = "Season",
    y = "Proportion of Feeding Events",
    fill = "Food Type") +
  scale_fill_manual(
    values = c("income" = "#99FF66", "capital" = "#FF9933"),
    labels = c("Income Feeding", "Capital Feeding")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"))

type_proportions

#save
ggsave("Output/type_proportions.jpeg", plot = type_proportions, width = 10, height = 6)

# plot detailed food types ------------------------------------------------
food_categories <- tibble(
  detail = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 27, 28, 29, 31, 32),
  category = c(
    "Animal Material", "Cone", "Buds", "Mushrooms/Truffles", "Bark", "New Cone", "Willow Leaves", 
    "Poplar Buds", "Bearberry Flower", "Pollen Cone", "New Spruce Needle", "Aspen Leaves",
    "Bearberry Berries", "Grass", "Fireweed", "Witches Broom", "Willow Bark Scales", 
    "Aspen Catkin", "Water Drops", "Lichen/Fungus", "Willow Buds", "Bark Beetle Larvae",
    "Insects", "New Mushroom", "Truffles"))

food_type_counts <- feeding %>%
  filter(detail %in% food_categories$detail) %>%
  left_join(food_categories, by = "detail") %>%
  group_by(category) %>%
  summarise(
    total_events = n(),
    .groups = "drop") %>%
  arrange(desc(total_events))

#condense food categories
main_food_categories <- c(2, 3, 4, 6, 31)

#recode the 'detail' column into food categories
feeding <- feeding %>%
  mutate(
    detail = case_when(
      detail == 2 ~ "old cone",
      detail == 3 ~ "spruce buds",
      detail == 4 ~ "old mushroom",
      detail == 6 ~ "new cone",
      detail == 31 ~ "new mushroom",
      TRUE ~ "other"))

feeding_summary <- feeding %>%
  group_by(sex, season, year_type, detail) %>%
  summarise(total_events = n(), .groups = "drop") %>%
  group_by(sex, season, year_type) %>%
  mutate(proportion_events = total_events / sum(total_events)) %>%
  ungroup()

#reorder 'season', 'year_type' and 'food group' for consistent display order
feeding_summary <- feeding_summary %>%
  mutate(
    season = factor(season, levels = c("winter", "mating", "lactation", "non-breeding")),
    year_type = factor(year_type, levels = c("mast", "post-mast", "non-mast")),
    detail = factor(detail, levels = c("new cone", "new mushroom", "old cone", "old mushroom", "spruce buds", "other")))

detailed_feeding <- ggplot(feeding_summary, aes(x = season, y = proportion_events, fill = detail)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8, color = "black") +
  facet_grid(sex ~ year_type, labeller = labeller(
    sex = c("F" = "Female", "M" = "Male"),
    year_type = c("mast" = "Mast", "post-mast" = "Post-Mast", "non-mast" = "Non-Mast"))) +
  labs(
    title = "Proportion of Feeding Events by Food Type, Season, Sex, and Year Type",
    x = "Season",
    y = "Proportion of Feeding Events",
    fill = "Food Type") +
  scale_fill_manual(
    values = c(
      "old cone" = "#E69F00",
      "spruce buds" = "#56B4E9",
      "old mushroom" = "#009E73",
      "new cone" = "#F0E442",
      "witch's broom" = "#D55E00",
      "new mushroom" = "#CC79A7",
      "other" = "#999999")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    panel.spacing = unit(1, "lines"))

detailed_feeding

#save
ggsave("Output/feeding_sources.jpeg", plot = detailed_feeding, width = 10, height = 6)


#Q1: Do food type proportions differ between sexes and across seasons? -----------------------------------------------
#ensure food_type is a factor
feeding <- feeding %>%
  mutate(food_type = factor(food_type),
         season = factor(season),
         sex = factor(sex),
         squirrel_id = factor(squirrel_id))

#relevel season to set non-breeding as the reference level
feeding <- feeding %>%
  mutate(season = relevel(factor(season), ref = "non-breeding"))

#let's run a GLMM binomial model to compare capital vs income feeding within one model while including squirrel id as a random effect
model_foodtype <- glmer(food_type ~ sex * season + (1 | squirrel_id), 
                        family = binomial, data = feeding,
                        control = glmerControl(optimizer = "bobyqa", 
                        optCtrl = list(maxfun = 200000, tol = 1e-06))) #increase maximum number of function evaluations
#model baselines = capital, females, non-breeding

summary(model_foodtype)
foodtype_summary <- tidy(model_foodtype)
#positive estimate = preference for income resources
#negative estimate = preference for capital resources (model baseline)

#calculate the odds ratios
foodtype_summary$odds_ratio <- exp(foodtype_summary$estimate)

#calculate 95% confidence intervals for the odds ratios
conf_int <- confint(model_foodtype, level = 0.95)

#add the confidence intervals to the summary
foodtype_summary$ci_lower <- exp(conf_int[, 1])  # lower bound
foodtype_summary$ci_upper <- exp(conf_int[, 2])  # upper bound

# Format the table (optional)
model_summary_table <- model_summary %>%
  select(term, estimate, std.error, z.value, p.value, odds_ratio, ci_lower, ci_upper) %>%
  arrange(term)



