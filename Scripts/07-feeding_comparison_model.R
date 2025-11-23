#load packages -----------------
source("Scripts/00-packages.R")

#read in data --------------------------------------------------------
mating_feeding <- read.csv("Input/male_mating_feeding.csv")
lactation_feeding <- read.csv("Input/female_lactation_feeding.csv")
tree_cones <- read.csv("Input/tree_cones.csv")

#combine feeding ----------------
feeding <- rbind(mating_feeding, lactation_feeding)

## *fix female sex column ------
feeding <- feeding %>%
  mutate(sex = case_when(
    sex == FALSE ~ "F",
    TRUE ~ as.character(sex)))

#create food groups -------------------------------------------
feeding_detailed <- feeding %>%
  mutate(food_group = case_when(
    detail == 2 ~ "cone",
    TRUE ~ "other")) %>% # everything else = other
  filter(!is.na(food_group)) %>%
  mutate(midden_status = ifelse(within_midden == TRUE, 1, 0))

## *ensure food_group is a factor ------
feeding_detailed$food_group <- factor(feeding_detailed$food_group,
                                           levels = c("cone", "other"))

# prepare cone production indices and join to feeding data --------
## want to join previous years cone index to following years feeding data
tree_cones_shifted <- tree_cones %>%
  mutate(next_year = year + 1) %>%
  rename(cone_index_previous = cone_index) %>%
  mutate(cone_index_previous_scaled = as.numeric(scale(cone_index_previous)))

feeding_detailed <- feeding_detailed %>%
  left_join(tree_cones_shifted %>% dplyr::select(next_year, cone_index_previous_scaled),
            by = c("year" = "next_year"))

#save
write.csv(feeding_detailed, "Output/feeding_detailed.csv", row.names = FALSE)

# model -------------------------------------------------------------------
# fit generalized linear mixed effects model with two-column binary response
model <- glmer(midden_status ~ food_group * sex + cone_index_previous_scaled + (1 | squirrel_id) + (1 | year),
               data = feeding_detailed,
               family = binomial(link = "logit"))

#check residuals
sim_res <- simulateResiduals(model) #remember: with large sample sizes, even very small deviations can become significant
plot(sim_res) 

testOutliers(sim_res) #no significant outliers
testDispersion(sim_res) #no overdispersion - dispersion = 0.94446 which is close to 1 (which is what you want)

#model summary
summary(model)

# generate model-based predictions: probability of a feeding event being within each food type grouping  --------------------------------------------
##note: model still only predicts for on-midden feeding, but off-midden feeding can be calculated as 1 - on-midden
## step 1: generate model predictions (on‑midden probabilities) by food group -------
pred_on_midden <- as.data.frame(emmeans(model, ~ food_group * sex, type = "response"))

### *create off‑midden predictions as the complement derived from on-midden predictions ------
pred_off_midden <- pred_on_midden %>%
  rename(old_LCL = asymp.LCL, old_UCL = asymp.UCL) %>%
  transmute(
    food_group,
    sex,
    prob       = 1 - prob,   
    SE,
    df,
    asymp.LCL  = 1 - old_UCL,
    asymp.UCL  = 1 - old_LCL)

## step 2: compute observed overall frequency of each (food_group and location)------
#### *observed on‑midden counts by food group: ---------
on_summary <- feeding_detailed %>%
  filter(midden_status == 1) %>%
  group_by(sex, food_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(sex) %>%
  mutate(prop_detail = count / sum(count)) %>%
  ungroup()

### *observed off‑midden counts by food group: -------
off_summary <- feeding_detailed %>%
  filter(midden_status == 0) %>%
  group_by(sex, food_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(sex) %>%
  mutate(prop_detail = count / sum(count)) %>%
  ungroup()

## step 3: merge observed summaries with model predictions - this gives us the weighted predictions for diet composition ------
on_summary <- on_summary %>%
  left_join(pred_on_midden, by = c("food_group", "sex")) %>%
  mutate(final_prop = prop_detail * prob,
         CI_lower   = prop_detail * asymp.LCL,
         CI_upper   = prop_detail * asymp.UCL,
         midden_status = "on")

off_summary <- off_summary %>%
  left_join(pred_off_midden, by = c("food_group", "sex")) %>%
  mutate(final_prop = prop_detail * prob,
         CI_lower   = prop_detail * asymp.LCL,
         CI_upper   = prop_detail * asymp.UCL,
         midden_status = "off")

## step 4: combine on‑ and off‑midden summaries into one data frame --------
final_predicted <- bind_rows(on_summary, off_summary) %>%
  mutate(
    midden_status = factor(midden_status, levels = c("on", "off")),
    sex = factor(sex, levels = c("M", "F")),
    Overall = ifelse(sex == "M", "Male: Mating", "Female: Lactation")) %>%
  ungroup() %>%
  dplyr::select(Overall, midden_status, sex, food_group, count, prop_detail,
                prob, SE, df, CI_lower, CI_upper, final_prop)

final_predicted$midden_status <- factor(final_predicted$midden_status, levels = c("on", "off"))

#save as csv
write.csv(final_predicted, "Output/final_weighted_predictions.csv", row.names = FALSE)

# create the bar plot with patterned aesthetics - plotting predicted diet compositions -------------------------------------------------------------------
#adjust factor order so male comes first
final_predicted$Overall <- factor(final_predicted$Overall,
                                  levels = c("Male: Mating", "Female: Lactation"))

pos <- position_dodge(width = 0.72)

#source theme
source("Scripts/00-plot_theme.R")

feeding_comparison <- ggplot(final_predicted, aes(x = food_group, y = final_prop,
      fill = food_group, pattern = midden_status, group = midden_status)) +
  geom_col_pattern(
    position = pos, width = 0.62,
    colour = "black",
    pattern_fill = "black",
    pattern_angle = 45,
    pattern_density = 0.12,
    pattern_spacing = 0.02) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), position = pos, width = 0.12, linewidth = 0.6) +
  facet_wrap(~ Overall, nrow = 1) +
  scale_x_discrete(expand = expansion(mult = c(0.4, 0.4))) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0), limits = c(0, 1.05)) +
  coord_cartesian(ylim = c(0, 1.0)) +
  scale_fill_manual(
    values = c("cone" = "#E69F00", "other" = "#009E73"),
    labels = c("Spruce cone seed", "Non-seed"),
    name = "Food type") +
  scale_pattern_manual(
    values = c("on" = "none", "off" = "stripe"),
    labels = c("On-midden", "Off-midden"),
    name = "Feeding location",
    guide = guide_legend(override.aes = list(fill = "white", colour = "black"))) +
  guides(
    fill = guide_legend(
      override.aes = list(pattern = "none", colour = "black"),
      order = 2),
    pattern = guide_legend(
      override.aes = list(fill = "white", colour = "black"),
      order = 1)) +
  labs(x = NULL, y = "Predicted proportion of total feeding events") +
  theme_thesis() +
  theme(strip.text = element_text(size = 21),
        legend.position = "right",
        axis.text.x = element_blank(),
        legend.key.height = unit(0.9, "cm"))
    
feeding_comparison

#save
ggsave("Output/feeding_comparison_model.jpeg", plot = feeding_comparison, width = 12, height = 7)

# male vs female feeding --------------------------------------------------
# predicted proportion of feeding events that occur on- vs off-midden by food group and sex
## this is for on-midden feeding
emm <- emmeans(model, ~ food_group * sex, type = "response")
summary(emm)
#off-midden feeding = 1 - on-midden prob (*100 for percent)
# interpretation is not, "males feed on cones on the midden for 55.9% of their diet, 
## but instead, "55.9% of cone feedings are on-midden for males during mating"
### this is telling me the probability of the proportion of food events of a food group occurring on- vs off-midden

# data summary ------------------------------------------------------------
summary_table <- feeding_detailed %>%
  mutate(`Feeding location` = ifelse(midden_status == 1, "On-midden", "Off-midden"),
         Sex = ifelse(sex == "M", "Male", "Female")) %>%
  group_by(Sex, `Feeding location`, food_group) %>%
  summarise(`Sample size (n)` = n(), .groups = "drop") %>%
  rename(`Food type` = food_group) %>%
  dplyr::select(Sex, `Feeding location`, `Food type`, `Sample size (n)`)

length(unique(feeding_detailed$squirrel_id))
length(unique(feeding_detailed$squirrel_id[feeding_detailed$sex == "M"]))
length(unique(feeding_detailed$squirrel_id[feeding_detailed$sex == "F"]))
length(unique(feeding_detailed$year))
