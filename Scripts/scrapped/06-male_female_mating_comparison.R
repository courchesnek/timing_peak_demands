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

# compare male vs female feeding during mating ----------------------------
feeding_mating <- feeding_within_territory %>%
  filter(repro_stage == "mating", year %in% mating_years)

#check sample sizes
length(unique(feeding_mating$squirrel_id))
length(unique(feeding_mating$year))
unique(feeding_mating$year)

#create detailed food groups + 8-level feeding category -------------------------------------------
feeding_detailed <- feeding_mating %>%
  mutate(food_group = case_when(
    detail == 2 ~ "cone",
    detail == 4 ~ "mushroom/truffle",
    detail == 3 ~ "spruce_bud",
    TRUE ~ "other"), # everything else = other
    midden_status = ifelse(within_midden, "on", "off"),
    sex           = factor(sex, levels = c("F","M")),
    feed_cat      = factor(
      paste(food_group, midden_status, sep = "_"),
      levels = c(
        "cone_on",               "cone_off",
        "mushroom/truffle_on",   "mushroom/truffle_off",
        "spruce_bud_on",         "spruce_bud_off",
        "other_on",              "other_off"))) %>% 
  filter(!is.na(food_group))

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

feeding_detailed <- feeding_detailed %>%
  left_join(food_production, by = c("year" = "next_year"))

# glmer model -------------------------------------------------------------------
# 8-model loop to compare male-mating vs female-mating feeding at population level
# vector of the 8 feedxlocation categories
cats <- levels(feeding_detailed$feed_cat)

# Loop: fit glm, pull sexM test AND get predicted probs
full_results <- map_dfr(cats, function(cat_level) {
  df <- feeding_detailed %>%
    mutate(is_this = as.integer(feed_cat == cat_level))
  
  # 1) fit the simple GLM
  mod <- glm(
    is_this ~ sex +
      cone_index_previous_scaled +
      mushroom_index_previous_scaled,
    data   = df,
    family = binomial(link="logit"))
  
  # 2) extract the sexM test
  sex_test <- broom::tidy(mod) %>%
    filter(term=="sexM") %>%
    transmute(
      feed_cat   = cat_level,
      estimate, std.error, statistic, p.value,
      odds_ratio = exp(estimate))
  
  # 3) get model‐adjusted probabilities for each sex
  em <- emmeans(mod, specs="sex", type="response")
  props <- as.data.frame(em) %>%
    rename(proportion = prob, SE_prop = SE) %>%
    transmute(
      feed_cat = cat_level,
      sex,
      proportion,
      SE_prop) 
  
  # 4) join them together
  left_join(sex_test, props, by="feed_cat")})

# 5) adjust p‐values across the 8 tests
full_results <- full_results %>%
  group_by(feed_cat) %>%
  mutate(p_adj = p.adjust(p.value, method="BH")) %>%
  ungroup()

print(full_results)

# more organizes results
# 1) extract one row per category of just the test stats
test_stats <- full_results %>%
  dplyr::select(feed_cat, estimate, std.error, statistic, p.value, odds_ratio, p_adj) %>%
  distinct(feed_cat, .keep_all = TRUE)

# 2) pivot the proportions into two columns (F vs. M)
props_wide <- full_results %>%
  dplyr::select(feed_cat, sex, proportion, SE_prop) %>%
  pivot_wider(
    names_from  = sex,
    values_from = c(proportion, SE_prop),
    names_sep   = "_")
# this gives columns: proportion_F, proportion_M, SE_prop_F, SE_prop_M

# 3) join them together
final_results <- left_join(test_stats, props_wide, by = "feed_cat")

print(final_results)

# save results as a csv
write.csv(final_results, file = "Output/feeding_sex_comparison.csv", row.names = FALSE)


