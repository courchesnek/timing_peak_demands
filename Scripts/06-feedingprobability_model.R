#load packages
source("Scripts/00-packages.R")

# read in data ----------------------------
feeding <- read.csv("Input/all_feeding_census.csv")
midden_cones <- read.csv("Input/midden_cones.csv")

feeding <- feeding %>%
  mutate(date = as.Date(date)) %>%
  na.omit()

#filter out unnecessary columns
feeding <- feeding %>%
  dplyr::select(-locx_obs, -locy_obs, -snow, -locx_census, -locy_census)

#how many ind squirrels?
length(unique(feeding$squirrel_id))

# align feeding repro_stage with DEE seasons ------------------------------
feeding <- feeding %>%
  mutate(
    date = as.Date(date),  #ensure the `date` column is in Date format
    season = case_when(
      #assign 'winter' if it's non-breeding before May 1
      repro_stage == "non-breeding" & format(date, "%m-%d") < "05-01" ~ "winter",
      #keep other values as they are from repro_stage
      TRUE ~ repro_stage))


# adjust levels and data classes ---------------------------------------------
feeding <- feeding %>%
  mutate(food_type = factor(food_type),
         season = factor(season),
         sex = factor(sex),
         squirrel_id = factor(squirrel_id))

midden_cones <- midden_cones %>%
  mutate(squirrel_id = factor(squirrel_id))

feeding <- feeding %>%
  mutate(season = relevel(factor(season), ref = "non-breeding"),
         year = as.numeric(year)) %>%
  rename(feeding_year = year)

# join feeding and cone data ----------------------------------------------
#need to make sure cones from year 1 are being compared to feeding in year +1 (ex: 2022 cones comapred to 2023 feeding)
midden_cones_adjusted <- midden_cones %>%
                      dplyr::rename(
                         cache_size_new_prev_year = cache_size_new,
                         total_cones_prev_year = total_cones,
                         log_cache_size_new_prev_year = log_cache_size_new,
                         log_total_cones_prev_year = log_total_cones) %>%
                      mutate(year = as.numeric(year), following_year = year + 1)

#join feeding and cones based on year +1 from cones
feeding_cache <- left_join(feeding, midden_cones_adjusted %>%
                           dplyr::select(squirrel_id, following_year, 
                           cache_size_new_prev_year, total_cones_prev_year, 
                           log_cache_size_new_prev_year, log_total_cones_prev_year),
                           by = c("squirrel_id", "feeding_year" = "following_year")) %>%
                 na.omit()

# make food type binary and address sample size issues  ------------------------------
#create a binary response variable: capital = 1, income = 0 - keep as numeric
feeding_cache_binomial <- feeding_cache %>%
  mutate(food_type_bin = ifelse(food_type == "capital", 1, 0))  #capital = 1, income = 0

#create a column to count feeding events by squirrel and by year
feeding_cache_binomial <- feeding_cache_binomial %>%
  group_by(squirrel_id, feeding_year) %>%
  mutate(feeding_events = n()) %>%
  ungroup()

#filter for a minimum of 30 feeding events 
feeding_cache_binomial_30 <- feeding_cache_binomial %>%
  group_by(squirrel_id, feeding_year) %>%
  mutate(feeding_events = n()) %>%
  ungroup() %>%
  filter(feeding_events >= 30) #want to minimize error to be reasonable while maintaining total sample size

#remove unnecessary columns
feeding_cache_binomial_30 <- feeding_cache_binomial_30 %>%
  dplyr::select(-grid, -date, -repro_stage, -cache_size_new_prev_year, -total_cones_prev_year,)

#summarise data ---------------------------------
feeding_summary <- feeding_cache_binomial_30 %>%
  group_by(sex, season, food_type) %>%
  summarise(
    total_observations = n(),
    .groups = "drop") %>%
  pivot_wider(
    names_from = food_type, 
    values_from = total_observations,
    values_fill = list(total_observations = 0))

#save
write.csv(feeding_summary, "Output/capvsinc_summary.csv", row.names = FALSE)

#how many squirrels left?
length(unique(feeding_cache_binomial_30$squirrel_id))

feeding_cache_binomial_30 %>%
  group_by(sex) %>%
  summarise(unique_squirrels = n_distinct(squirrel_id))

#how many years of data?
length(unique(feeding_cache_binomial_30$feeding_year))

# model -------------------------------------------------------------------
##re-level season so winter is the reference category
feeding_cache_binomial_30$season <- relevel(feeding_cache_binomial_30$season, ref = "winter")

#fit a GLMM model to predict capital feeding based on cones cached and sex - removing total cones variable
final_model <- glmer(food_type_bin ~ log_cache_size_new_prev_year * sex + sex * season + (1 | squirrel_id),
                     family = binomial(link = "logit"),
                     data = feeding_cache_binomial_30,
                     control = glmerControl(optimizer = "bobyqa", 
                     optCtrl = list(maxfun = 1000000)))  #increase maxfun (number of iterations) to fix convergence issues

#model reference categories?
contrasts(feeding_cache_binomial_30$sex) #female is reference category
contrasts(feeding_cache_binomial_30$season) #winter is reference category

model_summary <- summary(final_model)

#extract the model output into a tidy format
model_output <- tidy(final_model)

model_comparisons <- model_output %>%
  filter(term %in% c("(Intercept)",
                     "log_cache_size_new_prev_year", 
                     "seasonlactation", 
                     "seasonmating", 
                     "seasonnon-breeding",
                     "sexM",
                     "log_cache_size_new_prev_year:sexM",
                     "sexM:seasonlactation", 
                     "sexM:seasonmating", 
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
                 "log_cache_size_new_prev_year vs log_cache_size_new_prev_year:sexM", 
                 "seasonlactation vs sexM:seasonlactation", 
                 "seasonmating vs sexM:seasonmating", 
                 "seasonnon-breeding vs sexM:seasonnon-breeding"),
  Overlap = c(
    compare_intervals("(Intercept)", "sexM"),
    compare_intervals("log_cache_size_new_prev_year", "log_cache_size_new_prev_year:sexM"),
    compare_intervals("seasonlactation", "sexM:seasonlactation"),
    compare_intervals("seasonmating", "sexM:seasonmating"),
    compare_intervals("seasonnon-breeding", "sexM:seasonnon-breeding")))

#clean up model output to save as csv
model_output <- model_output %>%
  dplyr::select(-effect, -group)

model_output <- model_output[-11, ]

model_output <- model_output %>%
  rename(zvalue = statistic)

#save
write.csv(model_output, "Output/income_model_output.csv", row.names = FALSE)

# plots -------------------------------------------------------------------
#define the range of log_cache_size_new_prev_year - generate predictions only within the actual scope of the data (i.e. not huge unrealistic cache sizes)
cache_seq <- seq(
  from = min(feeding_cache_binomial_30$log_cache_size_new_prev_year, na.rm = TRUE),
  to   = max(feeding_cache_binomial_30$log_cache_size_new_prev_year, na.rm = TRUE),
  length.out = 100)

#use emmeans to get predicted probabilities on the response scale (i.e., 0-1)
emm <- emmeans(
  final_model, 
  ~ sex * season * log_cache_size_new_prev_year,
  at = list(log_cache_size_new_prev_year = cache_seq),
  type = "response") #this is crucial for probabilities instead of log-odds

emm_df <- as.data.frame(emm)

#rename columns for clarity
pred_data <- emm_df %>%
  rename(
    predicted_probability = prob,
    conf.low = asymp.LCL,
    conf.high = asymp.UCL)

#plot - probability of feeding on capital by number of cached cones and across seasons
capitalfeed_cache <- ggplot(pred_data, aes(x = log_cache_size_new_prev_year, y = predicted_probability, color = season, fill = season)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = season), alpha = 0.1, color = NA) +
  geom_line(linewidth = 1) +
  facet_wrap(~ sex) +
  scale_color_manual(
    name = "Season",
    breaks = c("winter", "mating", "lactation", "non-breeding"),
    labels = c("Winter", "Mating", "Lactation", "Non-breeding"),
    values = c(
      "winter"       = "#00BFC4",
      "mating"       = "#C77CFF",
      "lactation"    = "#7CAE00",
      "non-breeding" = "#F8766D")) +
  scale_fill_manual(
    name = "Season",
    breaks = c("winter", "mating", "lactation", "non-breeding"),
    labels = c("Winter", "Mating", "Lactation", "Non-breeding"),
    values = c(
      "winter"       = "#00BFC4",
      "mating"       = "#C77CFF",
      "lactation"    = "#7CAE00",
      "non-breeding" = "#F8766D")) +
  labs(
    title = "Predicted Probability of Capital Feeding by Sex and Season",
    x = "Number of new cones cached (log-scale)",
    y = "Probability of capital feeding") +
  theme_minimal()

capitalfeed_cache

#save
ggsave("Output/capitalfeed_cache.jpeg", plot = capitalfeed_cache, width = 8, height = 6)

#plot - probability of feeding on capital across seasons 
summaries <- pred_data %>%
  group_by(sex, season) %>%
  summarize(
    median_prob = median(predicted_probability),
    lower_prob = quantile(predicted_probability, 0.25),
    upper_prob = quantile(predicted_probability, 0.75),
    .groups = "drop")

probability_season <- ggplot(summaries, aes(x = season, y = median_prob, color = sex)) +
  #points for the median
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  #error bars for the 25th and 75th percentiles
  geom_errorbar(
    aes(ymin = lower_prob, ymax = upper_prob),
    position = position_dodge(width = 0.3),
    width = 0.2) +
  labs(
    title = "Predicted Probability of Capital Feeding by Sex and Season",
    x = "Season",
    y = "Predicted Probability",
    color = "Sex") +
  scale_x_discrete(limits = c("winter", "mating", "lactation", "non-breeding"),
                   labels = c("winter" = "Winter", "mating" = "Mating", "lactation" = "Lactation", "non-breeding" = "Non-breeding")) + #rearrange groups on x-axis 
  scale_color_manual(
    values = c("F" = "#FF3399", "M" = "#00CCFF"),
    labels = c("F" = "Females", "M" = "Males")) + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11))

probability_season

#save
ggsave("Output/probability_season.jpeg", plot = probability_season, width = 8, height = 6)

# down sampling to check variability between males and females ------------
female_ids <- feeding_cache_binomial_30 %>% 
  filter(sex == "F") %>% 
  distinct(squirrel_id)

male_ids <- feeding_cache_binomial_30 %>% 
  filter(sex == "M") %>% 
  distinct(squirrel_id)

#randomly choose 19 males to match number of females
male_ids <- male_ids %>% slice_sample(n = nrow(female_ids))

#now subset original data for both sexes
df_females <- feeding_cache_binomial_30 %>% filter(sex == "F")
df_males <- feeding_cache_binomial_30 %>% 
  filter(sex == "M", squirrel_id %in% male_ids$squirrel_id)

df_balanced <- bind_rows(df_females, df_males)
df_balanced %>% group_by(sex) %>% summarize(n_ids = n_distinct(squirrel_id)) #double check we have the same number of males as females

#refit the model on balanced data
final_model_down <- glmer(food_type_bin ~ log_cache_size_new_prev_year * sex + sex * season + (1 | squirrel_id),
                          family = binomial(link = "logit"),
                          data = df_balanced,
                          control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000)))

#define the range of log_cache_size_new_prev_year - generate predictions only within the actual scope of the data (i.e. not huge unrealistic cache sizes)
cache_seq_down <- seq(
  from = min(df_balanced$log_cache_size_new_prev_year, na.rm = TRUE),
  to   = max(df_balanced$log_cache_size_new_prev_year, na.rm = TRUE),
  length.out = 100)

#use emmeans to get predicted probabilities on the response scale (i.e., 0-1)
emm_down <- emmeans(
  final_model_down, 
  ~ sex * season * log_cache_size_new_prev_year,
  at = list(log_cache_size_new_prev_year = cache_seq_down),
  type = "response") #this is crucial for probabilities instead of log-odds

emm_df_down <- as.data.frame(emm_down)

#rename columns for clarity
pred_data_balanced <- emm_df_down %>%
  rename(
    predicted_probability = prob,
    conf.low = asymp.LCL,
    conf.high = asymp.UCL)

#plot - probability of feeding on capital by number of cached cones and across seasons
capitalfeed_cache_balanced <- ggplot(pred_data_balanced, aes(x = log_cache_size_new_prev_year, y = predicted_probability, color = season, fill = season)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = season), alpha = 0.1, color = NA) +
  geom_line(linewidth = 1) +
  facet_wrap(~ sex) +
  scale_color_manual(
    name = "Season",
    breaks = c("winter", "mating", "lactation", "non-breeding"),
    labels = c("Winter", "Mating", "Lactation", "Non-breeding"),
    values = c(
      "winter"       = "#00BFC4",
      "mating"       = "#C77CFF",
      "lactation"    = "#7CAE00",
      "non-breeding" = "#F8766D")) +
  scale_fill_manual(
    name = "Season",
    breaks = c("winter", "mating", "lactation", "non-breeding"),
    labels = c("Winter", "Mating", "Lactation", "Non-breeding"),
    values = c(
      "winter"       = "#00BFC4",
      "mating"       = "#C77CFF",
      "lactation"    = "#7CAE00",
      "non-breeding" = "#F8766D")) +
  labs(
    title = "Predicted Probability of Capital Feeding by Sex and Season (balanced data)",
    x = "Number of new cones cached (log-scale)",
    y = "Probability of capital feeding") +
  theme_minimal()

capitalfeed_cache_balanced

#save
ggsave("Output/capitalfeed_cache_balanced.jpeg", plot = capitalfeed_cache_balanced, width = 8, height = 6)

#plot - probability of feeding on capital across seasons 
summaries_down <- pred_data_balanced %>%
  group_by(sex, season) %>%
  summarize(
    median_prob = median(predicted_probability),
    lower_prob = quantile(predicted_probability, 0.25),
    upper_prob = quantile(predicted_probability, 0.75),
    .groups = "drop")

probability_season_balanced <- ggplot(summaries_down, aes(x = season, y = median_prob, color = sex)) +
  #points for the median
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  #error bars for the 25th and 75th percentiles
  geom_errorbar(
    aes(ymin = lower_prob, ymax = upper_prob),
    position = position_dodge(width = 0.3),
    width = 0.2) +
  labs(
    title = "Predicted Probability of Capital Feeding by Sex and Season (balanced data)",
    x = "Season",
    y = "Predicted Probability",
    color = "Sex") +
  scale_x_discrete(limits = c("winter", "mating", "lactation", "non-breeding"),
                   labels = c("winter" = "Winter", "mating" = "Mating", "lactation" = "Lactation", "non-breeding" = "Non-breeding")) + #rearrange groups on x-axis 
  scale_color_manual(
    values = c("F" = "#FF3399", "M" = "#00CCFF"),
    labels = c("F" = "Females", "M" = "Males")) + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11))

probability_season_balanced

#save
ggsave("Output/probability_season_balanced.jpeg", plot = probability_season_balanced, width = 8, height = 6)

