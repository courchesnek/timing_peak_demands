#load packages
source("Scripts/00-packages.R")

#read in data --------------------------------------------------------
feeding <- read.csv("Input/feeding_distances_all.csv")

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

#filtering ----------------------------------------
feeding_within_territory <- feeding %>%
  filter(within_territory == TRUE,
         snow == "no snow")

# model binary outcome for feeding off midden --------------------------
feeding_within_territory <- feeding_within_territory %>%
  mutate(year_type = factor(year_type, levels = c("non-mast", "mast", "post-mast")),
         sex = factor(sex),
         season = factor(season, levels = c("winter", "mating", "lactation", "non-breeding")),
         food_type = factor(food_type, levels = c("capital", "income")))

model <- glmer(as.numeric(!within_midden) ~ sex + food_type + season + year_type + (1 | squirrel_id), 
               data = feeding_within_territory, 
               family = binomial(link = "logit"),
               control = glmerControl(optimizer = "bobyqa", 
                                      optCtrl = list(maxfun = 1000000)))  #increase maxfun (number of iterations) to fix convergence issues)

summary(model)

#model reference categories?
contrasts(feeding_within_territory$year_type) #non-mast year is reference category
contrasts(feeding_within_territory$sex) #female is reference category
contrasts(feeding_within_territory$season) #winter is reference category
contrasts(feeding_within_territory$food_type) #capital is reference


# generate predictions and plot -------------------------------------------
emm <- emmeans(model, ~ sex + food_type, type = "response")
emm_df <- as.data.frame(emm)

ggplot(emm_df, aes(x = sex, y = response, fill = food_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Sex",
       y = "Predicted Proportion of Diet (Off-midden Feeding)",
       fill = "Food Type") +
  theme_classic()















