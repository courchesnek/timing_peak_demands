#load packages
source("Scripts/00-packages.R")

#read in data ----------------------------
feeding <- read.csv("Input/all_feeding_census.csv")

#filter out unnecessary columns
feeding <- feeding %>%
  dplyr::select(-locx_obs, -locy_obs, -snow, -locx_census, -locy_census )

#define DEE values (kJ/day) --------------------------------------------------------------
DEE_values <- data.frame(
  sex = c("M", "M", "M", "F", "F", "F"),
  season = c("winter", "mating", "non-breeding", "winter", "lactation", "non-breeding"),
  DEE_kJ_day = c(166, 407, 318, 247, 470, 318)  #assuming M non-breeding = F non-breeding
)

#match timing categories between feeding and DEE --------------------------------
early_year_observations <- feeding %>%
  mutate(month_day = format(as.Date(date), "%m-%d")) %>%
  filter(month_day >= "01-01" & month_day <= "04-30", sex == "M") %>%
  dplyr::select(date, month_day, repro_stage) %>%
  arrange(date) 

#need to match feeding and DEE season
feeding <- feeding %>%
  mutate(
    month_day = format(as.Date(date), "%m-%d"),
    season = case_when(
      #explicitly assign Mating for males
      sex == "M" & repro_stage == "mating" ~ "mating",
      
      #explicitly assign Lactation for females
      sex == "F" & repro_stage == "lactation" ~ "lactation",
      
      #winter conditions take precedence
      (month_day >= "01-01" & month_day <= "04-30") ~ "winter",
      (month_day >= "10-15" & month_day <= "12-31") ~ "winter",
      
      #non-Breeding for all remaining cases
      TRUE ~ "non-breeding"))

#drop the 'repro_stage' column
feeding <- feeding %>%
  dplyr::select(-repro_stage)


# calculate proportion of feeding events ----------------------------------
feeding_proportions <- feeding %>%
  group_by(sex, season, food_type) %>%  #group by sex, season, and food type
  summarise(
    total_events = n(),                #count total feeding events
    .groups = "drop"                   #ungroup after summarization
  ) %>%
  group_by(sex, season) %>%            #group by sex and season to calculate proportions
  mutate(
    proportion = total_events / sum(total_events)  #calculate proportion
  ) %>%
  arrange(sex, season, food_type)     #arrange for better readability

#add DEE estimates
feeding_DEE <- feeding_proportions %>%
  left_join(DEE_values, by = c("sex", "season"))


# energetics --------------------------------------------------------------
feeding_DEE <- feeding_DEE %>%
  mutate(
    energy_from_capital = proportion * DEE_kJ_day * (food_type == "capital"),
    energy_from_income = proportion * DEE_kJ_day * (food_type == "income"))

# plot --------------------------------------------------------------------
feeding_DEE <- feeding_DEE %>%
  mutate(season = factor(season)) %>%
  mutate(season = droplevels(season))

ggplot(feeding_DEE, aes(x = season, y = energy_from_capital + energy_from_income, fill = food_type)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~sex, ncol = 2) +
  labs(
    title = "Energy Input Contributions by Season and Sex",
    x = "Season",
    y = "Energy Input (kJ/day)",
    fill = "Food Type") +
  scale_fill_manual(
    values = c("capital" = "#33CC66", "income" = "#FF9933"),
    labels = c("Capital", "Income")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14))









