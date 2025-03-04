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

#filter for within territory and only capital
feeding_within_territory <- feeding %>%
  filter(within_territory == TRUE,
         food_type == "capital")

#compare prop of on vs off midden feeding events between the sexes ----------------
feeding_proportions <- feeding_within_territory %>%
  group_by(sex, snow, year_type, repro_stage, within_midden) %>%
  summarise(
    total_events = n(),  #count total feeding events
    .groups = "drop") %>%
  group_by(sex, snow, year_type) %>% 
  mutate(
    proportion = total_events / sum(total_events))

feeding_proportions$snow <- factor(feeding_proportions$snow, levels = c("snow", "no snow"))

#plot proportions
feeding_proportions_plot <- ggplot(feeding_proportions, 
                                   aes(x = year_type, y = proportion, fill = within_midden)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(repro_stage ~ snow + sex) +  #separate by repro stage, snow condition and sex
  scale_fill_manual(values = c("#33CC66", "#996600"), 
                    labels = c("Off Midden", "On Midden")) +
  labs(
    title = "Proportion of Cone Feeding Events On Midden vs Off Midden by Reproductive Stage and Snow Cover",
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


# male on- vs off-midden cone feeding during mating -----------------------
male_events <- feeding_within_territory %>%
  filter(sex == "M", season == "mating") %>%
  mutate(binary_response = if_else(within_midden, 1, 0),
         year = as.numeric(year))

length(unique(male_events$squirrel_id))  #number of unique squirrels

male_summary <- feeding_within_territory %>%
  filter(sex == "M", season == "mating") %>%
  group_by(squirrel_id, year) %>%
  summarise(total_events = n(), .groups = "drop")

# model -------------------------------------------------------------------
#estimate the probability of an on-midden feeding event across years, accounting for repeated measures on each squirrel via the random intercept.
model <- glmer(binary_response ~ factor(year) + (1 | squirrel_id),
               data = male_events,
               family = binomial)

summary(model)


# generate predictions and plot -------------------------------------------
#create a new data frame with unique years (assuming year is numeric)
newdata <- male_events %>%
  distinct(year) %>%
  arrange(year)

#get predicted probability for an on-midden event (using only fixed effects)
newdata <- newdata %>%
  mutate(predicted_on = predict(model, newdata = ., re.form = NA, type = "response"),
         predicted_off = 1 - predicted_on)

newdata_long <- newdata %>%
  pivot_longer(
    cols = c(predicted_on, predicted_off),
    names_to = "feeding_location",
    values_to = "proportion") %>%
  mutate(feeding_location = dplyr::recode(feeding_location,
                                          "predicted_on" = "On Midden",
                                          "predicted_off" = "Off Midden"))


#plot predictions
mast_years <- c(2014, 2022)

highlight_df <- data.frame(
  xmin = mast_years - 0.5,
  xmax = mast_years + 0.5)

ggplot(newdata_long, aes(x = year, y = proportion, fill = feeding_location)) +
  geom_rect(data = highlight_df,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey10", alpha = 0.5, inherit.aes = FALSE) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Year",
       y = "Proportion of Cone Feeding Events",
       fill = "Feeding Location",
       title = "On- vs Off-Midden Cone Feeding by Males During Mating") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
