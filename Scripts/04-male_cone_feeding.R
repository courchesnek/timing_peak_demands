#load packages
source("Scripts/00-packages.R")

#read in data --------------------------------------------------------
feeding <- read.csv("Input/feeding_distances_all.csv")

#filter for within territory and only capital
feeding_within_territory <- feeding %>%
  filter(within_territory == TRUE,
         food_type == "capital")

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

# male on- vs off-midden cone feeding during mating -----------------------
male_feeding <- feeding_within_territory %>%
  filter(sex == "M", repro_stage == "mating") %>%
  summarise(
    total_events = n(),
    off_midden = sum(within_midden == FALSE),
    on_midden = sum(within_midden == TRUE),
    .groups = "drop") %>%
  mutate(
    prop_off = off_midden / total_events,
    prop_on = on_midden / total_events)


# binomial test -----------------------------------------------------------
binomial_test <- binom.test(male_feeding$on_midden, 
                            male_feeding$total_events, 
                            p = 1)

print(binomial_test)
#roughly 50% of feeding events by males during mating are on-midden, strongly reject P1.


# plot --------------------------------------------------------------------
male_feeding_year <- feeding %>%
  filter(sex == "M", repro_stage == "mating") %>%
  group_by(year) %>%
  summarise(
    total_events = n(),
    on_midden = sum(within_midden, na.rm = TRUE)) %>%
  mutate(
    off_midden = total_events - on_midden,
    prop_on = on_midden / total_events,
    prop_off = off_midden / total_events)

male_feeding_long <- male_feeding_year %>%
  pivot_longer(cols = c(on_midden, off_midden),
               names_to = "location",
               values_to = "count") %>%
  mutate(location = dplyr::recode(location,
                           on_midden = "On Midden",
                           off_midden = "Off Midden"))

male_feeding_long <- male_feeding_long %>%
  mutate(year_f = factor(year, levels = sort(unique(year))))

mast_years <- c("2014", "2019", "2022")

mast_positions <- which(levels(male_feeding_long$year_f) %in% mast_years)

highlight_df <- data.frame(
  xmin = mast_positions - 0.5,
  xmax = mast_positions + 0.5)

#stacked bar graph
male_cone_feeding <- ggplot(male_feeding_long, aes(x = as.numeric(year_f), y = count, fill = location)) +
  geom_rect(data = highlight_df,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey10", alpha = 0.5, inherit.aes = FALSE) +
  geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
  scale_x_continuous(breaks = 1:length(levels(male_feeding_long$year_f)),
                     labels = levels(male_feeding_long$year_f)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("#33CC66", "#996600"), 
                    labels = c("Off Midden", "On Midden"),
                    guide = guide_legend(reverse = TRUE)) +
  labs(x = "Year",
       y = "Proportion of Cone Feeding Events",
       title = "On vs Off Midden Cone Feeding by Males During Mating",
       fill = "Feeding Location") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

male_cone_feeding

#save
ggsave("Output/male_cone_feeding.jpeg", plot = male_cone_feeding, width = 8, height = 6)







