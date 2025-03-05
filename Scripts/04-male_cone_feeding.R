#load packages
source("Scripts/00-packages.R")

#read in data --------------------------------------------------------
feeding <- read.csv("Input/feeding_distances.csv")

#filter for within territory and only capital
feeding_within_territory <- feeding %>%
  filter(within_territory == TRUE)

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
#roughly 70.7% of feeding events by males during mating occurred on-midden, reject P1.

# plot --------------------------------------------------------------------
male_feeding_year <- feeding_within_territory %>%
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

# mast_years <- c("2014", "2019", "2022")
# mast_positions <- which(levels(male_feeding_long$year_f) %in% mast_years)
# highlight_df <- data.frame(
#   xmin = mast_positions - 0.5,
#   xmax = mast_positions + 0.5)

highlight_years <- c("2001", "2004", "2007", "2009","2012", "2016", "2017", "2021")
highlight_positions <- which(levels(male_feeding_long$year_f) %in% highlight_years)
highlight_df <- data.frame(
  xmin = highlight_positions - 0.5,
  xmax = highlight_positions + 0.5)

sample_sizes <- male_feeding_long %>%
  group_by(year_f) %>%
  summarise(total_count = sum(count), .groups = "drop")

#stacked bar graph
male_cone_feeding <- ggplot(male_feeding_long, aes(x = as.numeric(year_f), y = count, fill = location)) +
  geom_rect(data = highlight_df,
            aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf),
            fill = "grey10", alpha = 0.5, inherit.aes = FALSE) +
  geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
  scale_x_continuous(breaks = 1:length(levels(male_feeding_long$year_f)),
                     labels = levels(male_feeding_long$year_f),
                     expand = c(0, 0),
                     limits = c(0.5, length(levels(male_feeding_long$year_f)) + 0.5)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 1.05), clip = "off") +
  scale_fill_manual(values = c("#33CC66", "#996600"), 
                    labels = c("Off Midden", "On Midden"),
                    guide = guide_legend(reverse = TRUE)) +
  labs(x = "Year",
       y = "Proportion of Cone Feeding Events",
       title = "On vs Off Midden Cone Feeding by Males During Mating",
       fill = "Feeding Location") +
  geom_text(data = sample_sizes,
            aes(x = as.numeric(year_f), y = 1.05, label = total_count),
            inherit.aes = FALSE,
            vjust = -0.4, size = 5) +
  theme_minimal(base_size = 14) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, margin = margin(b=30)),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20))

male_cone_feeding

#save
ggsave("Output/male_cone_feeding.jpeg", plot = male_cone_feeding, width = 8, height = 6)


# data summary ------------------------------------------------------------
feeding_location_summary <- feeding_within_territory %>%
  mutate(location = if_else(within_midden, "On Midden", "Off Midden")) %>%
  group_by(year, sex, repro_stage, location) %>%
  summarise(num_events = n(), .groups = "drop") %>%
  complete(year, sex, repro_stage, location, fill = list(num_events = 0))

#save
write.csv(feeding_location_summary, "Output/feeding_location_summary.csv", row.names = FALSE)

