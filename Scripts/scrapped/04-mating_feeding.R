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


# male on- vs off-midden feeding during mating -----------------------
male_feeding <- feeding_within_territory %>%
  filter(sex == "M", repro_stage == "mating", 
         year %in% mating_years) %>%
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

# binomial test -----------------------------------------------------------
binomial_test <- binom.test(male_feeding$on_midden, 
                            male_feeding$total_events, 
                            p = 1)

print(binomial_test)
#roughly 63% of feeding events by males during mating occurred on-midden, reject P1.

# plot by food detail -----------------------------------------------------
food_categories <- tibble(
  detail = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 27, 28, 29, 31, 32),
  category = c(
    "Animal Material", "Cone", "Buds", "Mushrooms/Truffles", "Bark", "New Cone", "Willow Leaves", 
    "Poplar Buds", "Bearberry Flower", "Pollen Cone", "New Spruce Needle", "Aspen Leaves",
    "Bearberry Berries", "Grass", "Fireweed", "Witches Broom", "Willow Bark Scales", 
    "Aspen Catkin", "Water Drops", "Lichen/Fungus", "Willow Buds", "Bark Beetle Larvae",
    "Insects", "New Mushroom", "Truffles"))

feeding_mating <- feeding_mating %>%
  mutate(food_group = case_when(
    #on-midden: only cones (detail 2) or mushrooms/truffles (detail 4)
    within_midden == TRUE & detail == 2 ~ "on_midden_cone",
    within_midden == TRUE & detail == 4 ~ "on_midden_mushroom",
    #on-midden but not the desired details: ignore (set to NA)
    within_midden == TRUE & !(detail %in% c(2, 4)) ~ NA_character_,
    #off-midden: classify based on detail code
    within_midden == FALSE & detail == 2 ~ "off_midden_cone",
    within_midden == FALSE & detail == 4 ~ "off_midden_mushroom",
    within_midden == FALSE & detail == 1 ~ "off_midden_animal_material",
    within_midden == FALSE & detail == 3 ~ "off_midden_spruce_bud",
    #all other off-midden food types
    within_midden == FALSE ~ "off_midden_other",
    TRUE ~ NA_character_)) %>%
  na.omit()

feeding_mating_summary <- feeding_mating %>%
  group_by(food_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(proportion = count / sum(count))

male_mating <- ggplot(feeding_mating_summary, aes(x = "", y = count, fill = food_group)) +
  geom_bar(stat = "identity", width = 1) +  
  coord_polar(theta = "y") + 
  scale_fill_manual(breaks = c(
                      "on_midden_cone",
                      "on_midden_mushroom",
                      "off_midden_cone",
                      "off_midden_mushroom",
                      "off_midden_spruce_bud",
                      "off_midden_other"),
                    values = c(
                      "on_midden_cone"            = "#E69F00",
                      "on_midden_mushroom"        = "#56B4E9",
                      "off_midden_cone"           = "#D55E00",
                      "off_midden_mushroom"       = "#F0E442",
                      "off_midden_spruce_bud"     = "#359B73",
                      "off_midden_other"          = "#F748A5"),
                    labels = c(
                      "on_midden_cone" = "On Midden: Cone",
                      "on_midden_mushroom" = "On Midden: Mushroom/Truffle",
                      "off_midden_cone" = "Off Midden: Cone",
                      "off_midden_mushroom" = "Off Midden: Mushroom/Truffle",
                      "off_midden_spruce_bud" = "Off Midden: Spruce Bud",
                      "off_midden_other" = "Off Midden: Other")) +
  labs(title = "Proportion of Food Types\nConsumed by Males During Mating", 
       fill = "Food Type") +
  theme_void(base_size = 14) +                                     
  theme(plot.title = element_text(size = 35, hjust = 0.5, face = "bold", margin = margin(b= -40, unit = "pt")),
        legend.title = element_text(size = 30, face = "bold"),
        legend.text = element_text(size = 25),
        legend.spacing.y = unit(2, "cm"),
        legend.key.height = unit(1.7, "cm"),
        plot.margin = margin(t = 25, r = 0, b = -40, l = 0),
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = -35))

male_mating

#save
ggsave("Output/male_mating.jpeg", plot = male_mating, width = 14, height = 8)

# plot --------------------------------------------------------------------
# male_feeding_year <- feeding_within_territory %>%
#   filter(sex == "M", repro_stage == "mating") %>%
#   group_by(year) %>%
#   summarise(
#     total_events = n(),
#     on_midden = sum(within_midden, na.rm = TRUE)) %>%
#   mutate(
#     off_midden = total_events - on_midden,
#     prop_on = on_midden / total_events,
#     prop_off = off_midden / total_events)
# 
# male_feeding_long <- male_feeding_year %>%
#   pivot_longer(cols = c(on_midden, off_midden),
#                names_to = "location",
#                values_to = "count") %>%
#   mutate(location = dplyr::recode(location,
#                            on_midden = "On Midden",
#                            off_midden = "Off Midden"))
# 
# male_feeding_long <- male_feeding_long %>%
#   mutate(year_f = factor(year, levels = sort(unique(year))))
# 
# # mast_years <- c("2014", "2019", "2022")
# # mast_positions <- which(levels(male_feeding_long$year_f) %in% mast_years)
# # highlight_df <- data.frame(
# #   xmin = mast_positions - 0.5,
# #   xmax = mast_positions + 0.5)
# 
# highlight_years <- c("2001", "2004", "2007", "2009","2012", "2016", "2017", "2021")
# highlight_positions <- which(levels(male_feeding_long$year_f) %in% highlight_years)
# highlight_df <- data.frame(
#   xmin = highlight_positions - 0.5,
#   xmax = highlight_positions + 0.5)
# 
# sample_sizes <- male_feeding_long %>%
#   group_by(year_f) %>%
#   summarise(total_count = sum(count), .groups = "drop")
# 
# #stacked bar graph
# male_cone_feeding <- ggplot(male_feeding_long, aes(x = as.numeric(year_f), y = count, fill = location)) +
#   geom_rect(data = highlight_df,
#             aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf),
#             fill = "red", alpha = 1, inherit.aes = FALSE) +
#   geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
#   scale_x_continuous(breaks = 1:length(levels(male_feeding_long$year_f)),
#                      labels = levels(male_feeding_long$year_f),
#                      expand = c(0, 0),
#                      limits = c(0.5, length(levels(male_feeding_long$year_f)) + 0.5)) +
#   scale_y_continuous(labels = percent_format(accuracy = 1),
#                      expand = c(0, 0)) +
#   coord_cartesian(ylim = c(0, 1.05), clip = "off") +
#   scale_fill_manual(values = c("#33CC66", "#996600"), 
#                     labels = c("Off Midden", "On Midden"),
#                     guide = guide_legend(reverse = TRUE)) +
#   labs(x = "Year",
#        y = "Proportion of Cone Feeding Events",
#        title = "On vs Off Midden Cone Feeding by Males During Mating",
#        fill = "Feeding Location") +
#   geom_text(data = sample_sizes,
#             aes(x = as.numeric(year_f), y = 1.05, label = total_count),
#             inherit.aes = FALSE,
#             vjust = -0.4, size = 5) +
#   theme_minimal(base_size = 18) +
#   theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
#         panel.grid = element_blank(),
#         plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b=30)),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.margin = margin(t = 20, r = 20, b = 10, l = 20),
#         legend.position = "bottom",
#         legend.box.margin = margin(t = -10, r = 0, b = 0, l = 0))
# 
# male_cone_feeding
# 
# #save
# ggsave("Output/male_cone_feeding.jpeg", plot = male_cone_feeding, width = 12, height = 6)

# data summary ------------------------------------------------------------
feeding_location_summary <- feeding_within_territory %>%
  mutate(location = if_else(within_midden, "On Midden", "Off Midden")) %>%
  group_by(year, sex, repro_stage, location) %>%
  summarise(num_events = n(), .groups = "drop") %>%
  complete(year, sex, repro_stage, location, fill = list(num_events = 0))

#save
write.csv(feeding_location_summary, "Output/feeding_location_summary.csv", row.names = FALSE)

