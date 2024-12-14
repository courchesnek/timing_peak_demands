#load packages
source("Scripts/00-packages.R")

#read in data
feeding <- read.csv("Input/feeding_distances.csv")

#filter for within territory
feeding_within_territory <- feeding %>%
  filter(within_territory == TRUE)

#check distribution of data ---------------------------------------------------------
distribution <- ggplot(feeding_within_territory, aes(x = distance_to_midden)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    x = "Distance to Midden (m)",
    y = "Frequency") +
  theme_minimal() +
  xlim(0,125)

distribution

ggsave("Output/distribution.jpeg", plot = distribution, width = 8, height = 6)



# subset data into 4 groups -----------------------------------------------
cone_mating <- feeding_within_territory %>%
  filter(food_type == "capital", repro_stage == "MATING")

income_mating <- feeding_within_territory %>%
  filter(food_type == "income", repro_stage == "MATING")

cone_lactating <- feeding_within_territory %>%
  filter(food_type == "capital", repro_stage == "LACTATING")

income_lactating <- feeding_within_territory %>%
  filter(food_type == "income", repro_stage == "LACTATING")


#create a function to calculate summary stats and apply to each subset --------------------------------
calculate_summary <- function(data) {
  data %>%
    group_by(sex) %>%
    summarize(
      mean_distance = mean(distance_to_midden, na.rm = TRUE),
      median_distance = median(distance_to_midden, na.rm = TRUE),
      max_distance = max(distance_to_midden, na.rm = TRUE),
      sd_distance = sd(distance_to_midden, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
}

#apply to each subset
cone_mating_summary <- calculate_summary(cone_mating)
income_mating_summary <- calculate_summary(income_mating)
cone_lactating_summary <- calculate_summary(cone_lactating)
income_lactating_summary <- calculate_summary(income_lactating)

#combine summaries into one table
summary_table <- bind_rows(
  cone_mating_summary %>% mutate(group = "Cone Mating"),
  income_mating_summary %>% mutate(group = "Income Mating"),
  cone_lactating_summary %>% mutate(group = "Cone Lactating"),
  income_lactating_summary %>% mutate(group = "Income Lactating")
)

summary_table

#save
write.csv(summary_table, "Output/feeding_summary_4way.csv", row.names = FALSE)


#Wilcoxon test and effect sizes ------------------------------------------
#creat a function to perform Wilcoxon and Cliff's Delta
calculate_stats <- function(data) {
  list(
    wilcox_p = wilcox.test(distance_to_midden ~ sex, data = data)$p.value,
    cliffs_delta = cliff.delta(distance_to_midden ~ sex, data = data)$estimate,
    cliffs_delta_lower = cliff.delta(distance_to_midden ~ sex, data = data)$conf.int[1],
    cliffs_delta_upper = cliff.delta(distance_to_midden ~ sex, data = data)$conf.int[2]
  )
}

#apply to each subset
cone_mating_stats <- calculate_stats(cone_mating)
income_mating_stats <- calculate_stats(income_mating)
cone_lactating_stats <- calculate_stats(cone_lactating)
income_lactating_stats <- calculate_stats(income_lactating)

#combine results into a table
test_results <- data.frame(
  group = c("Cone Mating", "Income Mating", "Cone Lactating", "Income Lactating"),
  wilcox_p = c(cone_mating_stats$wilcox_p, income_mating_stats$wilcox_p,
               cone_lactating_stats$wilcox_p, income_lactating_stats$wilcox_p),
  cliffs_delta = c(cone_mating_stats$cliffs_delta, income_mating_stats$cliffs_delta,
                   cone_lactating_stats$cliffs_delta, income_lactating_stats$cliffs_delta),
  cliffs_delta_lower = c(cone_mating_stats$cliffs_delta_lower, income_mating_stats$cliffs_delta_lower,
                         cone_lactating_stats$cliffs_delta_lower, income_lactating_stats$cliffs_delta_lower),
  cliffs_delta_upper = c(cone_mating_stats$cliffs_delta_upper, income_mating_stats$cliffs_delta_upper,
                         cone_lactating_stats$cliffs_delta_upper, income_lactating_stats$cliffs_delta_upper)
)

test_results

#save
write.csv(test_results, "Output/test_results_4way.csv", row.names = FALSE)


# plot --------------------------------------------------------------------
#add grouping columns to the main dataset
feeding_within_territory <- feeding_within_territory %>%
  mutate(
    group = case_when(
      food_type == "Cone Feeding" & repro_stage == "MATING" ~ "Cone Mating",
      food_type == "Income Feeding" & repro_stage == "MATING" ~ "Income Mating",
      food_type == "Cone Feeding" & repro_stage == "LACTATING" ~ "Cone Lactating",
      food_type == "Income Feeding" & repro_stage == "LACTATING" ~ "Income Lactating"))

#update repro_stage to reorder levels
feeding_within_territory <- feeding_within_territory %>%
  mutate(repro_stage = factor(repro_stage, levels = c("MATING", "LACTATING")))

#violin plot
four_panel_plot <- ggplot(feeding_within_territory, aes(x = sex, y = distance_to_midden, fill = sex)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +
  facet_grid(food_type ~ repro_stage) + # Rows = food type, Columns = reproductive stage
  labs(
    title = "Feeding Distance by Sex, Food Type, and Reproductive Stage (Within Territory)",
    x = "Sex",
    y = "Distance to Midden (m)"
  ) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 10)
  )

four_panel_plot

# Save the updated plot
ggsave("Output/violin.jpeg", plot = four_panel_plot, width = 12, height = 8)




