#load packages
source("Scripts/00-packages.R")

#read in data
feeding <- read.csv("Input/feeding_distances.csv")

#filter for within territory and only capital
feeding_within_territory <- feeding %>%
  filter(within_territory == TRUE,
         food_type == "capital")

#reorder the levels of repro_stage
feeding_within_territory <- feeding_within_territory %>%
  mutate(repro_stage = factor(repro_stage, levels = c("MATING", "LACTATING")))

#off-midden feeding events only for analysis, since on-midden events overwhelm the data
feed_offmid <- feeding_within_territory %>%
  filter(within_midden == FALSE)

#check distribution of data ---------------------------------------------------------
distribution <- ggplot(feed_offmid, aes(x = distance_to_midden)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    x = "Distance to Midden (m)",
    y = "Frequency") +
  theme_minimal() +
  xlim(0,125)

distribution

ggsave("Output/distribution_capital.jpeg", plot = distribution, width = 8, height = 6)

# summary statistics ------------------------------------------------------
summary_stats <- feed_offmid %>%
  group_by(sex, snow, repro_stage, year_type) %>%
  mutate(
    location = case_when(
      within_midden ~ "on_midden",
      within_territory ~ "off_midden")) %>%
  summarize(
    mean_distance = mean(distance_to_midden, na.rm = TRUE),
    median_distance = median(distance_to_midden, na.rm = TRUE),
    sd_distance = sd(distance_to_midden, na.rm = TRUE),
    max_distance = max(distance_to_midden, na.rm = TRUE),
    total_observations = n(),                            
    observations_on_midden = sum(within_midden, na.rm = TRUE), 
    .groups = "drop")

print(summary_stats)

write.csv(summary_stats, "Output/summary_stats_feeding_capital.csv", row.names = FALSE)

# Kruskal-Wallis, Dunn's test and Cliff's Delta --------------------------------
kruskal <- kruskal.test(distance_to_midden ~ interaction(sex, repro_stage, snow, year_type), 
                                data = feed_offmid)
print(kruskal)

#Dunn's post-hoc comparison
dunn <- dunn.test(
  x = feed_offmid$distance_to_midden,
  g = interaction(feed_offmid$sex, feed_offmid$repro_stage, feed_offmid$snow, feed_offmid$year_type),
  method = "bonferroni")

print(dunn)

#Cliff's Delta effect size
# Define a function for Cliff's Delta by year_type, repro_stage, sex, and snow
calculate_cliffs_delta <- function(data, group_var1, group_var2, condition_var) {
  cliff.delta(distance_to_midden ~ repro_stage,
              data = data %>% filter(sex == group_var1 | sex == group_var2, snow == condition_var))
}

# Create all possible comparisons programmatically
comparisons <- expand.grid(
  sex1 = c("F", "M"),
  sex2 = c("F", "M"),
  repro_stage = c("MATING", "LACTATING"),
  snow = c("snow", "no snow"),
  year_type = c("mast", "post-mast", "non-mast"),
  stringsAsFactors = FALSE
)

# Initialize an empty results dataframe
cliffs_delta_results <- data.frame(
  Comparison = character(),
  Delta_Estimate = numeric(),
  Lower_CI = numeric(),
  Upper_CI = numeric(),
  stringsAsFactors = FALSE
)

# Loop through comparisons and calculate Cliff's Delta
for (i in 1:nrow(comparisons)) {
  comparison <- comparisons[i, ]
  result <- cliff.delta(distance_to_midden ~ repro_stage,
                        data = feeding_within_territory %>%
                          filter(
                            sex %in% c(comparison$sex1, comparison$sex2),
                            snow == comparison$snow,
                            year_type == comparison$year_type
                          ))
  
  # Add results to the table
  cliffs_delta_results <- rbind(
    cliffs_delta_results,
    data.frame(
      Comparison = paste(
        comparison$sex1, comparison$repro_stage, comparison$year_type, comparison$snow,
        "vs", comparison$sex2, comparison$repro_stage, comparison$year_type, comparison$snow
      ),
      Delta_Estimate = result$estimate,
      Lower_CI = result$conf.int[1],
      Upper_CI = result$conf.int[2]
    )
  )
}

# Print results
print(cliffs_delta_results)

#F-test on standard deviations -------------------------------------------
#filter raw data for each group
F_mating_snow <- feeding_within_territory %>%
  filter(sex == "F", repro_stage == "MATING", snow == "snow") %>%
  pull(distance_to_midden)

F_mating_no_snow <- feeding_within_territory %>%
  filter(sex == "F", repro_stage == "MATING", snow == "no snow") %>%
  pull(distance_to_midden)

F_lactating_snow <- feeding_within_territory %>%
  filter(sex == "F", repro_stage == "LACTATING", snow == "snow") %>%
  pull(distance_to_midden)

F_lactating_no_snow <- feeding_within_territory %>%
  filter(sex == "F", repro_stage == "LACTATING", snow == "no snow") %>%
  pull(distance_to_midden)

M_mating_snow <- feeding_within_territory %>%
  filter(sex == "M", repro_stage == "MATING", snow == "snow") %>%
  pull(distance_to_midden)

M_mating_no_snow <- feeding_within_territory %>%
  filter(sex == "M", repro_stage == "MATING", snow == "no snow") %>%
  pull(distance_to_midden)

M_lactating_snow <- feeding_within_territory %>%
  filter(sex == "M", repro_stage == "LACTATING", snow == "snow") %>%
  pull(distance_to_midden)

M_lactating_no_snow <- feeding_within_territory %>%
  filter(sex == "M", repro_stage == "LACTATING", snow == "no snow") %>%
  pull(distance_to_midden)

# Perform F-tests for snow and no-snow conditions
f_test_mating_snow <- var.test(F_mating_snow, M_mating_snow)
f_test_mating_no_snow <- var.test(F_mating_no_snow, M_mating_no_snow)

f_test_lactating_snow <- var.test(F_lactating_snow, M_lactating_snow)
f_test_lactating_no_snow <- var.test(F_lactating_no_snow, M_lactating_no_snow)

#create a function to extract F-test results
extract_f_test_results <- function(f_test, comparison_label) {
  data.frame(
    Comparison = comparison_label,
    F_Statistic = f_test$statistic,
    P_Value = f_test$p.value,
    Lower_CI = f_test$conf.int[1],
    Upper_CI = f_test$conf.int[2]
  )
}

#extract results for each test
f_test_results <- rbind(
  extract_f_test_results(f_test_mating_snow, "Mating (Snow)"),
  extract_f_test_results(f_test_mating_no_snow, "Mating (No Snow)"),
  extract_f_test_results(f_test_lactating_snow, "Lactating (Snow)"),
  extract_f_test_results(f_test_lactating_no_snow, "Lactating (No Snow)"))

print(f_test_results)

# plot --------------------------------------------------------------------
#interaction plot for mean feeding distance
interaction_plot <- feed_offmid %>%
  group_by(sex, repro_stage, snow) %>%
  summarize(mean_distance = mean(distance_to_midden, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = repro_stage, y = mean_distance, group = interaction(sex, snow), color = sex, linetype = snow)) +
  geom_line(linewidth = 1.2) +  # Updated from size to linewidth
  geom_point(size = 3) +
  labs(
    title = "Off-midden Feeding Distance to Midden by Sex, Reproductive Stage, and Snow",
    x = "Reproductive Stage",
    y = "Mean Distance to Midden (m)",
    color = "Sex",
    linetype = "Snow Condition") +
  scale_color_manual(values = c("#FF66FF", "#00CCFF")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    strip.text = element_text(size = 10))

interaction_plot

#save
ggsave("Output/interaction_plot.jpeg", plot = interaction_plot, width = 8, height = 6)


