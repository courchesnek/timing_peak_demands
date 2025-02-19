#load packages
source("Scripts/00-packages.R")

#read in data
feeding <- read.csv("Input/feeding_distances.csv")

#filter for within territory
feeding_within_territory <- feeding %>%
  filter(within_territory == TRUE)

#reorder the levels of repro_stage
feeding_within_territory <- feeding_within_territory %>%
  mutate(repro_stage = factor(repro_stage, levels = c("MATING", "LACTATING")))

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


# summary statistics ------------------------------------------------------
summary_stats <- feeding_within_territory %>%
  group_by(sex, repro_stage) %>%
  summarize(
    mean_distance = mean(distance_to_midden, na.rm = TRUE),
    median_distance = median(distance_to_midden, na.rm = TRUE),
    sd_distance = sd(distance_to_midden, na.rm = TRUE),
    max_distance = max(distance_to_midden, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

print(summary_stats)

write.csv(summary_stats, "Output/summary_stats_feeding.csv", row.names = FALSE)

# subset data -------------------------------------------------------------
#for capital resources
capital_feeding <- feeding_within_territory %>%
  filter(food_type == "capital")

#for income resources
income_feeding <- feeding_within_territory %>%
  filter(food_type == "income")


# Kruskal-Wallis, Dunn's test and Cliff's Delta for each food type -----------------------
#Kruskal-Wallis Test: capital feeding
kruskal_capital <- kruskal.test(distance_to_midden ~ interaction(sex, repro_stage), 
                                data = capital_feeding)
print(kruskal_capital)

#pairwise comparisons: Dunn's Test for capital
dunn_capital <- dunn.test(
  x = capital_feeding$distance_to_midden,
  g = interaction(capital_feeding$sex, capital_feeding$repro_stage),
  method = "bonferroni"
)

print(dunn_capital)

#Cliff's Delta effect size for capital
##define a function for Cliff's Delta
calculate_cliffs_delta <- function(data, group1, group2) {
  cliff.delta(distance_to_midden ~ repro_stage, 
              data = data %>% filter(sex == group1 | sex == group2))
}

capital_F_mating_vs_lac <- cliff.delta(distance_to_midden ~ repro_stage, 
                                        data = capital_feeding %>% filter(sex == "F"))
capital_M_mating_vs_lac <- cliff.delta(distance_to_midden ~ repro_stage, 
                                        data = capital_feeding %>% filter(sex == "M"))

print(capital_F_mating_vs_lac)
print(capital_M_mating_vs_lac)

#Kruskal-Wallis Test: income feeding
kruskal_income <- kruskal.test(distance_to_midden ~ interaction(sex, repro_stage), 
                               data = income_feeding)
print(kruskal_income)

#pairwise comparisons: Dunn's Test for income
dunn_income <- dunn.test(
  x = income_feeding$distance_to_midden,
  g = interaction(income_feeding$sex, income_feeding$repro_stage),
  method = "bonferroni"
)

print(dunn_income)

#Cliff's Delta effect size for income
income_F_mating_vs_lac <- cliff.delta(distance_to_midden ~ repro_stage, 
                                       data = income_feeding %>% filter(sex == "F"))
income_M_mating_vs_lac <- cliff.delta(distance_to_midden ~ repro_stage, 
                                       data = income_feeding %>% filter(sex == "M"))

print(income_F_mating_vs_lac)
print(income_M_mating_vs_lac)


#F-test on standard deviations -------------------------------------------
#filter raw data for each group
F_mating <- feeding_within_territory %>%
  filter(sex == "F", repro_stage == "MATING") %>%
  pull(distance_to_midden)

F_lactating <- feeding_within_territory %>%
  filter(sex == "F", repro_stage == "LACTATING") %>%
  pull(distance_to_midden)

M_mating <- feeding_within_territory %>%
  filter(sex == "M", repro_stage == "MATING") %>%
  pull(distance_to_midden)

M_lactating <- feeding_within_territory %>%
  filter(sex == "M", repro_stage == "LACTATING") %>%
  pull(distance_to_midden)

#perform F-tests
f_test_mating <- var.test(F_mating, M_mating) 
f_test_lactating <- var.test(F_lactating, M_lactating)

#print results
print(f_test_mating)
print(f_test_lactating)

# plot --------------------------------------------------------------------
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










