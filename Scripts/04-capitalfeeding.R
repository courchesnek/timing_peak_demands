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

#check distribution of data ---------------------------------------------------------
distribution <- ggplot(feeding_within_territory, aes(x = distance_to_midden)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    x = "Distance to Midden (m)",
    y = "Frequency") +
  theme_minimal() +
  xlim(0,125)

distribution

ggsave("Output/distribution_capital.jpeg", plot = distribution, width = 8, height = 6)

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

write.csv(summary_stats, "Output/summary_stats_feeding_capital.csv", row.names = FALSE)

# Kruskal-Wallis, Dunn's test and Cliff's Delta for each food type -----------------------
#Kruskal-Wallis Test: capital feeding
kruskal <- kruskal.test(distance_to_midden ~ interaction(sex, repro_stage), 
                                data = feeding_within_territory)
print(kruskal)

#Dunn's post-hoc comparison
dunn <- dunn.test(
  x = feeding_within_territory$distance_to_midden,
  g = interaction(feeding_within_territory$sex, feeding_within_territory$repro_stage),
  method = "bonferroni"
)

print(dunn)

#Cliff's Delta effect size
##define a function for Cliff's Delta
calculate_cliffs_delta <- function(data, group1, group2) {
  cliff.delta(distance_to_midden ~ repro_stage, 
              data = data %>% filter(sex == group1 | sex == group2))
}

F_mating_vs_lac <- cliff.delta(distance_to_midden ~ repro_stage, 
                                       data = feeding_within_territory %>% filter(sex == "F"))
M_mating_vs_lac <- cliff.delta(distance_to_midden ~ repro_stage, 
                                       data = feeding_within_territory %>% filter(sex == "M"))
F_vs_M_mating <- cliff.delta(distance_to_midden ~ sex, 
                             data = feeding_within_territory %>% filter(repro_stage == "MATING"))
F_vs_M_lac <- cliff.delta(distance_to_midden ~ sex, 
                           data = feeding_within_territory %>% filter(repro_stage == "LACTATING"))

print(F_mating_vs_lac)
print(M_mating_vs_lac)
print(F_vs_M_mating)
print(F_vs_M_lac)

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
capital_feeding <- ggplot(feeding_within_territory, aes(x = sex, y = distance_to_midden, fill = sex)) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.5) +
  facet_grid(. ~ repro_stage) + 
  labs(
    title = "Within-territory Feeding Distance to Midden by Sex and Reproductive Stage",
    x = "Sex",
    y = "Distance to Midden (m)"
  ) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 10))

capital_feeding

# Save the updated plot
ggsave("Output/capital_violin.jpeg", plot = capital_feeding, width = 12, height = 8)






