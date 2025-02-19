#load packages
source("Scripts/00-packages.R")

#read in data
feeding <- read.csv("Input/feeding_distances.csv")

#filter for income events
income_feeding <- feeding %>%
  filter(food_type == "income")

#reorder the levels of repro_stage
income_feeding <- income_feeding %>%
  mutate(repro_stage = factor(repro_stage, levels = c("MATING", "LACTATING")))

# calculate proportions for income feeding events -----------------------------------
proportion <- income_feeding %>%
  group_by(repro_stage, sex) %>%
  summarize(total_events = n(),
            .groups = "drop") %>%
  mutate(proportion = total_events / sum(total_events))

proportion

# stats -------------------------------------------------------------------
#Kruskal-Wallis - since working with proportions
kruskal_test <- kruskal.test(proportion ~ interaction(sex, repro_stage), data = proportion)
print(kruskal_test)

#Dunn's post-hoc
dunn_income <- dunn.test(
  x = proportion$proportion,
  g = interaction(proportion$sex, proportion$repro_stage),
  method = "bonferroni")
print(dunn_income)

#Cohen's h effect size
#define a function to calculate Cohen's h
calculate_cohens_h <- function(p1, p2) {
  h <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))
  return(h)
}

#extract proportions and calculate Cohen's h for each comparison
F_mating <- proportion %>% filter(sex == "F", repro_stage == "MATING") %>% pull(proportion)
F_lactating <- proportion %>% filter(sex == "F", repro_stage == "LACTATING") %>% pull(proportion)
M_mating <- proportion %>% filter(sex == "M", repro_stage == "MATING") %>% pull(proportion)
M_lactating <- proportion %>% filter(sex == "M", repro_stage == "LACTATING") %>% pull(proportion)

#calculate Cohen's h
F_mating_vs_F_lactating <- calculate_cohens_h(F_mating, F_lactating)
M_mating_vs_M_lactating <- calculate_cohens_h(M_mating, M_lactating)
F_mating_vs_M_mating <- calculate_cohens_h(F_mating, M_mating)
F_lactating_vs_M_lactating <- calculate_cohens_h(F_lactating, M_lactating)

#print results
list(
  F_mating_vs_F_lactating = F_mating_vs_F_lactating,
  M_mating_vs_M_lactating = M_mating_vs_M_lactating,
  F_mating_vs_M_mating = F_mating_vs_M_mating,
  F_lactating_vs_M_lactating = F_lactating_vs_M_lactating
)

# plot --------------------------------------------------------------------
# raw_counts_plot <- ggplot(income_feeding %>% filter(food_type == "income"),
#                           aes(x = repro_stage, fill = sex)) +
#   geom_bar(position = "dodge") +
#   labs(
#     title = "Count of Income Feeding Events by Sex and Reproductive Stage",
#     x = "Reproductive Stage",
#     y = "Count of Income Feeding Events",
#     fill = "Sex"
#   ) +
#   scale_fill_manual(values = c("red", "blue")) +
#   theme_minimal()
# 
# print(raw_counts_plot)

proportions <- ggplot(proportion, aes(x = repro_stage, y = proportion, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  labs(
    title = "Proportion of Income Feeding Events by Sex and Reproductive Stage",
    x = "Reproductive Stage",
    y = "Proportion of Income Feeding Events",
    fill = "Sex"
  ) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

print(proportions)

ggsave("Output/income_proportions.jpeg", plot = proportions, width = 8, height = 6)



# GLM ---------------------------------------------------------------------
#reorder the levels of repro_stage
feeding <- feeding %>%
  mutate(repro_stage = factor(repro_stage, levels = c("MATING", "LACTATING")))

#summarize feeding data
feeding_summary <- feeding %>%
  group_by(sex, repro_stage) %>%
  summarize(
    total_income = sum(food_type == "income"), 
    total_events = n(),                        
    total_non_income = total_events - total_income,
    .groups = "drop")

summary(feeding_summary)

write.csv(feeding_summary, "Output/feeding_summary.csv", row.names = FALSE)

#binomial GLM
glm_model <- glm(
  cbind(total_income, total_non_income) ~ sex * repro_stage,
  family = binomial,
  data = feeding_summary)

summary(glm_model)

#odds ratios for easier interpretation
exp(coef(glm_model))

##plot
#aggregate data to calculate proportions for both food types
feeding_summary <- feeding %>%
  group_by(sex, repro_stage, food_type) %>%
  summarize(
    total_events = n(),
    .groups = "drop"
  ) %>%
  group_by(sex, repro_stage) %>%
  mutate(
    proportion = total_events / sum(total_events) # Calculate proportions within each sex and stage
  )

stacked_bar_plot <- ggplot(feeding_summary, aes(x = repro_stage, y = proportion, fill = food_type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) + # Stacked bars
  facet_wrap(~ sex, ncol = 2, labeller = as_labeller(c(F = "Female", M = "Male"))) + # Separate by sex
  labs(
    title = "Proportion of Feeding Events by Food Type, Sex, and Reproductive Stage",
    x = "Reproductive Stage",
    y = "Proportion of Feeding Events",
    fill = "Food Type"
  ) +
  scale_fill_manual(values = c("income" = "green", "capital" = "orange")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  )

stacked_bar_plot


ggsave("Output/stacked_feeding.jpeg", plot = stacked_bar_plot, width = 8, height = 6)








