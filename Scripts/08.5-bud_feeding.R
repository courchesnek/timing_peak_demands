#load packages -----------------
source("Scripts/00-packages.R")

#read in data --------------------------------------------------------
feeding <- read.csv("Input/allfeedingobs.csv")
tree_cones <- read.csv("Input/tree_cones.csv")

#isolate bud feeding --------------
bud_feeding <- feeding %>%
  mutate(
    sex = factor(sex),
    date = as.Date(date),
    julian = yday(date),
    week = isoweek(date),
    bud = ifelse(detail == 3, 1, 0))  # 1 = bud feeding, 0 = other food

#weekly proportions of bud feeding --------------
weekly_prop <- bud_feeding %>%
  mutate(week_start = floor_date(date, "week"),
         julian_week = yday(week_start)) %>%
  group_by(sex, julian_week) %>%
  summarise(
    n_bud = sum(bud, na.rm = TRUE),
    n_total = n(),
    prop_bud = n_bud / n_total,
    .groups = "drop")

# prepare cone production indices and join to feeding data --------
## want to join previous years cone index to following years feeding data
tree_cones_shifted <- tree_cones %>%
  mutate(next_year = year + 1) %>%
  rename(cone_index_previous = cone_index) %>%
  mutate(cone_index_previous_scaled = as.numeric(scale(cone_index_previous)))

bud_feeding <- bud_feeding %>%
  left_join(tree_cones_shifted %>% dplyr::select(next_year, cone_index_previous_scaled),
            by = c("year" = "next_year"))

#GLMM & predictions ------------------------
mod <- glmmTMB(bud ~ sex * poly(julian, 2) + cone_index_previous_scaled + (1 | squirrel_id) + (1 | year),
               data = bud_feeding, family = binomial)

summary(mod)

pred <- ggpredict(
  mod,
  terms = c("julian [all]", "sex"),
  condition = c(cone_index_previous_scaled = 0),
  bias_correction = TRUE)

#plot ------------------------------
bud_feeding <- ggplot(pred, aes(x = x, y = predicted, colour = group, fill = group)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, colour = NA) +
  scale_x_continuous(
    name = "Julian Day",
    limits = c(60, 244),
    breaks = seq(60, 240, by = 30),
    sec.axis = sec_axis(~ .,
                        breaks = c(60, 91, 121, 152, 182, 213),
                        labels = c("Mar", "Apr", "May", "Jun", "Jul", "Aug"),
                        name = "Month")) +
  scale_y_continuous(
    name = "Predicted Proportion of Feeding Events\non Spruce Buds",
    limits = c(0, 0.6),
    breaks = seq(0, 0.6, by = 0.1),
    expand = c(0, 0)) +
  scale_colour_manual(
    values = c("M" = "#1F78B4", "F" = "#E31A1C"),
    labels = c("Male", "Female"),
    name = "Sex") +
  scale_fill_manual(
    values = c("M" = "#1F78B4", "F" = "#E31A1C"),
    labels = c("Male", "Female"),
    name = "Sex") +
  labs(
    title = NULL) +
  theme_classic(base_size = 21) +
  theme(
    axis.title.x.top = element_text(margin = margin(b = 10)),
    legend.position = "bottom",
    legend.box.margin = margin(t = -20, r = 10, b = 0, l = 0),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18))

bud_feeding

#save
ggsave("Output/bud_feeding.png", plot = bud_feeding, width = 12, height = 7)










