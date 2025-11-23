#load packages
source("Scripts/00-packages.R")

#set krsp username and password
#connection to KRSP database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password"))

#pull in behaviour table
behaviour <- tbl(con,"behaviour") %>%
  collect()

#1) Prepare feeding data -----
#pull feeding obs from behaviour table
feeding <- behaviour %>%
  collect() %>%
  dplyr::select(id, behaviour, date, detail, grid, mode, squirrel_id, time, locx, locy) %>%
  mutate(year = year(ymd(date))) %>%
  filter(behaviour == 1,  #feeding observations
         mode %in% c(1,3), #cas obs or focals
         grid %in% c("KL", "SU", "CH")) %>% #keep control grids
  na.omit()

#we still need the sex of the squirrels here so let's connect to the flastall (first_last_all contains first last records of squirrels and is really handy for this type of stuff)... 
# ...pull squirrel_id and sex, then link that to the feeding table
squirrel_sex <- tbl(con,"flastall") %>%
  collect() %>%
  dplyr::select(squirrel_id, sex)

#join squirrel sex info to feeding obs
feeding <- left_join(feeding, squirrel_sex, by = "squirrel_id") %>%
  filter(is.na(sex) == FALSE)

#double check if any NAs in sex column
length(feeding$sex[is.na(feeding$sex) == TRUE])

#keep only relevant columns
feeding <- feeding %>%
  dplyr::select(squirrel_id, sex, date, detail, grid, locx, locy)

#fix weird detail entries 
feeding <- feeding %>%
  filter(detail != "0" & detail != "") %>% #remove "other" and blank entries
  mutate(detail = as.character(detail)) %>% #convert to character to remove leading zeros
  mutate(detail = sub("^0", "", detail)) %>% #remove leading zeros
  mutate(detail = as.numeric(detail)) %>% #convert back to numeric
  filter(!is.na(detail)) #remove NAs

#remove non-natural food sources
feeding <- feeding %>%
  filter(!(detail %in% c(10, 12, 14, 24, 25, 26, 30)))

#sort food types into three categories: dry mushroom (detail = 4), fresh mushroom (detail = 23, 31, 32), cached cone (detail = 2)
feeding <- feeding %>%
  mutate(
    date = as.Date(date),
    year = lubridate::year(date),
    food_type = case_when(
      detail == 2 ~ "cached_cone",
      detail == 4 ~ "dry_mushroom",
      detail %in% c(23, 31, 32) ~ "fresh_mushroom",
      TRUE ~ NA_character_)) %>%
  filter(!is.na(food_type)) %>%
  mutate(
    food_type = factor(
      food_type,
      levels = c("cached_cone", "dry_mushroom", "fresh_mushroom")))

#2) Pull in cone and mushroom indices and add to feeding table -------
tree_cones <- read.csv("Input/tree_cones.csv")
mushrooms <- read.csv("Input/mushrooms.csv")

feeding <- feeding %>%
  left_join(tree_cones %>% dplyr::select(year, cone_index), by = "year") %>%
  left_join(mushrooms %>% dplyr::select(year, mushroom_index), by = "year") %>%
  mutate(
    cone_index_scaled = as.numeric(scale(cone_index)),
    mushroom_index_scaled = as.numeric(scale(mushroom_index)))


#3: Model 1 — Cached vs. Fresh Mushrooms ------------------------------
#data prep
mush_feeding <- feeding %>%
  filter(food_type %in% c("dry_mushroom", "fresh_mushroom")) %>%
  mutate(
    sex = factor(sex),
    year = factor(year),
    dry = ifelse(food_type == "dry_mushroom", 1, 0))

#model
mod_dry_fresh <- glmmTMB(
  dry ~ sex + cone_index_scaled + mushroom_index_scaled + (1 | squirrel_id) + (1 | year),
  data = mush_feeding,
  family = binomial(link = "logit"))

summary(mod_dry_fresh)

#predictions for plotting - model predicts dry feeding
pred_dry_fresh <- ggpredict(
  mod_dry_fresh,
  terms = "sex",
  type = "fixed",
  bias_correction = TRUE)

pred_dry_fresh <- as.data.frame(pred_dry_fresh) %>%
  mutate(across(c(predicted, conf.low, conf.high), as.numeric))

cached <- pred_dry_fresh %>%
  mutate(
    mushroom_type = "cached")

fresh <- pred_dry_fresh %>%
  mutate(
    mushroom_type = "fresh",
    predicted = 1 - predicted,
    conf.low = 1 - conf.high,
    conf.high = 1 - conf.low)

pred_dry_fresh_full <- bind_rows(cached, fresh)

#3: Model 2 — Mushroom feeding vs other food types ------------------------------
#Goal: Test whether squirrels (males vs. females) differ in their overall reliance 
# on mushrooms (fresh + cached) relative to all other foods, while controlling for 
# year-to-year variation in cone and mushroom availability.
#data prep
feeding_mush_compare <- feeding %>%
  filter(food_type %in% c("cached_cone", "dry_mushroom")) %>%
  mutate(
    # create binary variable: 1 = dry_mushroom, 0 = cached_cone
    mushroom_feed = ifelse(food_type == "dry_mushroom", 1, 0),
    sex = factor(sex),
    year = as.factor(year))

#model
mod_mush_cone <- glmmTMB(
  mushroom_feed ~ sex + cone_index_scaled + mushroom_index_scaled + (1 | squirrel_id) + (1 | year),
  data = feeding_mush_compare,
  family = binomial(link = "logit"))

summary(mod_mush_cone)

#predictions for plotting - model predicts mushroom feeding
pred_mush_cone <- ggpredict(
  mod_mush_cone,
  terms = "sex",
  condition = c(cone_index_scaled = 0, mushroom_index_scaled = 0),
  bias_correction = TRUE)

pred_mush_cone <- as.data.frame(pred_mush_cone)

#calculate cone feeding predictions manually
pred_mush_cone <- pred_mush_cone %>%
  mutate(predicted_cone = 1 - predicted,
         conf.low_cone = 1 - conf.high,
         conf.high_cone = 1 - conf.low)

#reshape for plotting
pred_long <- pred_mush_cone %>%
  dplyr::select(x, predicted, conf.low, conf.high) %>%
  mutate(food_type = "Mushroom") %>%
  bind_rows(
    pred_mush_cone %>%
      dplyr::select(x, predicted = predicted_cone,
             conf.low = conf.low_cone,
             conf.high = conf.high_cone) %>%
      mutate(food_type = "Cone")) %>%
  mutate(x = factor(x, levels = c("M", "F")))

#plot
mush_cone <- ggplot(pred_long, aes(x = x, y = predicted, fill = food_type)) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.6,
           colour = "black",
           linewidth = 0.8) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.15, 
                position = position_dodge(width = 0.8),
                linewidth = 0.9) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.0),
    expand = c(0, 0)) +
  scale_x_discrete(labels = c("F" = "Female", "M" = "Male"),
                   expand = expansion(add = 0.4)) +
  scale_fill_manual(
    values = c("Cone" = "#E69F00", "Mushroom" = "#009E73"),
    labels = c("Spruce Cone Seed", "Mushroom"),
    name = "Food Type") +  
  labs(x = NULL,
       y = "Predicted proportion of total feeding events") +
  theme_classic(base_size = 22) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.0),
    axis.line = element_line(colour = "black", linewidth = 0),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.minor.y = element_line(color = "grey95", linewidth = 0.5),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.text.y = element_text(size = 18, color = "black"),
    legend.position = "bottom",
    legend.title = element_text(size = 21, face = "bold"),
    legend.text = element_text(size = 20),
    plot.margin = margin(t = 35, r = 20, b = 20, l = 10))

mush_cone

#save
ggsave("Output/mushroom_vs_cone_feeding.png", plot = mush_cone, width = 12, height = 9)


