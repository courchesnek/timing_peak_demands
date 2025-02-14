#load packages
source("Scripts/00-packages.R")

# read in data ----------------------------
feeding <- read.csv("Input/all_feeding_census.csv")
midden_cones <- read.csv("Input/midden_cones.csv")

feeding <- feeding %>%
  mutate(date = as.Date(date)) %>%
  na.omit()

#filter out unnecessary columns
feeding <- feeding %>%
  dplyr::select(-locx_obs, -locy_obs, -snow, -locx_census, -locy_census)

#how many ind squirrels?
length(unique(feeding$squirrel_id))

# align feeding repro_stage with DEE seasons ------------------------------
feeding <- feeding %>%
  mutate(
    date = as.Date(date),  #ensure the `date` column is in Date format
    season = case_when(
      #assign 'winter' if it's non-breeding before May 1
      repro_stage == "non-breeding" & format(date, "%m-%d") < "05-01" ~ "winter",
      #keep other values as they are from repro_stage
      TRUE ~ repro_stage))


# adjust levels and data classes ---------------------------------------------
feeding <- feeding %>%
  mutate(food_type = factor(food_type),
         season = factor(season),
         sex = factor(sex),
         squirrel_id = factor(squirrel_id))

midden_cones <- midden_cones %>%
  mutate(squirrel_id = factor(squirrel_id))

feeding <- feeding %>%
  mutate(season = relevel(factor(season), ref = "non-breeding"),
         year = as.numeric(year)) %>%
  rename(feeding_year = year)


# join feeding and cone data ----------------------------------------------
#need to make sure cones from year 1 are being compared to feeding in year +1 (ex: 2022 cones comapred to 2023 feeding)
midden_cones_adjusted <- midden_cones %>%
  dplyr::rename(
    cache_size_new_prev_year = cache_size_new,
    total_cones_prev_year = total_cones,
    log_cache_size_new_prev_year = log_cache_size_new,
    log_total_cones_prev_year = log_total_cones) %>%
    mutate(year = as.numeric(year),
    following_year = year + 1)

#join feeding and cones based on year +1 from cones
feeding_cache <- left_join(feeding, midden_cones_adjusted %>%
                 dplyr::select(squirrel_id, following_year, 
                 cache_size_new_prev_year, total_cones_prev_year, 
                 log_cache_size_new_prev_year, log_total_cones_prev_year),
                 by = c("squirrel_id", "feeding_year" = "following_year")) %>%
                 na.omit()

#summarise data ###########################################################

# build model -------------------------------------------------------------
#create a binary response variable: capital = 1, income = 0
feeding_cache_binomial <- feeding_cache %>%
  mutate(food_type_bin = ifelse(food_type == "capital", 1, 0))  #capital = 1, income = 0

feeding_cache_binomial$food_type_bin <- relevel(feeding_cache_binomial$food_type_bin, ref = "1")  #make capital the reference

#group by squirrel_id and count the number of feeding events per squirrel
feeding_counts <- feeding_cache %>%
  group_by(squirrel_id, sex, year) %>%
  summarise(feeding_events = n())

##add cache size to counts
feeding_counts <- feeding_counts %>%
  left_join(
    feeding_cache %>%
      dplyr::select(squirrel_id, sex, year, log_cache_size_new) %>%
      distinct(),
    by = c("squirrel_id", "sex", "year"))

##some squirrels don't have enough obs - what should the threshold be?
subset(feeding_counts, feeding_events >= 1 & feeding_events <= 4) %>% nrow() #604 squirrels with only 1-4 feeding events in a year

feeding_cache_binomial <- feeding_cache_binomial %>%
  group_by(squirrel_id, feeding_year) %>%
  mutate(feeding_events = n()) %>%
  ungroup()

feeding_cache_5 <- feeding_cache_binomial %>%
  filter(!(feeding_events >= 1 & feeding_events <= 4)) #still left with 12,342 feeding events from 298 squirrel-year combinations

#fit a GLMM model to predict capital feeding vs income feeding based on cones cached and sex
model <- glmer(food_type_bin ~ log_cache_size_new_prev_year * sex + log_total_cones_prev_year + season + sex * season + (1 | squirrel_id),
               family = binomial(link = "logit"),
               data = feeding_cache_binomial,
               control = glmerControl(optimizer = "bobyqa", 
               optCtrl = list(maxfun = 1000000)))  # Increase maxfun (number of iterations))
#season alone = overall effect on season on both sexes vs season*sex interaction = seasonal differences between sexes

summary(model)
#positive estimate = effect is stronger for compared ground vs negative = effect is stronger for reference group

#model reference categories?
feeding_cache_binomial$food_type_bin <- factor(feeding_cache_binomial$food_type_bin)
contrasts(feeding_cache_binomial$food_type_bin) #capital is reference category
contrasts(feeding_cache_binomial$sex) #female is reference category
contrasts(feeding_cache_binomial$season) #non-breeding is reference category




