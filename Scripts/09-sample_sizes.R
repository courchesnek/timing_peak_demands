#load packages
source("Scripts/00-packages.R")

#set krsp username and password
#connection to KRSP database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password"))

#pull in tables
behaviour <- tbl(con,"behaviour") %>%
  collect()

#feeding observations
feeding <- behaviour %>%
  collect() %>%
  dplyr::select(id, behaviour, date, detail, grid, mode, squirrel_id, time, locx, locy) %>%
  collect() %>%
  filter(behaviour == 1,  #feeding observations
         grid %in% c("KL", "SU"), #only control grids
         month(as.Date(date)) %in% c(3,4)) #only March and April obs

#add in sex
squirrel_sex <- tbl(con,"flastall") %>%
  collect() %>%
  dplyr::select(squirrel_id, sex)

#join squirrel sex info to feeding obs
feeding <- left_join(feeding, squirrel_sex, by = "squirrel_id") %>%
  filter(is.na(sex) == FALSE) #remove UTS'

#remove non-natural food sources
feeding <- feeding %>%
  filter(!(detail %in% c(10, 12, 14, 24, 25, 26, 30)))

#add year column from date
feeding <- feeding %>%
  mutate(year = year(ymd(date)))

feeding_obs <- feeding %>%
  mutate(food_group = case_when(
    detail == 2 ~ "old_cones",           
    detail == 3 ~ "buds",                
    detail == 4 ~ "old_mushrooms",           
    TRUE       ~ "other")) %>%
  group_by(sex, food_group) %>%
  summarise(num_obs = n(), .groups = "drop") %>%
  complete(sex, food_group, fill = list(num_obs = 0))

feeding_obs

#save table
write.csv(feeding_obs, "Output/feeding_obs.csv", row.names = FALSE)


feeding_obs_year <- feeding %>%
  mutate(food_group = case_when(
    detail == 2 ~ "old_cones",           
    detail == 3 ~ "buds",                
    detail == 4 ~ "old_mushrooms",           
    TRUE       ~ "other")) %>%
  group_by(year, sex, food_group) %>%
  summarise(num_obs = n(), .groups = "drop") %>%
  complete(year, sex, food_group, fill = list(num_obs = 0))

feeding_obs_year

#save table
write.csv(feeding_obs_year, "Output/feeding_obs_year.csv", row.names = FALSE)

feed_obs_totals <- feeding %>%
  group_by(year) %>%
  summarise(num_obs = n(), .groups = "drop") %>%
  complete(year, fill = list(num_obs = 0))

#save
write.csv(feed_obs_totals, "Output/feeding_obs_KL_SU.csv", row.names = FALSE)


