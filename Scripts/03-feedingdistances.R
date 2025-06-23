#load packages
source("Scripts/00-packages.R")

#read in data
feeding <- read.csv("Input/all_feeding_census_exact_locs.csv")

#make sure there are no remaining NAs in locx and locy
feeding %>%
  summarise(
    n_na_locx = sum(is.na(locx_feed)),
    n_na_locy = sum(is.na(locy_feed)),
    n_na_locx_census = sum(is.na(locx_census)),
    n_na_locy_census = sum(is.na(locy_census)))

#fix one weird grid entry
#feeding$grid[feeding$squirrel_id == 20942] <- "SU"

feeding <- feeding %>%
  mutate(
    locx_census_numeric = loc_to_numeric(locx_census), 
    locx_feed_numeric = loc_to_numeric(locx_feed))

# create midden buffers to represent territory sizes and middens "islands" --------------------------
#define the buffer sizes in "grid logic"
buffer_radius <- 52 / 30 #52m buffer in grid terms - max territory size of 0.86ha according to LaMontagne
midden_radius <- 10 / 30 #10m buffer in grid terms

#add buffer to feeding table
feeding <- feeding %>%
  mutate(buffer_radius = buffer_radius,
         midden_radius = midden_radius)

#plot with a panel for each grid
# census_mids_buff <- ggplot(feeding, aes(x = locx_census_numeric, y = as.numeric(locy_census))) +
#   #plot midden locations, colored by grid
#   geom_point(aes(color = grid), size = 1, show.legend = FALSE) +
#   #add the 52m buffer (territory size)
#   geom_circle(
#     aes(x0 = locx_census_numeric, 
#         y0 = as.numeric(locy_census),
#         r = buffer_radius),
#         color = "red", fill = NA, alpha = 0.5) +
#   #add the 10m buffer (midden "island")
#   geom_circle(
#     aes(x0 = locx_census_numeric, 
#         y0 = as.numeric(locy_census),
#         r = midden_radius),
#         color = "blue", fill = NA, alpha = 0.5) +
#   #facet by grid
#   facet_wrap(~ grid, scales = "free") + 
#   #add labels and titles
#   labs(title = "Midden and Territory Boundaries by Grid",
#     x = "locx",
#     y = "locy") +
#   theme_minimal() +
#   theme(
#     strip.text = element_text(size = 10),
#     plot.title = element_text(hjust = 0.5))
# 
# census_mids_buff
# 
# #save
# ggsave("Output/census_mids_buff.jpeg", plot = census_mids_buff, width = 8, height = 6)


# fix incorrectly entered locx and locy feeding obs -----------------------
feeding <- feeding %>%
  mutate(locy_feed = if_else(squirrel_id == 22358 & date == "2019-09-11" & locx_feed == -2.7 & locy_feed == -10.1, "10.1", locy_feed))

#add trailing 0's
feeding <- feeding %>%
  mutate(
    locx_feed = if_else(str_detect(locx_feed, "\\."), locx_feed, paste0(locx_feed, ".0")),
    locy_feed = if_else(str_detect(locy_feed, "\\."), locy_feed, paste0(locy_feed, ".0")))

#some locs were entered with commas
feeding <- feeding %>%
  mutate(
    locx_feed = str_replace_all(locx_feed, ",", "."),
    locy_feed = str_replace_all(locy_feed, ",", "."))

#fix bad loc entries
feeding <- feeding %>%
  mutate(
    locx_feed = if_else(locx_feed == "D5.0", "D.5", locx_feed),
    locx_feed = if_else(locx_feed == "B.10", "B.3", locx_feed),
    locy_feed = if_else(locy_feed == "0.7.0", "0.7", locy_feed),
    locx_feed = if_else(locx_feed == "E.10", "E.3", locx_feed),
    locx_feed = if_else(locx_feed == "E.11", "E.4", locx_feed),
    locx_feed = if_else(locx_feed == "E.12", "E.4", locx_feed),
    locx_feed = if_else(locx_feed == "E.13", "E.4", locx_feed),
    locx_feed = if_else(locx_feed == "C.10", "C.3", locx_feed),
    locx_feed = if_else(locx_feed == "C.12", "C.4", locx_feed),
    locx_feed = if_else(locx_feed == "C.13", "C.4", locx_feed),
    locx_feed = if_else(locx_feed == "C.14", "C.5", locx_feed),
    locx_feed = if_else(locx_feed == "C.15", "C.5", locx_feed),
    locx_feed = if_else(locx_feed == "C.16", "C.5", locx_feed),
    locx_feed = if_else(locx_feed == "C.19", "C.6", locx_feed))

#fix bad loc entries - income feeding
feeding <- feeding %>%
  mutate(
    locx_feed = if_else(locx_feed == "J.10", "J.3", locx_feed),
    locx_feed = if_else(locx_feed == "K.10", "K.3", locx_feed),
    locx_feed = if_else(locx_feed == "N.o", "N.0", locx_feed),
    locx_feed = if_else(locx_feed == "L.10", "L.3", locx_feed),
    locx_feed = if_else(locx_feed == "L.11", "L.4", locx_feed))

#remove O.9/P.0 and J./11.7 locs - can't do anything with those
feeding <- feeding %>%
  filter(!(locx_feed == "O.9" | locy_feed == "P.0" | locx_feed == "J." | locy_feed == "11.7"))

#remove G.0/(null) locs - can't do anything with that
feeding <- feeding %>%
  filter(!(locx_feed == "G.0" | locy_feed == "(null).0"))

#locx's to numeric again
feeding <- feeding %>%
  mutate(
    locx_census_numeric = loc_to_numeric(locx_census), 
    locx_feed_numeric = loc_to_numeric(locx_feed))

# determine if feeding observations fall within: ------------------------------------------
# a) the territory boundary (52m radius) and
# b) the midden "island" (10m radius)

buffer_radius_meters <- 52 #52m radius in meteres
midden_radius_meters <- 13 #10m midden "island" radius +/- 3m error

#calculate Euclidean distance and determine if within buffer
feeding_distances <- feeding %>%
  filter(food_type == "capital") %>% #filter for only old cone feeding events
  mutate(
    #calculate Euclidean distance (in meters)
    distance_to_midden = sqrt(
      ((locx_feed_numeric - locx_census_numeric) * 30)^2 + #convert x-difference to meters
        ((as.numeric(locy_feed) - as.numeric(locy_census)) * 30)^2), #convert y-difference to meters
    #flag whether the feeding event falls within territory and within the midden
    within_territory = distance_to_midden <= buffer_radius_meters,
    within_midden = distance_to_midden <= midden_radius_meters)

#extract rows where distance calc resulted in NA - should be 0 now
NA_rows <- feeding_distances %>%
  filter(is.na(distance_to_midden) & is.na(within_territory))

#save
write.csv(feeding_distances, "Input/feeding_distances.csv", row.names = FALSE)

# income plus capital -----------------------------------------------------
feeding_distances_all <- feeding %>%
  mutate(
    #calculate Euclidean distance (in meters)
    distance_to_midden = sqrt(
      ((locx_feed_numeric - locx_census_numeric) * 30)^2 + #convert x-difference to meters
        ((as.numeric(locy_feed) - as.numeric(locy_census)) * 30)^2), #convert y-difference to meters
    #check if the observation is within the buffer radius and within the midden "island"
    within_territory = distance_to_midden <= buffer_radius_meters,
    within_midden = distance_to_midden <= midden_radius_meters)

#extract rows where distance calc resulted in NA - should be 0 now
NA_rows_all <- feeding_distances_all %>%
  filter(is.na(distance_to_midden) & is.na(within_territory))

#save
write.csv(feeding_distances_all, "Input/feeding_distances_all.csv", row.names = FALSE)
