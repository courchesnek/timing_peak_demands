#load packages
source("Scripts/00-packages.R")

#set krsp username and password
#connection to KRSP database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

#pull in the behaviour table 
behaviour <- tbl(con,"behaviour") %>%
  collect()

#pull feeding obs from behaviour table
numeric_to_month_abbrev <- function(month_num) {
  month.abb[month_num]
}

feeding <- behaviour %>%
  collect() %>%
  dplyr::select(id, behaviour, date, detail, grid, mode, squirrel_id, time, locx, locy) %>%
  collect() %>%
  filter(behaviour == 1,  #feeding observations
         mode %in% c(1,3))  %>% #casual observations & focal observations
  mutate(year = year(date),
         month_num = month(ymd(date)),  #extract numeric month
         month = numeric_to_month_abbrev(month(ymd(date))),  #convert numeric month to abbreviation
         week = week(ymd(date)),
         mast = case_when(
           year(date) %in% c(1993, 1998, 2005, 2010, 2014, 2019, 2022) ~ "MAST",
           year(date) %in% c(1994, 1999, 2006, 2011, 2015, 2020, 2023) ~ "POSTMAST",
           TRUE ~ "NONMAST") #want years with "typical" breeding windows
  )

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

#create a column for repro stage by month for mating and lactating
feeding <- feeding %>%
  mutate(repro_stage = case_when(
    month %in% c("Mar", "Apr") ~ "MATING",
    month %in% c("May", "Jun", "Jul") ~ "LACTATING",
    TRUE ~ NA_character_  # For other months, NA or any other default value
  )) %>%
  filter(!is.na(repro_stage))

#fix weird detail entries
feeding <- feeding %>%
  filter(detail != "0") %>% #remove "other" entries
  mutate(detail = as.character(detail)) %>% #convert to character to remove leading zeros
  mutate(detail = sub("^0", "", detail)) %>% #remove leading zeros
  mutate(detail = as.numeric(detail)) #convert back to numeric

#filter out for non-natural food sources
feeding <- feeding %>%
  filter(!(detail %in% c(10, 12, 14, 24, 25, 26, 30)))

#remove unnecessary columns
feeding <- feeding %>%
  dplyr::select(-id, -date, -grid, -month_num, -week, -mast)

#let's group food types for comparisons
other_food_types <- c(1, 3, 4, 5, 6, 7, 8, 9, 11, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 27, 28, 29, 31, 32)

feeding <- feeding %>%
  mutate(food_type = ifelse(detail %in% other_food_types, "other", 
                            ifelse(detail == 2, "cone", NA))) %>%
  filter(!is.na(food_type))

#look at the sample size
mating_count <- sum(feeding$repro_stage == "MATING", na.rm = TRUE)
lactating_count <- sum(feeding$repro_stage == "LACTATING", na.rm = TRUE)

#check for NAs
length(feeding$locx[is.na(feeding$locx) == TRUE])
length(feeding$locy[is.na(feeding$locy) == TRUE])

#rename
KLfeedingobs <- feeding %>%
  na.omit()

length(KLfeedingobs$locx[is.na(KLfeedingobs$locx) == TRUE])
length(KLfeedingobs$locy[is.na(KLfeedingobs$locy) == TRUE])

#check sample sizes
print(nrow(KLfeedingobs))


#save --------------------------------------------------------------------
write.csv(KLfeedingobs, "Input/KLfeedingobs.csv", row.names = FALSE)

#feeding obs proportions -------------------------------------------------

#hmm, but maybe i need to account for the amount of effort going into collecting these observations. more effort in lactating period (i.e. summer)?
#group by repro_stage, sex and food_type, and calculate frequencies
feeding_summary <- KLfeedingobs %>%
  group_by(repro_stage, sex, food_type) %>%
  summarise(frequency = n(), .groups = "drop") %>%
  ungroup()

#calculate total feeding events per repro_stage and sex
total_events <- feeding_summary %>%
  group_by(repro_stage, sex) %>%
  summarise(total = sum(frequency), .groups = "drop")

#join total events with summmary and compute total for each row
feeding_summary <- feeding_summary %>%
  left_join(total_events, by = c("repro_stage", "sex"))

#rearrange rows to have mating before lactating in table
feeding_summary <- feeding_summary %>%
  arrange(factor(repro_stage, levels = c("MATING", "LACTATING")))

#calculate proportions
feeding_summary <- feeding_summary %>%
  mutate(proportion = frequency / total)

feeding_summary$repro_stage <- factor(feeding_summary$repro_stage, levels = c("MATING", "LACTATING"))

proportion_of_feeding <- ggplot(feeding_summary, aes(x = sex, y = proportion, fill = food_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(.~ repro_stage) +
  labs(x = "Sex", y = "Proportion of Feeding Events", fill = "Food Type") +
  scale_fill_manual(values = c("cone" = "blue", "other" = "red")) +
  theme_minimal()

proportion_of_feeding

#save
ggsave("Output/proportion_of_feeding.jpeg", proportion_of_feeding, width = 6, height = 4, units = "in")
write.csv(feeding_summary, "Output/proportion_of_feeding.csv", row.names = FALSE)

#feeding obs rates -------------------------------------------------------

#lets calculate the rates of feeding obs in each category to try and account for effort differences
#feeding_rates <- KLfeedingobs %>%
  #group_by(repro_stage, year, sex) %>%
  #summarise(cone_obs = sum(detail == 2, na.rm = TRUE),  #count cone observations
            #other_obs = sum(detail != 2, na.rm = TRUE),  #count observations for other food types
            #total_obs = n()) %>%  #total number of observations
  #mutate(cone_rate = cone_obs / total_obs,  #calculate rate of cone observations
         #other_rate = other_obs / total_obs)  #calculate rate of observations for other food types

#plot
#reshape the data to long format for easier plotting
#feeding_rates_long <- tidyr::pivot_longer(feeding_rates, cols = c(cone_rate, other_rate), names_to = "food_type", values_to = "rate")

#convert 'food_type' to factor with correct levels
#feeding_rates_long$food_type <- factor(feeding_rates_long$food_type, labels = c("Cone", "Other Food Types"))

#create the grouped bar chart comparing rates for both food types
#rate_of_feeding <- ggplot(feeding_rates_long, aes(x = sex, y = rate, fill = food_type)) +
  #geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  #facet_grid(.~ repro_stage) +
  #labs(x = "Sex", y = "Rate of Feeding Observations", fill = "Food Type") +
  #scale_fill_manual(values = c("Cone" = "blue", "Other Food Types" = "red")) +
  #theme_minimal()

#rate_of_feeding

#ggsave("Output/rate_of_feeding.jpeg", rate_of_feeding, width = 6, height = 4, units = "in")