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

litters <- tbl(con,"litter") %>%
  collect()

#pull feeding obs from behaviour table
feeding <- behaviour %>%
  collect() %>%
  dplyr::select(id, behaviour, date, detail, grid, mode, squirrel_id, time, locx, locy) %>%
  mutate(year = year(ymd(date))) %>%
  filter(behaviour == 1,  #feeding observations
         mode %in% c(1,3), #cas obs or focals
         grid %in% c("KL", "SU", "CH", "BT") | (grid == "JO" & year >= 2013)) %>% #keep control grids and JO post-food-add (food-add = 2006-2012)
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

##calculate mating windows; -35 days from earliest fieldBDate = start and -35 days from latest = end
#ensure fieldBDate is in date format
litters$fieldBDate <- as.Date(litters$fieldBDate)

mating_lac <- litters %>%
  filter(year(fieldBDate) >= 1987 & year(fieldBDate) <= 2024, ln ==1) %>% #first litters only
  group_by(year = year(fieldBDate)) %>%
  summarise(
    earliest_birth_date = min(fieldBDate),
    latest_birth_date = max(fieldBDate),
    mating_start = min(fieldBDate) - days(35),
    mating_end = max(fieldBDate) - days(35))

##calculate lac windows; earliest fieldBDate = start and +70 days from latest fieldBDate = end
mating_lac <- mating_lac %>%
  mutate(
    lactation_start = earliest_birth_date,  
    lactation_end = latest_birth_date + 70)

#save
write.csv(mating_lac, "Input/reproductive_windows.csv", row.names = FALSE)

#create a column for repro stage by year for mating and lactating
feeding <- feeding %>%
  left_join(mating_lac, by = "year") %>%  #join the mating window data by year
  mutate(
    repro_stage = case_when(
      date >= mating_start & date <= mating_end ~ "mating",
      date >= lactation_start & date <= lactation_end ~ "lactation",
      TRUE ~ "non-breeding")) #anything outside of mating and lactation = non-breeding

feeding <- feeding %>%
  dplyr::select(squirrel_id, sex, date, repro_stage, detail, grid, locx, locy)

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

#group food types for comparisons
income <- c(1, 3, 5, 6, 7, 8, 9, 11, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 27, 28, 29, 31, 32) #income = all other natural resources found fresh on the landscape
capital <- c(2,4) #capital = old cones and mushrooms/truffle

feeding <- feeding %>%
  mutate(food_type = ifelse(detail %in% income, "income", 
                            ifelse(detail %in% capital, "capital", NA))) %>%
  filter(!is.na(food_type))

#create a column for snow cover
# feeding <- feeding %>%
#   mutate(
#     snow = case_when(
#       #snow period: October 15 to May 14
#       (month(date) %in% c(11, 12, 1, 2, 3, 4) | 
#          (month(date) == 10 & day(date) >= 15) | 
#          (month(date) == 5 & day(date) <= 14)) ~ "snow",
#       
#       #no-snow period: May 15 to October 14
#       (month(date) %in% c(6, 7, 8, 9) | 
#          (month(date) == 10 & day(date) <= 14) | 
#          (month(date) == 5 & day(date) >= 15)) ~ "no snow",
#       
#       #default: this should never trigger if the above cases are correct
#       TRUE ~ "error"))

#add year type column
##define mast years
mast_years <- c(1993, 1998, 2005, 2010, 2014, 2019, 2022)

#add a column for year_type
feeding <- feeding %>%
  mutate(
    year = as.numeric(format(as.Date(date, format = "%Y-%m-%d"), "%Y")), #extract year from date
    year_type = case_when(
      year %in% mast_years ~ "mast",
      year %in% (mast_years + 1) ~ "post-mast",
      TRUE ~ "non-mast"))

#reorder columns
feeding <- feeding %>%
  dplyr::select(year, year_type, date, repro_stage, squirrel_id, sex, grid, locx, locy, detail, food_type)

#save
write.csv(feeding, "Input/allfeedingobs.csv", row.names = FALSE)

# data summary ------------------------------------------------------------
feeding_summary <- feeding %>%
  group_by(year, sex, repro_stage, food_type) %>%
  summarise(num_events = n(), .groups = "drop") %>%
  complete(year, sex, repro_stage, food_type, fill = list(num_events = 0))

#save
write.csv(feeding_summary, "Output/feeding_summary.csv", row.names = FALSE)
