#load packages
source("Scripts/00-packages.R")

#set krsp username and password
#connection to KRSP database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

#pull in census table
census <- tbl(con,"census") %>%
  collect()

#pull in feeding obs
# cone_feeding <- read.csv("Input/conefeedingobs.csv")
# income_feeding <- read.csv("Input/incomefeedingobs.csv")
all_feeding <- read.csv("Input/allfeedingobs.csv")

#clean up census data before merging
census <- census %>%
  mutate(census_date = as.Date(census_date))

census_locs <- census %>%
  filter(format(census_date, "%m-%d") == "05-15") %>%
  dplyr::select(census_date, gr, squirrel_id, locx, locy, reflo) %>%
  na.omit()


# fix weird/missing locs/reflos -------------------------------------------
#missing reflos
census_locs$reflo[census_locs$locx == "A.0" & census_locs$locy == 5.5 &
                       (is.na(census_locs$reflo) | census_locs$reflo == "")] <- "A5."

census_locs$reflo[census_locs$locx == "0.0" & census_locs$locy == 1.5 &
                    (is.na(census_locs$reflo) | census_locs$reflo == "")] <- "01."

census_locs$reflo[census_locs$locx == "K.0" & census_locs$locy == 2.5 &
                    (is.na(census_locs$reflo) | census_locs$reflo == "")] <- "K2."

census_locs$reflo[census_locs$locx == "A.0" & census_locs$locy == 4.5 &
                    (is.na(census_locs$reflo) | census_locs$reflo == "")] <- "A4."

census_locs <- census_locs %>%
  mutate(reflo = ifelse(census_date == as.Date("2016-05-15") & squirrel_id == 22476, "K.8", reflo))

census_locs <- census_locs %>%
  mutate(reflo = ifelse(census_date == as.Date("2016-05-15") & squirrel_id == 22229, "K.7", reflo))

#weird reflos
census_locs$reflo[census_locs$locx == "-0.4" & census_locs$locy == "13.6"] <- "-0.13."
census_locs$reflo[census_locs$locx == "-0.5" & census_locs$locy == "5.5"] <- "-0.5."
census_locs$reflo[census_locs$locx == "-1.2" & census_locs$locy == "14.5"] <- "-114."


# create new locx/locy columns --------------------------------------------
#split into subtables
negatives <- census_locs %>%
  filter(grepl("^-", reflo)) %>%
  dplyr::select(-locx, -locy)

letters <- census_locs %>%
  filter(grepl("^[A-Za-z]", reflo)) %>%
  dplyr::select(-locx, -locy)

zeros <- census_locs %>%
  filter(grepl("^0", reflo)) %>%
  dplyr::select(-locx, -locy)

#letters --------------------------------------------------------------------
##split based on dots
letter_with_dot <- letters %>%
  filter(grepl("^[A-Za-z]\\.", reflo) & !is.na(reflo) & nchar(reflo) >= 2)

letter_without_dot <- letters %>%
  filter(!grepl("^[A-Za-z]\\.", reflo) & !is.na(reflo) & nchar(reflo) >= 2)

##letters with dots
#add locx/locy
letter_with_dot <- letter_with_dot %>%
  mutate(
    locx = str_extract(reflo, "^[A-Za-z]\\."),  #extract letter and decimal point
    locy = str_extract(reflo, "(?<=\\.)[0-9]+\\.?")  #remove locx part to get locy
  )

#add the .0's and .5's
##add .5 to locx
letter_with_dot <- letter_with_dot %>%
  mutate(locx = paste0(locx, "5"))

##add either .0 or .5 to locy
letter_with_dot <- letter_with_dot %>%
  mutate(
    locy = ifelse(
      nchar(reflo) >= 4 & (substr(reflo, 4, 4) == "." | substr(reflo, 5, 5) == "."),
      paste0(locy, "5"),  #add .5 if decimal is in the 4th or 5th position of reflo
      paste0(locy, ".0")   #add .0 if no decimal in the 4th or 5th position of reflo
    )
  )

#fix weird reflos/locs
letter_with_dot <- letter_with_dot %>%
  mutate(locy = ifelse(reflo == "V.-0." & locx == "V.5", "-0.5", locy))

letter_with_dot <- letter_with_dot %>%
  mutate(locy = ifelse(reflo == "K.-0." & locx == "K.5", "-0.5", locy))

letter_with_dot <- letter_with_dot %>%
  mutate(locy = ifelse(reflo == "Q.-0." & locx == "Q.5", "-0.5", locy))

letter_with_dot <- letter_with_dot %>%
  mutate(locy = ifelse(reflo == "U.-0." & locx == "U.5", "-0.5", locy))

letter_with_dot <- letter_with_dot %>%
  mutate(locy = ifelse(reflo == "O.-0." & locx == "O.5", "-0.5", locy))

letter_with_dot <- letter_with_dot %>%
  mutate(locy = ifelse(reflo == "P.-0" & locx == "P.5", "-0.0", locy))

letter_with_dot <- letter_with_dot %>%
  mutate(locy = ifelse(reflo == "U.-1" & locx == "U.5", "-1.0", locy))

letter_with_dot <- letter_with_dot %>%
  mutate(locy = ifelse(reflo == "O.-0" & locx == "O.5", "-0.0", locy))

letter_with_dot <- letter_with_dot %>%
  mutate(
    reflo = ifelse(reflo == "B. 9.", "B.9.", reflo),
    locy = ifelse(reflo == "B.9." & squirrel_id == 24549, "9.5", locy))

letter_with_dot <- letter_with_dot %>%
  mutate(
    reflo = ifelse(census_date == as.Date("2020-05-15") & gr == "SU" & squirrel_id == 24314, "T.4.", reflo),
    locy = ifelse(census_date == as.Date("2020-05-15") & gr == "SU" & squirrel_id == 24314, "4.5", locy)
  )

letter_with_dot <- letter_with_dot %>%
  mutate(
    reflo = ifelse(census_date == as.Date("2012-05-15") & gr == "SU" & squirrel_id == 13130, "H4", reflo),
    locx = ifelse(census_date == as.Date("2012-05-15") & gr == "SU" & squirrel_id == 13130, "H.0", locx),
    locy = ifelse(census_date == as.Date("2012-05-15") & gr == "SU" & squirrel_id == 13130, "4.0", locy)
  )

letter_with_dot$reflo[letter_with_dot$locx == "A.5" & letter_with_dot$locy == "12.5"] <- "A.12."

letter_with_dot$reflo[letter_with_dot$locx == "L.5" & letter_with_dot$locy == "3.5"] <- "L.3."

#now for letters without dots
letter_without_dot <- letter_without_dot %>%
  mutate(
    locx = str_extract(reflo, "^[A-Za-z]"),  #extract letter
    locy = str_remove(reflo, "^[A-Za-z]")  #remove the letter to get locy
  ) %>%
  mutate(locy = str_trim(locy))

#add the .0's and .5's
##add .0 to locx
letter_without_dot <- letter_without_dot %>%
  mutate(locx = paste0(locx, ".0"))

##add either .0 or .5 to locy
letter_without_dot <- letter_without_dot %>%
  mutate(
    locy = ifelse(
      nchar(locy) >= 2 & (substr(locy, 2, 2) == "." | substr(locy, 3, 3) == "."),
      paste0(locy, "5"),  #append 5 if there is a decimal in the 2nd or 3rd position
      paste0(locy, ".0")   #append .0 if no decimal in the 2nd or 3rd position
    )
  )

#fix weird reflos/locs
letter_without_dot <- letter_without_dot %>%
  mutate(
    reflo = ifelse(census_date == as.Date("2012-05-15") & gr == "LL" & squirrel_id == 12372, "U-0.", reflo),
    locy = ifelse(census_date == as.Date("2012-05-15") & gr == "LL" & squirrel_id == 12372, "-0.5", locy)
  )



#split again based on dots for negatives ---------------------------------------------------------------
negatives_with_dot <- negatives %>%
  filter(grepl("^-\\d*\\.\\d", reflo) & !is.na(reflo) & nchar(reflo) >= 3)  #decimal in the third position

negatives_without_dot <- negatives %>%
  filter(!grepl("^-\\d*\\.\\d", reflo) & !is.na(reflo) & nchar(reflo) >= 3)  #no decimal in the third position

#make locx and locy columns 
##with dot 
negatives_with_dot <- negatives_with_dot %>%
  mutate(
    locx = substr(reflo, 1, 3),  #extract the first three characters for locx
    locy = str_remove(reflo, "^.{3}")  #remove the first three characters to get locy, keeping everything after
  )

##add .5 to locx
negatives_with_dot <- negatives_with_dot %>%
  mutate(
    locx = paste0(locx, "5")  #add 5 after the decimal in locx
  )

##add either .0 or .5 to locy
negatives_with_dot <- negatives_with_dot %>%
  mutate(
    locy = ifelse(
      nchar(locy) >= 2 & (substr(locy, 2, 2) == "." | substr(locy, 3, 3) == "."),
      paste0(locy, "5"),  #append 5 if there is a decimal in the 2nd or 3rd position
      paste0(locy, ".0")   #append .0 if no decimal in the 2nd or 3rd position
    )
  )

##without dot
#create locx and locy columns
negatives_without_dot <- negatives_without_dot %>%
  mutate(
    locx = str_extract(reflo, "^-\\d"),  #extract the negative sign and first digit
    locy = str_remove(reflo, "^-\\d")  #remove the locx part to get locy (everything after)
  )

##add .0 to locx
negatives_without_dot <- negatives_without_dot %>%
  mutate(locx = paste0(locx, ".0"))

negatives_without_dot <- negatives_without_dot %>%
  mutate(
    locy = ifelse(
      nchar(locy) >= 2 & (substr(locy, 2, 2) == "." | substr(locy, 3, 3) == "."),
      paste0(locy, "5"),  #append 5 if there is a decimal in the 2nd or 3rd position
      paste0(locy, ".0")   #append .0 if no decimal in the 2nd or 3rd position
    )
  )



#split the zeros based on dots ----------------------------------------
zeros_with_dot <- zeros %>%
  filter(grepl("^0\\.\\d", reflo) & !is.na(reflo))

zeros_without_dot <- zeros %>%
  filter(!grepl("^0\\.\\d", reflo) & !is.na(reflo))

#create locx and locy columns
zeros_with_dot <- zeros_with_dot %>%
  mutate(
    locx = str_extract(reflo, "^0\\."),
    locy = str_remove(reflo, "^0\\."))

#add .5 to locx
zeros_with_dot <- zeros_with_dot %>%
  mutate(
    locx = paste0(locx, "5"))

#add either .0 or .5 to locy
zeros_with_dot <- zeros_with_dot %>%
  mutate(
    locy = ifelse(
      nchar(locy) >= 2 & (substr(locy, 2, 2) == "." | substr(locy, 3, 3) == "."),
      paste0(locy, "5"),  #append 5 if there is a decimal in the 2nd or 3rd position
      paste0(locy, ".0")   #append .0 if no decimal in the 2nd or 3rd position
    )
  )


#create locx and locy columns
zeros_without_dot <- zeros_without_dot %>%
  mutate(
    locx = str_extract(reflo, "^0") ,  # Extract the "0" for locx
    locy = str_remove(reflo, "^0")  # Everything after "0" becomes locy
  )

#add .0 to locx
zeros_without_dot <- zeros_without_dot %>%
  mutate(
    locx = paste0(locx, ".0"))

#add either .0 or .5 to locy
zeros_without_dot <- zeros_without_dot %>%
  mutate(
    locy = ifelse(
      nchar(locy) >= 2 & (substr(locy, 2, 2) == "." | substr(locy, 3, 3) == "."),
      paste0(locy, "5"),  #append 5 if there is a decimal in the 2nd or 3rd position
      paste0(locy, ".0")   #append .0 if no decimal in the 2nd or 3rd position
    )
  )


#fix weird reflos/locs
zeros_without_dot <- zeros_without_dot %>%
  mutate(
    locx = ifelse(reflo == "0.-0", "0.5", locx),
    locy = ifelse(reflo == "0.-0", "-0.0", locy))




























# merge all tables back together ------------------------------------------
census_clean <- bind_rows(
  letter_with_dot,
  letter_without_dot,
  negatives_with_dot,
  negatives_without_dot,
  zeros_with_dot,
  zeros_without_dot
)

write.csv(census_clean, "Input/census_clean.csv", row.names = FALSE)


# add census locs to feeding obs ------------------------------------------

#add year columns
# cone_feeding <- cone_feeding %>%
#   mutate(year = format(as.Date(date), "%Y"))
# 
# income_feeding <- income_feeding %>%
#   mutate(year = format(as.Date(date), "%Y"))

all_feeding <- all_feeding %>%
  mutate(year = format(as.Date(date), "%Y"))

census_clean <- census_clean %>%
  mutate(year = format(as.Date(census_date), "%Y"))

#merge tables by squirrel_id and by year
# cone_feeding_census <- cone_feeding %>%
#   left_join(census_clean %>%
#       dplyr::select(squirrel_id, year, locx, locy),
#     by = c("squirrel_id", "year"), relationship = "many-to-many") %>%
#   rename(locx_census = locx.y, locy_census = locy.y, locx_obs = locx.x, locy_obs = locy.x)
# 
# write.csv(cone_feeding_census, "Input/cone_feeding_census.csv", row.names = FALSE)
# 
# income_feeding_census <- income_feeding %>%
#   left_join(census_clean %>%
#       dplyr::select(squirrel_id, year, locx, locy),
#       by = c("squirrel_id", "year"), relationship = "many-to-many") %>%
#   rename(locx_census = locx.y, locy_census = locy.y, locx_obs = locx.x, locy_obs = locy.x)
# 
# write.csv(income_feeding_census, "Input/income_feeding_census.csv", row.names = FALSE)

all_feeding_census <- all_feeding %>%
  left_join(census_clean %>%
              dplyr::select(squirrel_id, year, locx, locy),
            by = c("squirrel_id", "year"), relationship = "many-to-many") %>%
  rename(locx_census = locx.y, locy_census = locy.y, locx_obs = locx.x, locy_obs = locy.x)

write.csv(all_feeding_census, "Input/all_feeding_census.csv", row.names = FALSE)







