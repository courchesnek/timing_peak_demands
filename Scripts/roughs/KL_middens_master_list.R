#load packages
source("Scripts/00-packages.R")

#connection to KRSP database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

# midden list prep --------------------------------------------------------
#load census table
census <- collect(tbl(con, "census"))

#filter for KL
census_KL <- census %>%
  filter(gr == "KL")

#let's create a master list of all middens on KL and their exact locs
middens <- unique(census_KL)
middens <- middens %>%
  select(reflo, locx, locy) %>%
  distinct() %>%
  na.omit()

#save and fix incorrect entries manually
write.csv(middens, "Output/middens.csv", row.names = FALSE)


# start here --------------------------------------------------------------
#load in fixed midden list
KLmiddens <- read.csv("Input/middens.csv")

#create copy for comparison
middens <- KLmiddens

#we need one unique entry for each midden reflo
middens$locx = loc_to_numeric(middens$locx)

avg_middens <- middens %>%
  group_by(reflo) %>%
  summarise(avg_locx = mean(locx),
            avg_locy = mean(locy))

avg_middens <- avg_middens %>%
  mutate(avg_locx = round(avg_locx, 1),
         avg_locy = round(avg_locy, 1))

#convert back to original format 
#define a function to change back to letters
numeric_to_alphabet <- function(numeric_value) {
  if (numeric_value < 1) {
    return(as.character(numeric_value))  # Return negative numbers and zeros unchanged
  } else {
    integer_part <- floor(numeric_value)
    decimal_part <- as.numeric(sub("^.*\\.", "", as.character(numeric_value)))
    if (decimal_part == integer_part) {
      return(paste0(LETTERS[integer_part], ".0"))
    } else {
      alphabet_part <- LETTERS[integer_part]
      return(paste0(alphabet_part, ".", decimal_part))
    }
  }
}

avg_middens$avg_locx <- sapply(avg_middens$avg_locx, numeric_to_alphabet)

#rename columns and create a copy to save
avg_middens <- avg_middens %>%
  rename(locx = avg_locx,
         locy = avg_locy)

KLmidsmaster <- avg_middens

# save --------------------------------------------------------------------
write.csv(KLmidsmaster, "Input/KLmidsmaster.csv", row.names = FALSE)



