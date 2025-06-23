#load packages
source("Scripts/00-packages.R")

#connection to KRSP database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password"))

#pull in squirrel census and midden table
census_squirrels <- tbl(con,"census") %>%
  collect()

census_middens <- tbl(con, "dbamidden") %>%
  collect()

#grids of interest
grids <- c("KL","SU","CH","BT","JO")

# get one exact loc set for each unique reflo -----------------------------
#first, retain only relevant columns from census tables and join them into one table
squirrels <- census_squirrels %>%
  dplyr::select(gr, census_date, reflo, locx, locy, squirrel_id) %>%
  rename(grid = gr)

middens <- census_middens %>%
  dplyr::select(grid, date, reflo, locX, locY, squirrel_id) %>%
  rename(census_date = date,
         locx = locX,
         locy = locY)

census_master <- bind_rows(squirrels, middens)

#filter for grid/years of interest
census_exactlocs <- census_master %>%
  mutate(
    census_date = as.Date(census_date),
    year        = year(census_date)) %>%
  filter(grid %in% grids,
         year <= 2020, #every grid through 2020 (when exact locs were last used in census)
         !(grid == "JO" & year < 2013)) %>% #drop JO before 2013 (avoid food add)
  dplyr::select(-year)

#numeric locx to letters
num_to_letter <- setNames(LETTERS, as.character(1:26))

numeric_to_locx <- function(x) {
  x_chr <- as.character(x)
  
  # 1) If it already starts with a letter, do nothing
  if (grepl("^[A-Za-z]", x_chr)) return(x_chr)
  
  # 2) If it's zero or negative, leave it untouched
  #    (we assume anything starting with "-" or "0" should stay)
  if (grepl("^0", x_chr) || grepl("^-", x_chr)) return(x_chr)
  
  # 3) Split into integer-part before dot and the rest
  base  <- sub("\\.(.*)$", "", x_chr)      # e.g. "3" from "3.57"
  after <- sub("^[^.]*", "", x_chr)        # e.g. ".57" from "3.57"
  
  # 4) Look up letter (only if base in 1:26)
  letter <- num_to_letter[base]
  if (is.na(letter)) {
    warning("Integer part out of A–Z range (or not 1–26): ", base)
    return(x_chr)}
  
  # 5) Reassemble
  paste0(letter, after)}

numeric_to_locx_vec <- Vectorize(numeric_to_locx)

census_final <- census_exactlocs %>%
  mutate(locx = numeric_to_locx_vec(locx))

#fix missing reflos
compute_reflo <- function(locx, locy) {
  #split into “base” and “decimal” for each
  parts_x <- strsplit(as.character(locx), "\\.", perl=TRUE)[[1]]
  base_x <- toupper(parts_x[1])
  dec_x  <- if (length(parts_x)>1) as.numeric(paste0("0.", parts_x[2])) else 0
  
  parts_y <- strsplit(as.character(locy), "\\.", perl=TRUE)[[1]]
  base_y <- parts_y[1]
  dec_y  <- if (length(parts_y)>1) as.numeric(paste0("0.", parts_y[2])) else 0
  
  #prepare the “integer” strings
  x_str <- base_x
  y_str <- as.character(floor(as.numeric(base_y)))
  
  #flags
  dot_x <- dec_x >= 0.3
  dot_y <- dec_y >= 0.3
  
  #build reflo
  if (dot_x && !dot_y) {
    # x between lines
    paste0(x_str, ".", y_str)
  } else if (!dot_x && dot_y) {
    # y between lines
    paste0(x_str, y_str, ".")
  } else if (dot_x && dot_y) {
    # both between — dot on both sides
    paste0(x_str, ".", y_str, ".")
  } else {
    # neither between
    paste0(x_str, y_str)}}

census_fixed <- census_final %>%
  mutate(
    reflo = ifelse(
      is.na(reflo) | reflo == "",
      mapply(compute_reflo, locx, locy),
      reflo))

#remove rows where reflo, locx and locy are all blank/NA - can't do anything with these
census_fixed <- census_fixed %>%
  mutate(across(c(reflo, locx, locy), ~ {
  x <- trimws(as.character(.))
  x[toupper(x) %in% c("", "-", "NA", "N/A", "NANA")] <- NA
  x})) %>%
  filter(!( is.na(reflo) & is.na(locx) & is.na(locy)))

#fix remaining weird reflo entries manually
census_fixed <- census_fixed %>%
  mutate(
    reflo = case_when(
      locx == "Q.8" & locy == "-0.1" ~ "Q.-0",
      locx == "-0.6" & locy == "-0.2" ~ "-0.-0",
      locx == "A.5" & locy == "-0.3" ~ "A.-0.",
      locx == "F.0" & locy == "-0.5" ~ "F-0.",
      locx == "D.2" & locy == "-0.6" ~ "D-0.",
      locx == "E.0" & locy == "-1.0" ~ "E-1",
      locx == "E.0" & locy == "-1.4" ~ "E-1.",
      locx == "-1.0" & locy == "0.0" ~ "-10",
      TRUE ~ reflo))
 
#fix remaining weird locx/locy manually
census_fixed <- census_fixed %>%
  mutate(
    locx = case_when(
      reflo == "-118." & census_date == as.Date("2001-08-15") ~ "-1.0",
      reflo == "-118." & census_date == as.Date("2002-08-15") ~ "-1.0",
      reflo == "-118." & census_date == as.Date("2003-05-15") ~ "-1.0",
      TRUE ~ locx),
    locy = case_when(
      reflo == "-118." & census_date == as.Date("2001-08-15") ~ "18.5",
      reflo == "-118." & census_date == as.Date("2002-08-15") ~ "18.5",
      reflo == "-118." & census_date == as.Date("2003-05-15") ~ "18.5",
      TRUE ~ locy))

#see if we can salvage locx/locy = NA
missing_coords <- census_fixed %>%
  filter(
    !is.na(reflo),       
    is.na(locx),       
    is.na(locy))

recent_coords <- census_fixed %>%
  filter(!is.na(locx) & !is.na(locy)) %>%     
  group_by(grid, reflo) %>%
  arrange(desc(census_date)) %>%              
  slice(1) %>%                                
  ungroup() %>%
  dplyr::select(grid, reflo, locx_recent = locx, locy_recent = locy)

census_updated <- census_fixed %>%
  left_join(recent_coords, by = c("grid","reflo")) %>%
  mutate(
    locx = coalesce(locx, locx_recent),
    locy = coalesce(locy, locy_recent)) %>%
  dplyr::select(-locx_recent, -locy_recent)

census_updated %>%
  filter(!is.na(reflo) & (is.na(locx) | is.na(locy))) %>%
  nrow()
#should be 0 rows with missing locx or locy now - it's not... can't do anything about those remaining

census_lastversion <- census_updated %>%
  filter(
    !is.na(locx),
    !is.na(locy))

# one set of locx/locy for each unique reflo based on which set appears most often --------
unique_reflos <- census_lastversion %>%
  # 1) count occurrences of each locx/locy per grid+reflo
  count(grid, reflo, locx, locy, name = "freq") %>%
  # 2) for each grid+reflo, keep only the row with highest freq
  group_by(grid, reflo) %>%
  slice_max(order_by = freq, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(grid, reflo, locx, locy)

#join exact locs into census table ---------------------------------------
squirrel_census <- census_master %>%
  mutate(census_date = as.Date(census_date),
         year        = year(census_date)) %>%
  filter(grid %in% grids,
         format(census_date, "%m-%d") == "05-15",
         !(grid == "JO" & year < 2013)) %>%
  dplyr::select(-locx, -locy)

squirrel_census <- squirrel_census %>%
  left_join(
    unique_reflos %>% dplyr::select(grid, reflo, locx, locy),
    by = c("grid", "reflo"))

#reorder columns and remove NAs
squirrel_census <- squirrel_census %>%
  dplyr::select(grid, census_date, year, reflo, locx, locy, squirrel_id) %>%
  na.omit()

#save the final census table
write.csv(squirrel_census, "Output/squirrel_census_exact_locs.csv", row.names = FALSE)


# add census locs to feeding obs ------------------------------------------
#pull in feeding obs table
all_feeding <- read.csv("Input/allfeedingobs.csv")

all_feeding <- all_feeding %>%
  mutate(year = format(as.Date(date), "%Y"))

squirrel_census <- squirrel_census %>%
  mutate(year = format(as.Date(census_date), "%Y"))

all_feeding_census <- all_feeding %>%
  left_join(squirrel_census %>%
              dplyr::select(squirrel_id, year, locx, locy),
            by = c("squirrel_id", "year"), relationship = "many-to-many") %>%
  rename(locx_census = locx.y, locy_census = locy.y, locx_feed = locx.x, locy_feed = locy.x) %>%
  na.omit() #remove feeding events by squirrels with no census record

#save
write.csv(all_feeding_census, "Input/all_feeding_census_exact_locs.csv", row.names = FALSE)

# feeding summary ---------------------------------------------------------
feeding_summary_census <- all_feeding_census %>%
  group_by(year, sex, repro_stage, food_type) %>%
  summarise(num_events = n(), .groups = "drop") %>%
  complete(year, sex, repro_stage, food_type, fill = list(num_events = 0))

#save
write.csv(feeding_summary_census, "Output/feeding_summary_census.csv", row.names = FALSE)

