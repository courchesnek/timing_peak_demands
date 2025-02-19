#load packages
source("Scripts/00-packages.R")

#read in data
middens <- read.csv("Input/KLmidsmaster.csv")
middens$locx <- loc_to_numeric(middens$locx)
middens$locy <- loc_to_numeric(middens$locy)
feeding <- read.csv("Input/KLfeedingobs.csv")

#database connect --------------------------------------------------------
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

con_suppl <- krsp_connect(host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                          dbname ="krsp_suppl",
                          username = Sys.getenv("krsp_user"),
                          password = Sys.getenv("krsp_password")
)

#pull in grid stakes table
grid_stakes <- tbl(con_suppl, "grid_stakes") %>%
  filter(!is.na(north),!is.na(west),
         grid =="KL",
         north > 55) %>%
  collect() %>% 
  mutate(east = -west)

locs_to_utms <- function(grid_stakes, x){
  #split stake names from XY to X and Y
  split = separate(grid_stakes[1:105,],stake,c("x","y"),sep=2)
  split[105,"y"] = "0"
  split2 = separate(grid_stakes[106:121,],stake,c("x","y"),sep=0,fill="left") 
  split2$x = "0"
  split3 = separate(grid_stakes[122:425,],stake,c("x","y"),sep=1)
  #recombine and make x and y numeric
  stakes = rbind(split,split2,split3)
  stakes$x = loc_to_numeric(stakes$x)
  stakes$y = as.numeric(stakes$y)
  stakes = na.omit(stakes)
  #run regression to calculate the northings and westings from locx and locy
  xfit = lm(stakes$x~stakes$north+stakes$west)
  yfit = lm(stakes$y~stakes$north+stakes$west)
  #pull coefficients to calculate the line
  a=coef(xfit)[1]
  b=coef(xfit)[2]
  c=coef(xfit)[3]
  
  d=coef(yfit)[1]
  e=coef(yfit)[2]
  f=coef(yfit)[3]
  #pull target locx and locy from x data table
  locx = x$locx
  locy = x$locy
  #apply line to locx and locy
  x$north = as.numeric((f*locx-a*f+c*d-c*locy)/(b*f - c*e))
  x$west = as.numeric((b*locy-d*b+e*a-e*locx)/(b*f - c*e))
  x$east = -x$west
  #convert northing and easting coordinates to a spatial points object
  cord.dec = st_as_sf(x, coords=c("east","north"), crs=4326)
  cord.UTM <- st_transform(cord.dec, crs=3154)
  return(cord.UTM)
}

#run the function
middens.UTM <- locs_to_utms(grid_stakes = grid_stakes, x = middens)

middens.UTM <- middens.UTM %>%
  dplyr::select(-west)

head(middens.UTM)

#plot to see if it worked
ggplot() +
  geom_sf(data = middens.UTM, color = "red", size = 1) +
  theme_void()

#create a buffer around middens to represent midden surface area
middens_buffered <- st_buffer(middens.UTM, dist = 6)

#plot to see if it worked
ggplot() +
  geom_sf(data = middens_buffered, fill = NA, color = "black") +
  theme_void()

#transform feeding locs to utm
feeding_locs <- feeding %>%
  dplyr::select(locx, locy)

feeding_locs$locx <- loc_to_numeric(feeding_locs$locx)
feeding_locs$locy <- loc_to_numeric(feeding_locs$locy)

feeding_locs <- feeding_locs %>%
  filter(!is.na(locx) & !is.na(locy))
  
feeding.UTM <- locs_to_utms(grid_stakes = grid_stakes, x = feeding_locs)

feeding.UTM <- feeding.UTM %>%
  dplyr::select(-west)

feeding.UTM <- feeding.UTM %>%
  mutate(locx = as.numeric(st_coordinates(.)[,1]),
         locy = as.numeric(st_coordinates(.)[,2]))








