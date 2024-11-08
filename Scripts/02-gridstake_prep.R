#load packages
source("Scripts/00-packages.R")

#connect to KRSP database
con_suppl <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                           dbname ="krsp_suppl",
                           username = Sys.getenv("krsp_user"),
                           password = Sys.getenv("krsp_password")
)

#pull in grid stakes
grid_stakes<-tbl(con_suppl, "grid_stakes") %>% 
  filter(!is.na(north),
         !is.na(west)) %>% 
  collect() %>%
  dplyr::select(-comments)

#convert to spatial cords
grid_stakes$north = as.numeric(grid_stakes$north)
class(grid_stakes$north)
grid_stakes$west = as.numeric(grid_stakes$west)
grid_stakes$east = -grid_stakes$west

#fix coordinate system
cord.dec = st_as_sf(grid_stakes, coords=c("east","north"), crs=4326)
grid_UTM <- st_transform(cord.dec, crs=3154) %>%
  dplyr::select(-west)


# save --------------------------------------------------------------------
fwrite(grid_stakes, "Output/grid_stakes.csv")
fwrite(grid_UTM, "Input/grid_utm.csv")














