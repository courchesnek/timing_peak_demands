#load packages
source("scripts/00-packages.R")

#create grid polygons
grid_UTM <- read.csv("Input/grid_utm.csv")

#ensure the data frame is ordered by x and y coordinates
grid_UTM <- grid_UTM %>% arrange(geometry)
class(grid_UTM)

#remove strange entries
grid_UTM <- grid_UTM[!(grid_UTM$stake == 'K3' & grid_UTM$grid == 'KL'), ]
grid_UTM <- grid_UTM[!(grid_UTM$stake == 'L3' & grid_UTM$grid == 'KL'), ]
grid_UTM <- grid_UTM[!(grid_UTM$stake == '-10' & grid_UTM$grid == 'KL'), ]
grid_UTM <- grid_UTM[!(grid_UTM$stake == 'I17' & grid_UTM$grid == 'JO'), ]
grid_UTM <- grid_UTM[!(grid_UTM$stake == 'N11' & grid_UTM$grid == 'JO'), ]

#create separate tables for each grid
BT <- grid_UTM %>% filter(grid == "BT")
JO <- grid_UTM %>% filter(grid == "JO")
KL <- grid_UTM %>% filter(grid == "KL")
SU <- grid_UTM %>% filter(grid == "SU")

#transform into a spatial features object with separate coordinates for spatial processing
BT <- BT %>%
  separate(geometry, into = c("x", "y"), sep = "\\|") %>%
  st_as_sf(coords = c("x", "y"), crs = 3154)

JO <- JO %>%
  separate(geometry, into = c("x", "y"), sep = "\\|") %>%
  st_as_sf(coords = c("x", "y"), crs = 3154)

KL <- KL %>%
  separate(geometry, into = c("x", "y"), sep = "\\|") %>%
  st_as_sf(coords = c("x", "y"), crs = 3154)

SU <- SU %>%
  separate(geometry, into = c("x", "y"), sep = "\\|") %>%
  st_as_sf(coords = c("x", "y"), crs = 3154)

#create polygons for each grid
BT_points <- st_cast(BT$geometry, "POINT")
BT_polygon <- concaveman(st_union(BT$geometry))












