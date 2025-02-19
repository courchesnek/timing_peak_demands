#load packages
source("scripts/00-packages.R")

# create KL grid polygon --------------------------------------------------

KL_utm <- read.csv("input/KL_utm.csv")
cord_utm <- read.csv("input/cord.UTM.csv")

# Ensure the data frame is ordered by x and y coordinates
KL_utm <- KL_utm %>% arrange(geometry)
class(KL_utm)

#Remove outliers
KL_utm <- KL_utm[!(KL_utm$stake %in% c('K3', 'L3', '-10')), ]

# Set geometry as a spatial feature for grids
grids <- KL_utm %>%
  separate(geometry, into = c("x", "y"), sep = "\\|") %>%
  st_as_sf(coords=c("x", "y"), crs=3154)

# Create the concave hull polygon
concave_polygon <- concaveman(grids)

# Set geometry as a spatial feature for midden
midden <- cord_utm %>%
  separate(geometry, into = c("x", "y"), sep = "\\|") %>%
  st_as_sf(coords=c("x", "y"), crs=3154)

# Extract the geometry column from midden
midden_points <- st_geometry(midden)

# Convert concave hull polygon and points to data frames with correct column names
grid_df <- as.data.frame(st_coordinates(concave_polygon))
midden_df <- as.data.frame(st_coordinates(midden))

# Plot the concave hull polygon
KLgrid_middens <- ggplot() +
  geom_polygon(data = grid_df, aes(x = X, y = Y), color = "black", fill = NA) +
  #geom_point(data = midden_df, aes(x = X, y = Y), color = "red", size = 1) +
  theme_void()

ggsave("output/KLgrid_middens.png", KLgrid_middens, width = 6, height = 4, units = "in")

# Midden buffers ----------------------------------------------------------

# Convert data frame back to an sf object
midden_sf <- st_as_sf(midden_df, coords = c("X", "Y"), crs = 3154)

# Create a 15m buffer around each point
buffered_sf <- st_buffer(midden_sf, dist = 15)

ggplot() +
  #geom_point(data = midden_df, aes(x = X, y = Y), color = "red", size = 1) +
  geom_polygon(data = grid_df, aes(x = X, y = Y), color = "black", fill = NA) +
  theme_void() +
  geom_sf(data = buffered_sf, fill = NA, color = "blue")

ggsave("output/KLgrid_middens_buffered.png", KLgrid_middens_buffered, width = 6, height = 4, units = "in")

# Filter for middens only within KL grid polygon

KLmiddens_buffered <- st_intersection(buffered_sf, concave_polygon)

ggplot() +
  geom_polygon(data = grid_df, aes(x = X, y = Y), color = "black", fill = NA) +
  theme_void() +
  geom_sf(data = KLmiddens_buffered, fill = NA, color = "blue")

ggsave("output/grid_middens_clipped.png", grid_middens_clipped, width = 6, height = 4, units = "in")


# Create midden raster

# Create an empty raster layer with the desired extent and resolution
empty_raster <- raster(extent(KLmiddens_buffered), crs = 3154, ncols = 3000, nrows = 3000 )

# Rasterize KLmiddens_buffered
midden_raster <- rasterize(KLmiddens_buffered, empty_raster, field = 1, crs = 3154)
plot(midden_raster)

# save --------------------------------------------------------------------

writeRaster(midden_raster, "output/midden_raster2.tif", overwrite = TRUE)
st_write(concave_polygon, "output/kloo_polygon.shp", append = FALSE)
