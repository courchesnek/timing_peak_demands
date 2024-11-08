#load packages
source("Scripts/00-packages.R")

#read in data
middens <- read.csv("Input/KLmidsmaster.csv")
feeding <- read.csv("Input/KLfeedingobs.csv")

# database connect --------------------------------------------------------
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


# map out middens and feeding obs ---------------------------------------
#midden locs to numeric
middens$locx <- loc_to_numeric(middens$locx)
middens$locy <- loc_to_numeric(middens$locy)

#create a buffer around midden locs to represent midden area
middens$locx_buffer <- ifelse(middens$locx < 0, middens$locx - 0.2, middens$locx + 0.2)
middens$locy_buffer <- middens$locy + 0.2

#plot buffered middens
p <- ggplot(middens, aes(x = locx_buffer, y = locy_buffer)) +
  geom_point(shape = 1, size = 3, color = "black") +
  labs(x = "locx", y = "locy")
  
#feeding data to numeric and add to plot
feeding$locx <- loc_to_numeric(feeding$locx)
feeding$locy <- loc_to_numeric(feeding$locy)

p <- p + geom_point(data = feeding, aes(x = locx, y = locy, shape = sex, color = food_type), size = 1.5)

p <- p + scale_shape_manual(values = c("M" = 1, "F" = 2)) +
         scale_color_manual(values = c("cone" = "blue", "other" = "red"))

print(p)

#what proportion of feeding obs are on middens? --------------------------
feeding$within_buffer <- logical(nrow(feeding)) #create a new column to store the result

#loop over each feeding observation
for (i in 1:nrow(feeding)) {
  #calculate the distance between the feeding observation and each buffered midden
  distances <- sqrt((abs(feeding$locx[i]) - abs(middens$locx_buffer))^2 + (feeding$locy[i] - middens$locy_buffer)^2)
  
  #debugging: Check if there are any missing values in distances
  print(paste("Row", i, ": Any missing values in distances?", any(is.na(distances))))
  
  #debugging: Print row numbers where missing values occur in distances
  if (any(is.na(distances))) {
    print(paste("Row", i, ": Missing values in distances:", which(is.na(distances))))
  }
  
  #check if any distance is within the buffer radius
  if (any(distances <= 0.1, na.rm = TRUE)) {
    feeding$within_buffer[i] <- TRUE
  }
}

#calculate the proportion of feeding observations that fall within the buffer
proportion_overlap <- sum(feeding$within_buffer, na.rm = TRUE) / nrow(feeding)

#create a new data frame to store proportions
proportions <- data.frame(food_type = character(), sex = character(), proportion_overlap = numeric())

for (ftype in unique(feeding$food_type)) {
  for (s in unique(feeding$sex)) {
    #subset feeding data for the current food type and sex
    subset_feeding <- feeding[feeding$food_type == ftype & feeding$sex == s, ]
    
    #calculate the proportion of feeding observations that overlap with middens
    overlap_count <- sum(subset_feeding$within_buffer == TRUE, na.rm = TRUE)
    total_count <- nrow(subset_feeding)
    proportion <- overlap_count / total_count
    
    #append the results to the proportions data frame
    proportions <- rbind(proportions, data.frame(food_type = ftype, sex = s, proportion_overlap = proportion))
  }
}

#print the proportions table
print(proportions)

# save --------------------------------------------------------------------
write.csv(proportions, "Output/feedingproportionsmapped.csv", row.names = FALSE)
ggsave("Output/feedingmapped.jpeg", p, width = 6, height = 4, units = "in")


# heatmap -----------------------------------------------------------------

#define criteria for each category
feeding <- feeding %>%
  mutate(
    category = case_when(
      sex == "M" & within_buffer == TRUE ~ "male on midden",
      sex == "M" & within_buffer == FALSE ~ "male off midden",
      sex == "F" & within_buffer == TRUE ~ "female on midden",
      sex == "F" & within_buffer == FALSE ~ "female off midden",
      TRUE ~ "other"
    )
  )

#aggregate the data by month and category and calculate proportions
feeding_aggregated <- feeding %>%
  group_by(month, category) %>%
  summarise(frequency = n()) %>%
  ungroup()

feeding_aggregated <- feeding_aggregated %>%
  group_by(month) %>%
  mutate(proportion = frequency / sum(frequency))

#heatmap
heatmap <- ggplot(feeding_aggregated, aes(x = month, y = category, fill = proportion)) +
  geom_tile() +
  labs(x = "Month", y = "Category", fill = "Proportion of feeding events") +
  scale_fill_gradient(low = "white", high = "red", limits = c(0, max(feeding_aggregated$proportion))) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#print the heatmap
print(heatmap)

#bar graph
feeding_aggregated$month <- as.Date(feeding_aggregated$month)

ggplot(feeding_aggregated, aes(x = month, y = proportion, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Month", y = "Proportion of Feeding Events", fill = "Category") +
  theme_minimal()

# save --------------------------------------------------------------------
write.csv(feeding_aggregated, "Output/feedinglocs.csv", row.names = FALSE)
ggsave("Output/feedingheatmap.jpeg", heatmap, width = 6, height = 4, units = "in")




