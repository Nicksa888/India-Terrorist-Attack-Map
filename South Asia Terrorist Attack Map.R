#########################
#########################
#### Clear Workspace ####
#########################
#########################

rm(list = ls()) 
# clear global environment to remove all data sets, functions and so on.

####################
####################
# Source Documents #
####################
####################

source("C:/R Portfolio/South_Asia_Terrorism_Map_Shiny/Functions.R")
source("C:/R Portfolio/South_Asia_Terrorism_Map_Shiny/South_Asia_Data_Object_Locations.R")
# South Asia ---------------------------------------------

Region_Name <- "South Asia"
SA <- Region_Prep_geo(GTD_WD_geo, Region_Name)
SA %>% 
  map(~count(data.frame(x=.x), x, sort = T))

glimpse(SA)

SA_Select <- select(SA, c(Year, Dead, Group, Target, Attack, Weapon, Country, Province, City, Latitude, Longitude))
glimpse(SA_Select)

# Assuming SA_Select is your dataset
# Filter data for India
SA_India <- filter(SA_Select, Country == "India")

# Filter out rows with missing Latitude or Longitude
SA_India <- SA_India[complete.cases(SA_India$Latitude, SA_India$Longitude), ]

# Remove Outliers in Latitude and Longitude Columns

# Creating a Matrix of Coordinates:
data_matrix <- as.matrix(SA_India[, c("Longitude", "Latitude")])
# This line extracts the 'Longitude' and 'Latitude' columns from a data frame SA_Select and converts these columns into a matrix called data_matrix. This format is required for the subsequent Mahalanobis distance calculation.

# Calculate the Mahalanobis distance for each observation
mahalanobis_dist <- mahalanobis(data_matrix, colMeans(data_matrix), cov(data_matrix))

# Set a threshold for outlier detection
mahalanobis_threshold <- qchisq(0.975, df = 2)

# Identify bivariate outliers
bivariate_outliers <- which(mahalanobis_dist > mahalanobis_threshold)

# Remove bivariate outliers from the original dataset
India_Interim <- SA_India[-bivariate_outliers, ]

# Convert latitude and longitude to spatial points
India_Final <- India_Interim %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Load India map data (assuming you have a shapefile or other format)
# You can download India shapefile from various sources
# Here, assuming 'india_map.shp' as the shapefile for India
india_map <- sf::st_read("C:/R Portfolio/South_Asia_Terrorism_Map_Shiny/gadm41_IND_shp/gadm41_IND_0.shp")

# Plot the map
ggplot() +
  geom_sf(data = india_map, fill = "lightblue") + # India map
  geom_sf(data = India_Final, aes(color = Dead)) + # Points for attacks with color by number of dead
  scale_color_gradient(low = "green", high = "red", name = "Number of Dead") + # Color scale with legend title
  labs(title = "Attacks in India", x = "Longitude", y = "Latitude") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5), # Center plot title
    legend.title = element_text(hjust = 0.5) # Center legend title
  )
