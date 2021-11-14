# To drive a yellow New York taxi, you have to hold a "medallion" from 
# the city's Taxi and Limousine Commission. Recently, one of those changed hands 
# for over one million dollars, which shows how lucrative the job can be.
# 
# But this is the age of business intelligence and analytics! Even taxi drivers 
# can stand to benefit from some careful investigation of the data, guiding them to 
# maximize their profits. In this project, we will analyze a random sample of 49999 New York
# journeys made in 2013. We will also use regression trees and random forests to build a model 
# that can predict the locations and times when the biggest fares can be earned.


# Loading the tidyverse
library(tidyverse)

# Reading in the taxi data - 14 billion rows, so will only read in the 50000 we need.
taxi_data <- read.csv('trip_data.csv', nrows = 50000)
fare_data <- read.csv('trip_fare.csv',nrows = 50000)

# join taxi and fare data
taxi <- taxi_data %>%
  left_join(fare_data)

# Taking a look at the first few rows in taxi
head(taxi)

# Renaming the location variables,
# dropping any journeys with zero fares and zero tips,
# and creating the total variable as the log sum of fare and tip
taxi <- taxi %>%
  rename(lat = pickup_latitude, long = pickup_longitude) %>%
  filter(fare_amount > 0, tip_amount > 0) %>%
  mutate(total = log(fare_amount + tip_amount))

# Reducing the data to taxi trips starting in Manhattan
# Manhattan is bounded by the rectangle with 
# latitude from 40.70 to 40.83 and 
# longitude from -74.025 to -73.93
taxi <- taxi  %>% 
  filter(lat > 40.70 & lat < 40.83, lat > -74.025 & long < -73.93)

# Loading in ggmap and viridis for nice colors
library(ggmap)
library(viridis)

# Map object
#install.packages('rstudio.api')
library(ggmap)
library(rstudioapi)
api_key = read_file('api_key.txt')
register_google(key = api_key)
manhattan <- get_map("manhattan", zoom = 12)


# Drawing a density map with the number of journey start locations
ggmap(manhattan, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') +
  geom_bin2d(data = taxi, aes(x = long, y = lat), bins = 60, alpha = 0.6)+
  labs(title = 'Map of number of trips', x = 'Longitude', y = 'Latitude') 

