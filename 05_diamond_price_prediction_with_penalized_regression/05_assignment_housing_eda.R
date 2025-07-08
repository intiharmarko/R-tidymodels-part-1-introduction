# Assignment - California Housing Prices EDA

rm(list = ls())
graphics.off()

# Load packages
library(tidyverse)

# Data

## California housing prices
df <- readr::read_csv(file = "./data/housing.csv",
                      col_names = T)


# EDA

## data set size
df %>% ncol() # number of variables
df %>% nrow() # number of units (diamonds) - measurements

## variable types
map_df(.x = df, 
       .f = class)


## Exploratory Data Analysis (EDA) - Distribution of each variable

### median house value
df %>% 
  ggplot(aes(x = median_house_value)) +
  geom_density(fill = "gray80",
               color = "black",
               alpha = 0.6) +
  xlab("Median house price in USD") +
  ylab("Density") +
  ggtitle("House prices") + 
  theme_minimal()

ggsave(filename = "dist_house_prices.png", plot = last_plot(), device = "png", 
       units = "cm", width = 22, height = 15, dpi = 600)
        
### median household income
df %>% 
  ggplot(aes(x = median_income)) +
  geom_density(fill = "gray80",
               color = "black",
               alpha = 0.6) +
  scale_x_continuous(breaks = seq(0,15,1)) +
  xlab("Median household income in 10k USD") +
  ylab("Density") +
  ggtitle("Household income") + 
  theme_minimal()

ggsave(filename = "dist_household_income.png", plot = last_plot(), device = "png", 
       units = "cm", width = 22, height = 15, dpi = 600)

### median house age
df %>% 
  ggplot(aes(x = housing_median_age)) +
  geom_density(fill = "gray80",
               color = "black",
               alpha = 0.6) +
  xlab("Median house age in years") +
  ylab("Density") +
  scale_x_continuous(breaks = seq(0,100,5)) +
  ggtitle("House age") + 
  theme_minimal()

ggsave(filename = "dist_house_age.png", plot = last_plot(), device = "png", 
       units = "cm", width = 22, height = 15, dpi = 600)

### total rooms
df %>% 
  ggplot(aes(x = total_rooms)) +
  geom_histogram(binwidth = 100,
                 fill = "gray80",
                 color = "black",
                 alpha = 0.6) +
  xlab("Total number of rooms (per district)") +
  ylab("Frequency") +
  scale_x_continuous(breaks = seq(0,40000,2500)) +
  ggtitle("Number of rooms") + 
  theme_minimal()

ggsave(filename = "dist_rooms.png", plot = last_plot(), device = "png", 
       units = "cm", width = 22, height = 15, dpi = 600)

### total bedrooms
df %>% 
  ggplot(aes(x = total_bedrooms)) +
  geom_histogram(binwidth = 50,
                 fill = "gray80",
                 color = "black",
                 alpha = 0.6) +
  xlab("Total number of bedrooms (per district)") +
  ylab("Frequency") +
  scale_x_continuous(breaks = seq(0,10000,500)) +
  ggtitle("Number of bedrooms") + 
  theme_minimal()

ggsave(filename = "dist_bedrooms.png", plot = last_plot(), device = "png", 
       units = "cm", width = 22, height = 15, dpi = 600)

### total population
df %>% 
  ggplot(aes(x = population)) +
  geom_histogram(binwidth = 100,
                 fill = "gray80",
                 color = "black",
                 alpha = 0.6) +
  xlab("Total number of people (per district)") +
  ylab("Frequency") +
  scale_x_continuous(breaks = seq(0,40000,2500)) +
  ggtitle("Number of people") + 
  theme_minimal()

ggsave(filename = "dist_pop.png", plot = last_plot(), device = "png", 
       units = "cm", width = 22, height = 15, dpi = 600)

### total number of households
df %>% 
  ggplot(aes(x = households)) +
  geom_histogram(binwidth = 50,
                 fill = "gray80",
                 color = "black",
                 alpha = 0.6) +
  xlab("Total number of households (per district)") +
  ylab("Frequency") +
  scale_x_continuous(breaks = seq(0,10000,500)) +
  ggtitle("Number of households") + 
  theme_minimal()

ggsave(filename = "dist_households.png", plot = last_plot(), device = "png", 
       units = "cm", width = 22, height = 15, dpi = 600)

### latitude & longitude
df %>% 
  ggplot(aes(x = longitude,
             y = latitude)) +
  geom_point(alpha = 0.25,
             color = "brown3") +
  xlab("Longitude (district)") +
  ylab("Latitude (district)") +
  ggtitle("District's longitude & latitude") + 
  theme_minimal()

ggsave(filename = "dist_long_lat.png", plot = last_plot(), device = "png", 
       units = "cm", width = 17, height = 25, dpi = 600)

### ocean proximity
df %>% 
  count(ocean_proximity) %>% 
  ggplot(aes(x = ocean_proximity,
             y = n)) +
  geom_col(fill = "gray80",
           color = "black",
           alpha = 0.6) +
  scale_y_continuous(breaks = seq(0,10000,1000)) +
  xlab("Ocean proximity") +
  ylab("Frequency") +
  ggtitle("Ocean proximity") + 
  theme_minimal()

ggsave(filename = "dist_ocean_prox.png", plot = last_plot(), device = "png", 
       units = "cm", width = 22, height = 15, dpi = 600)


## Exploratory Data Analysis (EDA) - single variable VS median house value

### median house value ~ median household income
df %>% 
  ggplot(aes(x = median_income,
             y = median_house_value)) +
  geom_jitter(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0,15,1)) +
  xlab("Median household income in 10k USD") +
  ylab("Median house price in USD") +
  ggtitle("Household income VS house price") + 
  theme_minimal()

ggsave(filename = "price_income.png", plot = last_plot(), device = "png", 
       units = "cm", width = 22, height = 15, dpi = 600)

### median house value ~ house age
df %>% 
  ggplot(aes(x = housing_median_age,
             y = median_house_value)) +
  geom_jitter(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0,100,5)) +
  xlab("Median house age in years") +
  ylab("Median house price in USD") +
  ggtitle("Household age VS house price") + 
  theme_minimal()

ggsave(filename = "price_house_age.png", plot = last_plot(), device = "png", 
       units = "cm", width = 22, height = 15, dpi = 600)

### median house value ~ total rooms
df %>% 
  ggplot(aes(x = total_rooms,
             y = median_house_value)) +
  geom_jitter(alpha = 0.2) +
  scale_x_log10() +
  xlab("Total number of rooms (per district) - log10 scale") +
  ylab("Median house price in USD") +
  ggtitle("Number of rooms VS house price") + 
  theme_minimal()

ggsave(filename = "price_rooms.png", plot = last_plot(), device = "png", 
       units = "cm", width = 22, height = 15, dpi = 600)

### median house value ~ total bedrooms
df %>% 
  ggplot(aes(x = total_bedrooms,
             y = median_house_value)) +
  geom_jitter(alpha = 0.2) +
  scale_x_log10() +
  xlab("Total number of bedrooms (per district) - log10 scale") +
  ylab("Median house price in USD") +
  ggtitle("Number of bedrooms VS house price") + 
  theme_minimal()

ggsave(filename = "price_bedrooms.png", plot = last_plot(), device = "png", 
       units = "cm", width = 22, height = 15, dpi = 600)

### median house value ~ population
df %>% 
  ggplot(aes(x = population,
             y = median_house_value)) +
  geom_jitter(alpha = 0.2) +
  scale_x_log10() +
  xlab("Total number of people (per district) - log10 scale") +
  ylab("Median house price in USD") +
  ggtitle("Number of people VS house price") + 
  theme_minimal()

ggsave(filename = "price_pop.png", plot = last_plot(), device = "png", 
       units = "cm", width = 22, height = 15, dpi = 600)

### median house value ~ latitude & longitude
df %>% 
  ggplot(aes(x = longitude,
             y = latitude,
             color = median_house_value)) +
  geom_point() +
  scale_color_viridis_c(option = "inferno") +
  xlab("Longitude (district)") +
  ylab("Latitude (district)") +
  labs(title = "District's location VS house price",
       color = "Med. house price (USD)") +
  theme_minimal()

ggsave(filename = "price_loc.png", plot = last_plot(), device = "png", 
       units = "cm", width = 17 + 5, height = 25, dpi = 600)

### median house value ~ ocean proximity
df %>% 
  ggplot(aes(x = ocean_proximity,
             y = median_house_value,
             fill = ocean_proximity)) +
  geom_boxplot(show.legend = FALSE) +
  xlab("Ocean proximity") +
  ylab("Median house price in USD") +
  ggtitle("Ocean proximity VS house price") + 
  theme_minimal()

ggsave(filename = "price_ocean_prox.png", plot = last_plot(), device = "png", 
       units = "cm", width = 22, height = 15, dpi = 600)
