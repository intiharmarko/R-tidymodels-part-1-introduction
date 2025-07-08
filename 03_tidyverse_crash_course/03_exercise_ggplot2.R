# Exercise - ggplot2

rm(list = ls())
graphics.off()


# Load packages
library(dplyr)
library(ggplot2)
library(hflights)
library(tidyr)
library(stringr)
library(lubridate)


# Load data
df.mpg <- mpg
df.hf <- hflights



# Example 1
# Use cars data set:
# - draw histogram using "cty"
# - use argument "bins" to control number of bins (choose 30 bins)
# - set bin "fill color" to "orange"
df.mpg %>% 
  ggplot(aes(x = hwy)) +
  geom_histogram(binwidth = 2,
                 color = "black",
                 fill = "orange") +
  xlab("City miles per gallon") +
  ylab("Frequency (cars)") +
  ggtitle("Distribution of city miles per gallon")



# Example 2
# Use cars data set:
# - show number of cars per car model (bar plot)
# - set plot title and axis title
# - and add "theme_minimal()"
df.mpg %>% 
  ggplot(aes(x = model)) +
  geom_bar(color = "black",
           fill = "deepskyblue3") +
  scale_y_continuous(breaks = seq(0,15)) +
  xlab("Car model") +
  ylab("Frequency (number of cars)") +
  ggtitle("Number of cars per car model") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10, 
                                   angle = 90, 
                                   hjust = 1))



# Example 3
# Use cars data set:
# - create a scatter plot
# - where "cty" is on x axis
# - and "hwy" is on y axis
# - set dot color to "red"
# - set dot size to 3
# - use classic theme ("theme_classic()")
df.mpg %>% 
  ggplot(aes(x = cty,
             y = hwy)) +
  geom_point(size = 3,
             color = "red",
             position = position_jitter()) + 
  xlab("City miles per gallon") +
  ylab("Highway miles per gallon") +
  ggtitle("Relation between cty and hwy") +
  theme_classic()



# Example 4
# Use cars data set:
# - show number of cars per car model (bar plot)
# - where you also add break down by car class (use bar position "stack" 
#   and "fill" argument should be mapped to car class)
# - add plot title, axis titles and fill label
# - choose plot theme based on your preference
df.mpg %>% 
  ggplot(aes(x = model,
             fill = class)) +
  geom_bar(color = "black",
           position = position_stack()) +
  scale_fill_viridis_d() +
  xlab("Car model") +
  ylab("Frequency (number of cars)") +
  labs(title = "Number of cars per model break down by car class",
       fill = "Car type") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10, 
                                   angle = 90))



# Example 5
# Is there any relation between distance flown and air time in flights data set?
# - pick the graph that will help you answer given question
# - set plot settings based on you preferences
# - (HINT: sample data before drawing a plot, for example you can use 10% 
# - of all rows | use sample_frac())
set.seed(2142)

df.hf %>% 
  # sample data
  sample_frac(0.1) %>% 
  ggplot(aes(x = Distance,
             y = AirTime)) +
  geom_jitter() +
  xlab("Distance flown in miles") +
  ylab("Air time in minutes") +
  ggtitle("Flight disance VS time spetn in the air") +
  theme_minimal()



# Example 6
# What is distribution of number of flights per each day of week?
# Can you see any patterns (working week VS weekend, certain days)?
# - when you count number of flights per day of week, do not omit date
# - if oyu omit date (year, month, day of month) you won't be able to draw 
# - pick the graph that will help you answer given question
# - set plot settings based on you preferences
df.hf %>% 
  # calculate number of flights per each day of week
  group_by(Year, Month, DayofMonth, DayOfWeek) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  # create plot 
  ggplot(aes(x = as.factor(DayOfWeek),
             y = n)) +
  geom_boxplot() +
  xlab("Day of week") +
  ylab("Number of flights") +
  ggtitle("Number of flights per each day of week") +
  theme_minimal()
  



# Example 7
# Does engine displacement change over the years?
# - compare distribution od engine displacement break down by year
# - pick the graph that will help you answer given question
# - set plot settings based on you preferences
df.mpg %>% 
  ggplot(aes(x = displ,
             fill = as.factor(year))) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(values = c("brown3", "deepskyblue3")) +
  xlab("Engine displacement, in litres") +
  ylab("Density") +
  labs(title = "Engine displacement break down by year",
       fill = "Year") +
  theme_minimal()



# Example 8
# Is there any obvious pattern between number of flights, number of cancelled flight,
# and number of diverted flights if we compare flight carriers?
# - try to put all three values on a single graph
# - pick the graph that will help you answer given question
# - set plot settings based on you preferences
df.hf %>% 
  # calculate values
  group_by(UniqueCarrier) %>% 
  summarise(flights = n(),
            cancelled = sum(Cancelled),
            diverted = sum(Diverted)) %>% 
  ungroup() %>% 
  # create plot
  ggplot(aes(x = cancelled,
             y = diverted,
             size = flights,
             color = UniqueCarrier)) +
  geom_point(alpha = 0.5) +
  scale_size_area(max_size = 50) +
  scale_color_viridis_d() +
  xlab("Number of cancelled flights") +
  ylab("Number of diverted flights") +
  labs(title = "Number of different flights per carrier",
       color = "Carrier",
       size = "Number of total flights") +
  theme_minimal()



# Example 9
# Do carriers have different distribution of flight's distance flown?
# - draw distribution for each carrier in its own plot
# - using facets (HINT: check facet_wrap() or facet_grid())
# - (HINT: also check argument "scales", when creating facets)
# - pick the graph that will help you answer given question
# - set plot settings based on you preferences
df.hf %>% 
  ggplot(aes(x = Distance, 
             fill = UniqueCarrier)) +
  geom_density(show.legend = F) +
  facet_wrap(. ~ UniqueCarrier, 
             scales = "free") +
  xlab("Distance flown in miles") +
  ylab("Density") +
  ggtitle("Distance flown break down by carrier") +
  theme_minimal()



# Example 10
# For the last example, you will create a heat map:
# - (HINT: check https://r-graph-gallery.com/79-levelplot-with-ggplot2.html)
# - first try to extract only hour part from flight departure time
# - (HINT: you can use integer divisor operator in R %/% to get only hour part)
# - then aggregate number of flights per month and departure time (hour part) 
# - then draw a heat map, where you put month on x-axis, departure time (hour part)
# - on y-axis and fill color is determined by number of flights
# - set other plot settings based on you preferences
# - can you see any specific patterns:
#    - are there some hours where there is no flights, a lot of flights?
#    - is pattern changing over months?
#    - etc.
df.hf %>% 
  mutate(`DepTime hour` = DepTime %/% 100) %>% 
  group_by(Month, `DepTime hour`) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = Month,
             y = `DepTime hour`,
             fill = n)) +
  geom_tile(color = "white") +
  scale_x_continuous(breaks = seq(1,12)) +
  scale_y_continuous(breaks = seq(0,24)) +
  scale_fill_viridis_c(option = "magma") +
  xlab("Month") +
  ylab("Departure time (hour part)") +
  labs(title = "Number of different flights by month and departure time",
       fill = "Number of flights") +
  theme_minimal()

