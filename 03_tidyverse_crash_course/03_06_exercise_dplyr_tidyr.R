# Exercise - dplyr & tidyr

rm(list = ls())
graphics.off()


# Load packages
library(dplyr)
library(ggplot2)
library(hflights)
library(tidyr)

install.packages("lubridate")
install.packages("stringr")
library(lubridate)
library(stringr)


# Load data
df.mpg <- mpg
df.hf <- hflights


# Example 1
# How many rows and columns are in cars and in flights table?
df.mpg %>% nrow(); df.mpg %>% ncol()
df.hf %>% nrow(); df.hf %>% ncol()


# Example 2
# How many columns start with letter "D" in flights table?
df.hf %>% 
  select(starts_with("D")) %>%
  ncol()


# Example 3
# Filter cars table, where manufacturer is "jeep" and year is 1999
df.mpg %>% 
  filter(manufacturer == "jeep" & year == 1999) 


# Example 4
# Using cars data first select columns "manufacturer", "model" and "cty", 
# then filter cars with "cty" greater than 13, and keep only distinct values.
df.mpg %>% 
  select(manufacturer, model, cty) %>% 
  filter(cty > 13) %>% 
  distinct()


# Example 5
# Use flights data and calculate total distance flown and total flights cancelled 
# per each carrier. Arrange your output based on total distance flown in decreasing order.
df.hf %>% 
  group_by(UniqueCarrier) %>% 
  summarise(`tot distance` = sum(Distance),
            `flights cancelled` = sum(Cancelled)) %>% 
  ungroup() %>% 
  arrange(desc(`tot distance`))


# Example 6
# Use cars data, and try to answer which manufacturer has the highest number of models?  
# Is this the same manufacturer that has the highest number of different car classes? 
df.mpg %>% 
  group_by(manufacturer) %>% 
  summarise(`different models` = n_distinct(model),
            `different classes` = n_distinct(class)) %>% 
  ungroup() %>% 
  arrange(desc(`different models`))


# Example 7
# Use flights data and create a table where number of flights is calculated per each day 
# (HINT: use Year, Month, DayofMonth columns).
# - which day has the highest number of flights?
# - how many different days have at least one flight
# - compare number of flights per each month
df.hf.days <- df.hf %>% 
  group_by(Year, Month, DayofMonth) %>% 
  summarise(n = n()) %>% 
  ungroup()

df.hf.days %>% 
  arrange(desc(n)) %>% 
  head(4)

df.hf.days %>% 
  nrow()

df.hf.days %>% 
  group_by(Year, Month) %>% 
  summarise(n = sum(n)) %>% 
  ungroup()


# Example 8
# Use flights data and:
# - add week (of the year) column to the table (HINT: check library lubridate and its function week() | 
#                                                     also check library stringr and its function str_pad() 
#                                                     for adding leading zeros when creating date column)  
# - then calculate number of flights per week
# - calculate difference between number of flights between two successive weeks (HINT: dplyr's lag() or lead() functions)
# - what is the number of weeks, when number of flights was higher then previous week
df.hf.weeks <- df.hf %>% 
  # add week column
  mutate(date = paste0(Year, "-", 
                       str_pad(Month, width = 2, side = "left", pad = "0"), "-", 
                       str_pad(DayofMonth, width = 2, side = "left", pad = "0")),
         date = ymd(date),
         week = week(date)) %>% 
  # calculate number of flights per week
  group_by(week) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  # sort rows by week
  arrange(week) %>% 
  # add number of flights from previous week
  mutate(`n prev week` = lag(n),
         diff = n - `n prev week`)

df.hf.weeks %>% 
  filter(diff > 0) %>% 
  nrow()


# Example 9
# For this example you will use flights data:
# - and first calculate total number of flights per each carrier and month
# - then you will need to pivot your output table in order to get table
# - where rows represent carriers, columns represent months
# - and cells show number of flights (per carrier ~ month)
df.hf %>% 
  # create year-month column
  mutate(yearmonth = paste0(Year, "-", 
                            str_pad(Month, width = 2, side = "left", pad = "0"))) %>% 
  group_by(UniqueCarrier, yearmonth) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "yearmonth", 
              values_from = "n", 
              values_fill = 0)


# Example 10
# For this example you will use flights data:
# - first calculate percentage of cancelled flights per each carrier
# - then create a sample of flights table (randomly select 10% of rows)
#   (HINT: for row sampling you can use dplyr's sample_frac() function)
# - also calculate percentage of cancelled flights in sample table
# - merge both tables you have produced, and compared calculated percentages
#   (HINT: for merging two tables check dplyr's inner_join() function)

## function for calculating percentages of cancelled flights per carrier
cancelled_percent <- function(df){
  
  df.cancelled <- df %>% 
    group_by(UniqueCarrier) %>% 
    summarise(n = n(),
              cancelled = sum(Cancelled)) %>% 
    ungroup() %>% 
    mutate(`%` = cancelled / n * 100)
  
  return(df.cancelled)
}

## sample initial table
set.seed(12424)

df.hf.sample <- df.hf %>% 
  sample_frac(size = .1, replace = F)

## calculate percentages on both tables
df.cancelled <- cancelled_percent(df.hf)
df.cancelled.sample <- cancelled_percent(df.hf.sample)

## merge tables
df.cancelled.merged <- inner_join(x = df.cancelled,
                                  y = df.cancelled.sample %>% rename(`n sample` = n,
                                                                     `cancelled sample` =  cancelled,
                                                                     `% sample` = `%`),
                                  by = "UniqueCarrier")
