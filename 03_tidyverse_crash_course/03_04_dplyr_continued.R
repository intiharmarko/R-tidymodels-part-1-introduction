# 3 tidyverse Crash Course

rm(list = ls())
graphics.off()

# Install packages and load packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("hflights") 

library(dplyr)
library(ggplot2)
library(hflights)


# Data
help("mpg") # help about selected data

df <- mpg # assign table to df
View(mpg) # inspect table
str(df) # check structure
class(df) # check type 
nrow(df); ncol(df) # df dimension



# 3.3 dplyr

# select() - Select columns

## select columns car: manufacturer, model, year, hwy
select(df, manufacturer, model, year, hwy) 
df.sel.cols <- select(df, manufacturer, model, year, hwy)
rm(df.sel.cols)

## select columns that begin with letter "c"
select(df, starts_with("c"))

## select columns that contain letter "t"
select(df, contains("t"))

## select columns that end with letter "l"
select(df, ends_with("l"))

## select column and rename it
select(df,  `car company` = manufacturer) # only given column selected
rename(df,  `car company` = manufacturer) # just rename column!


# mutate() - Create new variables

## create variable: "car" & "cyl / trans"
df <- mutate(df,
             car = paste(manufacturer, model, sep = " "),
             `cyl / trans` = paste(cyl, " cylinders", " / ", trans, " transmission", sep = ""))

## create variable: average highway and city miles per gallon
df <- mutate(df,
             `avg miles per gallon` = (cty + hwy) / 2)


# filter() - Filter rows by condition

## filter rows where manufacturer is "dodge"
filter(df, manufacturer == "dodge") 

## filter rows where manufacturer is "dodge" and year is 2008
filter(df, manufacturer == "dodge" & year == 2008) 

## filter rows where manufacturer is either "dodge" or "ford"
df1 <- filter(df, manufacturer == "dodge" | manufacturer == "ford")
df2 <- filter(df, manufacturer %in% c("dodge", "ford")) 

## filter rows where cyl is greater than 6
filter(df, cyl > 6)

## filter rows where manufacturer is not "dodge"
filter(df, manufacturer != "dodge")


# arrange() - Sort rows

## sort rows by year (ascending order)
arrange(df, year)

## sort rows by year (descending order)
arrange(df, desc(year))

## sort rows by year (ascending order), model and hwy
df.sort <- arrange(df, year, model, hwy)


# distinct() - Get only unique (distinct) values

## lets create a table with duplicates
df.dupl <- select(df, manufacturer, model, year)

## remove duplicates
df.nodupl <- distinct(df.dupl)


# pipe operator %>%
# - chain dplyr functions using pipe operator
# - each verb executed in given pipeline

## select columns, filter rows and arrange rows in single take
df %>% 
  select(model, year, hwy, cty) %>% 
  filter(year == 1999) %>% 
  arrange(model, desc(hwy))

## count number of filtered rows
df %>% 
  filter(manufacturer == "dodge" & year == 2008) %>% 
  count()

## select filter rows, columns and keep only distinct values
df %>% 
  filter(hwy > 20) %>% 
  distinct(model, year, hwy)



# 3.4 dplyr continued

# summarise() - apply summary functions on table and create summaries

## calculate average cty
df %>% 
  summarise(`mean cty` = mean(cty))

## count table rows, and count distinct car manufacturers
df %>% 
  summarise(rows = n(),
            `nr car companies` = n_distinct(manufacturer))

## calculate min / max hwy & cty
df %>% 
  summarise(`min hwy` = min(hwy),
            `min cty` = min(cty),
            `max hwy` = max(hwy),
            `max cty` = max(cty))


# group_by() - group cases using one or more grouping variables

## group cars by model
df %>% 
  group_by(model)

## group cars by manufacturer & model
df %>% 
  group_by(manufacturer, model)


# combine summarise() & group_by() - summary statistics for grouped data

## count number of cars for each manufacturer
df %>% 
  group_by(manufacturer) %>% 
  summarise(cars = n()) %>% 
  ungroup()

## calculate mean / min / max hwy for each model
df %>% 
  group_by(model) %>% 
  summarise(`mean hwy` = mean(hwy),
            `min hwy` = min(hwy),
            `max hwy` = max(hwy)) %>% 
  ungroup()


# count() - count rows for grouped variables

## count number of table rows
count(df)

## count number of cars per manufacturer
df %>% 
  group_by(manufacturer) %>% 
  count() %>% 
  ungroup()


# Explore hflights dataset

help("hflights") # help about selected data
df.hf <- hflights
View(df.hf) 
str(df.hf) 
dim(df.hf)

## count number of rows (flights) per each carrier
df.hf %>% 
  group_by(UniqueCarrier) %>% 
  count() %>% 
  ungroup()

## how many cancelled flights?
df.hf %>% 
  summarise(`all flights` = n(),
            `cancelled flights` = sum(Cancelled))

## show different origin-destination pairs for each carrier
## calculate number of flights and total distance flown
df.hf %>% 
  group_by(UniqueCarrier, Origin, Dest) %>% 
  summarise(flights = n(),
            `to distance` = sum(Distance)) %>% 
  ungroup()

