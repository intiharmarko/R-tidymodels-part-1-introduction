# 3 tidyverse Crash Course

rm(list = ls())
graphics.off()

# Install packages and load packages
install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)


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
  
