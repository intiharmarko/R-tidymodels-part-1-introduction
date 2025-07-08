# 3 tidyverse Crash Course

rm(list = ls())
graphics.off()

# Install packages and load packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("hflights") 
install.packages("tidyr")

library(dplyr)
library(ggplot2)
library(hflights)
library(tidyr)
library(stringr)
library(lubridate)


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



# 3.5 tidyr

# pivoting: convert table from long to wide format and vice versa

## Let's create a simple table in long format
table.long <- data.frame(id = 1:5,
                         type = c("A", "B", "C", "D", "E"),
                         count = seq(1,5) * 10)
table.long


# pivot_wider() - convert long data to wide data

## convert table to wide format - each "type" in its own column
table.wide <- pivot_wider(table.long, 
                          names_from = type, 
                          values_from = count,
                          #values_fill = 0
                          )
table.wide


# pivot_longer() - convert wide data to long data

## convert table back to long format
table.long1 <- pivot_longer(table.wide, 
                            cols = c("A", "B", "C"), 
                            names_to = "type", 
                            values_to = "count", 
                            values_drop_na = T)
table.long1


## Now let's pivot our car data set table

## filter only rows where manufacturer is "dodge" or "jeep"
## and select columns model, transmission type, hwy
## calculate max cty for each model and transmission type
## this will be our long format table
df.long <- df %>% 
  filter(manufacturer %in% c("dodge", "jeep")) %>% 
  select(model, trans, cty) %>% 
  group_by(model, trans) %>% 
  summarise(`max cty` = max(cty)) %>% 
  ungroup()
df.long

#   Now convert long table to wide format - where transmission type is transformed into columns
df.wide <- df.long %>% 
  pivot_wider(names_from = trans, 
              values_from = `max cty`)
df.wide

#   Convert df.wide back to long format
df.long1 <- df.wide %>% 
  pivot_longer(-model,  # exclude column "model" and use all remaining columns!!!
               names_to = "trans", 
               values_to = "max cty", 
               values_drop_na = T)
df.long1



# 3.8 ggplot2: Create Statistical Plots


# histogram

## question: What is the distribution of "highway miles per gallon" (cars data)?
df %>% 
  ggplot(aes(x = hwy)) +
  geom_histogram(binwidth = 2,
                 color = "black",
                 fill = "deepskyblue3") +
  xlab("Highway miles per gallon") +
  ylab("Frequency (cars)") +
  ggtitle("Distribution of highway miles per gallon")


# density plot

## question: What is the distribution of "city miles per gallon" (cars data)?
df %>% 
  ggplot(aes(x = cty)) +
  geom_density(color = "black",
               fill = "deepskyblue3") +
  scale_x_continuous(breaks = seq(0,40,2), limits = c(0,40)) +
  xlab("City miles per gallon") +
  ylab("Frequency (cars)") +
  ggtitle("Distribution of city miles per gallon") +
  theme_minimal()


# bar plot

## question: What is the distribution of cars per each car manufacturer (cars data)?
df %>% 
  ggplot(aes(x = manufacturer)) +
  geom_bar(color = "black",
           fill = "deepskyblue3") +
  xlab("Car manufacturer") +
  ylab("Frequency (number of cars)") +
  ggtitle("Number of cars per manufacturer") +
  theme(plot.title = element_text(size = 18),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10, 
                                   angle = 90))


## question: What is the distribution of number of flights per each flight carrier (flights data)?
df.hf %>% 
  ungroup() %>% 
  ggplot(aes(x = UniqueCarrier)) +
  geom_bar(color = "black",
           fill = "deepskyblue3") +
  scale_y_continuous(breaks = seq(0,80000,5000)) +
  xlab("Flight carrier") +
  ylab("Frequency (number of flights)") +
  ggtitle("Number of flights per flight carrier") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10))


# scatter plot (two continuous variables)

## question: What is the relation between hwy and cty (cars data)?
df %>% 
  ggplot(aes(x = hwy,
             y = cty)) +
  geom_point(size = 2,
             position = position_jitter()) + 
  xlab("Highway miles per gallon") +
  ylab("City miles per gallon") +
  ggtitle("Relation between hwy and cty") +
  theme_minimal()

## add linear regression line
df %>% 
  ggplot(aes(x = hwy,
             y = cty)) +
  geom_point(size = 2,
             position = position_jitter()) + 
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(0,50,2)) +
  scale_y_continuous(breaks = seq(0,50,2)) +
  xlab("Highway miles per gallon") +
  ylab("City miles per gallon") +
  ggtitle("Relation between hwy and cty") +
  theme_minimal()


# bar plot (position argument)

## question: What is the distribution of cars per each car manufacturer,
##           where we consider break down by type of car (cars data)?
df %>% 
  ggplot(aes(x = manufacturer,
             fill = class)) +
  geom_bar(color = "black",
           position = position_stack()) +
  scale_fill_viridis_d() +
  xlab("Car manufacturer") +
  ylab("Frequency (number of cars)") +
  labs(title = "Number of cars per manufacturer break down by car class",
       fill = "Car type") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10, 
                                   angle = 90))


## question: What is the distribution of cars per each car manufacturer,
##           where we consider break down by car drive type (cars data)?
df %>% 
  ggplot(aes(x = manufacturer,
             fill = drv)) +
  geom_bar(color = "black",
           position = position_dodge()) +
  xlab("Car manufacturer") +
  ylab("Frequency (number of cars)") +
  labs(title = "Number of cars per manufacturer break down by car drive type",
       fill = "Car drive type") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 10, 
                                   angle = 90))


# scatter plot (two discrete variables)

## question: What is the relation between car type and drive type (cars data)?
df %>% 
  ggplot(aes(x = class,
             y = drv)) +
  geom_jitter(size = 3) +
  xlab("Car type") +
  ylab("Drive type") +
  labs(title = "Relation between car type and drive type") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(size = 12))


# box plot

## question: How hwy is distributed based on year, when car was manufactured (cars data)?
df %>% 
  ggplot(aes(x = as.factor(year),
             y = hwy)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0,50,2)) +
  xlab("Year when car was manufactured") +
  ylab("Highway miles per gallon") +
  ggtitle("Relation between year and hwy") +
  theme_minimal() 

## alternative: violin plot
df %>% 
  ggplot(aes(x = as.factor(year),
             y = hwy)) +
  geom_violin(fill = "deepskyblue3") +
  scale_y_continuous(breaks = seq(0,50,2)) +
  xlab("Year when car was manufactured") +
  ylab("Highway miles per gallon") +
  ggtitle("Relation between year and hwy") +
  theme_minimal() 


# line chart

## question: What is trend of number of flights per week over time (flights data)?
df.hf %>% 
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
  # remove last week
  filter(week < 53) %>% 
  # draw line chart
  ggplot(aes(x = week,
             y = n)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(0,53,2)) +
  xlab("Week (of the year)") +
  ylab("Number of flights") +
  ggtitle("Number of flights over time") +
  theme_minimal() 

