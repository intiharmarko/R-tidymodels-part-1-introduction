# Assignment - tidyverse Crash Course

rm(list =ls())
graphics.off()

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)
library(readr)


# Data 
df.ins <- readr::read_csv(file = ".../insurance.csv",
                          col_names = T)
df.bikes.d <- readr::read_csv(file = ".../day.csv",
                              col_names = T)



# Exercise 1


# Insurance table dimensions
df.ins %>% dim()

# How many different regions
df.ins %>% distinct(region)

# Distribution of males and females?
df.ins %>% 
  group_by(sex) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(`%` = n / sum(n) * 100)

# Distribution of age?
# - can you see any specific patterns regarding age?
df.ins %>% 
  ggplot(aes(x = age)) +
  geom_density(fill = "deepskyblue3") +
  scale_x_continuous(breaks = seq(0,70,5)) +
  xlab("Beneficiary age") +
  ylab("Density") +
  ggtitle("Age of primary beneficiary") +
  theme_minimal()

# Is percentage of smokers similar in each region?
df.ins %>% 
  # add flag for smoker
  mutate(flag_smoker = if_else(smoker == "yes",1,0)) %>% 
  # calculate number of people and number of smokers per region
  group_by(region) %>% 
  summarise(people = n(),
            smokers = sum(flag_smoker)) %>% 
  ungroup() %>% 
  # calculate percentages
  mutate(`% smokers` = smokers / people * 100)

# How bmi, sex and age are related?
# - is there any connection between all three
df.ins %>% 
  ggplot(aes(x = age,
             y = bmi,
             color = sex)) +
  geom_jitter() +
  geom_smooth(method = "lm", 
              se = T) +
  xlab("Age in years") +
  ylab("Body mass index (BMI)") +
  ggtitle("Relation between age, BMI and sex") + 
  theme_minimal()

# Number of children by region?
# - create histogram for each region
df.ins %>% 
  mutate(region = factor(as.factor(region), 
                         levels = c("northwest", "northeast", 
                                    "southwest", "southeast"))) %>% 
  ggplot(aes(x = children,
             fill = region)) +
  geom_histogram(binwidth = 1,
                 color = "black", 
                 show.legend = F) +
  facet_wrap(region ~ .) +
  scale_x_continuous(breaks = seq(0,10)) +
  xlab("Number of children") +
  ylab("Frequency (beneficiary count)") +
  ggtitle("Number of children break down by region") + 
  theme_minimal()

# create summaries: 
# - min / max / median / mean bmi
# - break down by smoker
# - number of people included in the group
df.ins %>% 
  group_by(smoker) %>% 
  summarise(people = n(),
            `bmi min` = min(bmi),
            `bmi med` = median(bmi),
            `bmi mean` = mean(bmi),
            `bmi max` = max(bmi)) %>% 
  ungroup()



# Exercise 2

# Does age affect medical costs?
# - are costs higher for older or younger people?
# - can you see any specific patterns between and costs?
# - what do you think this is?
df.ins %>% 
  ggplot(aes(x = age,
             y = charges)) +
  geom_jitter() +
  scale_x_continuous(breaks = seq(0,70,5)) +
  xlab("Age") +
  ylab("Medical costs") +
  ggtitle("Medical costs VS age") + 
  theme_minimal()

# Lets explore age and costs a little more:
# - try to add different variables to the plot
# - and try to figure out what is happening
df.ins %>% 
  ggplot(aes(x = age,
             y = charges,
             color = bmi,
             size = children)) +
  geom_jitter() +
  facet_wrap(smoker ~ sex) +
  scale_x_continuous(breaks = seq(0,70,5)) +
  scale_color_viridis_c(option = "magma") +
  xlab("Age") +
  ylab("Medical costs") +
  ggtitle("Medical costs related to other variables") + 
  theme_minimal()

# Medical costs VS region?
# - is similar distribution of medical costs considering region
df.ins %>% 
  ggplot(aes(x = charges,
             fill = region)) +
  geom_density(alpha = 0.3) +
  scale_x_continuous(breaks = seq(0,100000,5000)) +
  xlab("Medical costs") +
  ylab("Density") +
  ggtitle("Medical costs break down by region") + 
  theme_minimal()

# Medical costs VS number of children
df.ins %>% 
  ggplot(aes(x = as.factor(children),
             y = charges)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0,100000,5000)) +
  xlab("Number of children") +
  ylab("Medical costs") +
  ggtitle("Medical costs VS number of children") + 
  theme_minimal()



# Exercise 3

# Which date had the highest number of rented bikes?
# - which had the lowest?
df.bikes.d %>% filter(cnt == df.bikes.d %>% pull(cnt) %>% max()) %>% select(dteday:workingday, cnt)
df.bikes.d %>% filter(cnt == df.bikes.d %>% pull(cnt) %>% min()) %>% select(dteday:workingday, cnt)

# What is total number of rented bikes per each year?
df.bikes.d %>% 
  mutate(year = year(dteday)) %>% 
  group_by(year) %>% 
  summarise(cnt = sum(cnt)) %>% 
  ungroup()

# Does the number of rented bikes changes over time?
# - can you see any seasonal pattern?
df.bikes.d %>% 
  ggplot(aes(x = dteday,
             y = cnt)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%y-%m-%d") +
  xlab("Date") +
  ylab("Number of rented bikes") +
  ggtitle("Number of rented bikes over time") + 
  theme_minimal()

# If you show number of rented bikes on weekly basis, are weekly numbers similar between weeks?
# - what if you compare monthly number of rented bikes?
df.bikes.d %>% 
  mutate(week = paste0(year(dteday), "-", str_pad(week(dteday), 2, "left", "0"))) %>% 
  group_by(week) %>% 
  summarise(`week date` = min(dteday),
            cnt = sum(cnt)) %>% 
  ungroup() %>% 
  ggplot(aes(x = `week date`,
             y = cnt)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 month", 
                date_labels = "%y-%m-%d") +
  xlab("Week") +
  ylab("Number of rented bikes") +
  ggtitle("Number of rented bikes over time") + 
  theme_minimal()

df.bikes.d %>% 
  mutate(month = paste0(year(dteday), "-", str_pad(month(dteday), 2, "left", "0"))) %>% 
  group_by(month) %>% 
  summarise(`month date` = min(dteday),
            cnt = sum(cnt)) %>% 
  ungroup() %>% 
  ggplot(aes(x = `month date`,
             y = cnt)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%y-%m-%d") +
  xlab("Month") +
  ylab("Number of rented bikes") +
  ggtitle("Number of rented bikes over time") + 
  theme_minimal()

# Are number of rented bikes similar between different week days?
df.bikes.d %>% 
  mutate(wday = wday(dteday, label=TRUE),
         weekend = if_else(wday %in% c("Sat", "Sun"), T, F)) %>% 
  ggplot(aes(x = as.factor(wday),
             y = cnt,
             fill = weekend)) +
  geom_boxplot() +
  scale_fill_manual(values = c("gray60", "brown1")) +
  xlab("Day of week") +
  ylab("Number of rented bikes") +
  ggtitle("Number of rented bikes break down by day of week") + 
  theme_minimal()
  
# Is bike rental pattern different between holidays, compared to days that are not holidays?
# - first check number of holidays (do get perspective)
df.bikes.d %>% pull(holiday) %>% sum()

df.bikes.d %>% 
  ggplot(aes(x = cnt,
             fill = as.factor(holiday))) +
  geom_density(alpha = 0.3,
               color = "black") +
  xlab("Number of rented bikes") +
  ylab("Density") +
  ggtitle("Number of rented bikes break down by holiday") + 
  labs(fill = "Is holiday?") +
  theme_minimal()


# Is percentage of casual bike users similar over time, or varies a lot?
# - can you see any special pattern?
df.bikes.d %>% 
  mutate(`% casual` = casual / cnt * 100,
         wday = wday(dteday, label=TRUE),
         weekend = if_else(wday %in% c("Sat", "Sun"), T, F)) %>% 
  ggplot(aes(x = dteday,
             y = `% casual`,
             color = weekend)) +
  geom_point() +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%y-%m-%d") +
  scale_color_manual(values = c("gray60", "brown1")) +
  xlab("Date") +
  ylab("% of casual bike users") +
  ggtitle("Casual bike users over time") + 
  labs(color = "Is weekend?") +
  theme_minimal()



# Exercise 4

# How does weather affect bike rental?
df.bikes.d %>% 
  ggplot(aes(x = as.factor(weathersit),
             y = cnt)) +
  geom_boxplot() +
  xlab("Weather (code)") +
  ylab("Number of rented bikes") +
  ggtitle("Bike rental VS weather") + 
  theme_minimal()

# What is connection between temperature, humidity and wind speed with respect to bike rental?
df.bikes.d %>% 
  mutate(year = year(dteday)) %>% 
  ggplot(aes(x = windspeed,
             y = cnt,
             size = temp,
             color = hum)) +
  geom_jitter() +
  facet_grid(year ~ weathersit) +
  scale_color_viridis_c(option = "magma") +
  xlab("Wind speed (normalized)") +
  ylab("Number of rented bikes") +
  ggtitle("Bike rental VS different weather conditions") + 
  labs(color = "Humidity (normalized)",
       size = "Temperature (normlaized)") +
  theme_minimal()

# What is relation between temperature and feeling temperature?
# - relation on a daily basis over time
# - correlation between variables
df.bikes.d %>% 
  select(dteday, temp, atemp) %>% 
  rename(temperature = temp,
         `feeling temperature` = atemp) %>% 
  pivot_longer(cols = contains("temp"), 
               names_to = "type", 
               values_to = "temperature") %>% 
  ggplot(aes(x = dteday,
             y = temperature,
             color = type,
             group = type)) +
  geom_line() +
  geom_point() +
  xlab("Date") +
  ylab("Temperature (normalized value)") +
  ggtitle("How temperature and feeling temperature dance together") + 
  labs(color = "Type:") +
  theme_minimal()

df.bikes.d %>% 
  ggplot(aes(x = temp,
             y = atemp)) + 
  geom_jitter() +
  xlab("Temperature (normalized)") +
  ylab("Feeling temperature (normalized)") +
  ggtitle("Correlation between temperature and feeling temperature") + 
  theme_minimal()
 