# Assignment - Regression Analysis with tidymodels

rm(list =ls())
graphics.off()

library(tidyverse)
library(tidymodels)


# Data 
?mtcars
df <- mtcars


# Exercise 1

# Distribution of car's fuel consumption
df %>% 
  ggplot(aes(x = mpg)) +
  geom_density(alpha = 0.3,
               fill = "gray60") +
  scale_x_continuous(breaks = seq(0,50,5)) +
  xlab("Fuel consumption (miles per gallon)") +
  ylab("Density") +
  ggtitle("Car's fuel consumption") + 
  theme_minimal()

# Car's fuel consumption break down by car model
df %>% 
  mutate(model = factor(rownames(df))) %>% 
  ggplot(aes(x = mpg,
             y = fct_reorder(model, mpg))) +
  geom_col(fill = "gray60",
           color = "black",
           width = 0.5) +
  scale_x_continuous(breaks = seq(0,50,5)) +
  xlab("Fuel consumption (miles per gallon)") +
  ylab("Model") +
  ggtitle("Car's fuel consumption") + 
  theme_minimal()

# How is fuel consumption related to car's weight?
df %>% 
  ggplot(aes(x = wt,
             y = mpg)) +
  geom_point(size = 3.5) +
  scale_x_continuous(breaks = seq(0,10,0.5)) +
  scale_y_continuous(breaks = seq(0,50,5)) +
  xlab("Weight (in 1000 lbs)") +
  ylab("Fuel consumption (miles per gallon)") +
  ggtitle("Car's fuel consumption and weight") + 
  theme_minimal()

# How is fuel consumption related to car's horse power?
df %>% 
  ggplot(aes(x = hp,
             y = mpg)) +
  geom_point(size = 3.5) +
  scale_x_continuous(breaks = seq(0,400,50)) +
  scale_y_continuous(breaks = seq(0,50,5)) +
  xlab("Horse power") +
  ylab("Fuel consumption (miles per gallon)") +
  ggtitle("Car's fuel consumption and horse power") + 
  theme_minimal()

# How is fuel consumption related to number of cylinders?
df %>% 
  ggplot(aes(x = as.factor(cyl),
             y = mpg)) +
  geom_boxplot(fill = "gray60") +
  scale_y_continuous(breaks = seq(0,50,5)) +
  xlab("Number of cylinders") +
  ylab("Fuel consumption (miles per gallon)") +
  ggtitle("Car's fuel consumption and cylinders") + 
  theme_minimal()

# How is fuel consumption related to transmission type?
df %>% 
  ggplot(aes(x = factor(am, labels = c("automatic", "manual")),
             y = mpg)) +
  geom_boxplot(fill = "gray60") +
  scale_y_continuous(breaks = seq(0,50,5)) +
  xlab("Transmission type") +
  ylab("Fuel consumption (miles per gallon)") +
  ggtitle("Car's fuel consumption and tramsmission type") + 
  theme_minimal()



# Exercise 2

# How is fuel consumption related to:
# - car's weight
# - horse power
df %>% 
  ggplot(aes(x = wt,
             y = mpg,
             color = hp)) +
  geom_point(size = 3.5) +
  scale_x_continuous(breaks = seq(0,10,0.5)) +
  scale_y_continuous(breaks = seq(0,50,5)) +
  scale_color_viridis_c(option = "magma") +
  xlab("Weight (in 1000 lbs)") +
  ylab("Fuel consumption (miles per gallon)") +
  ggtitle("Car's fuel consumption break down by weight and horse power") + 
  labs(color = "Horse power:") +
  theme_minimal()

# How is fuel consumption related to:
# - car's weight
# - horse power
# - transmission type
# - cylinders
df %>%
  ggplot(aes(x = wt,
             y = mpg,
             color = hp,
             shape = factor(am, labels = c("automatic", "manual")))) +
  geom_point(size = 5) +
  facet_wrap(vars(cyl)) +
  scale_x_continuous(breaks = seq(0,10,0.5)) +
  scale_y_continuous(breaks = seq(0,50,5)) +
  scale_color_viridis_c(option = "magma") +
  xlab("Weight (in 1000 lbs)") +
  ylab("Fuel consumption (miles per gallon)") +
  ggtitle("Car's fuel consumption break down by weight, horse power, cylinders and transmission type") + 
  labs(color = "Horse power:",
       shape = "Transmission type:") +
  theme_minimal() +
  theme(strip.text = element_text(size = 14))

# pair lots
df %>% 
  mutate(across(.cols = c(cyl, vs, am), 
                .fns = as.factor)) %>% 
GGally::ggpairs(.)



# Exercise 3

# prepare selected factor variables
df <- df %>% 
  mutate(cyl = factor(cyl),
         am = factor(am, labels = c("automatic", "manual")))

# split data
set.seed(1123)

ins_split <- initial_split(df, prop = 0.7)
df.train <- training(ins_split)
df.test  <- testing(ins_split)


# model 1: simple linear regression 
# - using only weight as predictor

# define recipe
rec <- recipe(mpg ~ wt, df.train)

# define model specification
mod <- linear_reg() %>% 
  set_engine("lm") %>%
  set_mode("regression")

# create a workflow (combine recipe and model)
wflow <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(mod)

# fit (train) model (on train set)
mod_fit <- wflow %>% 
  fit(df.train)

# make predictions on test data & calculate RMSE
test_rmse <- predict(mod_fit, df.test) %>% 
  bind_cols(df.test) %>% 
  metrics(truth = mpg, estimate = .pred) %>% 
  filter(.metric == "rmse")

test_rmse



# Exercise 4

# model 2: multiple linear regression
# - using multiple variables predictors
# - weight, horse power, cylinders and transmission type

# define recipe
rec_m <- recipe(mpg ~ wt + hp + cyl + am, 
                df.train) %>%
  step_dummy(all_nominal_predictors())

# define model specification
mod_m <- linear_reg() %>% 
  set_engine("lm") %>%
  set_mode("regression")

# create a workflow (combine recipe and model)
wflow_m <- workflow() %>% 
  add_recipe(rec_m) %>% 
  add_model(mod_m)

# fit (train) model (on train set)
mod_fit_m <- wflow_m %>% 
  fit(df.train)

# make predictions on test data & calculate RMSE
test_rmse_m <- predict(mod_fit_m, df.test) %>% 
  bind_cols(df.test) %>% 
  metrics(truth = mpg, estimate = .pred) %>% 
  filter(.metric == "rmse")

test_rmse_m
