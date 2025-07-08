# 4 Regression Analysis with tidymodels

rm(list = ls())
graphics.off()

# Load packages
library(tidyverse)

#install.packages("tidymodels")
library(tidymodels)

# Data

## males' heights and weights (sample data)
df.hw.s <- readr::read_csv(file = "height_weight_sample.csv", #file = ".../height_weight_sample.csv",
                           col_names = T)



# 4.2 Linear regression model

## Visualize data using scatter plot
df.hw.s %>% 
  ggplot(aes(x = height,
             y = weight)) +
  geom_point(size = 7, 
             color = "gray15") +
  scale_x_continuous(breaks = seq(100,250,5), 
                     limits = c(155, 195)) +
  scale_y_continuous(breaks = seq(50,150,5),
                     limits = c(70, 95)) +
  xlab("Height (cm) ~ x") +
  ylab("Weight (kg) ~ y") +
  ggtitle("Males - heights & weights (sample data)") +
  theme_bw() +
  theme(text = element_text(size = 18))


## fit linear (regression) model
mod <- lm(formula = weight ~ height, data = df.hw.s) # fit model

print(mod) # show model's estimated parameters


## predict male weights with the model (assign predicted values to df)
df.hw.s <- df.hw.s %>% 
  mutate(weight_pred = predict(mod))


## visualize the fitted line (predictions)
df.hw.s %>% 
  ggplot(aes(x = height,
             y = weight)) +
  geom_point(size = 7, 
             color = "gray15") +
  geom_line(aes(y = weight_pred), 
             linewidth = 1.5, 
             color = "brown1") +
  scale_x_continuous(breaks = seq(100,250,5), 
                     limits = c(155, 195)) +
  scale_y_continuous(breaks = seq(50,150,5),
                     limits = c(70, 95)) +
  xlab("Height (cm) ~ x") +
  ylab("Weight (kg) ~ y") +
  labs(title = "Males - heights & weights (sample data)",
       subtitle = "Fited regression line (red) - model's predictions") + 
  theme_bw() +
  theme(text = element_text(size = 18),
        plot.subtitle = element_text(size = 14))


## check model summary
summary(mod)



# 4.7 Linear regression with tidymodels

# data 1: males' heights and weights (sample data: n = 1000)

## import data
df.hw <- readr::read_csv(file = "height_weight_sample_1k.csv",
                         col_names = T)

## split data
set.seed(1123)

hw_split <- initial_split(df.hw, prop = 0.8) # save split information

df.hw.train <- training(hw_split) # train set
df.hw.test  <- testing(hw_split)  # test set

## define recipe
hw_recipe <- recipe(formula = weight ~ height, data = df.hw.train)

hw_recipe

## define model specification
lm_model <- linear_reg() %>% 
  set_engine("lm") %>%
  set_mode("regression")

lm_model
lm_model %>% translate() # details on how parsnip converts user's code to package syntax

## create a workflow (combine recipe and model)
hw_wflow <- workflow() %>% 
  add_recipe(hw_recipe) %>% 
  add_model(lm_model)

hw_wflow

## fit (train) model (on train set)
model_fit <- hw_wflow %>% 
  fit(data = df.hw.train)

model_fit

## make predictions on train & test data
df.hw.train <- predict(model_fit, df.hw.train) %>% 
  bind_cols(df.hw.train) %>% 
  select(height, weight, weight_pred = .pred)

df.hw.test <- predict(model_fit, df.hw.test) %>% 
  bind_cols(df.hw.test) %>% 
  select(height, weight, weight_pred = .pred)

## evaluate model performance (RMSE)
hw_train_rmse <- df.hw.train %>% 
  metrics(truth = weight, estimate = weight_pred) %>% 
  filter(.metric == "rmse")

hw_test_rmse <- df.hw.test %>% 
  metrics(truth = weight, estimate = weight_pred) %>% 
  filter(.metric == "rmse")

## visualize RMSE
hw_train_rmse <- hw_train_rmse %>% 
  mutate(set = "train")
hw_test_rmse <- hw_test_rmse %>% 
  mutate(set = "test")

bind_rows(hw_train_rmse, hw_test_rmse) %>% 
  mutate(set = factor(set, 
                      levels = c("train", "test"))) %>% 
  ggplot(aes(x = set,
             y = .estimate,
             fill = set)) +
  geom_col(color = "black",
           width = 0.25,
           show.legend = F) +
  scale_y_continuous(breaks = seq(0,3,0.25)) +
  scale_fill_manual(values = c("deepskyblue2", "chocolate1")) +
  xlab("Data set") +
  ylab("RMSE") +
  labs(title = "Model (Males - heights & weights) performance evaluation") + 
  theme_bw() +
  theme(text = element_text(size = 18),
        plot.subtitle = element_text(size = 14))



# data 2: Insurance data

## import data
df.ins <- readr::read_csv(file = "insurance.csv",
                          col_names = T)

## Quick EDA 
## - we explore medical costs - model's response / target variable
## - its relation with model's predictor variables
## - create each combination of pair plots one predictor variable VS target variable
## - and pair plot 

## Medical costs VS region?
## - does client's region affect medical costs?
df.ins %>% 
  ggplot(aes(x = charges,
             fill = region)) +
  geom_density(alpha = 0.3) +
  scale_x_continuous(breaks = seq(0,100000,5000)) +
  xlab("Medical costs") +
  ylab("Density") +
  ggtitle("Medical costs break down by region") + 
  theme_minimal()

## Medical costs VS smoking?
## - does smoking affect medical costs?
df.ins %>% 
  ggplot(aes(x = charges,
             fill = smoker)) +
  geom_density(alpha = 0.3) +
  scale_x_continuous(breaks = seq(0,100000,5000)) +
  scale_fill_manual(values = c("gray", "brown1")) +
  xlab("Medical costs") +
  ylab("Density") +
  ggtitle("Medical costs break down by smoking") + 
  theme_minimal()

## Medical costs VS number of children
## - does number of children affect medical costs?
df.ins %>% 
  ggplot(aes(x = as.factor(children),
             y = charges)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0,100000,5000)) +
  xlab("Number of children") +
  ylab("Medical costs") +
  ggtitle("Medical costs VS number of children") + 
  theme_minimal()

## Medical costs VS BMI
## - does BMI affect medical costs?
df.ins %>% 
  ggplot(aes(x = bmi,
             y = charges)) +
  geom_jitter() +
  scale_x_continuous(breaks = seq(0,60,5)) +
  xlab("BMI") +
  ylab("Medical costs") +
  ggtitle("Medical costs VS BMI") + 
  theme_minimal()

## add smoker / non smoker info
df.ins %>% 
  ggplot(aes(x = bmi,
             y = charges,
             color = smoker)) +
  geom_jitter() +
  scale_x_continuous(breaks = seq(0,60,5)) +
  scale_color_manual(values = c("gray", "brown1")) +
  xlab("BMI") +
  ylab("Medical costs") +
  ggtitle("Medical costs VS BMI break down by smoker / non-smoker") + 
  theme_minimal()

## Medical costs VS gender?
## - does sex affect medical costs?
df.ins %>% 
  ggplot(aes(x = charges,
             fill = sex)) +
  geom_density(alpha = 0.3) +
  scale_x_continuous(breaks = seq(0,100000,5000)) +
  xlab("Medical costs") +
  ylab("Density") +
  ggtitle("Medical costs break down by gender") + 
  theme_minimal()

## Medical costs VS age
## - does age affect medical costs?
df.ins %>% 
  ggplot(aes(x = age,
             y = charges)) +
  geom_jitter() +
  scale_x_continuous(breaks = seq(0,70,5)) +
  xlab("Age") +
  ylab("Medical costs") +
  ggtitle("Medical costs VS age") + 
  theme_minimal()

## add smoker / non smoker info
df.ins %>% 
  ggplot(aes(x = age,
             y = charges,
             color = smoker)) +
  geom_jitter() +
  scale_x_continuous(breaks = seq(0,70,5)) +
  scale_color_manual(values = c("gray", "brown1")) +
  xlab("Age") +
  ylab("Medical costs") +
  ggtitle("Medical costs VS age break down by smoker / non-smoker") + 
  theme_minimal()


## pair plots

## install and load additional libraries
#install.packages("GGally")
#install.packages("scatterPlotMatrix")
library(GGally)
library(scatterPlotMatrix)

## create plots
ggpairs(df.ins) # all variables shown
scatterPlotMatrix(df.ins, zAxisDim = "charges") # only continuous variables shown


## Build models

# modeling plan:
#
# - model 1:
#   - all variables
#   - recipes step (binary and other categorical variables)
#   - inspect variable importance from regression model
#
# - model 2:
#   - only smoking
#   - recipes step (binary and other categorical variables)
#
# - model 3:
#   - smoking + age + BMI
#   - recipes step (binary and other categorical variables)
#
# - first split data
# - create workflow for each model
# - fit model
# - predict on train and on test
# - estimate RMSE
# - compare results (all 3 models)

## split data
set.seed(1123)

ins_split <- initial_split(df.ins, prop = 0.8) # save split information

df.ins.train <- training(ins_split) # train set
df.ins.test  <- testing(ins_split)  # test set

## define model specification (the same algorithm used in all models)
ins_lm_model <- linear_reg() %>% 
  set_engine("lm") %>%
  set_mode("regression")


## Model 1: predictors - all variables

### define recipe
ins_rec_m1 <- recipe(formula = charges ~ ., 
                     data = df.ins.train) %>% 
  step_dummy(all_nominal_predictors())

### create a workflow
ins_wflow_m1 <- workflow() %>% 
  add_recipe(ins_rec_m1) %>% 
  add_model(ins_lm_model)

### fit (train) model (on train set)
ins_m1_fit <- ins_wflow_m1 %>% 
  fit(data = df.ins.train)

### make predictions on train & test data
df.ins.train_m1 <- predict(ins_m1_fit, df.ins.train) %>% 
  bind_cols(df.ins.train) %>% 
  rename(charges_pred = .pred) %>% 
  relocate(charges_pred, .after = last_col())

df.ins.test_m1 <- predict(ins_m1_fit, df.ins.test) %>% 
  bind_cols(df.ins.test) %>% 
  rename(charges_pred = .pred) %>% 
  relocate(charges_pred, .after = last_col())

### evaluate model performance (RMSE)
ins_train_rmse_m1 <- df.ins.train_m1 %>% 
  metrics(truth = charges, 
          estimate = charges_pred) %>% 
  filter(.metric == "rmse")

ins_test_rmse_m1 <- df.ins.test_m1 %>% 
  metrics(truth = charges, 
          estimate = charges_pred) %>% 
  filter(.metric == "rmse")

ins_train_rmse_m1
ins_test_rmse_m1

### model summary
ins_m1_fit_ <- extract_fit_parsnip(ins_m1_fit) # extract fitted object
summary(ins_m1_fit_$fit) # show summary
tidy(ins_m1_fit_$fit) # tidy model summary (coefficients) with broom


## Model 2: predictors - smoker

### define recipe
ins_rec_m2 <- recipe(formula = charges ~ smoker, 
                     data = df.ins.train) %>% 
  step_dummy(all_nominal_predictors())

### create a workflow
ins_wflow_m2 <- workflow() %>% 
  add_recipe(ins_rec_m2) %>% 
  add_model(ins_lm_model)

### fit (train) model (on train set)
ins_m2_fit <- ins_wflow_m2 %>% 
  fit(data = df.ins.train)

### make predictions on train & test data
df.ins.train_m2 <- predict(ins_m2_fit, df.ins.train) %>% 
  bind_cols(df.ins.train) %>% 
  rename(charges_pred = .pred) %>% 
  relocate(charges_pred, .after = last_col())

df.ins.test_m2 <- predict(ins_m2_fit, df.ins.test) %>% 
  bind_cols(df.ins.test) %>% 
  rename(charges_pred = .pred) %>% 
  relocate(charges_pred, .after = last_col())

### evaluate model performance (RMSE)
ins_train_rmse_m2 <- df.ins.train_m2 %>% 
  metrics(truth = charges, estimate = charges_pred) %>% 
  filter(.metric == "rmse")

ins_test_rmse_m2 <- df.ins.test_m2 %>% 
  metrics(truth = charges, estimate = charges_pred) %>% 
  filter(.metric == "rmse")

ins_train_rmse_m2
ins_test_rmse_m2


## Model 3: predictors - age, smoker, BMI

### define recipe
ins_rec_m3 <- recipe(formula = charges ~ age + smoker + bmi, 
                     data = df.ins.train) %>% 
  step_dummy(all_nominal_predictors())

### create a workflow
ins_wflow_m3 <- workflow() %>% 
  add_recipe(ins_rec_m3) %>% 
  add_model(ins_lm_model)

### fit (train) model (on train set)
ins_m3_fit <- ins_wflow_m3 %>% 
  fit(data = df.ins.train)

### make predictions on train & test data
df.ins.train_m3 <- predict(ins_m3_fit, df.ins.train) %>% 
  bind_cols(df.ins.train) %>% 
  rename(charges_pred = .pred) %>% 
  relocate(charges_pred, .after = last_col())

df.ins.test_m3 <- predict(ins_m3_fit, df.ins.test) %>% 
  bind_cols(df.ins.test) %>% 
  rename(charges_pred = .pred) %>% 
  relocate(charges_pred, .after = last_col())

### evaluate model performance (RMSE)
ins_train_rmse_m3 <- df.ins.train_m3 %>% 
  metrics(truth = charges, estimate = charges_pred) %>% 
  filter(.metric == "rmse")

ins_test_rmse_m3 <- df.ins.test_m3 %>% 
  metrics(truth = charges, estimate = charges_pred) %>% 
  filter(.metric == "rmse")

ins_train_rmse_m3
ins_test_rmse_m3


## compare all 3 models' RMSE
## - we need first to merge all results into single data frame
## - we will use two approaches to collect results
##   - for loop
##   - map_dfr()
## - show results (RMSE) on a plot 

## option 1: for loop - collect RMSE

### list all data frame names that match the pattern "ins_{set}_rmse_m{model}"
df_names <- ls(pattern = "ins_(train|test)_rmse_m(1|2|3)")

### initialize an empty data frame to store the combined RMSE results
df_rmse <- tibble()

### loop over each data frame name, process it, and combine them
for(df_name in df_names){
  
  # get the actual data frame using get()
  df <- get(df_name)
  
  # extract the set and model numbers from the name
  set_value <- ifelse(grepl("train", df_name), "train", "test")
  model_value <- stringr::str_extract(df_name, "m[1-3]") %>% stringr::str_remove("m")
  
  # add set and model columns to the data frame
  df <- df %>%
    mutate(set = set_value, 
           model = as.integer(model_value))
  
  # bind the current data frame to the combined result
  df_rmse <- bind_rows(df_rmse, df)
}


## option 2: map_dfr()- collect RMSE

### list all data frame names that match the pattern "ins_{set}_rmse_m{model}"
df_names <- ls(pattern = "ins_(train|test)_rmse_m(1|2|3)")

# loop over each data frame name, process it, and combine them
df_rmse_ <- df_names %>%
  map_dfr(~ {
    df <- get(.x) # get the data frame by name
    # extract the set and model numbers from the name
    set_value <- ifelse(grepl("train", .x), "train", "test")
    model_value <- stringr::str_extract(.x, "m[1-3]") %>% stringr::str_remove("m")
    # Add set and model columns to the data frame
    df %>%
      mutate(set = set_value, model = as.integer(model_value))
  })


## visualize RMSE
df_rmse %>% 
  mutate(set = factor(set, 
                      levels = c("train", "test")),
         model = factor(model)) %>% 
  ggplot(aes(x = model,
             y = .estimate,
             fill = set,
             label = round(.estimate,0))) +
  geom_col(color = "black",
           width = 0.25,
           show.legend = F) +
  geom_text(aes(y = .estimate + 120),
            size = 5) +
  scale_y_continuous(breaks = seq(0,10000,500)) +
  facet_grid(cols = vars(set)) +
  scale_fill_manual(values = c("deepskyblue2", "chocolate1")) +
  xlab("Model") +
  ylab("RMSE") +
  labs(title = "Model (insurance data) performance evaluation") + 
  theme_bw() +
  theme(text = element_text(size = 18),
        plot.subtitle = element_text(size = 14))

