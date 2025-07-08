# Assignment - California Housing Price Prediction with Penalized Regression

rm(list =ls())
graphics.off()

library(tidyverse)
library(tidymodels)


# Data

## California housing prices
df <- readr::read_csv(file = "./data/housing.csv",
                      col_names = T)


# Data Prep

## check missing values (per each column)
missing <- df %>% 
  summarise(across(everything(), ~sum(is.na(.))))

df %>% 
  filter(is.na(total_bedrooms))

## replace missing values using KNN imputation

### first convert character var to factor var
df <- df %>% 
  mutate(ocean_proximity = as.factor(ocean_proximity))

### recipe for KNN imputation
rec_imp <- recipe(~ ., data = df) %>%
  step_impute_knn(all_numeric(), neighbors = 5)

### prep and bake the recipe
df <- rec_imp %>%
  prep() %>%
  bake(new_data = df)

## check missing values after imputation
missing <- df %>% 
  summarise(across(everything(), ~sum(is.na(.))))

## aggregate selected totals columns & drop initial columns
## - totals per district: total_rooms, total_bedrooms, population 
## - average per household: avg_rooms, avg_bedrooms, avg_people 
df <- df %>% 
  # calculate average values
  mutate(avg_rooms = total_rooms / households,
         avg_bedrooms = total_bedrooms / households,
         avg_people = population / households) %>% 
  # re-arrange columns & drop initial columns
  select(median_house_value,
         median_income,
         housing_median_age,
         avg_rooms,
         avg_bedrooms,
         avg_people,
         households,
         latitude,
         longitude,
         ocean_proximity)

## split data
##
## - train    70%
##   - CV (k =5)
## - validate 20%
## - test     10%
set.seed(1123)

### train VS validate + test split
split_init  <- initial_split(df, prop = 0.7) # initial split train VS validate + test
df.train    <- training(split_init)          # train data
df.val_test <- testing(split_init)           # validate + test data

### validate VS test split
split_val_tes <- initial_split(df.val_test, prop = 2/3) # split validate VS test
df.validate   <- training(split_val_tes)                # validate data
df.test       <- testing(split_val_tes)                 # test data

### check sizes
round(nrow(df.train)    / nrow(df), 2) # 70% of original df
round(nrow(df.validate) / nrow(df), 2) # 20% of original df
round(nrow(df.test)     / nrow(df), 2) # 10% of original df

### set up CV for hyperpar. tuning
### - we will use k=5 fold CV
cv_folds <- vfold_cv(df.train, v = 5)



# Model Training

## define model specification (for LRM, FRW & BCK)
mod <- linear_reg() %>% 
  set_engine("lm") %>%
  set_mode("regression")


## Model 1 - LRM: Linear regression model with selected features
## - selected features based on EDA

### define recipe
LRM_rec <- recipe(formula = median_house_value ~ median_income + housing_median_age + latitude + longitude + ocean_proximity, 
                  data = df.train) %>% 
  step_dummy(all_nominal_predictors())

### create a workflow
LRM_wflow <- workflow() %>% 
  add_recipe(LRM_rec) %>% 
  add_model(mod)

### fit model (train set)
LRM_fit <- LRM_wflow %>% 
  fit(data = df.train)


## Model 2 - FRW: Forward feature selection LRM model

### full model with all predictors
mod_full <- lm(median_house_value ~ ., 
               data = df.train)

### minimal starting model (intercept only)
mod_min <- lm(median_house_value ~ 1, 
              data = df.train)

### perform forward feature selection
mod_forw <- MASS::stepAIC(mod_min, 
                          scope = list(lower = mod_min, 
                                       upper = mod_full), 
                          direction = "forward")

### view the selected model
summary(mod_forw)

### extract the final model formula - for tidymodels framework
forw_fin_formula <- formula(mod_forw)

### define recipe (formula extracted before)
FRW_rec <- recipe(formula = forw_fin_formula, 
                  data = df.train)%>% 
  step_dummy(all_nominal_predictors())

### create the workflow
FRW_wflow <- workflow() %>%
  add_recipe(FRW_rec) %>%
  add_model(mod)

### fit model (train set)
FRW_fit <- FRW_wflow %>%
  fit(data = df.train)


## Model 3 - BCK: Backward feature selection LRM model

### perform backward feature selection
mod_back <- MASS::stepAIC(mod_full, direction = "backward")

### view the selected model
summary(mod_back)

### extract the final model formula - for tidymodels framework
back_fin_formula <- formula(mod_back)

### define recipe (formula extracted before)
BCK_rec <- recipe(formula = back_fin_formula, 
                  data = df.train) %>% 
  step_dummy(all_nominal_predictors())

### create the workflow
BCK_wflow <- workflow() %>%
  add_recipe(BCK_rec) %>%
  add_model(mod)

### fit (train) model (on train set)
BCK_fit <- BCK_wflow %>%
  fit(data = df.train)


## Model 4 - RID: Ridge penalized regression model

### grid for tuning (RID & LAS)
### - we tune only penalty term
RID_LAS_grid_regular <- grid_regular(
  penalty(range = c(-4, 2)),
  levels = 10
)

### define model specification
RID_mod <- linear_reg(penalty = tune(), 
                      mixture = 0) %>%
  set_engine("glmnet")

### define recipe
RID_rec <- recipe(formula = median_house_value ~ ., 
                  data = df.train) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors())

### create workflow 
RID_wflow <- workflow() %>%
  add_recipe(RID_rec) %>%
  add_model(RID_mod)

### hyperpar. tuning
RID_tune_rez <- tune_grid(RID_wflow,
                          resamples = cv_folds,
                          grid = RID_LAS_grid_regular)

### select best model 
RID_mod_best <- select_best(RID_tune_rez, "rmse")

### finalize model & model fit
RID_wflow_fin <- finalize_workflow(RID_wflow, RID_mod_best)
RID_fit <- fit(RID_wflow_fin, df.train)


## Model 5 - LAS: Lasso penalized regression model

### define model specification
LAS_mod <- linear_reg(penalty = tune(), 
                      mixture = 1) %>%
  set_engine("glmnet")

### define recipe
LAS_rec <- recipe(formula = median_house_value ~ ., 
                  data = df.train) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors())

### create workflow 
LAS_wflow <- workflow() %>%
  add_recipe(LAS_rec) %>%
  add_model(LAS_mod)

### hyperpar. tuning
LAS_tune_rez <- tune_grid(LAS_wflow,
                          resamples = cv_folds,
                          grid = RID_LAS_grid_regular)

### select best model 
LAS_mod_best <- select_best(LAS_tune_rez, "rmse")

### finalize model & model fit
LAS_wflow_fin <- finalize_workflow(LAS_wflow, LAS_mod_best)
LAS_fit <- fit(LAS_wflow_fin, df.train)


## Model 6 - ENT: Elastic net penalized regression model

### grid for tuning
### - we two hyperpar. 
### - penalty & mixture
ENT_grid_regular <- grid_regular(
  penalty(range = c(-4, 2)),
  mixture(range = c(0, 1)),
  levels = 10
)

### define model specification
ENT_mod <- linear_reg(penalty = tune(), 
                      mixture = tune()) %>%
  set_engine("glmnet")

### define recipe
ENT_rec <- recipe(formula = median_house_value ~ ., 
                  data = df.train) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors())

### create workflow 
ENT_wflow <- workflow() %>%
  add_recipe(ENT_rec) %>%
  add_model(ENT_mod)

### hyperpar. tuning
ENT_tune_rez <- tune_grid(ENT_wflow,
                          resamples = cv_folds,
                          grid = ENT_grid_regular)

### select best model 
ENT_mod_best <- select_best(ENT_tune_rez, "rmse")

### finalize model & model fit
ENT_wflow_fin <- finalize_workflow(ENT_wflow, ENT_mod_best)
ENT_fit <- fit(ENT_wflow_fin, df.train)



# Model Validation
# - find best performing model
# - using validation set

## nest fitted models in df
## - table with model names & model fit  
mod_val <- tibble(
  model = c("LRM", "FRW", "BCK", "RID", "LAS", "ENT"),
  wflow = list(LRM_fit, FRW_fit, BCK_fit, RID_fit, LAS_fit, ENT_fit))

## apply predictions on validate set
## - for each model calculate predictions
## - add predictions to new column (original value & predicted value)
mod_val <- mod_val %>%
  mutate(pred = map(wflow, 
                    ~ predict(.x, 
                              new_data = df.validate) %>%
                      bind_cols(df.validate %>% select(median_house_value)))) 

## evaluate model performance on validate set
## - calculate model performance metrics (RMSE)
## - for each model
mod_val <- mod_val %>%
  mutate(rmse = map(pred, 
                    ~ rmse(.x, 
                           truth = median_house_value, 
                           estimate = .pred) %>% 
                      pull(.estimate))) %>%
  unnest(rmse)

## visualize model validation results
## - choose top performing model

### add factor variable for model column
### - results based on best RMSE (lowest!)
### - also add info about model group
mod_val <- mod_val %>% 
  mutate(model_f = as.factor(model),
         model_f = fct_reorder(model_f, rmse),
         group = case_when(model == "LRM" ~ "simple LRM",
                             model %in% c("FRW", "BCK") ~ "feature selec",
                             model %in% c("RID", "LAS", "ENT") ~ "penalized reg",
                             T ~ "NA"))

### visualize results
mod_val %>% 
  ggplot(aes(x = model_f,
             y = rmse,
             fill = group)) +
  geom_col(color = "black",
           alpha = 0.6) +
  geom_text(aes(label = round(rmse,1),
                y = rmse + 1000),
            size = 5) +
  scale_fill_viridis_d() +
  xlab("Model") +
  ylab("RMSE") +
  ggtitle("Model performance on validation dataset") + 
  theme_minimal()


### collect top performing model
### - final model
FIN_fit <- mod_val %>% 
  arrange(rmse) %>% 
  slice(1) %>% 
  pull(wflow) %>% 
  pluck(1)

  

# Model Testing
# - test best performing model
# - using test set

## test set prediction & RMSE
rmse_test <- predict(FIN_fit, df.test) %>% 
  bind_cols(df.test) %>% 
  select(median_house_value, .pred) %>% 
  metrics(truth = median_house_value, 
          estimate = .pred) %>% 
  filter(.metric == "rmse")

## print top model results
rmse_val <- mod_val %>% arrange(rmse) %>% slice(1) %>% pull(rmse)

print("Top model:")
print(mod_val %>% arrange(rmse) %>% slice(1) %>% pull(model))
print(paste0("RMSE (validate set): ", round(rmse_val, 1)))
print(paste0("RMSE (validate set): ", round(rmse_test %>% select(.estimate), 1)))
