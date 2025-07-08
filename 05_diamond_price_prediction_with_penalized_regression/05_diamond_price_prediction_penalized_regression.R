# 5 Diamond Price Prediction with Penalized Regression

rm(list = ls())
graphics.off()

# Load packages
library(tidyverse)
library(tidymodels)
library(gridExtra)


# Data

## diamonds data set
df <- diamonds

## create smaller diamonds data sets 
## - (if code runs too long)
## - assign df to one of smaller data sets if needed!
set.seed(1123)

df.t <- diamonds %>% sample_n(size = 100,   replace = F) # tiny
df.s <- diamonds %>% sample_n(size = 1000,  replace = F) # small
df.m <- diamonds %>% sample_n(size = 10000, replace = F) # medium
df.b <- diamonds %>% sample_n(size = 25000, replace = F) # big



# 5.3 Explore Diamonds

## data set size
df %>% ncol() # number of variables
df %>% nrow() # number of units (diamonds) - measurements

## variable types
map_df(.x = df, 
       .f = class)

## reorder columns
## - predicted variable first
## - then numeric predictors
## - then factor predictors
df  <- df %>% 
  select(price, 
         carat, x, y, z, depth, table,
         cut, color, clarity)

## Exploratory Data Analysis (EDA) - Distribution of each variable

### diamond price
df %>% 
  ggplot(aes(x = price)) +
  geom_density(fill = "gray80",
               color = "black",
               alpha = 0.6) +
  scale_x_continuous(breaks = seq(0,25000,1000)) +
  xlab("Diamond price in USD") +
  ylab("Density") +
  ggtitle("Diamond price distribution") + 
  theme_minimal()
  
### diamond carat
df %>% 
  ggplot(aes(x = carat)) +
  geom_density(fill = "gray80",
               color = "black",
               alpha = 0.6) +
  scale_x_continuous(breaks = seq(0,5,0.25)) +
  xlab("Diamond weight in carat") +
  ylab("Density") +
  ggtitle("Diamond carat distribution") + 
  theme_minimal()

### diamond dimensions: x y z
fig.x <- df %>% 
  ggplot(aes(x = x)) +
  geom_density(fill = "gray80",
               color = "black",
               alpha = 0.6) +
  scale_x_continuous(breaks = seq(0,10,0.5),
                     limits = c(0, 11)) +
  xlab("Diamond length ~ x (in mm)") +
  ylab("Density") +
  ggtitle("Diamond length (x) distribution") + 
  theme_minimal()

fig.y <- df %>% 
  ggplot(aes(x = y)) +
  geom_density(fill = "gray80",
               color = "black",
               alpha = 0.6) +
  scale_x_continuous(breaks = seq(0,60,0.5),
                     limits = c(0, 11)) +
  xlab("Diamond width ~ y (in mm)") +
  ylab("Density") +
  ggtitle("Diamond width (y) distribution") + 
  theme_minimal()

fig.z <- df %>% 
  ggplot(aes(x = z)) +
  geom_density(fill = "gray80",
               color = "black",
               alpha = 0.6) +
  scale_x_continuous(breaks = seq(0,30,0.5),
                     limits = c(0, 11)) +
  xlab("Diamond depth ~ z (in mm)") +
  ylab("Density") +
  ggtitle("Diamond depth (z) distribution") + 
  theme_minimal()

grid.arrange(fig.x, fig.y, fig.z, nrow = 3)

### diamond total depth percentage
df %>% 
  ggplot(aes(x = depth)) +
  geom_histogram(fill = "gray80",
                 color = "black",
                 alpha = 0.6,
                 bins = 30) +
  scale_x_continuous(breaks = seq(0,100,5)) +
  xlab("Diamond total depth percentage depth (in %)") +
  ylab("Density") +
  ggtitle("Diamond total depth percentage distribution") + 
  theme_minimal()

### width of top of diamond relative to widest point
df %>% 
  ggplot(aes(x = table)) +
  geom_histogram(fill = "gray80",
                 color = "black",
                 alpha = 0.6,
                 bins = 30) +
  scale_x_continuous(breaks = seq(0,100,5)) +
  xlab("Top relative to widest point") +
  ylab("Density") +
  ggtitle("Diamond top relative to widest point distribution") + 
  theme_minimal()

### diamond cut
df %>% 
  ggplot(aes(x = cut)) +
  geom_bar(fill = "gray80",
           color = "black",
           alpha = 0.6) +
  scale_y_continuous(breaks = seq(0,25000,1000)) +
  xlab("Diamond cut") +
  ylab("Number of diamonds") +
  ggtitle("Diamond cut distribution") + 
  theme_minimal()

### diamond color
df %>% 
  ggplot(aes(x = color)) +
  geom_bar(fill = "gray80",
           color = "black",
           alpha = 0.6) +
  scale_y_continuous(breaks = seq(0,25000,1000)) +
  xlab("Diamond color") +
  ylab("Number of diamonds") +
  ggtitle("Diamond color distribution") + 
  theme_minimal()

#### reverse order of cut variable (from worse -> best) after reverse
df <- df %>% 
  mutate(color = fct_rev(color))

### diamond clarity
df %>% 
  ggplot(aes(x = clarity)) +
  geom_bar(fill = "gray80",
           color = "black",
           alpha = 0.6) +
  scale_y_continuous(breaks = seq(0,25000,1000)) +
  xlab("Diamond clarity") +
  ylab("Number of diamonds") +
  ggtitle("Diamond clarity distribution") + 
  theme_minimal()


## Exploratory Data Analysis (EDA) - single variable VS price

### price ~ carat
df %>% 
  ggplot(aes(x = carat,
             y = price)) +
  geom_jitter(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0,5,0.25)) +
  scale_y_continuous(breaks = seq(0,25000,1000)) +
  xlab("Diamond weight in carat") +
  ylab("Diamond price in USD") +
  ggtitle("Diamond price VS diamond weight") + 
  theme_minimal()

### price ~ xyz
fig.x <- df %>% 
  ggplot(aes(x = x,
             y = price)) +
  geom_jitter(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0,10,0.5),
                     limits = c(0, 11)) +
  scale_y_continuous(breaks = seq(0,25000,1000)) +
  xlab("Diamond length ~ x (in mm)") +
  ylab("Diamond price in USD") +
  ggtitle("Diamond price VS diamond length") + 
  theme_minimal()

fig.y <- df %>% 
  ggplot(aes(x = y,
             y = price)) +
  geom_jitter(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0,10,0.5),
                     limits = c(0, 11)) +
  scale_y_continuous(breaks = seq(0,25000,1000)) +
  xlab("Diamond width ~ y (in mm)") +
  ylab("Diamond price in USD") +
  ggtitle("Diamond price VS diamond width") + 
  theme_minimal()

fig.z <- df %>% 
  ggplot(aes(x = z,
             y = price)) +
  geom_jitter(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0,10,0.5),
                     limits = c(0, 11)) +
  scale_y_continuous(breaks = seq(0,25000,1000)) +
  xlab("Diamond depth ~ z (in mm)") +
  ylab("Diamond price in USD") +
  ggtitle("Diamond price VS diamond depth") + 
  theme_minimal()

grid.arrange(fig.x, fig.y, fig.z, nrow = 3)

### price ~ total depth percentage
df %>% 
  ggplot(aes(x = depth,
             y = price)) +
  geom_jitter(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0,100,5)) +
  scale_y_continuous(breaks = seq(0,25000,1000)) +
  xlab("Diamond total depth percentage depth (in %)") +
  ylab("Diamond price in USD") +
  ggtitle("Diamond price VS diamond total depth percentage") + 
  theme_minimal()

### price ~ width of top relative to widest point
df %>% 
  ggplot(aes(x = table,
             y = price)) +
  geom_jitter(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0,100,5)) +
  scale_y_continuous(breaks = seq(0,25000,1000)) +
  xlab("Top relative to widest point") +
  ylab("Diamond price in USD") +
  ggtitle("Diamond price VS diamond top relative to widest point") + 
  theme_minimal()

### price ~ cut
df %>% 
  ggplot(aes(x = cut,
             y = price)) +
  geom_boxplot(fill = "gray80",
               color = "black",
               alpha = 0.6) +
  scale_y_continuous(breaks = seq(0,25000,1000)) +
  xlab("Diamond cut") +
  ylab("Diamond price in USD") +
  ggtitle("Diamond price VS diamond cut") + 
  theme_minimal()

### price ~ color
df %>% 
  ggplot(aes(x = color,
             y = price)) +
  geom_boxplot(fill = "gray80",
               color = "black",
               alpha = 0.6) +
  scale_y_continuous(breaks = seq(0,25000,1000)) +
  xlab("Diamond color") +
  ylab("Diamond price in USD") +
  ggtitle("Diamond price VS diamond color") + 
  theme_minimal()

### price ~ clarity
df %>% 
  ggplot(aes(x = clarity,
             y = price)) +
  geom_boxplot(fill = "gray80",
               color = "black",
               alpha = 0.6) +
  scale_y_continuous(breaks = seq(0,25000,1000)) +
  xlab("Diamond clarity") +
  ylab("Diamond price in USD") +
  ggtitle("Diamond price VS diamond clarity") + 
  theme_minimal()

### pair plots
GGally::ggpairs(df.m)


## Exploratory Data Analysis (EDA) - adding log price

### calculate log price (natural logarithm)
df <- df %>% 
  mutate(price_log = log(price))

### diamond log price distribution
df %>% 
  ggplot(aes(x = price_log)) +
  geom_density(fill = "gray80",
               color = "black",
               alpha = 0.6) +
  xlab("Diamond log(price)") +
  ylab("Density") +
  ggtitle("Diamond log price distribution") + 
  theme_minimal()

### log price ~ carat
df %>% 
  ggplot(aes(x = carat,
             y = price_log)) +
  geom_jitter(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0,5,0.25)) +
  xlab("Diamond weight in carat") +
  ylab("Diamond log(price)") +
  ggtitle("Diamond log price VS diamond weight") + 
  theme_minimal()

### calculate log carat (natural logarithm)
df <- df %>% 
  mutate(carat_log = log(carat))

### log price ~ log carat
df %>% 
  ggplot(aes(x = carat_log,
             y = price_log)) +
  geom_jitter(alpha = 0.2) +
  xlab("Diamond log(carat)") +
  ylab("Diamond log(price)") +
  ggtitle("Diamond log price VS log carat") + 
  theme_minimal()

### log price ~ xyz
fig.x <- df %>% 
  ggplot(aes(x = x,
             y = price_log)) +
  geom_jitter(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0,10,0.5),
                     limits = c(0, 11)) +
  xlab("Diamond length ~ x (in mm)") +
  ylab("Diamond log(price)") +
  ggtitle("Diamond log price VS diamond length") + 
  theme_minimal()

fig.y <- df %>% 
  ggplot(aes(x = y,
             y = price_log)) +
  geom_jitter(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0,10,0.5),
                     limits = c(0, 11)) +
  xlab("Diamond width ~ y (in mm)") +
  ylab("Diamond log(price)") +
  ggtitle("Diamond log price VS diamond width") + 
  theme_minimal()

fig.z <- df %>% 
  ggplot(aes(x = z,
             y = price_log)) +
  geom_jitter(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0,10,0.5),
                     limits = c(0, 11)) +
  xlab("Diamond depth ~ z (in mm)") +
  ylab("Diamond log(price)") +
  ggtitle("Diamond log price VS diamond depth") + 
  theme_minimal()

grid.arrange(fig.x, fig.y, fig.z, nrow = 3)

### log price ~ total depth percentage
df %>% 
  ggplot(aes(x = depth,
             y = price_log)) +
  geom_jitter(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0,100,5)) +
  xlab("Diamond total depth percentage depth (in %)") +
  ylab("Diamond log(price)") +
  ggtitle("Diamond log price VS diamond total depth percentage") + 
  theme_minimal()

### log price ~ width of top relative to widest point
df %>% 
  ggplot(aes(x = table,
             y = price_log)) +
  geom_jitter(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0,100,5)) +
  xlab("Top relative to widest point") +
  ylab("Diamond log(price)") +
  ggtitle("Diamond log price VS diamond top relative to widest point") + 
  theme_minimal()

### log price ~ cut
df %>% 
  ggplot(aes(x = cut,
             y = price_log)) +
  geom_boxplot(fill = "gray80",
               color = "black",
               alpha = 0.6) +
  xlab("Diamond cut") +
  ylab("Diamond log(price)") +
  ggtitle("Diamond log price VS diamond cut") + 
  theme_minimal()

### log price ~ color
df %>% 
  ggplot(aes(x = color,
             y = price_log)) +
  geom_boxplot(fill = "gray80",
               color = "black",
               alpha = 0.6) +
  xlab("Diamond color") +
  ylab("Diamond log(price)") +
  ggtitle("Diamond log price VS diamond color") + 
  theme_minimal()

### log price ~ clarity
df %>% 
  ggplot(aes(x = clarity,
             y = price_log)) +
  geom_boxplot(fill = "gray80",
               color = "black",
               alpha = 0.6) +
  xlab("Diamond clarity") +
  ylab("Diamond log(price)") +
  ggtitle("Diamond log price VS diamond clarity") + 
  theme_minimal()

### pair plots
df.m %>% 
  mutate(price_log = log(price),
         carat_log = log(carat)) %>% 
  select(-price) %>% 
  GGally::ggpairs(.)


## Exploratory Data Analysis (EDA) - multiple variables VS price

### log price ~ log carat & xyz (volume)
df %>% 
  filter(x <= 10 & y <= 10 & z <= 10) %>% 
  ggplot(aes(x = carat_log,
             y = price_log,
             color = x*y*z)) +
  geom_jitter() +
  scale_color_viridis_c(option = "magma") +
  xlab("Diamond log(carat)") +
  ylab("Diamond log(price)") +
  labs(title = "Diamond log price VS log carat & diamond volume",
       color = "Diamond volume (mm3):") + 
  theme_minimal()

### log price ~ cut/color/clarity
df %>% 
  ggplot(aes(x = color,
             y = price_log,
             fill = color)) +
  geom_boxplot() +
  facet_grid(rows = vars(cut),
             cols = vars(clarity)) +
  scale_color_viridis_d(option = "viridis") +
  xlab("Diamond color") +
  ylab("Diamond log(price)") +
  labs(title = "Diamond log price VS diamond color, cut and clarity",
       color = "Diamond color:") + 
  theme_minimal()



# 5.4 Feature Selection

## Function: prepare data (diamonds) - from exercise!
data_prep_diamonds <- function(){
  
  # load data
  df <- ggplot2::diamonds
  
  # cut-color factor levels
  cut_color_lev <- expand.grid(df %>% pull(cut) %>% levels(), 
                               df %>% mutate(color = fct_rev(color)) %>% pull(color) %>% levels()) %>% 
    mutate(lev = paste0(Var1, "_", Var2)) %>% 
    pull(lev)
  
  # generate features
  df <- df %>% 
    # reverse levels - color
    mutate(color = fct_rev(color)) %>% 
    # generate new features
    mutate(price_log     = log(price),  # log price
           carat_log     = log(carat),  # log carat
           volume        = x * y * z,   # volume
           volume_log    = log(volume), # log volume
           aspect_rat    = x / y,       # aspect ratio
           q_index       = as.numeric(cut) * as.numeric(color) * as.numeric(clarity), # quality index
           cut_color     = paste0(cut, "_", color)) %>% # category combo: cut_color
    # convert to factor
    mutate(cut_color = factor(cut_color, levels = cut_color_lev)) %>% 
    # reorder columns
    select(price, price_log, 
           carat, x, y, z, depth, table, cut, color, clarity,
           carat_log, volume, volume_log, aspect_rat, q_index, cut_color)
  
  # remove diamonds with missing data (value 0) for x,y,z features
  df <- df %>% 
    filter(x > 0 & y > 0 & z > 0) # alternative "volume > 0"
  
  # create smaller diamonds data sets 
  set.seed(1123)
  
  df.t <- df %>% sample_n(size = 100,   replace = F) # tiny
  df.s <- df %>% sample_n(size = 1000,  replace = F) # small
  df.m <- df %>% sample_n(size = 10000, replace = F) # medium
  df.b <- df %>% sample_n(size = 25000, replace = F) # big
  
  rez <- list(df.t = df.t,
              df.s = df.s,
              df.m = df.m,
              df.b = df.b,
              df = df)
  
  return(rez)
}

## function call - prepare data
df.all <- data_prep_diamonds()
df.t <- df.all$df.t
df.s <- df.all$df.s
df.m <- df.all$df.m 
df.b <- df.all$df.b 
df   <- df.all$df 
rm(df.all); gc()


## split data
set.seed(1123)

split <- initial_split(df, prop = 0.8)
df.train <- training(split)
df.test  <- testing(split)


## Custom Backward/Forward Feature Selection 

## backward selection

### full model using stepAIC (for comparison) - function from MASS library
mod_full <- lm(price_log ~ ., 
               data = df.train %>% select(-c("price", "carat", "volume")))

### perform backward feature selection
mod_back <- MASS::stepAIC(mod_full, direction = "backward")

### view the selected model
summary(mod_back)

### extract the final model formula - for tidymodels framework
back_fin_formula <- formula(mod_back)

### define model specification
lm_mod <- linear_reg() %>% 
  set_engine("lm") %>%
  set_mode("regression")

### define recipe (formula extracted before)
back_rec <- recipe(formula = back_fin_formula, 
                   data = df.train %>% select(-c("price", "carat", "volume")))

### create the workflow
back_wflow <- workflow() %>%
  add_recipe(back_rec) %>%
  add_model(lm_mod)

### fit (train) model (on train set)
back_mod_fit <- back_wflow %>%
  fit(data = df.train %>% select(-c("price", "carat", "volume")))


## forward selection

### full model with all predictors
mod_full <- lm(price_log ~ ., 
               data = df.train %>% select(-c("price", "carat", "volume")))

### minimal starting model (intercept only)
mod_min <- lm(price_log ~ 1, 
              data = df.train %>% select(-c("price", "carat", "volume")))

### perform forward feature selection
mod_forw <- MASS::stepAIC(mod_min, 
                          scope = list(lower = mod_min, 
                                       upper = mod_full), 
                          direction = "forward")

### view the selected model
summary(mod_forw)

### extract the final model formula - for tidymodels framework
forw_fin_formula <- formula(mod_forw)

### define model specification
lm_mod <- linear_reg() %>% 
  set_engine("lm") %>%
  set_mode("regression")

### define recipe (formula extracted before)
forw_rec <- recipe(formula = forw_fin_formula, 
                   data = df.train %>% select(-c("price", "carat", "volume")))

### create the workflow
forw_wflow <- workflow() %>%
  add_recipe(forw_rec) %>%
  add_model(lm_mod)

### fit (train) model (on train set)
forw_mod_fit <- forw_wflow %>%
  fit(data = df.train %>% select(-c("price", "carat", "volume")))


## compare prediction results of both models

### check formulas
back_fin_formula
forw_fin_formula

### make predictions on test data
df.test_back_pred <- predict(back_mod_fit, df.test) %>% # predict with backward selection model
  rename(price_log_pred_back = .pred)

df.test_forw_pred <- predict(forw_mod_fit, df.test) %>% # predict with forward selection model
  rename(price_log_pred_forw = .pred) 

df.test <- df.test %>% 
  bind_cols(df.test_back_pred, df.test_forw_pred)

### evaluate both models' performance (RMSE)
test_rmse_back <- df.test %>% 
  metrics(truth = price_log, 
          estimate = price_log_pred_back) %>% 
  filter(.metric == "rmse")

test_rmse_forw <- df.test %>% 
  metrics(truth = price_log, 
          estimate = price_log_pred_forw) %>% 
  filter(.metric == "rmse")



# 5.6 Penalized Regression

## Ridge regression

### define model specification
### - we tune penalty hyperpar.
### - mixture = 0 specify ridge regression model
r_mod <- linear_reg(penalty = tune(), 
                    mixture = 0) %>%
  set_engine("glmnet")

### define recipe
### - we predict log price & remove some features that are used with log version
### - first we normalize all numeric features
### - then we apply dummy encoding on category predictors (this step is automatically applied with lin. reg. mod)
r_rec <- recipe(formula = price_log ~ ., 
                data = df.train %>% select(-c("price", "carat", "volume"))) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors())
  
### create workflow 
r_wflow <- workflow() %>%
  add_recipe(r_rec) %>%
  add_model(r_mod)

### set up CV for hyperpar. tuning
### - we will use k=5 fold CV
cv_folds <- vfold_cv(df.train, v = 5)

### prepare grid for tuning
### - we will use manual grid 
### - 5 different values for penalty term (range from log scale)
grid_manual <- tibble(
  penalty = c(0.01, 0.1, 1, 10, 100) 
)

### hyperpar. tuning
### - execute tuning using CV data split & selected grid
r_tune_rez <- tune_grid(r_wflow,
                        resamples = cv_folds,
                        grid = grid_manual)

### select best model 
### - model with lowest RMSE
### - and selected value for penalty hyperpar.
r_mod_best <- select_best(r_tune_rez, "rmse")

### finalize model
### - finalize workflow with best model
### - train model (model fit) on whole train data
r_wflow_fin <- finalize_workflow(r_wflow, r_mod_best)
r_mod_fit <- fit(r_wflow_fin, df.train)

### evaluate model performance
### - on test data (RMSE metrics)
### - first predict output on test data
### - then extract RMSE
df.test_r_pred <- predict(r_mod_fit, df.test) %>% 
  bind_cols(df.test) %>% 
  select(price_log, .pred)

test_rmse_r <- df.test_r_pred %>% 
  metrics(truth = price_log, 
          estimate = .pred) %>% 
  filter(.metric == "rmse")


## Lasso regression

### define model specification
### - we tune penalty hyperpar.
### - mixture = 1 specify lasso regression model
l_mod <- linear_reg(penalty = tune(), 
                    mixture = 1) %>%
  set_engine("glmnet")

### define recipe
### - we predict log price & remove some features that are used with log version
### - first we normalize all numeric features
### - then we apply dummy encoding on category predictors (this step is automatically applied with lin. reg. mod)
l_rec <- recipe(formula = price_log ~ ., 
                data = df.train %>% select(-c("price", "carat", "volume"))) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors())

### create workflow 
l_wflow <- workflow() %>%
  add_recipe(l_rec) %>%
  add_model(l_mod)

### set up CV for hyperpar. tuning
### - we will use k=5 fold CV
cv_folds <- vfold_cv(df.train, v = 5)

### prepare grid for tuning
### - we will use regular grid 
### - 5 different values for penalty term 
### - (range from log scale - equally spaced between 0.0001 an 100)
grid_regular <- grid_regular(
  penalty(range = c(-4, 2)),
  levels = 5
)

### hyperpar. tuning
### - execute tuning using CV data split & selected grid
l_tune_rez <- tune_grid(l_wflow,
                        resamples = cv_folds,
                        grid = grid_regular)

### select best model 
### - model with lowest RMSE
### - and selected value for penalty hyperpar.
l_mod_best <- select_best(l_tune_rez, "rmse")

### finalize model
### - finalize workflow with best model
### - train model (model fit) on whole train data
l_wflow_fin <- finalize_workflow(l_wflow, l_mod_best)
l_mod_fit <- fit(l_wflow_fin, df.train)

### evaluate model performance
### - on test data (RMSE metrics)
### - first predict output on test data
### - then extract RMSE
df.test_l_pred <- predict(l_mod_fit, df.test) %>% 
  bind_cols(df.test) %>% 
  select(price_log, .pred)

test_rmse_l <- df.test_l_pred %>% 
  metrics(truth = price_log, 
          estimate = .pred) %>% 
  filter(.metric == "rmse")


## compare RMSE between ridge and lasso model
test_rmse_r
test_rmse_l

## extract final model's coefficients
tidy(r_mod_fit)
tidy(r_mod_fit) %>% arrange(desc(estimate)) # highest coefficient value ridge features
tidy(r_mod_fit) %>% filter(estimate == 0) # ridge can not remove features!

tidy(l_mod_fit)
tidy(l_mod_fit) %>% arrange(desc(estimate)) # highest coefficient value lasso features
tidy(l_mod_fit) %>% filter(estimate == 0) # removed features by lasso model



# 5.7 Elastic net regression

## define model specification
## - we now tune penalty and mixture hyperpar.
## - penalty ~ lambda (regularization stregth)
## - mixture ~ alpha  (ridge-lasso mixing parameter)
el_mod <- linear_reg(penalty = tune(), 
                     mixture = tune()) %>%
  set_engine("glmnet")

## define recipe
## - we predict log price & remove some features that are used with log version
## - first we normalize all numeric features
## - then we apply dummy encoding on category predictors (this step is automatically applied with lin. reg. mod)
el_rec <- recipe(formula = price_log ~ ., 
                 data = df.train %>% select(-c("price", "carat", "volume"))) %>%
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors())

## create workflow 
el_wflow <- workflow() %>%
  add_recipe(el_rec) %>%
  add_model(el_mod)

## set up CV for hyperpar. tuning
## - we will use k=5 fold CV
cv_folds <- vfold_cv(df.train, v = 5)

## prepare grid for tuning
## - we will use manual grid 
## - 5 different values for penalty term (range from log scale)
## - 5 different values for mixture term (range from 0 - ridge to 1 - lasso)
el_grid_manual <- tibble(
  penalty = c(0.01, 0.1, 1, 10, 100),
  mixture = seq(0, 1, by = 0.25)
)

## hyperpar. tuning
## - execute tuning using CV data split & grid
el_tune_rez <- tune_grid(el_wflow,
                         resamples = cv_folds,
                         grid = el_grid_manual)

## select best model 
## - model with lowest RMSE
## - and selected value for penalty & mixture hyperpar.
el_mod_best <- select_best(el_tune_rez, "rmse")

## finalize model
## - finalize workflow with best model
## - train model (model fit) on whole train data
el_wflow_fin <- finalize_workflow(el_wflow, el_mod_best)
el_mod_fit <- fit(el_wflow_fin, df.train)

## evaluate model performance
## - on test data (RMSE metrics)
## - first predict output on test data
## - then extract RMSE
df.test_el_pred <- predict(el_mod_fit, df.test) %>% 
  bind_cols(df.test) %>% 
  select(price_log, .pred)

test_rmse_el <- df.test_el_pred %>% 
  metrics(truth = price_log, 
          estimate = .pred) %>% 
  filter(.metric == "rmse")


## compare test RMSE - check which model has best performance
## - forward / backward feature selection model
## - ridge / lasso model
## - elastic net model
test_rmse <- tribble(~model,      ~RMSE,
                      "backward", test_rmse_back$.estimate,
                      "forward",  test_rmse_forw$.estimate,
                      "ridge",    test_rmse_r$.estimate,
                      "lasso",    test_rmse_l$.estimate,
                      "elastic",  test_rmse_el$.estimate)
test_rmse %>% arrange(RMSE)

