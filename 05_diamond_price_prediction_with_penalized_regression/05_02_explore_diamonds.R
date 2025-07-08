# 5 Diamond Price Prediction with Penalized Regression

rm(list = ls())
graphics.off()

# Load packages
library(tidyverse)
library(tidymodels)
#install.packages("gridExtra")
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
