# Exercise - Feature Engineering & EDA

rm(list = ls())
graphics.off()


# Load packages
library(tidyverse)
library(tidymodels)


# Data
df <- diamonds


# Feature Engineering

## cut-color factor levels
cut_color_lev <- expand.grid(df %>% pull(cut) %>% levels(), 
                             df %>% mutate(color = fct_rev(color)) %>% pull(color) %>% levels()) %>% 
  mutate(lev = paste0(Var1, "_", Var2)) %>% 
  pull(lev)

## generate features
df <- df %>% 
  # reverse levels - color
  mutate(color = fct_rev(color)) %>% 
  # generate new features
  mutate(price_log     = log(price),  # log price
         carat_log     = log(carat),  # log carat
         volume        = x * y * z,   # volume
         aspect_rat    = x / y,       # aspect ratio
         q_index       = as.numeric(cut) * as.numeric(color) * as.numeric(clarity), # quality index
         cut_color     = paste0(cut, "_", color)) %>% # category combo: cut_color
  # convert to factor
  mutate(cut_color = factor(cut_color, levels = cut_color_lev)) %>% 
  # reorder columns
  select(price, price_log, 
         carat, x, y, z, depth, table, cut, color, clarity,
         carat_log, volume, aspect_rat, q_index, cut_color)


# Function: prepare data (diamonds)
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


# Exploratory Data Analysis (EDA) - Distribution of each variable (only new features)

## log carat
df %>% 
  ggplot(aes(x = carat_log)) +
  geom_density(fill = "gray80",
               color = "black",
               alpha = 0.6) +
  scale_x_continuous(breaks = seq(-2,2,0.25)) +
  xlab("Diamond log carat") +
  ylab("Density") +
  ggtitle("Diamond log carat distribution") + 
  theme_minimal()

## volume
df %>% 
  ggplot(aes(x = volume)) +
  geom_density(fill = "gray80",
               color = "black",
               alpha = 0.6) +
  scale_x_continuous(breaks = seq(0,1000,50),
                     limits = c(0,550)) +
  xlab("Diamond 'volume' (in mm3)") +
  ylab("Density") +
  ggtitle("Diamond 'volume' distribution") + 
  theme_minimal()

## log volume
df %>% 
  ggplot(aes(x = volume_log)) +
  geom_density(fill = "gray80",
               color = "black",
               alpha = 0.6) +
  scale_x_continuous(breaks = seq(0,10,0.5),
                      limits = c(0,10)) +
  xlab("Diamond log('volume')") +
  ylab("Density") +
  ggtitle("Diamond log 'volume' distribution") + 
  theme_minimal()

## aspect ratio
df %>% 
  ggplot(aes(x = aspect_rat)) +
  geom_density(fill = "gray80",
               color = "black",
               alpha = 0.6) +
  scale_x_continuous(breaks = seq(0.75,1.25,0.05),
                     limits = c(0.90,1.10)) +
  xlab("Aspect ratio") +
  ylab("Density") +
  ggtitle("Diamond aspect ratio distribution") + 
  theme_minimal()

## quality index
df %>% 
  ggplot(aes(x = q_index)) +
  geom_histogram(fill = "gray80",
                 color = "black",
                 alpha = 0.6,
                 bins = 30) +
  scale_x_continuous(breaks = seq(0,300,25)) +
  xlab("Quality index") +
  ylab("Number of diamonds") +
  ggtitle("Diamond quality index distribution") + 
  theme_minimal()

## diamond cut-color
df %>% 
  ggplot(aes(x = cut_color)) +
  geom_bar(fill = "gray80",
           color = "black",
           alpha = 0.6) +
  xlab("Diamond cut-color") +
  ylab("Number of diamonds") +
  ggtitle("Diamond cut-color distribution") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))


# Exploratory Data Analysis (EDA) - single variable VS log price (only new features)

## log price ~ volume
df %>% 
  ggplot(aes(x = volume,
             y = price_log)) +
  geom_jitter(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0,1000,100),
                     limits = c(0,1000)) +
  xlab("Diamond 'volume' (in mm3)") +
  ylab("Diamond log(price)") +
  ggtitle("Diamond log price VS diamond 'volume'") + 
  theme_minimal()

## log price ~ log volume
df %>% 
  ggplot(aes(x = volume_log,
             y = price_log)) +
  geom_jitter(alpha = 0.2) +
  xlab("Diamond log('volume')") +
  ylab("Diamond log(price)") +
  ggtitle("Diamond log price VS log 'volume'") + 
  theme_minimal()

## log price ~ aspect ratio
df %>% 
  ggplot(aes(x = aspect_rat,
             y = price_log)) +
  geom_jitter(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0.75,1.25,0.05),
                     limits = c(0.90,1.10)) +
  xlab("Aspect ratio") +
  ylab("Diamond log(price)") +
  ggtitle("Diamond log price VS aspect ratio") + 
  theme_minimal()

## log price ~ quality index
df %>% 
  ggplot(aes(x = q_index,
             y = price_log)) +
  geom_jitter(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0,300,25)) +
  xlab("Quality index") +
  ylab("Diamond log(price)") +
  ggtitle("Diamond log price VS quality index") + 
  theme_minimal()

## log price ~ cut_color
df %>% 
  ggplot(aes(x = cut_color,
             y = price_log)) +
  geom_boxplot(fill = "gray80",
               color = "black",
               alpha = 0.6) +
  xlab("Diamond cut-color") +
  ylab("Diamond log(price)") +
  ggtitle("Diamond log price VS diamond cut-color") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))


# Exploratory Data Analysis (EDA) - multiple variables VS log price (one selected figure)

## log price ~ log carat & log volume & quality index
df %>% 
  ggplot(aes(x = carat_log,
             y = price_log,
             color = volume_log,
             size = q_index)) +
  geom_jitter(alpha = 0.4) +
  scale_color_viridis_c(option = "magma") +
  scale_size_continuous(range = c(0,10)) +
  xlab("Diamond log(carat)") +
  ylab("Diamond log(price)") +
  labs(title = "Diamond log price VS log carat & log volume & quality index",
       color = "log(volume):",
       size = "quality index:") + 
  theme_minimal()
