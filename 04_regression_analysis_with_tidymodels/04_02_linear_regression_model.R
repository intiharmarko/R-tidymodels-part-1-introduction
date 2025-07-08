# 4 Regression Analysis with tidymodels

rm(list = ls())
graphics.off()

# Load packages
library(tidyverse)

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

