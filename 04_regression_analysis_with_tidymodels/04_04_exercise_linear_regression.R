# Exercise - Linear regression

rm(list = ls())
graphics.off()


# Load packages
library(tidyverse)


# Load data (Faithful geyser observations data)
df <- faithful


# Visualize data 
df %>% 
  ggplot(aes(x = eruptions,
             y = waiting)) +
  geom_point(size = 5, 
             color = "gray15") +
  scale_x_continuous(breaks = seq(0,10,0.5)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  xlab("Eruption duration (in minutes)") +
  ylab("Waiting time (in minutes)") +
  ggtitle("Faithful geyser observations data") +
  theme_bw() +
  theme(text = element_text(size = 18))


# fit linear (regression) model
mod <- lm(formula = waiting ~ eruptions, data = df)


# show model's estimated parameters
print(mod) 


# predict waiting time with eruption duration (and assign predicted values to df)
df <- df %>% 
  mutate(waiting_pred = predict(mod))


# visualize the fitted line (predictions)
df %>% 
  ggplot(aes(x = eruptions,
             y = waiting)) +
  geom_point(size = 5, 
             color = "gray15") +
  geom_line(aes(y = waiting_pred), 
            linewidth = 1.5, 
            color = "brown1") +
  scale_x_continuous(breaks = seq(0,10,0.5)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  xlab("Eruption duration (in minutes)") +
  ylab("Waiting time (in minutes)") +
  labs(title = "Faithful geyser observations data",
       subtitle = "Fited regression line (red) - model's predictions") + 
  theme_bw() +
  theme(text = element_text(size = 18),
        plot.subtitle = element_text(size = 14))


# check model summary
summary(mod)


# visualize residuals distribution

## calculate residuals
df <- df %>% 
  mutate(res = waiting - waiting_pred)

## residuals distribution
df %>% 
ggplot(aes(x = res)) +
  geom_histogram(bins = 30,
                 fill = "gray55",
                 color = "black") +
  scale_x_continuous(breaks = seq(-15,15,2.5)) +
  scale_y_continuous(breaks = seq(0,30,5)) +
  ggtitle("Distribution of residuals") +
  xlab("Residual (e) = waiting - waiting predicted") +
  ylab("Frequency") +
  theme_bw() +
  theme(text = element_text(size = 18))
