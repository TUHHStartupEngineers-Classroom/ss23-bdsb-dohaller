library(httr)
library(jsonlite)
library(tidyverse)
library(gridExtra)
library(ggplot2)

# Grab Weather API
resp <- GET("https://api.open-meteo.com/v1/forecast?latitude=53.55&longitude=9.99&hourly=temperature_2m")
hourly_temp <- rawToChar(resp$content) %>% fromJSON() 
time <- hourly_temp[["hourly"]][1]
temp <- hourly_temp[["hourly"]][2]

data <- data.frame(time, temp) %>% head(n = 30)
data %>% 
  ggplot(aes(x = time, y = temperature_2m)) +
  geom_col() +
  labs(
    title = "Hourly Temperature in Hamburg, Germany",
  ) + theme(axis.text.x = element_text(angle = 45, hjust = 1))