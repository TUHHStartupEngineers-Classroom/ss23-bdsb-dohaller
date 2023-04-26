library(tidyverse)

# Import Data
covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
countries <- covid_data_tbl %>%
  group_by(location) %>% 
  group_split(.keep = TRUE)

#Create Filter 
filter <- c("Germany", "United States", "United Kingdom", "France", "Spain")


getCountry <- function(x, country){
  countries_select <- data.frame()
  for(i in 1:length(x)){
    if(x[[i]]$location[[1]] %in% country){
      return (x[[i]])
    } else {
      next
    }
  }
  return (countries_select)
}
#Grab Countries
selection <- getCountry(countries, c("Germany")) %>% 
  rbind(getCountry(countries, c("United States"))) %>% 
  rbind(getCountry(countries, c("United Kingdom"))) %>% 
  rbind(getCountry(countries, c("France"))) %>% 
  rbind(getCountry(countries, c("Spain")))
 
selection %>% 
  ggplot(aes(date, total_cases, color = location))+
  geom_line(size = 1) + 
  scale_y_continuous(labels = scales::comma) +
  theme(
    legend.position = "bottom",
  ) + 
  labs(
    title = "Total Covid Cases by country",
    subtitle = "Sales Trending Upward",
    x = "",
    y = "Total Cases",
    color = "Country",
  )

#

