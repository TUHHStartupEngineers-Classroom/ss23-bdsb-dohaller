library(tidyverse)

# Import Data
covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

#Prepare Data
data <- covid_data_tbl %>% 
  select(location, total_deaths) %>% 
  group_by(location) %>% 
  slice_max(order_by = total_deaths, n = 1) %>% 
  mutate(location = case_when(
    
    location == "United Kingdom" ~ "UK",
    location == "United States" ~ "USA",
    location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    TRUE ~ location
    
  )) %>%
  distinct()
#Remove unneeded rows
data <- data[!grepl("World", data$location),]
data <- data[!grepl("income", data$location),]
data <- data[!grepl("Europe", data$location),]
data <- data[!grepl("North America", data$location),]
data <- data[!grepl("South America", data$location),]
data <- data[!grepl("Asia", data$location),]

#Import map
world <- map_data("world")

p <- ggplot() +
  geom_map(data = world, map = world,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f", linewidth = 0.5) +
  geom_map(data = data, map=world,
           aes(fill=total_deaths, map_id=location, color = total_deaths),
           colour="#7f7f7f", size=0.5)  +
  scale_fill_continuous(low="red", high="black", guide="colorbar") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="legend", title="Covid Mortality", x="", y="") +
  theme_bw()

p