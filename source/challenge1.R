library(tidyverse)
library(readxl)
library(lubridate)
library(writexl)

bikes <- read_excel("ds_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines <- read_excel("ds_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops <- read_excel("ds_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

## Wrangle Data

bike_orderlines <- orderlines %>%
  left_join(bikes, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_wrangled <- bike_orderlines %>%
  separate(col = category,
           into = c("category.1","category.2","category.3"),
           sep = " - ") %>%
  separate(col = location,
           into = c("city","state"),
           sep = ", ") %>%
  mutate(total.price = price * quantity) %>%
  select(-...1, -gender)%>%
  select(-ends_with(".id"))%>%
  bind_cols(bike_orderlines %>% select(order.id)) %>%
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# Sales By State
sales_by_state <- bike_orderlines_wrangled %>% 
  select(total_price, state) %>% 
  group_by(state) %>% 
  summarise(sales = sum(total_price)) %>% 
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_state %>% 
  ggplot(aes(x = state, y = sales))+
  geom_col(fill = "#2DC6D6")+
  geom_label(aes(label = sales_text))+
  geom_smooth(method = "lm", se = FALSE)+
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by state",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Sales by State and year
sales_by_state_year <- bike_orderlines_wrangled %>% 
  select(total_price, state, order_date) %>% 
  mutate(year = year(order_date)) %>% 
  group_by(state, year) %>% 
  summarise(sales = sum(total_price)) %>% 
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_state_year %>%
  ggplot(aes(x = year, y = sales, fill = state)) +
  geom_col() +
  facet_wrap(~ state) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and state",
  )