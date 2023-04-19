# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)
library(lubridate)
library(writexl)
# 2.0 Importing Files ----
bikes <- read_excel("ds_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines <- read_excel("ds_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops <- read_excel("ds_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----
glimpse(orderlines)

# 4.0 Joining Data ----
bike_orderlines <- orderlines %>%
  left_join(bikes, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops, by = c("customer.id" = "bikeshop.id"))

glimpse(bike_orderlines)
# 5.0 Wrangling Data ----
bike_orderlines_wranged <- bike_orderlines %>%
  separate(col = category,
           into = c("category.1","category.2","category.3"),
           sep = " - ") %>%
  mutate(total.price = price * quantity) %>%
  select(-...1, -gender)%>%
  select(-ends_with(".id"))%>%
  bind_cols(bike_orderlines %>% select(order.id)) %>%
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# 6.0 Business Insights ----
# 6.1 Sales by Year ----

# Step 1 - Manipulate
sales_by_year <- bike_orderlines_wranged %>%
  select(order_date, total_price)%>%
  mutate(year = year(order_date))%>%
  group_by(year) %>%
  summarize(sales = sum(total_price)) %>% 
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# Step 2 - Visualize
sales_by_year %>% 
  ggplot(aes(x = year, y = sales))+
  geom_col(fill = "#2DC6D6")+
  geom_label(aes(label = sales_text))+
  geom_smooth(method = "lm", se = FALSE)+
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

# 6.2 Sales by Year and Category 2 ----
# Step 1 - Manipulate

sales_by_year_cat <- bike_orderlines_wranged %>% 
  select(order_date, total_price, category_1) %>%
  mutate(year = year(order_date)) %>%
  group_by(year, category_1) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# Step 2 - Visualize
sales_by_year_cat %>%
  ggplot(aes(x = year, y = sales, fill = category_1)) +
  geom_col() +
  facet_wrap(~ category_1) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )


# 7.0 Writing Files ----

# 7.1 Excel ----
bike_orderlines_wranged %>%
  write_xlsx("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")

# 7.2 CSV ----
bike_orderlines_wranged %>% 
  write_csv("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")
# 7.3 RDS ----
bike_orderlines_wranged %>% 
  write_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")
