library(tidyverse)
library(ggplot2)
library(sqldf)
install.packages("stringr")
library(stringr)
library(dplyr)
library(lubridate)
library(sqldf)

df2<- read.csv("/Users/boca11_/Desktop/DataCoSupplyChainDataset.csv")

#cleaning columns of null variables, removing cols with high null count

colSums(is.na(df2))

df2$Product.Description <- NULL
df2$Order.Zipcode <- NULL
df_final <- na.omit(df2)
nrow(df_final)


#changing periods to underscores

df_final <- df_final %>%
  rename_with(~ gsub(".", "_", .x, fixed = TRUE))

names(df_final) <- gsub("__", "_", names(df_final))
names(df_final) <- gsub("_$", "", names(df_final))

colnames(df_final)

#converting months to date objects

df_final$shipping_date_DateOrders <- mdy_hm(df_final$shipping_date_DateOrders)
df_final$shipping.date.DateOrders.<- NULL
df_final$shipping_date_DateOrders. <- NULL

str(df_final)

#checking for impossible variables and removing outliers using histograms
library(ggplot2)

ggplot(df_final, aes(x = Product_Price)) +
  geom_histogram()
df_1 <- subset(df_final, Product_Price<1000)
ggplot(df_1, aes(x = Product_Price)) +
  geom_histogram()

ggplot(df_final, aes(x = Sales_per_customer)) +
  geom_histogram()
df_2 <- subset(df_1, Sales_per_customer<1000)
ggplot(df_2, aes(x = Sales_per_customer)) +
  geom_histogram()

ggplot(df_2, aes(Order_Item_Discount)) +
  geom_histogram()
df_3 <- subset(df_2, Order_Item_Discount<100)
ggplot(df_3, aes(Order_Item_Discount)) +
  geom_histogram()

ggplot(df_3, aes(Order_Item_Product_Price)) +
  geom_histogram()
ggplot(df_3, aes(Sales)) +
  geom_histogram()
ggplot(df_3, aes(Order_Item_Total)) +
  geom_histogram()
ggplot(df_3, aes(Order_Item_Quantity)) +
  geom_histogram()
ggplot(df_3, aes(Days_for_shipping_real)) +
  geom_histogram()

df_final <- df_3

#removing some columns that are not necessary 

df_clean <- sqldf("
  SELECT 
    Order_Id,
    Type,
    Sales,
    Benefit_per_order,
    Order_Item_Quantity,
    Product_Price,
    order_date_DateOrders,
    shipping_date_DateOrders,
    Days_for_shipping_real,
    Delivery_Status,
    Category_Name,
    Department_Name,
    Market,
    Order_Region,
    Order_Country,
    Order_City,
    Late_delivery_risk,
    Shipping_Mode,
    Customer_State,
    Days_for_shipment_scheduled,
    Order_Status,
    Order_Item_Discount_Rate,
    Product_Name
  FROM df_final
")

#Checking is fater shipping methods are actually faster

ship_performance <- sqldf("
  SELECT 
    Shipping_Mode,
    AVG(Days_for_shipping_real) as Avg_Real_Days,
    AVG(Days_for_shipment_scheduled) as Avg_Scheduled_Days,
    COUNT(*) as Total_Orders
  FROM df_clean
  WHERE Order_Status = 'COMPLETE' -- Only look at completed orders
  GROUP BY Shipping_Mode
")
ship_performance

#second class shipping is very delayed, visualizing the relationship across regions

second_class_issues <- sqldf("
  SELECT 
    Order_Region,
    AVG(Days_for_shipping_real) as Avg_Real_Days,
    AVG(Days_for_shipment_scheduled) as Avg_Target,
    (AVG(Days_for_shipping_real) - AVG(Days_for_shipment_scheduled)) as Delay_Gap,
    COUNT(*) as Order_Volume
  FROM df_clean
  WHERE Shipping_Mode = 'Second Class' 
  AND Order_Status = 'COMPLETE'
  GROUP BY Order_Region
  ORDER BY Delay_Gap DESC
")

second_class_issues

ggplot(second_class_issues, aes(x = reorder(Order_Region, Delay_Gap), y = Delay_Gap)) +
  geom_segment(aes(xend = Order_Region, yend = 0), color = "gray") +
  geom_point(size = 4, color = "darkred") +
  coord_flip() + 
  labs(title = "Where is 'Second Class' Shipping Failing?",
       subtitle = "Gap between Actual and Scheduled Days (Positive = Late)",
       y = "Avg Days Late", x = "Region") +
  theme_minimal()

#is this causing more returns?

unique(df_clean$Order_Status)

#checking for the 80/20 rule, a common assumption in supply chain

unique(df_final$Customer_Segment)
customer_value <- sqldf("
  SELECT 
    Customer_Segment,
    SUM(Sales) as Total_Sales,
    COUNT(Order_Id) as Total_Orders,
    SUM(Benefit_per_order) as Total_Profit
  FROM df_final
  GROUP BY Customer_Segment
  ORDER BY Total_Sales DESC
")
customer_value

ggplot(customer_value, aes(x = Customer_Segment, y = Total_Sales, fill = Total_Profit)) +
  geom_col() +
  labs(title = "Sales Volume by Customer Segment", 
       y = "Total Sales ($)", x = "Segment") 



