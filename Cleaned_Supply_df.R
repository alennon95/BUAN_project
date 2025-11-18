library(tidyverse)
library(lubridate)
library(sqldf)

df <- read.csv("/Users/boca11_/Downloads/supply_chain.csv")
str(df)
View(df)

# Added two new columns describing days until the Order was dispatched
# and days until the order was delivered as numerics to make the date 
# columns more useful. Also checked distribution for outliers


Clean_df_date <- df %>% 
  mutate(Days_Until_Delivered = as.numeric(ymd(Delivery_Date) - ymd(Order_Date)),
         Days_Until_Dispatch = as.numeric(ymd(Dispatch_Date) - ymd(Order_Date)))


ggplot(Clean_df_date, aes(Days_Until_Delivered)) +
  geom_histogram() +
  facet_wrap(~Shipping_Mode)

ggplot(Clean_df_date, aes(Days_Until_Dispatch)) +
  geom_histogram() +
  facet_wrap(~Shipping_Mode)

# Changed categorical variables to factors, this will make exploratory analysis
# easier. For severity, I used the levels function to specify orders

Clean_df_factor <- Clean_df_date %>%
  mutate(Product_Category = as.factor(Product_Category),
    Shipping_Mode = as.factor(Shipping_Mode),
    Disruption_Type = as.factor(Disruption_Type),
    Disruption_Severity = factor(Disruption_Severity, 
                                 levels = c("None", "Low", "Medium", "High")))

# Added binary column "On_Time_Flag", basically making more use of the 
# Delay_Days column. We can use this column to meausure the OTD rate by supplier
# later on if we find this meaningful.


Clean_df_OTF <- Clean_df_factor %>%
  mutate(On_Time_Flag = ifelse(Delay_Days == 0, 1, 0))

str(Clean_df_factor)


# Added time_in_transit, column describing the actual time the order 
#was in transit

Clean_df_transit <- Clean_df_factor %>%
  mutate(Actual_Transit_Days = Days_Until_Delivered - Days_Until_Dispatch)

# removed the unnecessary columns that are redundant after cleaning.
# But, keeping order date may be important to visualize seasonality

Clean_df_stripped <- Clean_df_transit %>%
  select(-Dispatch_Date, -Delivery_Date)


#finding the correlation between all numeric columns and the Supply Risk 
# Flag column

numeric_cols <- Clean_df_stripped %>%
  select(where(is.numeric))

risk_correlations <- cor(numeric_cols, use = "complete.obs")["Supply_Risk_Flag", ]
sort(risk_correlations, decreasing = TRUE)



#just some random visualizations to consider
df1 <- df %>%
  filter(Dominant_Buyer_Flag == 1)

ggplot(df, aes(x = factor(Dominant_Buyer_Flag), y = Supplier_Reliability_Score)) + 
  geom_violin(fill = "lightblue", alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) 



ggplot(df, aes(x = Shipping_Mode, y = Supplier_Reliability_Score)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen", alpha = 0.6) +
  labs(
    title = "Supplier Reliability by Shipping Mode",
    x = "Shipping Mode",
    y = "Supplier Reliability Score"
  ) +
  theme_minimal()

ggplot(df, aes(x = Shipping_Mode, y = Supplier_Reliability_Score)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  facet_wrap(~Dominant_Buyer_Flag) +
  labs(
    title = "Reliability by Shipping Mode and Buyer Dominance",
    x = "Shipping Mode",
    y = "Supplier Reliability Score %"
  ) +
  theme_minimal()

ggplot(df, aes(x = Shipping_Mode, y = Order_Value_USD)) +
  geom_boxplot(fill = "skyblue", alpha = 0.6, outlier.shape = NA) +  # boxplot without double outliers
  geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +         # adds scattered points
  labs(
    title = "Order Value Distribution by Shipping Mode",
    x = "Shipping Mode",
    y = "Order Value (USD)"
  ) +
  theme_minimal()

ggplot(df, aes(x = reorder(Shipping_Mode, Order_Value_USD, FUN = median), 
               y = Order_Value_USD)) +
  geom_violin(fill = "lightblue", alpha = 0.6, trim = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
  labs(
    title = "Order Value by Shipping Mode (Violin Plot with Mean)",
    x = "Shipping Mode",
    y = "Order Value (USD)"
  ) +
  theme_minimal()


