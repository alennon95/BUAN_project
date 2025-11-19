###PART 1# LOAD DATA/LIBRARY'S
supply_chain_df<- read.csv("https://raw.githubusercontent.com/alennon95/BUAN_project/refs/heads/main/supply_chain.csv")
write.csv(supply_chain_df, "supply_chain_raw.csv", row.names = FALSE)

library(sqldf)
library(ggplot2)
library(dplyr)
library(corrplot)
library(tidyr)
library(lubridate)
library(scales)
library(gridExtra)


#COUNT DISTINCT
supply_chain_df %>% 
  summarise(
    total_records= n(),
    distinct_order_id= n_distinct(Order_ID),
            distinct_buyers=n_distinct(Buyer_ID),
            distinct_suppliers = n_distinct(Supplier_ID),
            distinct_organizations=n_distinct(Organization_ID),
            distinct_products=n_distinct(Product_Category))

###PART 2# SPLIT UP INTO THREE TABLES 

#order_df HAS Order_ID, Buyer_ID, Supplier_ID
order_df<- supply_chain_df[,c("Order_ID","Buyer_ID","Supplier_ID","Product_Category",
                           "Quantity_Ordered","Order_Date","Dispatch_Date",
                           "Delivery_Date","Shipping_Mode","Order_Value_USD","Delay_Days",
                           "Disruption_Type","Disruption_Severity","Supply_Risk_Flag")]
#supplier_df HAS Supplier_ID
supplier_df<-supply_chain_df[,c("Supplier_ID","Supplier_Reliability_Score",
                             "Historical_Disruption_Count","Available_Historical_Records",
                             "Data_Sharing_Consent", "Federated_Round","Parameter_Change_Magnitude",
                             "Communication_Cost_MB","Energy_Consumption_Joules")] 
#buyer_df HAS Buyer_ID
buyer_df<-supply_chain_df[,c("Buyer_ID", "Organization_ID","Dominant_Buyer_Flag")] 

###PART 3## CLEAN DATA##
#CHANGE DATES FROM chr TO as.date

order_df$Order_Date <- as.Date(order_df$Order_Date, format = "%Y-%m-%d")
order_df$Dispatch_Date <- as.Date(order_df$Dispatch_Date, format = "%Y-%m-%d")
order_df$Delivery_Date <- as.Date(order_df$Delivery_Date, format = "%Y-%m-%d")


#Create calculated fields
order_df$Days_Until_Delivered <- as.numeric(order_df$Delivery_Date - order_df$Order_Date)
order_df$Days_Until_Dispatch <- as.numeric(order_df$Dispatch_Date - order_df$Order_Date)
order_df$Actual_Transit_Days <- as.numeric(order_df$Delivery_Date - order_df$Dispatch_Date)
ncol(order_df)+ncol(supplier_df)+ncol(buyer_df)

#ON TIME DELIVER FLAG
order_df$On_Time_Flag <- ifelse(order_df$Delay_Days == 0, 1, 0)

#CATEGORICAL TO FACTORS TO HELP WITH GROUP BY & PLOTS
order_df$Product_Category <- as.factor(order_df$Product_Category)
order_df$Shipping_Mode <- as.factor(order_df$Shipping_Mode)
order_df$Disruption_Type <- as.factor(order_df$Disruption_Type)
order_df$Disruption_Severity <- factor(order_df$Disruption_Severity, 
                                       levels = c("None", "Low", "Medium", "High"))

            

#1 SQL QUERY

energy_analysis_1 <- sqldf("
  SELECT Shipping_Mode,
         SUM(Quantity_Ordered) AS sum_quantity_ordered,
         AVG(Energy_Consumption_Joules) AS avg_energy,
         AVG(Energy_Consumption_Joules / Quantity_Ordered) AS energy_per_unit
  FROM supplier_df
  INNER JOIN order_df
    ON order_df.Supplier_ID = supplier_df.Supplier_ID
  GROUP BY Shipping_Mode
  ORDER BY energy_per_unit desc
")
energy_analysis

#1 PLOT
plot_1<- ggplot(energy_analysis_1, aes(x = reorder(Shipping_Mode, -energy_per_unit), 
y = energy_per_unit)) +
geom_col(fill= 'steelblue')+
  geom_text(aes(label = round(energy_per_unit, 1)), vjust = -.5, size = 6) +
  labs(
    title = "Energy Consumption per Unit by Shipping Mode",
    subtitle = "Lower values = more energy efficient",
    x = "Shipping Mode",
    y = "Energy per Unit (Joules)"
  ) +
  theme_minimal()

plot_1

#SQL Query 2

risk_by_mode <- sqldf("
  SELECT Shipping_Mode,
         COUNT(*) AS total_orders,
         SUM(Supply_Risk_Flag) AS total_risk_events,
         SUM(Supply_Risk_Flag)*1.0 / COUNT(*) AS risk_rate
  FROM order_df
  GROUP BY Shipping_Mode
  ORDER BY total_risk_events DESC")
risk_by_mode

#Plot 2
plot_2<- ggplot(risk_by_mode, aes(x = reorder(Shipping_Mode, -risk_rate), y = risk_rate)) +
  geom_col(fill = "red") +
  geom_text(aes(label= scales::percent(risk_rate, accuracy = .1)), 
            vjust=-.5, size= 4 )+
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Supply Risk Rate by Shipping Mode",
    x = "Shipping Mode",
    y = "Risk Rate"
  ) +
  theme_minimal()
plot_2




#SQL Query 3
severity_dist <- sqldf("
  SELECT Shipping_Mode,
         SUM(Disruption_Severity = 'Low')    AS low,
         SUM(Disruption_Severity = 'Medium') AS medium,
         SUM(Disruption_Severity = 'High')   AS high
  FROM order_df
  GROUP BY Shipping_Mode")
severity_dist

# RESHAPE DATA to LONG?
#NEED TO ASK ABOUT THIS TRANSFORMATION
severity_long <- severity_dist %>%
  pivot_longer(cols = c(low, medium, high), 
               names_to = "Severity", 
               values_to = "Count") %>%
  mutate(Severity = factor(Severity, 
                           levels = c("low", "medium", "high"),
                           labels = c("Low", "Medium", "High")))
severity_long

plot_3 <- ggplot(severity_long, aes(x = Shipping_Mode, y = Count, fill = Severity)) +
  geom_col(position = "dodge") + 
  geom_text(aes(label= Count), position = position_dodge(width = .9), 
            vjust= -.5, size= 3)+
  scale_fill_manual(values = c("Low" = "#F4E04D", 
                               "Medium" ="#E8974D", 
                               "High" = "#D95B43"),
                    labels = c("Low", "Medium", "High")) +
  labs(
    title = "Disruption Severity Distribution by Shipping Mode",
    x = "Shipping Mode",
    y = "Number of Disruptions",
    fill = "Severity Level"
  ) +
  theme_minimal()

plot_3




#4 AVG-DELAY INCLUDING DAYS WHERE DELAY=0
max(order_df$Delivery_Date)
min(order_df$Delivery_Date)

#SQL QUERY 4
avg_delay<- sqldf("SELECT 
              Product_Category, Shipping_Mode, AVG(Delay_Days) AS avg_delay_days
              FROM order_df
              GROUP BY Product_Category, Shipping_Mode
              ORDER BY avg_delay_days desc")
avg_delay

#PLOT 4
plot_4 <- ggplot(avg_delay, aes(x = Shipping_Mode, y = Product_Category, 
                                fill = avg_delay_days)) +
  geom_tile() +
  geom_text(aes(label = round(avg_delay_days, 2))) +
  scale_fill_gradient(low = "lightgreen", high = "red") +
  labs(
    title = "Average Delay Days by Product and Shipping Mode",
    x = "Shipping Mode",
    y = "Product Category",
    fill = "Delay (Days)"
  ) +
  theme_minimal()

plot_4




#SQL query 5
supplier_performance<- sqldf("SELECT s.Supplier_ID, s.Supplier_Reliability_Score, 
                             AVG(o.Delay_Days) AS avg_delay_days
                             FROM order_df o
                             JOIN supplier_df s ON o.Supplier_ID= s.Supplier_ID
                             GROUP BY s.Supplier_ID, s.Supplier_Reliability_Score")

supplier_performance <- supplier_performance %>%
  mutate(reliability_group = cut(Supplier_Reliability_Score, 
                                 breaks = 5, # splits into 5 equal groups(increments of .1 starting at .5)
                                 labels = c("Lowest: .5-.6", "Low: .6-.7", "Medium: .7-.8", "High: .8-.9", "Highest: .9-1.0")))

#Plot 5
plot_5 <- ggplot(supplier_performance, aes(x = reliability_group, 
                                           y = avg_delay_days)) +
  geom_violin(fill = "steelblue", alpha = 0.6) +
  geom_boxplot(width = 0.3, fill = "white", alpha = 0.8) +  # ADD THIS - shows median
  stat_summary(fun = mean, geom = "point", shape = 23, 
               size = 10, fill = "red") +  # ADD THIS LINE
  labs(
    title = "Delay Distribution by Supplier Reliability Level",
    x = "Reliability Level",
    y = "Average Delay (Days)"
  ) +
  theme_minimal()

plot_5


#TEST to find reliability score## looks like Reliability score is not strongly correlated with anything
# Select only numeric columns from raw data
numeric_data <- supply_chain_df %>%
  select(where(is.numeric))

# Calculate ALL correlations
all_correlations <- cor(numeric_data, use = "complete.obs")

# Show correlations with Reliability Score
reliability_cors <- all_correlations["Supplier_Reliability_Score", ]
reliability_cors_sorted <- sort(abs(reliability_cors), decreasing = TRUE)

# Display results
print("Correlations with Supplier Reliability Score:")
reliability_cors_sorted
# Create a nice formatted table
correlation_table <- data.frame(
  Variable = names(reliability_cors_sorted),
  Correlation = round(reliability_cors_sorted, 4))
# Show top 10
head(correlation_table, 10)


#find corelation
# Select only numeric columns
numeric_data <- supply_chain_df %>%
  select(where(is.numeric))

# Calculate ALL correlations
all_correlations <- cor(numeric_data, use = "complete.obs")

# Convert correlation matrix to a table format
library(tidyr)

correlation_pairs <- as.data.frame(as.table(all_correlations)) %>%
  rename(Variable1 = Var1, Variable2 = Var2, Correlation = Freq) %>%
  filter(Variable1 != Variable2) %>%  # Remove self-correlations
  mutate(Abs_Correlation = abs(Correlation)) %>%
  arrange(desc(Abs_Correlation))

# Show top 20 strongest correlations
head(correlation_pairs, 20)

# Bar chart of top 15 correlations
top_correlations <- head(correlation_pairs, 15)

top_correlations <- top_correlations %>%
  mutate(Pair = paste(Variable1, "vs", Variable2))

ggplot(top_correlations, aes(x = reorder(Pair, Abs_Correlation), 
                             y = Correlation)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 15 Strongest Correlations in Dataset",
    x = "Variable Pair",
    y = "Correlation Coefficient"
  ) +
  theme_minimal()

# Visual correlation matrix
corrplot(all_correlations, 
         method = "color", 
         type = "upper",
         tl.cex = 0.7,
         tl.col = "black",
         title = "Correlation Matrix: All Numeric Variables")







#6

# 1. Join orders and suppliers (using sqldf)
delay_energy <- sqldf("
  SELECT o.Delay_Days,
         s.Energy_Consumption_Joules
  FROM order_df o
  JOIN supplier_df s
    ON o.Supplier_ID = s.Supplier_ID
")

# 2. Correlation in R
cor(delay_energy$Delay_Days,
    delay_energy$Energy_Consumption_Joules,
    use = 'complete.obs')

revenue_by_category <- sqldf("
  SELECT Product_Category,
         COUNT(*) AS total_orders,
         SUM(Order_Value_USD) AS total_revenue,
         AVG(Order_Value_USD) AS avg_order_value
  FROM order_df
  GROUP BY Product_Category
  ORDER BY total_revenue DESC
")
revenue_by_category


top_disruptive_suppliers <- sqldf("
  SELECT s.Supplier_ID,
         s.Historical_Disruption_Count,
         s.Supplier_Reliability_Score,
         COUNT(o.Order_ID) AS total_orders
  FROM supplier_df s
  LEFT JOIN order_df o ON s.Supplier_ID = o.Supplier_ID
  GROUP BY s.Supplier_ID, s.Historical_Disruption_Count, s.Supplier_Reliability_Score
  ORDER BY s.Historical_Disruption_Count DESC
  LIMIT 10
")
top_disruptive_suppliers

model_data <- sqldf("
  SELECT o.Delay_Days,
         o.Quantity_Ordered,
         o.Order_Value_USD,
         s.Energy_Consumption_Joules,
         s.Supplier_Reliability_Score
  FROM order_df o
  JOIN supplier_df s ON o.Supplier_ID = s.Supplier_ID
")

delay_model <- lm(Delay_Days ~ Energy_Consumption_Joules + 
                    Supplier_Reliability_Score + 
                    Quantity_Ordered, 
                  data = model_data)
delay_model













#### NOAH's code####

# Added two new columns describing days until the Order was dispatched
# and days until the order was delivered as numerics to make the date 
# columns more useful. Also checked distribution for outliers


Clean_df_date <- supply_chain_df %>% 
  mutate(Days_Until_Delivered = as.numeric(ymd(Delivery_Date) - ymd(Order_Date)),
         Days_Until_Dispatch = as.numeric(ymd(Dispatch_Date) - ymd(Order_Date)))
Clean_df_date

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

ggplot(supply_chain_df, aes(x = factor(Dominant_Buyer_Flag), y = Supplier_Reliability_Score)) + 
  geom_violin(fill = "lightblue", alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) 



ggplot(supply_chain_df, aes(x = Shipping_Mode, y = Supplier_Reliability_Score)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen", alpha = 0.6) +
  labs(
    title = "Supplier Reliability by Shipping Mode",
    x = "Shipping Mode",
    y = "Supplier Reliability Score"
  ) +
  theme_minimal()

ggplot(supply_chain_df, aes(x = Shipping_Mode, y = Supplier_Reliability_Score)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  facet_wrap(~Dominant_Buyer_Flag) +
  labs(
    title = "Reliability by Shipping Mode and Buyer Dominance",
    x = "Shipping Mode",
    y = "Supplier Reliability Score %"
  ) +
  theme_minimal()

ggplot(supply_chain_df, aes(x = Shipping_Mode, y = Order_Value_USD)) +
  geom_boxplot(fill = "skyblue", alpha = 0.6, outlier.shape = NA) +  # boxplot without double outliers
  geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +         # adds scattered points
  labs(
    title = "Order Value Distribution by Shipping Mode",
    x = "Shipping Mode",
    y = "Order Value (USD)"
  ) +
  theme_minimal()

ggplot(supply_chain_df, aes(x = reorder(Shipping_Mode, Order_Value_USD, FUN = median), 
               y = Order_Value_USD)) +
  geom_violin(fill = "lightblue", alpha = 0.6, trim = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
  labs(
    title = "Order Value by Shipping Mode (Violin Plot with Mean)",
    x = "Shipping Mode",
    y = "Order Value (USD)"
  ) +
  theme_minimal()





