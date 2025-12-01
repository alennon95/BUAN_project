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

#Create new columns
order_df$Delivery_Date <- as.Date(order_df$Delivery_Date, format = "%Y-%m-%d")
order_df$order_month<- format(order_df$Order_Date, "%Y-%m")


#Create calculated fields
order_df$Days_Until_Delivered <- as.numeric(order_df$Delivery_Date - order_df$Order_Date)
order_df$Days_Until_Dispatch <- as.numeric(order_df$Dispatch_Date - order_df$Order_Date)
order_df$Actual_Transit_Days <- as.numeric(order_df$Delivery_Date - order_df$Dispatch_Date)
ncol(order_df)+ncol(supplier_df)+ncol(buyer_df)
order_df$Order_Quarter <- quarters(order_df$Order_Date)


#ON TIME DELIVER FLAG
order_df$On_Time_Flag <- ifelse(order_df$Delay_Days == 0, 1, 0)

#CATEGORICAL TO FACTORS TO HELP WITH GROUP BY & PLOTS
order_df$Product_Category <- as.factor(order_df$Product_Category)
order_df$Shipping_Mode <- as.factor(order_df$Shipping_Mode)
order_df$Disruption_Type <- as.factor(order_df$Disruption_Type)
order_df$Disruption_Severity <- factor(order_df$Disruption_Severity, 
                                       levels = c("None", "Low", "Medium", "High"))

## QUERY 1: Energy Analysis by Shipping Mode
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

# PLOT 1: Energy Efficiency by Shipping Mode
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

# QUERY 2: Supply Risk by Shipping Mode
risk_by_mode <- sqldf("
  SELECT Shipping_Mode,
         COUNT(*) AS total_orders,
         SUM(Supply_Risk_Flag) AS total_risk_events,
         SUM(Supply_Risk_Flag)*1.0 / COUNT(*) AS risk_rate
  FROM order_df
  GROUP BY Shipping_Mode
  ORDER BY total_risk_events DESC")

# PLOT 2: Risk Rate by Shipping Mode
plot_2<- ggplot(risk_by_mode, aes(x = reorder(Shipping_Mode, -risk_rate), y = risk_rate)) +
  geom_col(fill = "red") +
  geom_text(aes(label= scales::percent(risk_rate, accuracy = .1)), 
            vjust=-.5, size= 4 )+
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Supply Risk Rate by Shipping Mode",
    x = "Shipping Mode",
    y = "Risk Rate") +
  theme_minimal()
plot_2


# QUERY 3: Disruption Severity Distribution
severity_dist <- sqldf("
  SELECT Shipping_Mode,
         SUM(Disruption_Severity = 'Low')    AS low,
         SUM(Disruption_Severity = 'Medium') AS medium,
         SUM(Disruption_Severity = 'High')   AS high
  FROM order_df
  GROUP BY Shipping_Mode")
severity_dist

## Reshape data for plotting(long)
severity_long <- severity_dist %>%
  pivot_longer(cols = c(low, medium, high), 
               names_to = "Severity", 
               values_to = "Count") %>%
  mutate(Severity = factor(Severity, 
                           levels = c("low", "medium", "high"),
                           labels = c("Low", "Medium", "High")))
severity_long

# PLOT 3: Disruption Severity by Shipping Mode
plot_3 <- ggplot(severity_long, aes(x = Shipping_Mode, y = Count, fill = Severity)) +
  geom_col(position = "dodge") + 
  geom_text(aes(label= Count), position = position_dodge(width = .9), 
            vjust= -.5, size= 3)+
  scale_fill_manual(values = c("Low" = "#F4E04D", 
                               "Medium" ="#E8974D", 
                               "High" = "#D95B43"),
                    labels = c("Low", "Medium", "High")) +
  labs(title = "Disruption Severity Distribution by Shipping Mode",
    x = "Shipping Mode",
    y = "Number of Disruptions",
    fill = "Severity Level") +
  theme_minimal()
plot_3






# QUERY 4: Average Delays by Product and Shipping Mode
avg_delay<- sqldf("SELECT 
              Product_Category, Shipping_Mode, AVG(Delay_Days) AS avg_delay_days
              FROM order_df
              GROUP BY Product_Category, Shipping_Mode
              ORDER BY avg_delay_days desc")
avg_delay

# PLOT 4: Heatmap of Delays
plot_4 <- ggplot(avg_delay, aes(x = Shipping_Mode, y = Product_Category, 
                                fill = avg_delay_days)) +
  geom_tile() +
  geom_text(aes(label = round(avg_delay_days, 2))) +
  scale_fill_gradient(low = "lightgreen", high = "red") +
  labs(title = "Average Delay Days by Product and Shipping Mode",
    x = "Shipping Mode",
    y = "Product Category",
    fill = "Delay (Days)") +
  theme_minimal()

plot_4

# QUERY 5: Top 10 Worst Delay Combinations
top_worst_delays <- sqldf("
  SELECT Product_Category, Shipping_Mode, Disruption_Type,
         AVG(Delay_Days) AS avg_delay,
         COUNT(*) AS occurrences
  FROM order_df
  WHERE Disruption_Type != 'None'
  GROUP BY Product_Category, Shipping_Mode, Disruption_Type
  ORDER BY avg_delay DESC
  LIMIT 10
")
top_worst_delays

# PLOT 5: Top 10 Worst Delay Scenarios
plot_5 <- ggplot(top_worst_delays, 
                  aes(x = reorder(paste(Product_Category, Shipping_Mode, Disruption_Type, sep = " + "), 
                                  avg_delay),
                      y = avg_delay)) +
  geom_col(aes(fill = Disruption_Type)) +
  geom_text(aes(label = round(avg_delay, 1)), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(title = "Top 10 Worst Delay Scenarios",
       x = "Product + Shipping + Disruption",
       y = "Average Delay (Days)") +
  theme_minimal()
plot_5

#QUERY 6 Supplier Performance by Reliability Level
supplier_performance<- sqldf("SELECT s.Supplier_ID, s.Supplier_Reliability_Score, 
                             AVG(o.Delay_Days) AS avg_delay_days
                             FROM order_df o
                             JOIN supplier_df s ON o.Supplier_ID= s.Supplier_ID
                             GROUP BY s.Supplier_ID, s.Supplier_Reliability_Score")

# Create reliability groups
supplier_performance <- supplier_performance %>%
  mutate(reliability_group = cut(Supplier_Reliability_Score, 
                                 breaks = 5, # splits into 5 equal groups(increments of .1 starting at .5)
                                 labels = c("Lowest: .5-.6", "Low: .6-.7", "Medium: .7-.8", "High: .8-.9", "Highest: .9-1.0")))

#Plot 6 Delay Distribution by Reliability Level

plot_6 <- ggplot(supplier_performance, aes(x = reliability_group, 
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

plot_6

# QUERY 7: Monthly Trends by Shipping Mode
monthly_trends_mode <- sqldf("
  SELECT order_month,
         Shipping_Mode,
         COUNT(*) AS total_orders,
         AVG(Delay_Days) AS avg_delay
  FROM order_df
  GROUP BY order_month, Shipping_Mode
  ORDER BY order_month
")

# PLOT 7: Monthly delay trends faceted by shipping mode
plot_7<- ggplot(monthly_trends_mode, aes(x = order_month, y = avg_delay, 
                                          group = Shipping_Mode, color = Shipping_Mode)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Shipping_Mode, ncol = 2) +
  labs(title = "Monthly Delay Trends by Shipping Mode",
       x = "Month",
       y = "Average Delay (Days)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
plot_7

# QUERY 8: Revenue Analysis by Product Category
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

# PLOT 8: Total revenue by product category
plot_8 <- ggplot(revenue_by_category, 
                 aes(x = reorder(Product_Category, -total_revenue), 
                     y = total_revenue)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = paste0("$", round(total_revenue/1000000, 2), "M")), 
            vjust = -0.5, size = 4) +
  labs(title = "Total Revenue by Product Category",
       x = "Product Category",
       y = "Total Revenue (USD)") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal()
plot_8

# QUERY 9: Top 10 Most Disruptive Suppliers
top_disruptive_suppliers <- sqldf("
  SELECT DISTINCT s.Supplier_ID,
         s.Historical_Disruption_Count,
         s.Supplier_Reliability_Score,
         COUNT(o.Order_ID) AS total_orders
  FROM supplier_df s
  LEFT JOIN order_df o ON s.Supplier_ID = o.Supplier_ID
  GROUP BY s.Supplier_ID
  ORDER BY s.Historical_Disruption_Count DESC
  LIMIT 10
")
top_disruptive_suppliers

# Plot 9: Scatter plot - disruptions vs reliability
plot_9 <- ggplot(top_disruptive_suppliers, 
                 aes(x = Historical_Disruption_Count, 
                     y = Supplier_Reliability_Score)) +
  geom_point(size = 4, color = "red") +
  geom_text(aes(label = Supplier_ID), vjust = -1, size = 3) +
  labs(title = "Top 10 Most Disruptive Suppliers: Disruptions vs Reliability",
       x = "Historical Disruption Count",
       y = "Reliability Score") +
  theme_minimal()
plot_9

# QUERY 10: Order Value by Shipping Mode
order_value_stats <- sqldf("
  SELECT Shipping_Mode,
         AVG(Order_Value_USD) AS avg_value,
         MIN(Order_Value_USD) AS min_value,
         MAX(Order_Value_USD) AS max_value,
         COUNT(*) AS order_count
  FROM order_df
  GROUP BY Shipping_Mode
  ORDER BY avg_value DESC
")

plot_10 <- ggplot(order_df, aes(x = reorder(Shipping_Mode, Order_Value_USD, FUN = median), 
                               y = Order_Value_USD)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Order Value Distribution by Shipping Mode",
       x = "Shipping Mode",
       y = "Order Value (USD)") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal()

plot_10

# QUERY 11: Delay Distribution
delay_distribution <- sqldf("
  SELECT Delay_Days
  FROM order_df
")

plot_11 <- ggplot(delay_distribution, aes(x = Delay_Days)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  geom_vline(aes(xintercept = mean(Delay_Days)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of Delivery Delays",
       subtitle = paste("Mean delay:", round(mean(delay_distribution$Delay_Days), 2), "days"),
       x = "Delay (Days)",
       y = "Density") +
  theme_minimal()
plot_11

# QUERY 12: Delivery Time Distribution by Shipping Mode
delivery_times <- sqldf("
  SELECT Days_Until_Delivered, Shipping_Mode
  FROM order_df
")

# PLOT 12: Histogram of total delivery time, faceted by shipping mode
plot_12 <- ggplot(delivery_times, aes(x = Days_Until_Delivered)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "black") +
  facet_wrap(~ Shipping_Mode, ncol = 2) +
  labs(title = "Distribution of Total Delivery Time by Shipping Mode",
       x = "Days Until Delivered",
       y = "Frequency") +
  theme_minimal()

plot_12

# QUERY 13: Dispatch Time Distribution by Shipping Mode
dispatch_times <- sqldf("
  SELECT Days_Until_Dispatch, Shipping_Mode
  FROM order_df
")

# PLOT 13: Histogram of dispatch time, faceted by shipping mode
plot_13 <- ggplot(dispatch_times, aes(x = Days_Until_Dispatch)) +
  geom_histogram(bins = 15, fill = "coral", color = "black") +
  facet_wrap(~ Shipping_Mode, ncol = 2) +
  labs(title = "Distribution of Dispatch Time by Shipping Mode",
       x = "Days Until Dispatch",
       y = "Frequency") +
  theme_minimal()
plot_13

#TEST to find reliability score## looks like Reliability score is not strongly correlated with anything
# Select only numeric columns from raw data
numeric_data <- supply_chain_df %>%
  select(where(is.numeric))

# Calculate ALL correlations. # Delay_Days & Supply_Risk_Flag: r = 0.66 ← STRONGEST
all_correlations <- cor(numeric_data, use = "complete.obs")

# Extract correlations with Supplier_Reliability_Score
reliability_cors <- all_correlations["Supplier_Reliability_Score", ]
reliability_cors_sorted <- sort(abs(reliability_cors), decreasing = TRUE)

# Format as table
correlation_table <- data.frame(
  Variable = names(reliability_cors_sorted),
  Correlation = round(reliability_cors_sorted, 4))

head(correlation_table, 10)

# Show correlations with Reliability Score
reliability_cors <- all_correlations["Supplier_Reliability_Score", ]
reliability_cors_sorted <- sort(abs(reliability_cors), decreasing = TRUE)
reliability_cors
# Display results
print("Correlations with Supplier Reliability Score:")
reliability_cors_sorted
# Create a nice formatted table
correlation_table <- data.frame(
  Variable = names(reliability_cors_sorted),
  Correlation = round(reliability_cors_sorted, 4))
# Show top 10
head(correlation_table, 10)


#find correlation# Select only numeric columns
numeric_data <- supply_chain_df %>%
  select(where(is.numeric))

# Calculate ALL correlations
all_correlations <- cor(numeric_data, use = "complete.obs")

# Convert correlation matrix to a table format
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







 #Multiple predictors together?
combined_model <- lm(Delay_Days ~ Disruption_Severity + Shipping_Mode + Product_Category, 
                     data = order_df)
summary(combined_model)
confint(combined_model)


# Does transit time predict delays?
transit_model <- lm(Delay_Days ~ Actual_Transit_Days, data = order_df)
summary(transit_model)
confint(transit_model)

# Does historical disruption count predict supplier reliability score? NO
reliability_model <- lm(Supplier_Reliability_Score ~ Historical_Disruption_Count, 
                        data = supplier_df)
summary(reliability_model)
confint(reliability_model)

# Test 1: Is it just inverse of disruption count?
cor(supplier_df$Supplier_Reliability_Score, supplier_df$Historical_Disruption_Count)

# Test 2: Is it related to any other metrics?
cor(supplier_df$Supplier_Reliability_Score, supplier_df$Energy_Consumption_Joules)
cor(supplier_df$Supplier_Reliability_Score, supplier_df$Communication_Cost_MB)
cor(supplier_df$Supplier_Reliability_Score, supplier_df$Parameter_Change_Magnitude)

# Test 3: Check all correlations with reliability
reliability_cors

# Add this to end of your code
write.csv(order_df, "supply_chain_clean.csv", row.names = FALSE)























