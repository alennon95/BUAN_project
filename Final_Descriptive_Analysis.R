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

###PART 3## CLEAN DATA##
#CHANGE DATES FROM chr TO as.date
supply_chain_df$Order_Date <- as.Date(supply_chain_df$Order_Date, format = "%Y-%m-%d")
supply_chain_df$Dispatch_Date <- as.Date(supply_chain_df$Dispatch_Date, format = "%Y-%m-%d")

#Create new columns
supply_chain_df$Delivery_Date <- as.Date(supply_chain_df$Delivery_Date, format = "%Y-%m-%d")
supply_chain_df$order_month<- format(supply_chain_df$Order_Date, "%Y-%m")


#Create calculated fields
supply_chain_df$Days_Until_Delivered <- as.numeric(supply_chain_df$Delivery_Date - supply_chain_df$Order_Date)
supply_chain_df$Days_Until_Dispatch <- as.numeric(supply_chain_df$Dispatch_Date - supply_chain_df$Order_Date)
supply_chain_df$Actual_Transit_Days <- as.numeric(supply_chain_df$Delivery_Date - supply_chain_df$Dispatch_Date)
supply_chain_df$Order_Quarter <- quarters(supply_chain_df$Order_Date)


#ON TIME DELIVER FLAG
supply_chain_df$On_Time_Flag <- ifelse(supply_chain_df$Delay_Days == 0, 1, 0)

#CATEGORICAL TO FACTORS TO HELP WITH GROUP BY & PLOTS
supply_chain_df$Product_Category <- as.factor(supply_chain_df$Product_Category)
supply_chain_df$Shipping_Mode <- as.factor(supply_chain_df$Shipping_Mode)
supply_chain_df$Disruption_Type <- as.factor(supply_chain_df$Disruption_Type)
supply_chain_df$Disruption_Severity <- factor(supply_chain_df$Disruption_Severity, 
                                              levels = c("None", "Low", "Medium", "High"))
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
order_df <- supply_chain_df[, c("Order_ID", "Buyer_ID", "Supplier_ID", "Product_Category",
                                "Quantity_Ordered", "Order_Date", "Dispatch_Date",
                                "Delivery_Date", "Shipping_Mode", "Order_Value_USD", "Delay_Days",
                                "Disruption_Type", "Disruption_Severity", "Supply_Risk_Flag")]
#supplier_df HAS Supplier_ID
supplier_df <- supply_chain_df %>%
  select(Supplier_ID, Supplier_Reliability_Score,
         Historical_Disruption_Count, Available_Historical_Records,
         Data_Sharing_Consent, Federated_Round, Parameter_Change_Magnitude,
         Communication_Cost_MB, Energy_Consumption_Joules)

### Adding dataframes that groups by Supplier_ID and Order_ID, 

supplier_df_grp <- sqldf(
  "SELECT Supplier_ID, AVG(Supplier_Reliability_Score) AS Avg_Reliability,
         AVG(Historical_Disruption_Count) AS avg_disruption_count, Available_Historical_Records,
         Data_Sharing_Consent, Federated_Round, Parameter_Change_Magnitude,
         AVG(Communication_Cost_MB) AS avg_communication_cost
  FROM supply_chain_df
    GROUP BY Supplier_ID")

# 3. Buyer Table (Dimension Data - MUST BE DISTINCT)
buyer_df_grp <- supply_chain_df %>%
  select(Buyer_ID, Organization_ID, Dominant_Buyer_Flag) %>%
  distinct(Buyer_ID, .keep_all = TRUE)    # <--- THIS IS THE KEY FIX
#buyer_df HAS Buyer_ID
buyer_df<-supply_chain_df[,c("Buyer_ID", "Organization_ID","Dominant_Buyer_Flag")] 



#Checking Distributions
ggplot(supply_chain_df, aes(Supply_Risk_Flag)) +
  geom_histogram()
ggplot(supply_chain_df, aes(Supplier_Reliability_Score)) +
  geom_histogram()

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
energy_analysis_1

# PLOT 1: Energy Efficiency by Shipping Mode
plot_1 <- ggplot(energy_analysis_1, aes(x = reorder(Shipping_Mode, -energy_per_unit), 
                                        y = energy_per_unit)) +
  geom_col(fill= 'steelblue')+
  geom_text(aes(label = round(energy_per_unit, 1)), vjust = -.5, size = 6) +
  labs(
    title = "Energy Consumption per Unit by Shipping Mode",
    subtitle = "Lower values = more energy efficient",
    x = "Shipping Mode",
    y = "Energy per Unit (Joules)"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal()
plot_1

####Analysis reveals Air shipping is the most energy-efficient mode at 1.2 
### Joules per unit, 25% more efficient than Sea freight (1.6 J/unit). This 
### finding can inform sustainability initiatives and shipping mode optimization


# QUERY 2: Disruption Severity Distribution
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

# PLOT 2: Disruption Severity by Shipping Mode
plot_2 <- ggplot(severity_long, aes(x = Shipping_Mode, y = Count, fill = Severity)) +
  geom_col(position = "stack") + 
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5), 
            size = 3.5, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("Low" = "#F4E04D", 
                               "Medium" = "#E8974D", 
                               "High" = "#D95B43")) +
  labs(title = "Disruption Severity Distribution by Shipping Mode",
       x = "Shipping Mode",
       y = "Number of Disruptions",
       fill = "Severity Level")  +
  theme_minimal()
plot_2


# QUERY 3: Average Delays by Product and Shipping Mode
avg_delay<- sqldf("SELECT 
              Product_Category, Shipping_Mode, AVG(Delay_Days) AS avg_delay_days
              FROM order_df
              GROUP BY Product_Category, Shipping_Mode
              ORDER BY avg_delay_days desc")
avg_delay

# PLOT 4: Heatmap of Delays
plot_3 <- ggplot(avg_delay, aes(x = Shipping_Mode, y = Product_Category, 
                                fill = avg_delay_days)) +
  geom_tile() +
  geom_text(aes(label = round(avg_delay_days, 2))) +
  scale_fill_gradient(low = "lightgreen", high = "red") +
  labs(title = "Average Delay Days by Product and Shipping Mode",
       x = "Shipping Mode",
       y = "Product Category",
       fill = "Delay (Days)") +
  theme_minimal()

plot_3

# QUERY 4: Top 10 Worst Delay Combinations
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

# PLOT 4: Top 10 Worst Delay Scenarios
plot_4 <- ggplot(top_worst_delays, 
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
plot_4

#QUERY 5 Supplier Performance by Reliability Level
supplier_performance<- sqldf("SELECT s.Supplier_ID, AVG(s.Supplier_Reliability_Score) AS Avg_Reliability, 
                             AVG(o.Delay_Days) AS avg_delay_days
                             FROM order_df o
                             JOIN supplier_df s ON o.Supplier_ID= s.Supplier_ID
                             GROUP BY s.Supplier_ID, s.Supplier_Reliability_Score")

# Create reliability groups
supplier_performance <- supplier_performance %>%
  mutate(reliability_group = cut(Avg_Reliability, 
                                 breaks = 5, # splits into 5 equal groups(increments of .1 starting at .5)
                                 labels = c("Lowest: .5-.6", "Low: .6-.7", "Medium: .7-.8", "High: .8-.9", "Highest: .9-1.0")))

#Plot 5 Delay Distribution by Reliability Level

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

# QUERY 6: Monthly Trends by Shipping Mode
monthly_trends_mode <- sqldf("
  SELECT order_month,
         Shipping_Mode,
         COUNT(*) AS total_orders,
         AVG(Delay_Days) AS avg_delay
  FROM supply_chain_df
  GROUP BY order_month, Shipping_Mode
  ORDER BY order_month
")

# PLOT 6: Monthly delay trends faceted by shipping mode
plot_6<- ggplot(monthly_trends_mode, aes(x = order_month, y = avg_delay, 
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
plot_6


# QUERY 7: Top 10 Most Disruptive Suppliers
worst_coms_suppliers <- sqldf("
  SELECT s.Supplier_ID,
         s.avg_communication_cost,
          s.Avg_Reliability,
         COUNT(s.Supplier_ID) AS total_orders
  FROM supplier_df_grp AS s
  LEFT JOIN order_df AS o ON s.Supplier_ID = o.Supplier_ID
  GROUP BY s.Supplier_ID
  ORDER BY s.avg_communication_cost DESC
  LIMIT 10
")
worst_coms_suppliers

# Plot 7: Scatter plot - comunications vs. reliability
plot_7 <- ggplot(worst_coms_suppliers, 
                 aes(x = avg_communication_cost, 
                     y = Avg_Reliability)) +
  geom_point(size = 4, color = "red") +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_text(aes(label = Supplier_ID), vjust = -1, size = 3) +
  labs(title = "Least Communicative Suppliers: Coms Cost vs Reliability",
       x = "Average Communications Cost",
       y = "Reliability Score") +
  theme_minimal()

plot_7

#Plot 7.1, visualizing a story from the least communicative suppliers, after normalizing

worst_coms_norm <- worst_coms_suppliers %>%
  mutate(
    Coms_Cost = (avg_communication_cost - min(avg_communication_cost)) / 
      ifelse(max(avg_communication_cost) == min(avg_communication_cost), 1, 
             max(avg_communication_cost) - min(avg_communication_cost)),
    Reliability = (Avg_Reliability - min(Avg_Reliability)) / 
      ifelse(max(Avg_Reliability) == min(Avg_Reliability), 1, 
             max(Avg_Reliability) - min(Avg_Reliability)),
    Volume = (total_orders - min(total_orders)) / 
      ifelse(max(total_orders) == min(total_orders), 1, 
             max(total_orders) - min(total_orders))
  ) %>%
  select(Supplier_ID, Coms_Cost, Reliability, Volume) %>%
  pivot_longer(cols = c(Coms_Cost, Reliability, Volume), 
               names_to = "Metric", values_to = "Scaled_Value")

# Plotting the Communication Story
plot_7.1 <- ggplot(worst_coms_norm, aes(x = Metric, y = Scaled_Value, group = Supplier_ID)) +
  geom_line(aes(color = Supplier_ID), size = 1, alpha = 0.8) +
  geom_point(aes(color = Supplier_ID), size = 3) +
  annotate("text", x = 1, y = 1.05, label = "High", size = 3, fontface = "italic") +
  annotate("text", x = 1, y = -0.05, label = "Low", size = 3, fontface = "italic") +
  labs(title = "Supplier Communication Profiles (Parallel Coordinates)",
       subtitle = "Comparing Comm Cost, Reliability, and Volume on normalized scales",
       y = "Relative Scale (0=Min, 1=Max)",
       x = "") +
  theme_minimal() +
  theme(legend.position = "right")

plot_7.1
#After some analysis, it is clear that high reliability is more consistent with
#low volume, showing that your avg reliability will decrease overtime.

# QUERY 8: Order Value by Shipping Mode
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

plot_8 <- ggplot(order_df, aes(x = reorder(Shipping_Mode, Order_Value_USD, FUN = median), 
                                y = Order_Value_USD)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Order Value Distribution by Shipping Mode",
       x = "Shipping Mode",
       y = "Order Value (USD)") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal()

plot_8

#plot 9: Delay Distribution

ggplot(supply_chain_df, aes(x = Actual_Transit_Days, y = Delay_Days, 
                            color = Disruption_Type)) + 
  geom_jitter(alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  labs(
    title = "Anatomy of Delays: Disruption Type vs. Transit Time",
    subtitle = "In the dataframe, all delays are caused by specific disruptions",
    x = "Actual Days in Transit",
    y = "Days Delayed",
    color = "Disruption Type"
  ) +
  theme_minimal()

# this visualization proves that if there is any delay at all, it is due 
# to a determined disruption


#Query 10, breaking down leadtime across suppliers

lead_time_gap <- sqldf("
  SELECT Supplier_ID,
         AVG(Days_Until_Dispatch) as avg_dispatch_time,
         AVG(Actual_Transit_Days) as avg_transit_time
  FROM supply_chain_df
  GROUP BY Supplier_ID
  ORDER BY avg_transit_time DESC
")
lead_time_gap
plot_10 <- ggplot(lead_time_gap) +
  geom_segment(aes(x = avg_dispatch_time, xend = avg_transit_time, 
                   y = reorder(Supplier_ID, avg_transit_time), yend = reorder(Supplier_ID, avg_transit_time)), 
               color = "grey50", size = 1.5) +
  geom_point(aes(x = avg_dispatch_time, y = Supplier_ID, color = "Dispatch Time"), size = 4) +
  geom_point(aes(x = avg_transit_time, y = Supplier_ID, color = "Transit Time"), size = 4) +
  labs(title = "Lead Time Gap Analysis: Dispatch vs. Transit",
       x = "Average Days",
       y = "Supplier_ID",
       color = "Metric") +
  theme_minimal() +
  scale_color_manual(values = c("Dispatch Time" = "#E7B800", "Transit Time" = "#2E9FDF"))
plot_10

#Query 11

cooperation_analysis <- sqldf("
  SELECT Data_Sharing_Consent, Supplier_Reliability_Score, Dominant_Buyer_Flag
  FROM supply_chain_df")

plot_11 <- ggplot(cooperation_analysis, aes(x = Supplier_Reliability_Score, fill = factor(Data_Sharing_Consent))) +
  geom_density(alpha = 0.6) +
  facet_wrap(~Dominant_Buyer_Flag) +
  labs(title = "Impact of Data Sharing on Reliability",
       subtitle = "Density of Reliability Scores by Consent Status by Dominant Buyer Flag",
       x = "Reliability Score", fill = "Data Sharing Consent") +
  theme_minimal() +
  theme(legend.position = "top")
plot_11

#Query 12

relationship_risk <- sqldf("
  SELECT Buyer_ID, Supplier_ID, 
         Historical_Disruption_Count AS Avg_Disruptions
  FROM supply_chain_df
  GROUP BY Buyer_ID, Supplier_ID
  ORDER BY Avg_Disruptions DESC
  LIMIT 25")
relationship_risk

plot_12 <- ggplot(relationship_risk, aes(x = Buyer_ID, y = Supplier_ID)) +
  geom_tile(aes(fill = Avg_Disruptions), color = "white") +
  labs(title = "Which Supplier Relationships are failing?",
       subtitle = "Tiles show Buyer-Supplier pairs with highest disruption frequency",
       x = "Buyer ID",
       y = "Supplier ID") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 8))
plot_12

relationship_count <- sqldf("SELECT COUNT(Buyer_ID) AS failing_relationships, Supplier_ID
                            FROM relationship_risk
                            GROUP BY Supplier_ID
                            HAVING failing_relationships>1
                            ORDER BY failing_relationships DESC")

plot_12.1 <- ggplot(relationship_count, aes(Supplier_ID, failing_relationships)) + 
  geom_col(fill = 'blue') +
  labs(title = "Suppliers with most failed relationships",
       subtitle = "Suppliers to Avoid")

plot_12.1
