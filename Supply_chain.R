supply_chain<- read.csv("/Users/andrewlennon/Downloads/supply_chain.csv")
library(sqldf)
library(ggplot2)
library(dplyr)
library(corrplot)
dim(supply_chain)
str(supply_chain)


#SPLIT UP INTO TABLES THREE TABLES 

#order_df HAS Order_ID, Buyer_ID, Supplier_ID
order_df<- supply_chain[,c("Order_ID","Buyer_ID","Supplier_ID","Product_Category",
                           "Quantity_Ordered","Order_Date","Dispatch_Date",
                           "Delivery_Date","Shipping_Mode","Order_Value_USD","Delay_Days",
                           "Disruption_Type","Disruption_Severity","Supply_Risk_Flag")]
#supplier_df HAS Supplier_ID
supplier_df<-supply_chain[,c("Supplier_ID","Supplier_Reliability_Score",
                             "Historical_Disruption_Count","Available_Historical_Records",
                             "Data_Sharing_Consent", "Federated_Round","Parameter_Change_Magnitude",
                             "Communication_Cost_MB","Energy_Consumption_Joules")] 
#buyer_df HAS Buyer_ID
buyer_df<-supply_chain[,c("Buyer_ID", "Organization_ID","Dominant_Buyer_Flag")] 

#CHANGE DATES FROM chr TO as.date
order_df$Order_Date <- as.Date(order_df$Order_Date, format = "%Y-%m-%d")
order_df$Dispatch_Date <- as.Date(order_df$Dispatch_Date, format = "%Y-%m-%d")
order_df$Delivery_Date <- as.Date(order_df$Delivery_Date, format = "%Y-%m-%d")

ncol(order_df)+ncol(supplier_df)+ncol(buyer_df)

#COUNT DISTINCT
supply_chain %>% 
  summarise(distinct_order_id= n_distinct(Order_ID),
            distinct_buyers=n_distinct(Buyer_ID),
            distinct_suppliers = n_distinct(Supplier_ID),
            distinct_organizations=n_distinct(Organization_ID),
            distinct_products=n_distinct(Product_Category))
            

#1

energy_analysis <- sqldf("
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

#2

risk_by_mode <- sqldf("
  SELECT Shipping_Mode,
         COUNT(*) AS total_orders,
         SUM(Supply_Risk_Flag) AS total_risk_events,
         SUM(Supply_Risk_Flag)*1.0 / COUNT(*) AS risk_rate
  FROM order_df
  GROUP BY Shipping_Mode
  ORDER BY total_risk_events DESC")
risk_by_mode

ggplot(risk_by_mode, aes(x = Shipping_Mode, y = risk_rate)) +
  geom_col(fill = "blue") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Supply Risk Rate by Shipping Mode",
    x = "Shipping Mode",
    y = "Risk Rate"
  ) +
  theme_minimal()
#TEST 
order_df %>% 
  filter(Shipping_Mode=='Air') %>% 
  count(Supply_Risk_Flag==1)



#3
severity_dist <- sqldf("
  SELECT Shipping_Mode,
         SUM(Disruption_Severity = 'Low')    AS low,
         SUM(Disruption_Severity = 'Medium') AS medium,
         SUM(Disruption_Severity = 'High')   AS high
  FROM order_df
  GROUP BY Shipping_Mode")
severity_dist

#plot3-1
low_disruption <- ggplot(severity_dist, aes(Shipping_Mode, low))+
  geom_col(fill='red')+
  geom_text(aes(label=low), vjust= -.5)+
  labs(title= 'Low Disruption Severity')
low_disruption
#plot3-2
medium_disruption <- ggplot(severity_dist, aes(Shipping_Mode, medium))+
  geom_col(fill='purple')+
  geom_text(aes(label=medium), vjust= -.5)+
  labs(title= 'Medium Disruption Severity')
medium_disruption
#plot3-3
high_disruption <- ggplot(severity_dist, aes(Shipping_Mode, high))+
  geom_col(fill='green')+
  geom_text(aes(label=high), vjust= -.5)+
  labs(title= 'High Disruption Severity')
high_disruption

#4 AVG-DELAY INCLUDING DAYS WHERE DELAY=0
max(order_df$Delivery_Date)
min(order_df$Delivery_Date)

avg_delay<- sqldf("SELECT 
              Product_Category, Shipping_Mode, AVG(Delay_Days) AS avg_delay_days
              FROM order_df
              GROUP BY Product_Category, Shipping_Mode
              ORDER BY avg_delay_days desc")
avg_delay


#5
supplier_performance<- sqldf("SELECT s.Supplier_ID, s.Supplier_Reliability_Score, 
                             AVG(o.Delay_Days) AS avg_delay_days
                             FROM order_df o
                             JOIN supplier_df s ON o.Supplier_ID= s.Supplier_ID
                             GROUP BY s.Supplier_ID, s.Supplier_Reliability_Score")
supplier_performance

