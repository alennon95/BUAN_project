supply_chain<- read.csv("/Users/andrewlennon/Downloads/supply_chain.csv")
library(sqldf)
library(ggplot2)
library(dplyr)
library(corrplot)


dim(supply_chain)
str(supply_chain)


#SPLIT UP INTO TABLES THREE TABLES 
#order_df HAS Order_ID,Buyer_ID",Supplier_ID
#supplier_df HAS Supplier_ID
#buyer_df HAS Buyer_ID

order_df<- supply_chain[,c("Order_ID","Buyer_ID","Supplier_ID","Product_Category",
                           "Quantity_Ordered","Order_Date","Dispatch_Date",
                           "Delivery_Date","Shipping_Mode","Order_Value_USD","Delay_Days",
                           "Disruption_Type","Disruption_Severity","Supply_Risk_Flag")]

supplier_df<-supply_chain[,c("Supplier_ID","Supplier_Reliability_Score",
                             "Historical_Disruption_Count","Available_Historical_Records",
                             "Data_Sharing_Consent", "Federated_Round","Parameter_Change_Magnitude",
                             "Communication_Cost_MB","Energy_Consumption_Joules")] 

buyer_df<-supply_chain[,c("Buyer_ID", "Organization_ID","Dominant_Buyer_Flag")] 

ncol(order_df)+ncol(supplier_df)+ncol(buyer_df)

#1
energy_vs_quantity<- sqldf("SELECT Shipping_Mode, sum(Quantity_Ordered) AS 'sum_quantity_ordered', AVG(Energy_Consumption_Joules) AS 'avg_energy'
FROM supplier_df
INNER JOIN order_df 
ON order_df.supplier_ID=supplier_df.supplier_ID
GROUP BY Shipping_Mode")
energy_vs_quantity
