library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)

onlineRetail <- read.csv('Online Retail.csv')

str(onlineRetail)
head(onlineRetail)
summary(onlineRetail)

onlineRetail <-  mutate(onlineRetail, InvoiceNo = as.integer(InvoiceNo), 
                        StockCode = as.integer(StockCode))
onlineRetail$InvoiceDate <- dmy_hm(onlineRetail$InvoiceDate)
onlineRetail$month <- month(onlineRetail$InvoiceDate)
onlineRetail$date <- as.Date(onlineRetail$InvoiceDate)
onlineRetail$weekD <- wday(onlineRetail$InvoiceDate, label = TRUE)
onlineRetail$time <- hour(onlineRetail$InvoiceDate)
onlineRetail$totalAmount <- (onlineRetail$Quantity * onlineRetail$UnitPrice)
onlineRetailV2 <- onlineRetail %>%  distinct()
sum(is.na(onlineRetailV2$StockCode))
sum(is.na(onlineRetailV2$Quantity))
onlineRetailV2 %>%  summarise(totalNulls = sum(is.null(Quantity)))
onlineRetailV2 <- na.omit(onlineRetailV2)
onlineRetailV2 <- mutate(onlineRetailV2, month = as.integer(month))
onlineRetailV2 <- rename(onlineRetailV2, Month = month,
                         Date = date, WeekDay = weekD, TimeOfTheDat = time,
                         TotalAmount = totalAmount)



onlineRetailV2 %>% group_by(weekD) %>% summarise(sales = sum(totalAmount))

onlineRetailV2 %>%  group_by(StockCode) %>%  filter(weekD== 'Thu') %>%  
  summarise(s = sum(totalAmount)) %>% arrange(desc(s)) 

onlineRetailV2 %>%  group_by(StockCode) %>%  filter(weekD== 'Thu') %>%  
  summarise(s = sum(Quantity)) %>% arrange(desc(s))
 
onlineRetailV2 %>%  select(c(month, weekD, totalAmount)) %>%  group_by(month, weekD) %>% 
  summarise(sales = sum(totalAmount)) %>% arrange(desc(sales)) %>%  slice(1:3) %>% 
  print(n = 36)

onlineRetailV2 %>%  group_by(StockCode) %>%  summarise(revenue = sum(totalAmount)) %>% 
  arrange(desc(revenue)) %>%  slice(1:5)

onlineRetailV2 %>%  group_by(CustomerID, InvoiceDate) %>%  
  summarise(spend = sum(totalAmount)) %>%  arrange(InvoiceDate) 

onlineRetailV2 %>% group_by(Country, CustomerID) %>% summarise(revenue = sum(totalAmount)) %>% 
  arrange(desc(revenue)) %>% slice(1:5) %>%  print(n=132)

onlineRetailV2 %>%  group_by(CustomerID) %>%  summarise(no = n()) %>%  
  filter(no > 1) %>%  arrange(desc(no)) %>%  slice(1:5)

onlineRetailV2 %>%  group_by(CustomerID, InvoiceDate) %>%  summarise(cnt = n()) %>% 
  count() %>%  arrange(desc(n))

onlineRetailV2 %>%  group_by(month, date) %>% summarise(revenue = sum(totalAmount)) %>% 
  arrange(desc(revenue)) %>% slice(1:5) %>%  print(n= 60)

onlineRetailV2 %>%  group_by(month) %>%  summarise(sales = sum(totalAmount))
onlineRetailV2 %>%  group_by(month) %>%  summarise(sales = sum(Quantity))

onlineRetailV2[order(onlineRetailV2$Quantity, decreasing = TRUE),] 

onlineRetailV2 %>% arrange(desc(Quantity))  %>% group_by(Country, CustomerID) %>%  
  summarise(total = sum(Quantity), quantity = n()) %>%  slice(1:5)

onlineRetailV2 %>%  group_by(Country) %>%  
  summarise(soldUnits = sum(Quantity)) %>%  arrange(soldUnits, FALSE)

onlineRetailV2 %>%  group_by(Country) %>%  
  summarise(soldUnits = sum(Quantity)) %>%  arrange(desc(soldUnits)) %>% 
  filter(Country != "United Kingdom")

onlineRetailV2 %>%  group_by(Country) %>%  arrange(desc(Quantity)) %>% 
  top_n(5) %>%  slice(1:5)

onlineRetailV2 %>%  group_by(Country) %>%  
  summarise(revenueGenerated = sum(Quantity * UnitPrice)) %>% 
  arrange(desc(revenueGenerated)) %>% filter(Country != "United Kingdom")

onlineRetailV2 %>%  group_by(Country) %>%  
  summarise(revenueGenerated = sum(Quantity * UnitPrice)) %>% 
  arrange((revenueGenerated)) %>% filter(Country != "United Kingdom")

onlineRetailV3 %>%  group_by(month, StockCode) %>%  summarise(summa = sum(totalAmount)) %>% 
  arrange(desc(summa)) %>%  slice(1:3)



onlineRetailV2 %>%  group_by(StockCode) %>%  summarise(total = sum(totalAmount)) %>% 
  arrange(desc(total)) %>%  top_n(5) %>%  
  ggplot(aes(x = as.factor(StockCode), y = total)) + geom_col(position = "dodge") +
  labs(x = "Stock Code", y = "Revenue Generated", title = "Top 5 Revenue Generating Items") +
  theme_classic()

topThreeItems <- onlineRetailV3 %>%  group_by(month, StockCode) %>%  summarise(summa = sum(totalAmount)) %>% 
  arrange(desc(summa)) %>%  slice(1:3) %>%  ggplot(aes(x= month, y= summa))

onlineRetailV2 %>%  group_by(StockCode) %>%  summarise(total = sum(Quantity)) %>% 
  arrange(desc(total)) %>%  top_n(5) %>%  
  ggplot(aes(x = as.factor(StockCode), y = total)) + geom_col(position = "dodge") +
  labs(x = "Stock Code", y = "Quantities Sold", title = "Top 5 Quantity selling Items") +
  theme_classic()

onlineRetailV2 %>%  group_by(month) %>%  summarise(sales = sum(totalAmount)) %>% 
  ggplot(aes(x= as.factor(month), y= sales)) + geom_col(position = "dodge") +
  labs(x= "Months", y= "Sales", title = "Sales throughtout the year") +
  theme_classic()

onlineRetailV2 %>%  group_by(month) %>%  summarise(sales = sum(totalAmount)) %>% 
  ggplot(aes(x= (month), y= sales)) + geom_line() +
  labs(x= "Months", y= "Sales", title = "Sales throughtout the year") +
  theme_classic()

onlineRetailV2 %>%  group_by(month) %>%  summarise(sales = sum(Quantity)) %>% 
  ggplot(aes(x= as.factor(month), y= sales)) + geom_col(position = "dodge") +
  labs(x= "Months", y= "Quantity", title = "Quantity throughtout the year") +
  theme_classic()

onlineRetailV2 %>%  group_by(month) %>%  summarise(sales = sum(Quantity)) %>% 
  ggplot(aes(x=(month), y= sales)) + geom_line() + 
  labs(x= "Months", y= "Quantity", title = "Quantity throughtout the year") +
  theme_bw() + geom_smooth()

onlineRetailV2 %>% group_by(weekD) %>% summarise(sales = sum(totalAmount)) %>% 
  ggplot(aes(x = weekD, y = sales)) + geom_col(position = "dodge") +
  labs(x= "Weekday", y = "Sales", title = "Sales throughout the Week") + 
  theme_classic()

onlineRetailV2 %>% group_by(weekD) %>% summarise(sales = sum(totalAmount)) %>% 
  ggplot(aes(x = weekD, y = sales)) + geom_line() +
  labs(x= "Weekday", y = "Sales", title = "Sales throughout the Week") + 
  theme_classic()

onlineRetailV2 %>% group_by(weekD) %>% summarise(sales = sum(Quantity)) %>% 
  ggplot(aes(x = weekD, y = sales)) + geom_col(position = "dodge") +
  labs(x= "Weekday", y = "Quantity", title = "Quantity sold throughout the Week") + 
  theme_classic()

onlineRetailV2 %>% group_by(weekD) %>% summarise(sales = sum(Quantity)) %>% 
  ggplot(aes(x = (weekD), y = (sales))) + geom_line() +
  labs(x= "Weekday", y = "Quantity", title = "Quantity sold throughout the Week") + 
  theme_classic()

onlineRetailV2 %>%  group_by(Country) %>%  
  summarise(soldUnits = sum(Quantity)) %>%  arrange(desc(soldUnits)) %>% 
  filter(Country != "United Kingdom") %>%top_n(5) %>%   
  ggplot(aes(x = Country, y = soldUnits)) + geom_col(position = "dodge") +
  labs(x = "Countries", y = "No of Units Sold", title = "Top 5 Seller Countries",
       caption = "Countries excluding United Kingdom") +
  theme_classic()

onlineRetailV2 %>%  group_by(CustomerID) %>%  summarise(no = n()) %>%  
  filter(no > 1) %>%  arrange(desc(no)) %>%  slice(1:5) %>% 
  ggplot(aes(x= as.factor(CustomerID), y= no)) +geom_col(position = "dodge") + 
  labs(x= "Customers", y= "No of Purchases", 
       title = "Top 5 Customers according to the Number of purchases made")+
  theme_classic()

onlineRetailV2 %>%  group_by(CustomerID, InvoiceDate) %>%  summarise(cnt = n()) %>% 
  count() %>%  arrange(desc(n)) %>%  ungroup() %>%  top_n(5) %>% 
  ggplot(aes(x= as.factor(CustomerID), y= n)) + geom_col(position = "dodge") +
  labs(x= "Customer Id", y="No of times visited", 
       title = "Top 5 Customer according to their their frequency of Purchase") + 
  theme_classic()

onlineRetailV2 %>%  group_by(Country) %>%  
  summarise(revenueGenerated = sum(Quantity * UnitPrice)) %>% 
  arrange(desc(revenueGenerated)) %>% 
  filter(Country != "United Kingdom") %>% top_n(5) %>%  
  ggplot(aes(x = Country, y = revenueGenerated)) + geom_col(position = "dodge") +
  labs(x = "Countries", y = "Revenue Generated", title = "Top 5 Revenue Generating Countries", 
       caption = "Countries Excluding United Kingdom")+
  theme_classic()

onlineRetailV2 %>%  group_by(Country) %>%  
  summarise(revenueGenerated = sum(Quantity * UnitPrice)) %>% 
  arrange(revenueGenerated) %>% filter(Country != "United Kingdom") %>% slice(1:5) %>%  
  ggplot(aes(x = Country, y = revenueGenerated)) + geom_col(position = "dodge") +
  labs(x = "Countries", y = "Revenue Generated", title = "Bottom 5 Revenue Generating Countries")+
  theme_classic() + theme(axis.text.x = element_text(angle = -22.5, vjust = 0.1, hjust = 0.2))


topThreeItems <- onlineRetailV3 %>%  group_by(month, StockCode) %>%  summarise(summa = sum(totalAmount)) %>% 
  arrange(desc(summa)) %>%  slice(1:3)
write.csv(topThreeItems, , file = "TopThreeItems.csv")

topFiveCustomers <- onlineRetailV2 %>% arrange(desc(Quantity))  %>% group_by(Country, CustomerID) %>%  
  summarise(total = sum(Quantity), quantity = n()) %>%  slice(1:5)
write.csv(topFiveCustomers, , file = 'TopFiveCustomers.csv')
