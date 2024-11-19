# Statistical Models Final Project Analysis: Data Wrangling & Creating Graphs

# PART I: DATA WRANGLING

## install packages
install.packages("dplyr")
install.packages("tidyr")

library(dplyr)
library(tidyr)

## load dataset
df_ecommerce <- read.csv("ecommerce_customer_data_custom_ratios.csv")

## check the structure
str(df_ecommerce)
head(df_ecommerce)

#1. Cleaning data: Missing Values
## check for missing values
summary(df_ecommerce)
colSums(is.na(df_ecommerce))

## plotting missing values in 'Returns' Column
df_ecommerce %>% 
  mutate(Returns_missing = ifelse(is.na(Returns), "Missing", "Not Missing")) %>%
  ggplot(aes(x = Returns_missing)) +
  geom_bar(fill = c("salmon", "skyblue")) +
  labs(title = "Missing Values in 'Returns' Column",
       x = "Returns Missing Status", y = "Frequency")

## replace missing values in 'Returns' with "0"
df_ecommerce_cleaned <- df_ecommerce %>%
  mutate(Returns = ifelse(is.na(Returns), 0, Returns))

#2. Cleaning data: Outliers (checking)
## plot a histogram for Product.Price to check outliers (none)
ggplot(df_ecommerce_cleaned, aes(x = Product.Price)) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Product Price",
       x = "Product Price", y = "Frequency") +
  theme_minimal()

## plot a histogram for Total.Purchase.Amount to check outliers (none)
ggplot(df_ecommerce_cleaned, aes(x = Total.Purchase.Amount)) +
  geom_histogram(binwidth = 10, fill = "skyblue", alpha = 0.7) +
  labs(title = "Distribution of Total Purchase Amount",
       x = "Product Price", y = "Frequency") +
  theme_minimal()

#3. Cleaning data: Removing Duplicates
## remove "Age" column as it is redundant with "Customer.Age"
df_ecommerce_cleaned <- df_ecommerce_cleaned %>%
  select(-Age)

#4. Cleaning data: Converting to Date Type
str(df_ecommerce_cleaned$`Purchase.Date`)

## convert to Date type
df_ecommerce_cleaned$`Purchase.Date` <- as.Date(df_ecommerce_cleaned$`Purchase.Date`, format = "%Y-%m-%d")

#5: Cleaning data: Other General Activities
## capping outliers in the Product.Price column (capping at 99th percentile)
quantile_cap <- quantile(df_ecommerce_cleaned$`Product.Price`, 0.99)
df_ecommerce_cleaned$`Product.Price` <- ifelse(df_ecommerce_cleaned$`Product.Price` > quantile_cap, quantile_cap, df_ecommerce_cleaned$`Product.Price`)

# Example of capping outliers in the Total.Purchase.Amount column (capping at 99th percentile)
quantile_cap1 <- quantile(df_ecommerce_cleaned$`Total.Purchase.Amount`, 0.99)
df_ecommerce_cleaned$`Total.Purchase.Amount` <- ifelse(df_ecommerce_cleaned$`Total.Purchase.Amount` > quantile_cap1, quantile_cap1, df_ecommerce_cleaned$`Total.Purchase.Amount`)

# PART II: VISUALIZATION

## install packages
install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)

#1. Creating linear graph: Total Purchase Amount by Month
## convert Purchase Date to date format if not already done
df_ecommerce_cleaned$`Purchase.Date` <- as.Date(df_ecommerce_cleaned$`Purchase.Date`)

## aggregate the total purchase amount by month
df_ecommerce_cleaned$Month <- format(df_ecommerce_cleaned$`Purchase.Date`, "%Y-%m")
monthly_data <- df_ecommerce_cleaned %>%
  group_by(Month) %>%
  summarize(Total_Purchase = sum(`Total.Purchase.Amount`, na.rm = TRUE))

## convert Month back to date format for proper plotting
monthly_data$Month <- as.Date(paste0(monthly_data$Month, "-01"))

## plot the aggregated monthly total purchase amount
ggplot(monthly_data, aes(x = Month, y = Total_Purchase)) +
  geom_line(color = "blue", alpha = 0.7) +
  labs(title = "Total Purchase Amount by Month",
       x = "Month",
       y = "Total Purchase Amount") +
  theme_minimal()

#2. Creating bar graph: Number of Transactions by Product Category
## bar plot: count of transactions by product category
ggplot(df_ecommerce_cleaned, aes(x = `Product.Category`)) +
  geom_bar(fill = "blue") +
  labs(title = "Number of Transactions by Product Category",
       x = "Product Category",
       y = "Count of Transactions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

#3. Creating heatmap: product category and payment method
install.packages("reshape2")
library(reshape2)

## create a table of counts by Product Category and Payment Method
heatmap_data <- table(df_ecommerce_cleaned$`Product.Category`, df_ecommerce_cleaned$`Payment.Method`)

## convert table to a data frame for ggplot
heatmap_df <- as.data.frame(heatmap_data)

## plot the heatmap
ggplot(heatmap_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heatmap of Product Category and Payment Method",
       x = "Product Category",
       y = "Payment Method",
       fill = "Frequency") +
  theme_minimal()








