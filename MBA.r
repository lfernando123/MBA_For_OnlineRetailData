# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(summarytools)
library(corrplot)
library(DataExplorer)
library(psych)
library(caret)


# Load the Excel file
data <- read_excel("online_retail.xlsx")
print(data)

# View basic structure and summary
str(data)
summary(data)

# Drop unnecessary columns
df <- data[, !(names(data) %in% c("StockCode", "InvoiceDate", "CustomerID"))]
print(df)

# Check for missing values
missing_data <- sapply(df, function(x) sum(is.na(x)))
print(missing_data)

# Remove rows with missing values
data_clean <- df %>%
  filter(Quantity >= 1, UnitPrice >= 1)
str(data_clean)

# Remove rows with irrelevant descriptions
data_clean <- data_clean[data_clean$Description != "DOTCOM POSTAGE", ]
data_clean <- data_clean[data_clean$Description != "POSTAGE", ]
str(data_clean)

# Visualize missing data
plot_missing(data_clean)

# Correlation analysis
numeric_data <- data_clean %>% select(where(is.numeric)) %>% na.omit()
cor_matrix <- cor(numeric_data)
corrplot(cor_matrix, method = "color", tl.cex = 0.8)

# Quick overview
introduce(data_clean)

# Get country wise transaction count
country_count <- data_clean %>%
  group_by(Country) %>%
  summarise(Transactions = n()) %>%
  arrange(desc(Transactions))


# Plot country wise transaction count
top3 <- head(country_count, 3)
ggplot(top3, aes(x = reorder(Country, -Transactions), y = Transactions)) +
  geom_bar(stat = "identity", fill = "#4a2924") +
  labs(title = "Top 3 Countries by Transactions",
       x = "Country",
       y = "Number of Transactions") +
  theme_minimal()

# Get country wise sales for top 3 countries
uk_data <- data_clean %>%
  filter(Country == "United Kingdom") %>%
  select(-Country, -Quantity, -UnitPrice)

ger_data <- data_clean %>%
  filter(Country == "Germany") %>%
  select(-Country, -Quantity, -UnitPrice)

fra_data <- data_clean %>%
  filter(Country == "France") %>%
  select(-Country, -Quantity, -UnitPrice)


library(arules)
library(arulesViz)

# Convert InvoiceNo to character (required by arules)
uk_data$InvoiceNo <- as.character(uk_data$InvoiceNo)

# Convert to transactions using split
transactions_list <- split(uk_data$Description, uk_data$InvoiceNo)

# Convert to "transactions" object
trans <- as(transactions_list, "transactions")

# Getting the rules for uk data
rules <- apriori(trans, parameter = list(supp = 0.005, conf = 0.7, maxlen = 3))
rules <- subset(rules, lift > 2)
plot(rules)

# inspect the rules
inspect(head(sort(rules, by = "count", decreasing = TRUE), 10))

# Getting the rules for Germany
# Convert InvoiceNo to character (required by arules)
ger_data$InvoiceNo <- as.character(ger_data$InvoiceNo)

# Convert to transactions using split
ger_transactions_list <- split(ger_data$Description, ger_data$InvoiceNo)

# Convert to "transactions" object
ger_trans <- as(ger_transactions_list, "transactions")

ger_rules <- apriori(ger_trans, parameter = list(supp = 0.02, conf = 0.6, maxlen = 3))

plot(ger_rules)

# inspect the rules
inspect(head(sort(ger_rules, by = "count", decreasing = TRUE), 10))

# Getting the rules for France
# Convert InvoiceNo to character (required by arules)
fra_data$InvoiceNo <- as.character(fra_data$InvoiceNo)

# Convert to transactions using split
fra_transactions_list <- split(fra_data$Description, fra_data$InvoiceNo)

# Convert to "transactions" object
fra_trans <- as(fra_transactions_list, "transactions")

fra_rules <- apriori(fra_trans, parameter = list(supp = 0.02, conf = 0.7, maxlen = 3))
fra_rules <- subset(fra_rules, lift > 1.2)

plot(fra_rules)

# inspect the rules
inspect(head(sort(fra_rules, by = "count", decreasing = TRUE), 10))
