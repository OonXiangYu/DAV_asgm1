# Part 1
# 1
data <- read.csv("hotel_bookings.csv")
str(data)

summary(data)

head(data)

# 2
# Count missing values per column
sapply(data, function(x) sum(is.na(x)))

# Impute missing children values with 0
data$children[is.na(data$children)] <- 0

# Count missing values per column
sapply(data, function(x) sum(is.na(x)))

# 3
# time and price shouldn't be negative
sum(data$lead_time < 0)
sum(data$adr < 0)

# adult / guest also cant be 0
sum(data$adults == 0)
sum(data$adults == 0 & (data$children > 0 | data$babies > 0))
sum((data$adults + data$children + data$babies) == 0)

# remove the rows where adult = 0 or adr is negative
data <- data[!(data$adr < 0), ]
data <- data[!(data$adults == 0), ]

# Display latest adr
sum(data$adr < 0)

# Display latest adult 
sum(data$adults == 0)
sum(data$adults == 0 & (data$children > 0 | data$babies > 0))
sum((data$adults + data$children + data$babies) == 0)

# 4
# box plot for numeric variables
# show the detail of the plot
num_vars <- c("adr", "lead_time", "stays_in_week_nights", "stays_in_weekend_nights", "adults")

for (var in num_vars) {
  boxplot(data[[var]], main = paste("Outliers in", var))
  print(summary(data[[var]]))
}

# delete outliers
data_clean <- data

for (v in num_vars) {
  
  Q1 <- quantile(data_clean[[v]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data_clean[[v]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR_value
  upper <- Q3 + 1.5 * IQR_value
  
  data_clean <- data_clean[data_clean[[v]] >= lower & data_clean[[v]] <= upper, ]
}

data <- data_clean

# Part 2
# 1
# show the plot again after delete outliers
for (var in num_vars) {
  boxplot(data[[var]], main = paste("After cleaning outliers: ", var))
  print(summary(data[[var]]))
}

# 2
cat_vars <- c("hotel", "meal", "country", "market_segment", "distribution_channel", "deposit_type", "customer_type")

# frequency table
freq_table <- function(var) {
  tbl <- table(data[[var]])
  pct <- prop.table(tbl) * 100
  
  result <- data.frame(
    Category = names(tbl),
    Count = as.vector(tbl),
    Percentage = round(as.vector(pct), 2)
  )
  
  return(result)
}

for (var in cat_vars) {
  cat("\n Frequency table for", var, "\n")
  print(freq_table(var))
}

# Bar chart for cancellation
library(dplyr)

cancel_rate_hotel <- data %>%
  group_by(hotel) %>%
  summarise(cancel_rate = mean(is_canceled))

library(ggplot2)

ggplot(cancel_rate_hotel, aes(x = hotel, y = cancel_rate, fill = hotel)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Cancellation Rate by Hotel Type",
    x = "Hotel Type",
    y = "Cancellation Rate"
  ) +
  theme_minimal()

# Histogram for lead time
ggplot(data, aes(x = lead_time)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of Lead Time",
    x = "Lead Time (days)",
    y = "Frequency"
  ) +
  theme_minimal()

# Boxplot for ADR
ggplot(data, aes(x = market_segment, y = adr)) +
  geom_boxplot() +
  labs(
    title = "ADR by Market Segment",
    x = "Market Segment",
    y = "Average Daily Rate (ADR)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter plot : Lead time VS ADR
ggplot(data, aes(x = lead_time, y = adr, color = factor(is_canceled))) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Lead Time vs ADR (Colored by Cancellation Status)",
    x = "Lead Time (days)",
    y = "Average Daily Rate (ADR)",
    color = "Canceled"
  ) +
  theme_minimal()