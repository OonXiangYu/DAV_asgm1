install.packages("ggplot2")
install.packages("dplyr")
install.packages("corrplot")
library(ggplot2)
library(dplyr)
library(corrplot)

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
cancel_rate_hotel <- data %>%
  group_by(hotel) %>%
  summarise(cancel_rate = mean(is_canceled))

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

# Part 3
# Overall cancellation rate
cancel_rate <- mean(data$is_canceled)
cancel_rate

round(cancel_rate * 100, 2)

# Cancellation by month
cancel_by_month <- data %>%
  group_by(arrival_date_month) %>%
  summarise(cancel_rate = mean(is_canceled))

month_order <- c("January","February","March","April","May","June",
                 "July","August","September","October","November","December")

cancel_by_month$arrival_date_month <- factor(
  cancel_by_month$arrival_date_month,
  levels = month_order
)

ggplot(cancel_by_month, aes(x = arrival_date_month, y = cancel_rate, group = 1)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(
    title = "Cancellation Rate by Month",
    x = "Month",
    y = "Cancellation Rate"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Lead time analysis
ggplot(data, aes(x = factor(is_canceled), y = lead_time, fill = factor(is_canceled))) +
  geom_boxplot() +
  labs(
    title = "Lead Time by Cancellation Status",
    x = "Cancellation Status (0 = Not Canceled, 1 = Canceled)",
    y = "Lead Time (days)",
    fill = "Canceled"
  ) +
  theme_minimal()

# Deposit type impact
round(prop.table(table(data$deposit_type, data$is_canceled), 1) * 100, 2)

# Special request
ggplot(data, aes(x = factor(is_canceled), y = total_of_special_requests, fill = factor(is_canceled))) +
  geom_boxplot() +
  labs(
    title = "Special Requests by Cancellation Status",
    x = "Cancellation Status (0 = Not Canceled, 1 = Canceled)",
    y = "Number of Special Requests",
    fill = "Canceled"
  ) +
  theme_minimal()

# Correlation analysis
num_data <- data[sapply(data, is.numeric)] # numeric variables
num_data_clean <- num_data[, sapply(num_data, function(x) var(x) != 0)]

cor_matrix <- cor(num_data_clean, use = "complete.obs") # Compute correlation matrix
cor_with_target <- cor_matrix[, "is_canceled"] # target value
cor_with_target <- cor_with_target[names(cor_with_target) != "is_canceled"]

top_vars <- sort(abs(cor_with_target), decreasing = TRUE) # top 3
top_3 <- top_vars[1:3]

cat("Top 3 variables most correlated with cancellation:\n")
print(top_3)


# Plot correlation heatmap
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         tl.cex = 0.7,
         tl.col = "black")
