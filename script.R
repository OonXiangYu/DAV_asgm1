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
for (v in num_vars) {
  
  Q1 <- quantile(data[[v]], 0.25)
  Q3 <- quantile(data[[v]], 0.75)
  IQR_value <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR_value
  upper <- Q3 + 1.5 * IQR_value
  
  data <- data[data[[v]] >= lower & data[[v]] <= upper, ]
}

# show the plot again after delete outliers
for (var in num_vars) {
  boxplot(data[[var]], main = paste("After cleaning outliers: ", var))
  print(summary(data[[var]]))
}