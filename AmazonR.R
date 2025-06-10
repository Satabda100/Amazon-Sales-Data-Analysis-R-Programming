library()
install.packages("lubridate", repos = "https://cloud.r-project.org/")


library(dplyr)
library(forecast)
library(lubridate)
library(ggplot2)
# Load data
df = read.csv("C:/Users/pc1/Downloads/amazon_sales_data_cleaned.csv")
df$Date <- as.Date(df$Date)

# 1. Descriptive Statistics
summary(df)

# 2. Clean data
df <- df[df$Status == 'Pending', ]

# 3. Monthly Sales Trend
df$Period <- paste(df$Month, df$Year)
monthly_sales <- df %>%
  group_by(Period) %>%
  summarise(Total_Sales = sum(Total.Sales)) %>%
  arrange(as.Date(paste0('01-', Period), format='%d-%B %Y'))

ggplot(monthly_sales, aes(x = as.Date(paste0('01-', Period), format='%d-%B %Y'), y = Total_Sales)) +
  geom_line() + geom_point() +
  labs(title = "Monthly Sales Trend", x = "Month", y = "Total Sales") +
  theme_minimal()

# 4. Sales by Category
ggplot(df, aes(x = Category, y = Total.Sales)) +
  geom_bar(stat = "summary", fun = sum, fill = "skyblue") +
  labs(title = "Sales by Product Category") +
  theme_minimal()

# 5. Payment Method Pie Chart
pie(table(df$Payment.Method), main="Payment Method Share")

names(df)



#6. ANOVA
anova_result <- aov(Total.Sales ~ Category, data = df)
summary(anova_result)


# 7. Regression
model0 <- lm(Total.Sales ~ Price + Quantity, data = df)
summary(model0)

# 8. Time Series Forecast
monthly_ts <- df %>%
  group_by(Date = floor_date(Date, "month")) %>%
  summarise(Total = sum(Total.Sales))

ts_data <- ts(monthly_ts$Total, frequency = 12)
fit <- auto.arima(ts_data)
summary(fit)



# 9. Heatmap: Total Sales by Category and City
library(dplyr)
library(ggplot2)

# Group and summarize sales
heatmap_data <- df %>%
  group_by(Category, Customer.Location) %>%
  summarise(Total.Sales = sum(Total.Sales, na.rm = TRUE)) %>%
  ungroup()

# Create heatmap
ggplot(heatmap_data, aes(x = Customer.Location , y = Category, fill = Total.Sales)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Heatmap of Total Sales by Category and City",
       x = "City", y = "Category", fill = "Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 10 visualizing Anova 

ggplot(df, aes(x = Category, y = Total.Sales)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Distribution of Total Sales by Product Category",
       x = "Category", y = "Total Sales") +
  theme_minimal()


# 11 visualizing Linear Regression

df$Predicted_Sales <- predict(model0)

ggplot(df, aes(x = Total.Sales, y = Predicted_Sales)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Sales",
       x = "Actual Sales", y = "Predicted Sales") +
  theme_minimal()



# 12 visualizing  ARIMA 

library(dplyr)
library(lubridate)

# Convert Month and Year to a proper Date object (assuming day = 1)
df <- df %>%
  mutate(Full_Date = as.Date(paste0("01-", Month, "-", Year), format = "%d-%B-%Y"))

monthly_ts <- df %>%
  filter(Status == "Pending") %>%  # Only include valid/pending sales
  group_by(Month = floor_date(Full_Date, "month")) %>%
  summarise(Total_Sales = sum(Total.Sales, na.rm = TRUE)) %>%
  ungroup()

library(forecast)
library(ggplot2)

ts_data <- ts(monthly_ts$Total_Sales,
              start = c(year(min(monthly_ts$Month)), month(min(monthly_ts$Month))),
              frequency = 12)

autoplot(ts_data) +
  labs(title = "Monthly Total Sales Time Series",
       x = "Month", y = "Total Sales") +
  theme_minimal()


fit <- auto.arima(ts_data)
forecasted <- forecast(fit, h = 3)
summary(forecasted)
autoplot(forecasted) +
  labs(title = "3-Month Sales Forecast", x = "Month", y = "Forecasted Sales") +
  theme_minimal()

# 13 Improving ARIMA Model


library(forecast)

# Seasonal ARIMA
fit_sarima <- auto.arima(ts_data, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
summary(fit_sarima)

# Forecast next 3 months
forecast_sarima <- forecast(fit_sarima, h = 3)
summary(forecast_sarima)
# Plot forecast
autoplot(forecast_sarima) +
  labs(title = "3-Month Sales Forecast (Seasonal ARIMA)",
       x = "Month", y = "Forecasted Sales") +
  theme_minimal()



accuracy(fit_sarima)
accuracy(fit)

length(ts_data)
print(ts_data)

str(df)

head(df)