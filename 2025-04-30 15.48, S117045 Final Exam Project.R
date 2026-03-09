###################################################################################################################


#   Plotting graphs for initial hypothesis visualisation


###################################################################################################################

# Bitcoin Blockchain reaction on demand decrease  

###################################################################################################################


# Load library
library(ggplot2)

# Define original slopes
b_demand <- -0.5
b_supply <- 0.4

# Define original intercepts
a_demand1 <- 10
a_supply1 <- 2
a_demand2 <- 8  # demand shifts down

# Define functions
demand1 <- function(q) a_demand1 + b_demand * q
demand2 <- function(q) a_demand2 + b_demand * q
supply1 <- function(q) a_supply1 + b_supply * q

# Find original and intermediate intersections
Q1 <- (a_demand1 - a_supply1) / (b_supply - b_demand)
P1 <- demand1(Q1)

Q2_star <- (a_demand2 - a_supply1) / (b_supply - b_demand)
P2_star <- demand2(Q2_star)

# Now calculate P2 from new demand at Q1
P2 <- demand2(Q1)

# Adjust intercept of supply2 to intersect D2 at Q1
a_supply2 <- P2 - b_supply * Q1
supply2 <- function(q) a_supply2 + b_supply * q

# Create plot data
q <- seq(0, 20, 0.1)
df <- data.frame(
  q = q,
  D1 = demand1(q),
  D2 = demand2(q),
  S1 = supply1(q),
  S2 = supply2(q)
)

# Plot
ggplot(df, aes(x = q)) +
  geom_line(aes(y = D1), color = "darkgreen", size = 1.2) +
  geom_line(aes(y = D2), color = "darkgreen", linetype = "dashed", size = 1.2) +
  geom_line(aes(y = S1), color = "steelblue", size = 1.2) +
  geom_line(aes(y = S2), color = "steelblue", linetype = "dashed", size = 1.2) +
  
  # Equilibrium points
  geom_point(aes(x = Q1, y = P1), color = "black", size = 3) +
  geom_point(aes(x = Q2_star, y = P2_star), color = "blue", size = 3) +
  geom_point(aes(x = Q1, y = P2), color = "purple", size = 3) +
  
  # Labels
  annotate("text", x = Q1 + 0.2, y = P1 + 1, label = "E1 ~ (Q1, P1)", size = 5) +
  annotate("text", x = Q2_star, y = P2_star + 0.7, label = "E2* ~ (Q2*, P2*)", size = 5, color = "blue") +
  annotate("text", x = Q1, y = P2 - 1, label = "E2 (Q2, P2)", size = 5, color = "purple") +
  
  # Arrows for shocks
  annotate("segment", x = 15, xend = 15, y = demand1(15), yend = demand2(15),
           arrow = arrow(length = unit(0.2, "cm")), color = "darkgreen") +
  annotate("text", x = 15.2, y = 1, label = "1. Demand ↓", hjust = 0, color = "darkgreen") +
  
  annotate("segment", x = 15, xend = 15, y = supply1(15), yend = supply2(15),
           arrow = arrow(length = unit(0.2, "cm")), color = "steelblue") +
  annotate("text", x = 15.2, y = 7.8, label = "2. Supply ↑ (mining)", hjust = 0, color = "steelblue") +
  
  labs(title = "Bitcoin Market Adjustment: Demand Drop and Mining-Based Supply Increase",
       x = "Quantity", y = "Price") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))




###################################################################################################################

# Bitcoin Blockchain reaction on demand increase  

###################################################################################################################


# Define original slopes
b_demand <- -0.5
b_supply <- 0.4

# Intercepts for demand and supply
a_demand1 <- 6      # Original demand
a_demand2 <- 8.5    # Increased demand
a_supply1 <- 0      # Original supply

# Define functions
demand1 <- function(q) a_demand1 + b_demand * q
demand2 <- function(q) a_demand2 + b_demand * q
supply1 <- function(q) a_supply1 + b_supply * q

# Original equilibrium
Q1 <- (a_demand1 - a_supply1) / (b_supply - b_demand)
P1 <- demand1(Q1)

# Intermediate equilibrium with demand shift only
Q2_star <- (a_demand2 - a_supply1) / (b_supply - b_demand)
P2_star <- demand2(Q2_star)

# New price after demand shift, at same Q1
P2 <- demand2(Q1)

# Calculate intercept of new supply curve (upward shift due to mining difficulty)
a_supply2 <- P2 - b_supply * Q1
supply2 <- function(q) a_supply2 + b_supply * q

# Data for plot
q <- seq(0, 20, 0.1)
df <- data.frame(
  q = q,
  D1 = demand1(q),
  D2 = demand2(q),
  S1 = supply1(q),
  S2 = supply2(q)
)

# Plot
ggplot(df, aes(x = q)) +
  geom_line(aes(y = D1), color = "darkgreen", size = 1.2) +
  geom_line(aes(y = D2), color = "darkgreen", linetype = "dashed", size = 1.2) +
  geom_line(aes(y = S1), color = "steelblue", size = 1.2) +
  geom_line(aes(y = S2), color = "steelblue", linetype = "dashed", size = 1.2) +
  
  # Equilibrium points
  geom_point(aes(x = Q1, y = P1), color = "black", size = 3) +
  geom_point(aes(x = Q2_star, y = P2_star), color = "blue", size = 3) +
  geom_point(aes(x = Q1, y = P2), color = "purple", size = 3) +
  
  # Labels
  annotate("text", x = Q1 - 0.3, y = P1 - 1, label = "E1 ~ (Q1, P1)", size = 5) +
  annotate("text", x = Q2_star + 0.5, y = P2_star + 1, label = "E2* ~ (Q2*, P2*)", size = 5, color = "blue") +
  annotate("text", x = Q1 + 0.5, y = P2 + 1, label = "E2 (Q2, P2)", size = 5, color = "purple") +
  
  # Arrows for shift 1 (demand ↑)
  annotate("segment", x = 15, xend = 15, y = demand1(15), yend = demand2(15),
           arrow = arrow(length = unit(0.2, "cm")), color = "darkgreen") +
  annotate("text", x = 15.2, y = -0.6 , label = "1. Demand ↑", hjust = 0, color = "darkgreen") +
  
  # Arrows for shift 2 (supply ↓)
  annotate("segment", x = 15, xend = 15, y = supply1(15), yend = supply2(15),
           arrow = arrow(length = unit(0.2, "cm")), color = "steelblue") +
  annotate("text", x = 15.2, y = supply2(15) - 0.3, label = "2. Supply ↓ (mining)", hjust = 0, color = "steelblue") +
  
  labs(title = "Bitcoin Market Adjustment: Demand Increase and Mining-Based Supply Decrease",
       x = "Quantity", y = "Price") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))




#######################################################################

#   Getting Data 

#   If you just want to replicate the results its not necessary run the this part of the code 

######################################################################


# Install necessary package
install.packages("writexl")
install.packages("quantmod")
install.packages(c("httr", "jsonlite"))
install.packages("readxl")

# Load package
library(quantmod)
library(xts)
library(httr)
library(jsonlite)
library(writexl)
library(readxl)


#setwd("") <-- SET HERE YOUR WORKING DIRECTORY WHERE YOU SAVED THE EXCEL I SHARED WITH THIS R FILE 


#######################################################################

#Return Time series Bitcoin-USD

########################################################################


# Download BTC data
getSymbols("BTC-USD", from = "2017-01-01", to = "2024-12-31")

BTC_USD <- `BTC-USD`
BTC_USD
BTC_USD$`BTC-USD.Adjusted`

# Ensure Bitcoin prices are in xts format for merging
BTC_price_xts <- xts(BTC_USD$`BTC-USD.Adjusted`, order.by = index(BTC_USD))
BTC_price_xts

# Calculate daily returns
BTC_returns <- dailyReturn(`BTC-USD`)

head(BTC_returns)

# Ensure Bitcoin returns are in xts format for merging
BTC_returns_xts <- xts(BTC_returns, order.by = index(BTC_returns))

# Rename column for clarity
colnames(BTC_returns_xts) <- "BTC_Returns"
head(BTC_returns_xts)
tail(BTC_returns_xts)


######################################################################

#Hash Rate Time Series 

######################################################################


# Get historical hash rate data (all available data)
response <- GET("https://api.blockchain.info/charts/hash-rate?timespan=all&format=json")

# Check response status
response$status_code # 200 means successful

# Parse the JSON content
content_json <- content(response, "text", encoding = "UTF-8")
hashrate_data <- fromJSON(content_json)

# Structure the data in a clean data.frame
hashrate_df <- data.frame(
  date = as.Date(as.POSIXct(hashrate_data$values$x, origin="1970-01-01")),
  hash_rate = hashrate_data$values$y
)

head(hashrate_df)


# Convert to xts
hashrate_xts <- xts(hashrate_df$hash_rate, order.by = hashrate_df$date)

tail(hashrate_xts)

########################################################################

#Mining difficulty

#######################################################################

difficulty_response <- GET("https://api.blockchain.info/charts/difficulty?timespan=all&format=json")
difficulty_json <- content(difficulty_response, "text", encoding = "UTF-8")
difficulty_data <- fromJSON(difficulty_json)

difficulty_df <- data.frame(
  date = as.Date(as.POSIXct(difficulty_data$values$x, origin="1970-01-01")),
  difficulty = difficulty_data$values$y
)

head(difficulty_df)

tail(difficulty_df)

plot(difficulty_df)


difficulty_xts <- xts(difficulty_df$difficulty, order.by = difficulty_df$date)

tail(difficulty_xts)

##########################################################################

#Transaction volumne 

###########################################################################

trans_vol_response <- GET("https://api.blockchain.info/charts/estimated-transaction-volume?timespan=all&format=json")
trans_vol_json <- content(trans_vol_response, "text", encoding = "UTF-8")
trans_vol_data <- fromJSON(trans_vol_json)

trans_vol_df <- data.frame(
  date = as.Date(as.POSIXct(trans_vol_data$values$x, origin="1970-01-01")),
  transaction_volume_btc = trans_vol_data$values$y
)

head(trans_vol_df)

plot(trans_vol_df)

trans_volume_xts <- xts(trans_vol_df$transaction_volume_btc, order.by = trans_vol_df$date)

tail(trans_volume_xts)

#############################################################################

#Macro indicators

############################################################################

#Etherium and S&P

############################################################################

# Ethereum (ETH-USD) and S&P 500 (^GSPC)
getSymbols(c("ETH-USD", "^GSPC"), from = "2017-01-01", to = "2024-12-31")

# Daily returns
ETH_returns <- dailyReturn(`ETH-USD`)
SP500_returns <- dailyReturn(`GSPC`)

head(SP500_returns)

# Ensure returns are in xts format for merging
ETH_returns_xts <- xts(ETH_returns, order.by = index(ETH_returns))
SP500_returns_xts <- xts(SP500_returns, order.by = index(SP500_returns))

# Rename column for clarity
colnames(ETH_returns_xts) <- "ETH_Returns"
colnames(SP500_returns_xts) <- "SP500_Returns"
head(ETH_returns_xts)
tail(ETH_returns_xts)
plot(ETH_returns_xts)

head(SP500_returns_xts)
tail(SP500_returns_xts)

##############################################################################

#Tresury Yield 

##############################################################################

# US Treasury Yields from FRED
getSymbols("DGS10", src = "FRED") # US 10-year Treasury Yield

# Inspect data
head(DGS10)

DGS10_xts <- xts(DGS10, order.by = index(DGS10))
head(DGS10_xts)
tail(DGS10_xts)

###############################################################################

#Behavioral Data (Download Google Trends data)

#(from https://trends.google.com/trends/explore?date=all&q=Bitcoin&hl=de)

##############################################################################

btc_trends <- read.csv("Bitcoin_google_searches-time_series.csv", skip = 2)

# View column names
colnames(btc_trends)

btc_trends$Date <- as.Date(paste0(btc_trends$Monat, "-01"))  # convert "2022-05" → "2022-05-01"

# Create a sequence of daily dates covering the whole period
daily_dates <- seq(min(btc_trends$Date), max(btc_trends$Date) + 31, by = "day")

# Monthly xts object
monthly_xts <- xts(btc_trends$Bitcoin, btc_trends$Date)

# Merge and forward-fill to daily data
btc_trends_xts <- na.locf(merge(xts(order.by=daily_dates), monthly_xts))

colnames(btc_trends_xts) <- "Bitcoin_Trends"

head(btc_trends_xts)

tail(btc_trends_xts)

plot(btc_trends_xts)

###########################################################################

#binding the data 

###########################################################################

# Merge everything using an inner join on dates
merged_data <- merge(BTC_price_xts, BTC_returns_xts, hashrate_xts, difficulty_xts, trans_volume_xts, ETH_returns_xts, SP500_returns_xts, DGS10_xts, btc_trends_xts, join = "outer")

# Rename columns clearly
colnames(merged_data) <- c("BTC-USD price", "BTC-USD returns", "Hash_Rate", "Difficulty", "Transaction_Volume_BTC", "ETH_returns", "SP500_returns", "10ys_US_treasury_yield", "Google_Searches_BTC")

# Fill NAs
merged_data <- na.locf(merged_data, na.rm = FALSE)  # forward-fill

merged_data <- window(merged_data, start = as.Date("2017-01-01"), end = as.Date("2024-12-31"))

merged_data_clean <- na.omit(merged_data)


head(merged_data_clean)
tail(merged_data_clean)


##########################################################################################

#storing the data 

#########################################################################################


# convert to data.frame
merged_df <- data.frame(Date = index(merged_data_clean), coredata(merged_data_clean))

write_xlsx(merged_df, "new_merged_data_bitcoin_project.xlsx") 


###################################################################################


#    Start time series analysis 


###################################################################################

#   Libraries and packages required 

###################################################################################

install.packages("rugarch")


library(rugarch)
library(forecast)
library(vars)
library(urca)
library(lmtest)


####################################################################################

#Getting data back 

##################################################################################

merged_df <- read_xlsx("new_merged_data_bitcoin_project.xlsx") 

head(merged_df)

##################################################################################

#  ARIMA

#################################################################################

# Fit an ARIMA model without external regressors
arima_basic <- auto.arima(merged_df$BTC.USD.returns,
                          seasonal = FALSE,
                          stepwise = FALSE,
                          approximation = FALSE,
                          ic = "bic")

summary(arima_basic)
checkresiduals(arima_basic)

# Extract residuals
residuals_arima <- residuals(arima_basic)

head(residuals_arima)


# Fit an ARIMA model without external regressors
arima_price <- auto.arima(merged_df$BTC.USD.price,
                          seasonal = FALSE,
                          stepwise = FALSE,
                          approximation = FALSE,
                          ic = "bic")
summary(arima_price)
checkresiduals(arima_price)


####################################################################################

# ARIMAX 

####################################################################################


# Pull y and xreg from merged_df
y <- merged_df$BTC.USD.returns
xreg <- as.matrix(merged_df[, c("Hash_Rate", "Difficulty", "Transaction_Volume_BTC", 
                                "ETH_returns", "SP500_returns", 
                                "X10ys_US_treasury_yield", "Google_Searches_BTC")])
length(y)     # Should match
nrow(xreg)    # Should be identical

xreg_scaled <- scale(xreg)

head(xreg)
head(xreg_scaled)
tail(xreg_scaled)

arimax_model <- auto.arima(y,
                           xreg = xreg_scaled,
                           seasonal = FALSE,
                           stepwise = FALSE,
                           approximation = FALSE, 
                           ic = "bic")

summary(arimax_model)
checkresiduals(arimax_model)

cor(merged_df$Hash_Rate, merged_df$Difficulty)

# exclude Hash rate to make lower danger of multicollinearity or simulteneity bias

xreg <- as.matrix(merged_df[, c("Difficulty", "Transaction_Volume_BTC", 
                                "ETH_returns", "SP500_returns", 
                                "X10ys_US_treasury_yield", "Google_Searches_BTC")])

xreg_scaled <- scale(xreg)

arimax_model <- auto.arima(y,
                           xreg = xreg_scaled,
                           seasonal = FALSE,
                           stepwise = FALSE,
                           approximation = FALSE, 
                           ic = "bic")

summary(arimax_model)
checkresiduals(arimax_model)

# check for multicollinearity 

cor(merged_df$SP500_returns, merged_df$X10ys_US_treasury_yield)

cor(merged_df$SP500_returns, merged_df$X10ys_US_treasury_yield)


# Bitcoin price as dependant varaible 

y <- merged_df$BTC.USD.price

arimax_model <- auto.arima(y,
                           xreg = xreg_scaled,
                           seasonal = FALSE,
                           stepwise = FALSE,
                           approximation = FALSE, 
                           ic = "bic")

summary(arimax_model)
checkresiduals(arimax_model)

####################################################################################################################

#   Granger Causality test

##################################################################################################################

# Select subset for VAR (e.g. returns, hash rate, difficulty, Google trends)
var_data <- merged_df[, c("BTC.USD.returns", "Hash_Rate", "Difficulty", "Google_Searches_BTC")]


apply(var_data, 2, function(x) summary(ur.df(x, type="drift", selectlags="AIC")))

# Difference non-stationary variables
var_data$Hash_Rate_diff <- c(NA, diff(var_data$Hash_Rate))
var_data$Difficulty_diff <- c(NA, diff(var_data$Difficulty))

# Rebuild the VAR dataset with differenced variables
var_ready <- var_data[, c("BTC.USD.returns", "Hash_Rate_diff", "Difficulty_diff", "Google_Searches_BTC")]


# Create a dataframe with only the two series
gc_data <- na.omit(data.frame(
  y = var_data$BTC.USD.returns,
  x = c(NA, diff(var_data$Difficulty))  # diff the difficulty manually
))


# Run Granger causality test: does x (difficulty) cause y (returns)?
grangertest(y ~ x, order = 4, data = gc_data)

# Run Granger causality test: does y (returns) cause x (difficulty)?
grangertest(x ~ y, order = 4, data = gc_data)

# Run Granger causality test: does x (difficulty) cause y (returns)?
grangertest(y ~ x, order = 14, data = gc_data)

#to take the difficulty adjustment after 2016 blocks (aimed to be 2 weeks) into account lags until 20 are modeled (in case difficulty is to high and it takes more than two weeks)

# Run Granger causality test: does x (difficulty) cause y (returns)?
grangertest(y ~ x, order = 20, data = gc_data)

# Run Granger causality test: does y (returns) cause x (difficulty)?
grangertest(x ~ y, order = 20, data = gc_data)

# Run Granger causality test: does x (difficulty) cause y (returns)?
grangertest(y ~ x, order = 150, data = gc_data)


######################################################################################################################

#  Garch(1,1) 

####################################################################################


# Plot PACF of residuals
pacf(residuals_arima, main = "PACF of Residuals from ARIMA(0,0,0)")

# Plot PACF of squared residuals (to check for ARCH effects)
pacf(residuals_arima^2, main = "PACF of Squared Residuals from ARIMA(0,0,0)")


# Specify GARCH(1,1) on residuals
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                         mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
                         distribution.model = "norm")

# Fit the model
garch_fit <- ugarchfit(spec = garch_spec, data = residuals_arima)

# Summarize results
show(garch_fit)


#####################################################################################


#    Forecasting accuracy 


####################################################################################

# splitting the data 

####################################################################################

# Ensure date types match
split_date <- as.POSIXct("2024-01-01")

# Split
train_data <- merged_df[merged_df$Date < split_date, ]
test_data <- merged_df[merged_df$Date >= split_date, ]


####################################################################################

#   ARIMA(0,0,0)

###################################################################################

# Fit ARIMA on training
arima_model <- Arima(train_data$BTC.USD.returns, order = c(0,0,0), include.mean = FALSE)

# Forecast 1-step ahead recursively
arima_forecast <- forecast(arima_model, h = nrow(test_data))
plot(arima_forecast)
accuracy(arima_forecast, test_data$BTC.USD.returns)

# Combine full series and forecast range
full_dates <- merged_df$Date
full_returns <- merged_df$BTC.USD.returns

# Create forecast time index
forecast_dates <- test_data$Date
forecast_mean <- as.numeric(arima_forecast$mean)
lower_bound <- arima_forecast$lower[, 2]
upper_bound <- arima_forecast$upper[, 2]

# Plot full BTC return series
plot(full_dates, full_returns, type = "l", col = "black", lwd = 1,
     xlab = "Date", ylab = "Returns", main = "ARIMA(0,0,0) Forecast vs Actual Returns")

# Overlay actual returns in test period
lines(forecast_dates, test_data$BTC.USD.returns, col = "red", lwd = 1)

# Overlay forecast (constant line around zero)
lines(forecast_dates, forecast_mean, col = "blue", lwd = 2)

# Overlay 95% prediction intervals
lines(forecast_dates, lower_bound, col = "blue", lty = "dashed")
lines(forecast_dates, upper_bound, col = "blue", lty = "dashed")

# Add legend
legend("bottomright", legend = c("Historical Returns", "Actual Test Returns", "Forecast", "95% CI"),
       col = c("black", "red", "blue", "blue"), lty = c(1, 1, 1, 2), lwd = c(1, 1, 2, 1))


##################################################################################

# Garch(1,1)

##################################################################################


garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                         mean.model = list(armaOrder = c(0,0), include.mean = FALSE))

garch_fit <- ugarchfit(spec = garch_spec, data = train_data$BTC.USD.returns)

# Forecast conditional variance (volatility)
garch_forecast <- ugarchforecast(garch_fit, n.ahead = nrow(test_data))
sigma_forecast <- sigma(garch_forecast)

# Plot volatility forecast
plot(sigma_forecast, main = "Forecasted Conditional Volatility")

# Squared returns from test set
realized_vol <- test_data$BTC.USD.returns^2 #since zero mean assumed  

# Forecasted conditional variance from GARCH
garch_forecast <- ugarchforecast(garch_fit, n.ahead = nrow(test_data))
predicted_vol <- as.numeric(sigma(garch_forecast))^2

# Evaluate MSE
mse <- mean((realized_vol - predicted_vol)^2)
mse

#plot realized and forecasted volatility together 

# Convert to time series
realized_vol_ts <- ts(realized_vol)
predicted_vol_ts <- ts(predicted_vol)

# Plot both
plot(realized_vol_ts, type = "l", col = "red", lwd = 1.5,
     main = "Forecasted vs. Realized Volatility",
     ylab = "Volatility", xlab = "Index")
lines(predicted_vol_ts, col = "blue", lwd = 1.5)
legend("topright", legend = c("Realized Volatility", "Forecasted Volatility"),
       col = c("red", "blue"), lty = 1, lwd = 1.5)

#   with exact mean 

mu <- mean(train_data$BTC.USD.returns)
realized_vol <- (test_data$BTC.USD.returns - mu)^2

# Evaluate MSE
mse <- mean((realized_vol - predicted_vol)^2)
mse

#plot realized and forecasted volatility together 

# Convert to time series
realized_vol_ts <- ts(realized_vol)
predicted_vol_ts <- ts(predicted_vol)

# Plot both
plot(realized_vol_ts, type = "l", col = "red", lwd = 1.5,
     main = "Forecasted vs. Realized Volatility",
     ylab = "Volatility", xlab = "Index")
lines(predicted_vol_ts, col = "blue", lwd = 1.5)
legend("topright", legend = c("Realized Volatility", "Forecasted Volatility"),
       col = c("red", "blue"), lty = 1, lwd = 1.5)

#################################################################################








