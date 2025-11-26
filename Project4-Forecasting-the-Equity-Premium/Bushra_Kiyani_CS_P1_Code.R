#Import necessary libraries
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(readr)
library(stats)
library(forecast)

# set working directory to source file location (only in rstudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#======================================================================
#a) Import the series of a given frequency as a data frame in R
#======================================================================

monthly_data <- read_csv("PredictorData2022.xlsx - Monthly.csv")
quarterly_data <- read_csv("PredictorData2022.xlsx - Quarterly.csv")
yearly_data <- read_csv("PredictorData2022.xlsx - Annual.csv")

colnames(monthly_data)[1:18]<-c("Date","Index","Dividends","Earnings","Book-to-Market-ratio",
                                "Treasury Bills","Corporate Bond Yields on AAA-rated Bonds",
                                "Corporate Bond Yields on BAA-rated Bonds",
                                "Long Term Yield", "Net Equity Expansion", "Risk-free-rate",
                                "Inflation", "Long-term Rate of Returns", 
                                "Long-term Corporate Bond Returns",
                                "Stock Variance","Cross-Sectional beta Premium",
                                "Stock Returns Including Dividends",
                                "Stock Returns Excluding Dividends")


colnames(quarterly_data)<-c("Date","Index","Dividends","Earnings","Book-to-Market-ratio",
                            "Treasury Bills","Corporate Bond Yields on AAA-rated Bonds",
                            "Corporate Bond Yields on BAA-rated Bonds",
                            "Long Term Yield", "Consumption-Wealth-Income-ratio", "Net Equity Expansion", "Risk-free-rate",
                            "Inflation", "Long-term Rate of Returns", 
                            "Long-term Corporate Bond Returns",
                            "Stock Variance","Cross-Sectional beta Premium", "Investment-to-Capital-ratio",
                            "Stock Returns Including Dividends",
                            "Stock Returns Excluding Dividends","Quarterly Dividends", "Quarterly Earnings")

colnames(yearly_data)[1:21]<-c("Date","Index","Dividends","Earnings","Book-to-Market-ratio",
                               "Treasury Bills","Corporate Bond Yields on AAA-rated Bonds",
                               "Corporate Bond Yields on BAA-rated Bonds",
                               "Long Term Yield", " Consumption-wealth-income ratio","Net Equity Expansion", "Risk-free-rate",
                               "Inflation", "Percent Equity Issuing","Long-term Rate of Returns", 
                               "Long-term Corporate Bond Returns",
                               "Stock Variance","Cross-Sectional beta Premium","Investment to Capital Ratio ",
                               "Stock Returns Including Dividends",
                               "Stock Returns Excluding Dividends")

#======================================================================
#b) Generate the excess returns series (stock returns minus risk-free rate, where the stock returns
#                                       are the growth rates of the series Index)
#======================================================================

# Excess return for yearly data
excess_return_annual<-(yearly_data$Index - lag(yearly_data$Index))/lag(yearly_data$Index)-yearly_data$`Risk-free-rate`
yearly_data$excess_return<-excess_return_annual

# Excess return for quarterly data
excess_return_quarter<-(quarterly_data$Index - lag(quarterly_data$Index))/lag(quarterly_data$Index)-quarterly_data$`Risk-free-rate`
quarterly_data$excess_return<-excess_return_quarter

# Excess return for monthly data
excess_return_month<-(monthly_data$Index - lag(monthly_data$Index))/lag(monthly_data$Index)-monthly_data$`Risk-free-rate`
monthly_data$excess_return<-excess_return_month

#======================================================================
#c) Further explore the structure of the excess returns series, this time exploring some time series
#aspects like serial correlation and variance. For that, you will produce plots of the ACF and
#PACF.
#======================================================================

#For yearly data
acf(yearly_data$excess_return ,na.action = na.pass)
pacf(yearly_data$excess_return ,na.action = na.pass)
#significant => 2,5
#For quarterly data
acf(quarterly_data$excess_return, na.action = na.pass)
pacf(quarterly_data$excess_return,na.action = na.pass)
#3,4,7,10
#For monthly data
acf(monthly_data$excess_return, na.action = na.pass)
pacf(monthly_data$excess_return, na.action = na.pass)

#======================================================================
#d) Fit an AR(p) model based on the information obtained from the ACF and PACF; select p using
#an information criterion.
#======================================================================

#========================================
#For yearly data
#========================================

possible_p_yearly <- c(1:5)  # You can adjust the range
q=1
# Initialize vectors to store AIC values
aic_values_yearly <- numeric(length(possible_p_yearly))
# Fit AR models and calculate AIC for different 'p' values
for (p in possible_p_yearly) {
  model <- arima(yearly_data$excess_return, order = c(p, 0, 0), method = "ML")  # Fit AR(p) model
  aic_values_yearly[q] <- AIC(model)  # Calculate AIC for the model
  q=q+1
}
#select the model with lowest AIC value( best model )
best_aic_p_yearly <- possible_p_yearly[which.min(aic_values_yearly)]
cat("Best p value selected for yearly data by AIC Criterion:", best_aic_p_yearly)

#========================================
#For quarterly data
#========================================

# A vector of possible 'p' values (orders)
possible_p_quarterly <- c(1:7)  # You can adjust the range
q=1
# Initialize vectors to store AIC values
aic_values_quarterly <- numeric(length(possible_p_quarterly))
# Fit AR models and calculate AIC for different 'p' values
for (p in possible_p_quarterly) {
  model <- arima(quarterly_data$excess_return, order = c(p, 0, 0), method = "ML")  # Fit AR(p) model
  aic_values_quarterly[q] <- AIC(model)  
  q=q+1
}
#select the model with lowest AIC value( best model )
best_aic_p_quarterly <- possible_p_quarterly[which.min(aic_values_quarterly)]
cat("Best p value selected for quarterly data by AIC Criterion:", best_aic_p_quarterly)

#========================================
#For monthly data
#========================================

# A vector of possible 'p' values (orders)
possible_p_monthly <- c(1)  # You can adjust the range
q=1
# Initialize vectors to store AIC values
aic_values_monthly <- numeric(length(possible_p_monthly))
# Fit AR models and calculate AIC for different 'p' values
for (p in possible_p_monthly) {
  model <- arima(monthly_data$excess_return, order = c(p, 0, 0), method = "ML")  # Fit AR(p) model
  aic_values_monthly[q] <- AIC(model)  # Calculate AIC for the model
  q=q+1
}

#select the model with lowest AIC value( best model )
best_aic_p_monthly <- possible_p_monthly[which.min(aic_values_monthly)]
cat("Best p value selected for monthly data by AIC Criterion:", best_aic_p_monthly)

#======================================================================
#Best p values for the data
#======================================================================
#Yearly Data
cat("Best p value selected for yearly data is by AIC Criterion and it is:", best_aic_p_yearly)
cat("Best p value selected for quarterly data is by AIC Criterion and it is:", best_aic_p_quarterly)
cat("Best p value selected for monthly data is by AIC Criterion and it is:", best_aic_p_monthly)

#======================================================================
#Fitted model based on best p values for the data
#======================================================================
#Best quarterly model
yearly_model <- arima(yearly_data$excess_return, order = c(best_aic_p_yearly, 0, 0))  

#Best quarterly model
quarterly_model <- arima(quarterly_data$excess_return, order = c(best_aic_p_quarterly, 0, 0))  

#Best monthly model
monthly_model <- arima(monthly_data$excess_return, order = c(best_aic_p_monthly, 0, 0))  

#======================================================================
#e) Based on this full-sample autoregressive fit, generate forecasts at all times in the sample and
#compute the corresponding MSFE. (Think why this would not exactly be doable in real life.)
#Illustrate the forecasts together with the actual returns in one plot.
#======================================================================
#=============================================================================
#For yearly data
#=============================================================================

# Generate forecasts for the full sample
forecast_values <- forecast(yearly_model, h = nrow(yearly_data))
# Calculate the MSFE
actual_returns <- yearly_data$excess_return
forecast_errors <- actual_returns - forecast_values$fitted
msfe_yearly <- sqrt(mean(forecast_errors[2:152]^2))#starting from index 2 because 1st element is NA
# Create a time series of years
years <- seq(1871, 2022)



plot_data_fitted <- data.frame(Year = years, ActualReturns = actual_returns, Forecast = forecast_values$fitted)

# Create the plot 
yearly_ar_plot<-ggplot(plot_data_fitted, aes(x = Year)) +
  geom_line(aes(y = ActualReturns, color = "Actual Returns"), size = 1, alpha = 0.7, group = 1) +
  geom_line(aes(y = Forecast, color = "Forecasted Returns"), size = 1, alpha = 0.7, group = 2) +
  labs(x = "Year", y = "Returns") +
  scale_x_continuous(breaks = years, labels = as.character(years)) +
  scale_color_manual(values = c("Actual Returns" = "#1b9e77", "Forecasted Returns" = "#d95f02")) +
  guides(color = guide_legend("Returns"), 
         size = guide_legend("Returns")) +
  theme_light(base_size = 10) + 
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14))
ggsave("yearly_ar_plot.pdf", yearly_ar_plot, width = 8, height = 3, dpi =400)
#=============================================================================
#For quartly data
#=============================================================================

# Generate forecasts for the full sample
forecast_values <- forecast(quarterly_model, h = nrow(quarterly_data))
# Calculate the MSFE
actual_returns <- quarterly_data$excess_return
forecast_errors <- actual_returns - forecast_values$fitted
msfe_quarterly <- sqrt(mean(forecast_errors[2:608]^2)) # again skipping 1st element because it is NA
# Create a time series of years
years <- quarterly_data$Date

plot_data_fitted <- data.frame(Year = years, ActualReturns = actual_returns, Forecast = forecast_values$fitted)
# Create the plot 
quarterly_ar_plot<-ggplot(plot_data_fitted, aes(x = Year)) +
  geom_line(aes(y = ActualReturns, color = "Actual Returns"), size = 1, alpha = 0.7, group = 1) +
  geom_line(aes(y = Forecast, color = "Forecasted Returns"), size = 1, alpha = 0.7, group = 2) +
  labs(x = "Year", y = "Returns") +
  scale_x_continuous(breaks = years, labels = as.character(years)) +
  scale_color_manual(values = c("Actual Returns" = "#1b9e77", "Forecasted Returns" = "#d95f02")) +
  guides(color = guide_legend("Returns"), 
         size = guide_legend("Returns")) +
  theme_light(base_size = 10) + 
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14))
ggsave("quarterly_ar_plot.pdf", quarterly_ar_plot, width = 8, height = 3, dpi = 400)
#=============================================================================
#For monthly data
#=============================================================================

# Generate forecasts for the full sample
forecast_values <- forecast(monthly_model, h = nrow(monthly_data))
# Calculate the MSFE
actual_returns <- monthly_data$excess_return
forecast_errors <- actual_returns - forecast_values$fitted
msfe_monthly <- sqrt(mean(forecast_errors[2:1824]^2)) # Skipping 1st element because of NA
# Create a time series of years
years <- monthly_data$Date

plot_data_forecast <- data.frame(Year = years, ActualReturns = actual_returns, Forecast = forecast_values$fitted)
# Create the plot 
monthly_ar_plot<-ggplot(plot_data_forecast, aes(x = Year)) +
  geom_line(aes(y = ActualReturns, color = "Actual Returns"), size = 1, alpha = 0.7, group = 1) +
  geom_line(aes(y = Forecast, color = "Forecasted Returns"), size = 1, alpha = 0.7, group = 2) +
  labs(x = "Year", y = "Returns") +
  scale_x_continuous(breaks = years, labels = as.character(years)) +
  scale_color_manual(values = c("Actual Returns" = "#1b9e77", "Forecasted Returns" = "#d95f02")) +
  guides(color = guide_legend("Returns"), 
         size = guide_legend("Returns")) +
  theme_light(base_size = 10) + 
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 14))

ggsave("monthly_ar_plot.pdf", monthly_ar_plot, width = 8, height = 3, dpi = 400)
#==================================================================================
#f) Set up linear predictive models using in turn each predictor alone2 and estimate them using OLS.
#(Don’t forget to lag the predictor before setting up the model.) Compute the corresponding
#MSFEs and compare them to the ones obtained from the autoregressive model.
#==================================================================================

#create dataframe excluding CRSP_. and Date,Index and `Risk-free-rate` (Index and `Risk-free-rate` is removed because the dependent variable is calculated based on them)
yearly_data_new <- yearly_data[, !(names(yearly_data) %in% c("Stock Returns Including Dividends", "Stock Returns Excluding Dividends","Date","Index","Risk-free-rate"))]
quarterly_data_new <- quarterly_data[, !(names(quarterly_data) %in% c("Stock Returns Including Dividends", "Stock Returns Excluding Dividends","Date","Index","Risk-free-rate"))]
monthly_data_new <- monthly_data[, !(names(monthly_data) %in% c("Stock Returns Including Dividends", "Stock Returns Excluding Dividends","Date","Index","Risk-free-rate"))]

#=============================================================================
#For yearly data
#=============================================================================

# Initialize a vector to store MSFE values for each predictor model
msfe_values_yearly <- c()

# Loop through each predictor
for (predictor_name in c("Dividends","Earnings","Book-to-Market-ratio",
                         "Treasury Bills","Corporate Bond Yields on AAA-rated Bonds",
                         "Corporate Bond Yields on BAA-rated Bonds",
                         "Long Term Yield", " Consumption-wealth-income ratio","Net Equity Expansion",
                         "Inflation", "Percent Equity Issuing","Long-term Rate of Returns", 
                         "Long-term Corporate Bond Returns",
                         "Stock Variance","Cross-Sectional beta Premium","Investment to Capital Ratio ")) {
  # Lag the predictor if needed
  yearly_data_new <- yearly_data_new %>%
    filter(!is.na(excess_return))
  lagged_predictor <- lag(yearly_data_new[, predictor_name])
  yearly_data_new$lagged_predictor<-as.double(lagged_predictor[[1]])
  yearly_data_new <- yearly_data_new %>%
    filter(!is.na(lagged_predictor))
  #print(yearly_data_new$lagged_predictor)
  #yearly_data_new$lagged_predictor<-lagged_predictor
  # Set up the linear model
  model <- lm(excess_return ~ lagged_predictor, data = yearly_data_new)
  # Estimate the model
  model_summary <- summary(model)
  
  # Compute the MSFE
  predicted_values <- predict(model)
  msfe <- sqrt(mean((yearly_data_new$excess_return-predicted_values)^2))
  msfe_values_yearly <- c(msfe_values_yearly, msfe)
  
  # Print model results and MSFE for the current predictor
  cat(paste("Predictor:", predictor_name, "\n"))
  cat("Model Summary:\n")
  print(model_summary)
  cat(paste("MSFE:", msfe, "\n\n"))
  yearly_data_new <- yearly_data_new[, !names(yearly_data_new) %in% "lagged_predictor"]
}

# Compare MSFE values with the AR model
ar_model_msfe <- msfe_yearly # MSFE from the AR model
cat(paste("AR Model MSFE for Yearly data:", ar_model_msfe, "\n"))
predictor_yearly <- data.frame(
  Name = c("Dividends","Earnings","Book-to-Market-ratio",
           "Treasury Bills","Corporate Bond Yields on AAA-rated Bonds",
           "Corporate Bond Yields on BAA-rated Bonds",
           "Long Term Yield", " Consumption-wealth-income ratio","Net Equity Expansion",
           "Inflation", "Percent Equity Issuing","Long-term Rate of Returns", 
           "Long-term Corporate Bond Returns",
           "Stock Variance","Cross-Sectional beta Premium","Investment to Capital Ratio ")
)

predictor_yearly$MSFE_value<-msfe_values_yearly
cat(paste("MSFE value for Yearly data after lm fit:"))

print(predictor_yearly)

#=============================================================================
#For quarterly data
#=============================================================================

# Initialize a vector to store MSFE values for each predictor model
msfe_values_quarterly <- c()

# Loop through each predictor
for (predictor_name in c("Dividends","Earnings","Book-to-Market-ratio",
                         "Treasury Bills","Corporate Bond Yields on AAA-rated Bonds",
                         "Corporate Bond Yields on BAA-rated Bonds",
                         "Long Term Yield", "Consumption-Wealth-Income-ratio", "Net Equity Expansion",
                         "Inflation", "Long-term Rate of Returns", 
                         "Long-term Corporate Bond Returns",
                         "Stock Variance","Cross-Sectional beta Premium", "Investment-to-Capital-ratio",
                         "Quarterly Dividends", "Quarterly Earnings")) {
  # Lag the predictor if needed
  quarterly_data_new <- quarterly_data_new %>%
    filter(!is.na(excess_return))
  lagged_predictor <- lag(quarterly_data_new[, predictor_name])
  quarterly_data_new$lagged_predictor<-as.double(lagged_predictor[[1]])
  quarterly_data_new <- quarterly_data_new %>%
    filter(!is.na(lagged_predictor))
  #print(quarterly_data_new$lagged_predictor)
  #quarterly_data_new$lagged_predictor<-lagged_predictor
  # Set up the linear model
  model <- lm(excess_return ~ lagged_predictor, data = quarterly_data_new)
  # Estimate the model
  model_summary <- summary(model)
  
  # Compute the MSFE
  predicted_values <- predict(model)
  msfe <- sqrt(mean((quarterly_data_new$excess_return-predicted_values)^2))
  msfe_values_quarterly <- c(msfe_values_quarterly, msfe)
  
  # Print model results and MSFE for the current predictor
  cat(paste("Predictor:", predictor_name, "\n"))
  cat("Model Summary:\n")
  print(model_summary)
  cat(paste("MSFE:", msfe, "\n\n"))
  quarterly_data_new <- quarterly_data_new[, !names(quarterly_data_new) %in% "lagged_predictor"]
}

# Compare MSFE values with the AR model
ar_model_msfe <- msfe_quarterly # MSFE from the AR model
cat(paste("AR Model MSFE for Quarterly data:", ar_model_msfe, "\n"))
predictor_quarterly <- data.frame(
  Name = c("Dividends","Earnings","Book-to-Market-ratio",
           "Treasury Bills","Corporate Bond Yields on AAA-rated Bonds",
           "Corporate Bond Yields on BAA-rated Bonds",
           "Long Term Yield", "Consumption-Wealth-Income-ratio", "Net Equity Expansion",
           "Inflation", "Long-term Rate of Returns", 
           "Long-term Corporate Bond Returns",
           "Stock Variance","Cross-Sectional beta Premium", "Investment-to-Capital-ratio",
           "Quarterly Dividends", "Quarterly Earnings")
)

predictor_quarterly$MSFE_value<-msfe_values_quarterly
cat(paste("MSFE value of Quarterly data after lm fit:"))
print(predictor_quarterly)


#=============================================================================
#For monthly data
#=============================================================================

# Initialize a vector to store MSFE values for each predictor model
msfe_values_monthly <- c()

# Loop through each predictor
for (predictor_name in c("Dividends", "Earnings", "Book-to-Market-ratio", "Treasury Bills", 
                         "Corporate Bond Yields on AAA-rated Bonds", "Corporate Bond Yields on BAA-rated Bonds", 
                         "Long Term Yield", "Net Equity Expansion", "Inflation", 
                         "Long-term Rate of Returns", "Long-term Corporate Bond Returns", "Stock Variance", 
                         "Cross-Sectional beta Premium")) {
  # Lag the predictor if needed
  monthly_data_new <- monthly_data_new %>%
    filter(!is.na(excess_return))
  lagged_predictor <- lag(monthly_data_new[, predictor_name])
  monthly_data_new$lagged_predictor<-as.double(lagged_predictor[[1]])
  monthly_data_new <- monthly_data_new %>%
    filter(!is.na(lagged_predictor))
  #print(monthly_data_new$lagged_predictor)
  #monthly_data_new$lagged_predictor<-lagged_predictor
  # Set up the linear model
  model <- lm(excess_return ~ lagged_predictor, data = monthly_data_new)
  # Estimate the model
  model_summary <- summary(model)
  
  # Compute the MSFE
  predicted_values <- predict(model)
  msfe <- sqrt(mean((monthly_data_new$excess_return-predicted_values)^2))
  msfe_values_monthly <- c(msfe_values_monthly, msfe)
  
  # Print model results and MSFE for the current predictor
  cat(paste("Predictor:", predictor_name, "\n"))
  cat("Model Summary:\n")
  print(model_summary)
  cat(paste("MSFE:", msfe, "\n\n"))
  monthly_data_new <- monthly_data_new[, !names(monthly_data_new) %in% "lagged_predictor"]
}

# Compare MSFE values with the AR model
ar_model_msfe <- msfe_monthly # MSFE from the AR model
cat(paste("AR Model for Monthly data MSFE:", ar_model_msfe, "\n"))
predictor_monthly <- data.frame(
  Name = c("Dividends", "Earnings", "Book-to-Market-ratio", "Treasury Bills", 
           "Corporate Bond Yields on AAA-rated Bonds", "Corporate Bond Yields on BAA-rated Bonds", 
           "Long Term Yield", "Net Equity Expansion", "Inflation", 
           "Long-term Rate of Returns", "Long-term Corporate Bond Returns", "Stock Variance", 
           "Cross-Sectional beta Premium")
)

predictor_monthly$MSFE_value<-msfe_values_monthly
cat(paste("MSFE value of Monthly data after lm fit:"))
print(predictor_monthly)

predictor_monthly

#=============================================================================
#g) Set up a predictive model using all predictors, generate the forecasts and compute the MSFE.
#Compare to the MSFEs of the forecasts based on one predictor at the time and give possible
#explanations for the resulting differences.
#=============================================================================

#we are using predict() instead of forecast()(which we have used before) because here
#the model is linear model fitted on data. Earlier the models were AR/ARIMA models 
#fitted on time series data.

#=============================================================================
#For yearly data
#=============================================================================

# Create a model using all predictors
#yearly_data_new <- yearly_data[, !(names(yearly_data) %in% c("Stock Returns Including Dividends", "Stock Returns Excluding Dividends"))]
model_all <- lm(excess_return ~ ., data = yearly_data_new)
summary(model_all)
forecasts_all <- predict(model_all)
lagged_predictor <- lag(yearly_data_new[, "excess_return"])
lagged_predictor <- lagged_predictor %>%
  filter(!is.na(lagged_predictor))
forecast_error <- forecasts_all - lagged_predictor
msfe_all_yearly <- sqrt(mean(forecast_error$excess_return^2))
cat("MSFE of the model after forecasting Excess Return for Yearly data using all predictors:\n")
print(msfe_all_yearly)
cat("MSFE of the model after forecasting Excess Return for Yearly data using one predictor at a time:\n")
print(predictor_yearly)

#=============================================================================
#For quarterly data
#=============================================================================

# Create a model using all predictors
#quarterly_data_new <- quarterly_data[, !(names(quarterly_data) %in% c("Stock Returns Including Dividends", "Stock Returns Excluding Dividends"))]
model_all <- lm(excess_return ~ ., data = quarterly_data_new)
summary(model_all)
forecasts_all <- predict(model_all)
lagged_predictor <- lag(quarterly_data_new[, "excess_return"])
lagged_predictor <- lagged_predictor %>%
  filter(!is.na(lagged_predictor))
forecast_error <- forecasts_all - lagged_predictor
msfe_all_quarterly <- sqrt(mean(forecast_error$excess_return^2))

cat("MSFE of the model after forecasting Excess Return for Quarterly data using all predictors:\n")
print(msfe_all_quarterly)
cat("MSFE of the model after forecasting Excess Return for Quarterly data using one predictor at a time:\n")
print(predictor_quarterly)

#=============================================================================
#For monthly data
#=============================================================================

# Create a model using all predictors
#monthly_data_new <- quarterly_data[, !(names(quarterly_data) %in% c("Stock Returns Including Dividends", "Stock Returns Excluding Dividends"))]
model_all <- lm(excess_return ~ ., data = monthly_data_new)
summary(model_all)
forecasts_all <- predict(model_all)
lagged_predictor <- lag(monthly_data_new[, "excess_return"])
lagged_predictor <- lagged_predictor %>%
  filter(!is.na(lagged_predictor))
forecast_error <- forecasts_all - lagged_predictor
msfe_all_monthly <- sqrt(mean(forecast_error$excess_return^2))

cat("MSFE of the model after forecasting Excess Return for Monthly data using all predictors:\n")
print(msfe_all_monthly)
cat("MSFE of the model after forecasting Excess Return for Monthly data using one predictor at a time:\n")
print(predictor_monthly)
#=============================================================================
#h) Select the most promising predictors when fitting a multiple predictive regression; use an information 
#criterion to this end. Generate the forecasts, compute the MSF E and compare it with your previous results.
#=============================================================================

#=============================================================================
#For yearly data
#=============================================================================

# Remove rows with missing values
yearly_data_new <- na.omit(yearly_data_new)
# Fit a model using all predictors
full_model_yearly <- lm(excess_return ~ ., data = yearly_data_new)
# Use AIC for predictor selection
selected_model_yearly_backward <- step(full_model_yearly, direction = "backward", k = 2)  # Adjust k as needed
forecasts_selected_yearly_backward <- predict(selected_model_yearly_backward)
msfe_selected_yearly_backward <- sqrt(mean((forecasts_selected_yearly_backward - yearly_data_new$excess_return)^2))
cat("MSFE of the model with the most promising predictors for the Yearly data using AIC and backward selection:\n")
print(msfe_selected_yearly_backward)
summary(selected_model_yearly_backward)

#=============================================================================
#For quarterly data
#=============================================================================

# Remove rows with missing values
quarterly_data_new <- na.omit(quarterly_data_new)
# Fit a model using all predictors
full_model_quarterly <- lm(excess_return ~ ., data = quarterly_data_new)
# Use AIC for predictor selection
selected_model_quarterly_backward <- step(full_model_quarterly, direction = "backward", k = 2)  # Adjust k as needed
forecasts_selected_quarterly_backward <- predict(selected_model_quarterly_backward)
msfe_selected_quarterly_backward <- sqrt(mean((forecasts_selected_quarterly_backward - quarterly_data_new$excess_return)^2))
cat("MSFE of the model with the most promising predictors for the Quarterly data using AIC and backward selection:\n")
print(msfe_selected_quarterly_backward)
summary(selected_model_quarterly_backward)

#=============================================================================
#For monthly data
#=============================================================================

# Remove rows with missing values
monthly_data_new <- na.omit(monthly_data_new)
# Fit a model using all predictors
full_model_monthly <- lm(excess_return ~ ., data = monthly_data_new)
# Use AIC for predictor selection
selected_model_monthly_backward <- step(full_model_monthly, direction = "backward", k = 2)  # Adjust k as needed
forecasts_selected_monthly_backward <- predict(selected_model_monthly_backward)
msfe_selected_monthly_backward <- sqrt(mean((forecasts_selected_monthly_backward - monthly_data_new$excess_return)^2))
cat("MSFE of the model with the most promising predictors for the Monthly data using AIC and backward selection:\n")
print(msfe_selected_monthly_backward)
summary(selected_model_monthly_backward)

#============================================================================
#i) Repeat the previous task but use forward stepwise model selection to this end.
#============================================================================

#=============================================================================
#For yearly data
#=============================================================================

# Remove rows with missing values
#yearly_data_new <- na.omit(yearly_data_new)
# Fit a model using all predictors
full_model_yearly <- lm(excess_return ~ ., data = yearly_data_new)
# Use AIC for predictor selection
selected_model_yearly_forward <- step(lm(excess_return ~ 1, data = yearly_data_new),scope=formula(full_model_yearly), direction = "forward", k = 2)
summary(selected_model_yearly_forward)
forecasts_selected_yearly_forward <- predict(selected_model_yearly_forward)
msfe_selected_yearly_forward <- sqrt(mean((forecasts_selected_yearly_forward - yearly_data_new$excess_return)^2))
cat("MSFE of the model with the most promising predictors for the Yearly data using AIC and forward selection:\n")
print(msfe_selected_yearly_forward)
#=============================================================================
#For quarterly data
#=============================================================================

# Remove rows with missing values
#quarterly_data_new <- na.omit(quarterly_data_new)
# Fit a model using all predictors
full_model_quarterly <- lm(excess_return ~ ., data = quarterly_data_new)
# Use AIC for predictor selection
selected_model_quarterly_forward <- step(lm(excess_return ~ 1, data = quarterly_data_new),scope=formula(full_model_quarterly), direction = "forward", k = 2)
summary(selected_model_quarterly_forward)
forecasts_selected_quarterly_forward <- predict(selected_model_quarterly_forward)
msfe_selected_quarterly_forward <- sqrt(mean((forecasts_selected_quarterly_forward - quarterly_data_new$excess_return)^2))
cat("MSFE of the model with the most promising predictors for the Quarterly data using AIC and forward selection:\n")
print(msfe_selected_quarterly_forward)

#=============================================================================
#For monthly data
#=============================================================================

# Remove rows with missing values
#monthly_data_new <- na.omit(monthly_data_new)
# Fit a model using all predictors
full_model_monthly <- lm(excess_return ~ ., data = monthly_data_new)
# Use AIC for predictor selection
selected_model_monthly_forward <- step(lm(excess_return ~1, data = monthly_data_new),scope=formula(full_model_monthly), direction = "forward", k = 2) 
summary(selected_model_monthly_forward)
forecasts_selected_monthly_forward <- predict(selected_model_monthly_forward)
msfe_selected_monthly_forward <- sqrt(mean((forecasts_selected_monthly_forward - monthly_data_new$excess_return)^2))
cat("MSFE of the model with the most promising predictors for the Monthly data using AIC and forward selection:\n")
print(msfe_selected_monthly_forward)

