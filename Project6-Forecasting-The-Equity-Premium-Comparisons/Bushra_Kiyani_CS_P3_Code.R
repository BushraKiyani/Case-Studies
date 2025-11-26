
#Import necessary libraries
library(readr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(stats)
library(forecast)
library(pheatmap)

# set working directory to source file location (only in rstudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# set seed
set.seed(4)
#===================================================================
#Import the series of a given frequency as a data frame in R
#======================================================================

monthly_raw_data <- read_csv("PredictorData2022.xlsx - Monthly.csv")
quarterly_raw_data <- read_csv("PredictorData2022.xlsx - Quarterly.csv")
annual_raw_data <- read_csv("PredictorData2022.xlsx - Annual.csv")
colnames(monthly_raw_data)<-c("Date","Index","Dividends","Earnings","Book-to-Market Ratio",
                              "Treasury Bills","Corporate Bond Yields on AAA-rated Bonds",
                              "Corporate Bond Yields on BAA-rated Bonds",
                              "Long Term Yield", "Net Equity Expansion", "Risk-free Rate",
                              "Inflation", "Long Term Rate of Returns", 
                              "Long-term Corporate Bond Returns",
                              "Stock Variance","Cross-Sectional beta Premium",
                              "Stock Returns Including Dividends",
                              "Stock Returns Excluding Dividends")
colnames(quarterly_raw_data)<-c("Date","Index","Dividends","Earnings","Book-to-Market Ratio",
                                "Treasury Bills","Corporate Bond Yields on AAA-rated Bonds",
                                "Corporate Bond Yields on BAA-rated Bonds",
                                "Long Term Yield", "Consumption-Wealth-Income-ratio", "Net Equity Expansion", "Risk-free Rate",
                                "Inflation", "Long Term Rate of Returns", 
                                "Long-term Corporate Bond Returns",
                                "Stock Variance","Cross-Sectional beta Premium", "Investment-to-Capital-ratio",
                                "Stock Returns Including Dividends",
                                "Stock Returns Excluding Dividends","Quarterly Dividends", "Quarterly Earnings")

colnames(annual_raw_data)<-c("Date","Index","Dividends","Earnings","Book-to-Market Ratio",
                             "Treasury Bills","Corporate Bond Yields on AAA-rated Bonds",
                             "Corporate Bond Yields on BAA-rated Bonds",
                             "Long Term Yield", " Consumption-wealth-income ratio","Net Equity Expansion", "Risk-free Rate",
                             "Inflation", "Percent Equity Issuing","Long Term Rate of Returns", 
                             "Long-term Corporate Bond Returns",
                             "Stock Variance","Cross-Sectional beta Premium","Investment to Capital Ratio",
                             "Stock Returns Including Dividends",
                             "Stock Returns Excluding Dividends")

#======================================================================
# New Data Frames including only variables of interest
#======================================================================
# Excess return for annual data
excess_returns<-(annual_raw_data$Index - dplyr::lag(annual_raw_data$Index))/dplyr::lag(annual_raw_data$Index)-annual_raw_data$`Risk-free Rate`
annual_data=as.data.frame(excess_returns)
# Add collumn vectors with lagged excess returns
for (i in 1:10) {
  annual_data[,paste0("excess_returns_lag_",i)]<-dplyr::lag(annual_data[,"excess_returns"],n=i)
}
# Add collumn vectors with lagged predictors
for (i in names(annual_raw_data)) {
  annual_data[,paste0("lagged_",i)]<-dplyr::lag(annual_raw_data[,i])
}
# excluding CRSP_. and Date, Index and `Risk-free Rate`
annual_data <- annual_data[, !(names(annual_data) %in% c("lagged_Stock Returns Including Dividends",
                                                         "lagged_Stock Returns Excluding Dividends",
                                                         "lagged_Date","lagged_Index",
                                                         "lagged_Risk-free Rate"))]
# Removing NA-Values
for (i in 1:ncol(annual_data))
{
  annual_data<-dplyr::filter(annual_data, !is.na(annual_data[,i]))
}
colnames(annual_data)=make.names(colnames(annual_data))

# Excess return for quarterly data
excess_returns<-(quarterly_raw_data$Index - dplyr::lag(quarterly_raw_data$Index))/dplyr::lag(quarterly_raw_data$Index)-quarterly_raw_data$`Risk-free Rate`
quarterly_data<-as.data.frame(excess_returns)
# Add collumn vectors with lagged excess returns
for (i in 1:10) {
  quarterly_data[,paste0("excess_returns_lag_",i)]<-dplyr::lag(quarterly_data[,"excess_returns"],n=i)
}
# Add collumn vectors with lagged predictors
for (i in names(quarterly_raw_data)) {
  quarterly_data[,paste0("lagged_",i)]<-dplyr::lag(quarterly_raw_data[,i])
}
# excluding CRSP_. and Date, Index and `Risk-free Rate`
quarterly_data <- quarterly_data[, !(names(quarterly_data) %in% c("lagged_Stock Returns Including Dividends",
                                                                  "lagged_Stock Returns Excluding Dividends",
                                                                  "lagged_Date","lagged_Index",
                                                                  "lagged_Risk-free Rate"))]
# Removing NA-Values
for (i in 1:ncol(quarterly_data))
{
  quarterly_data<-dplyr::filter(quarterly_data, !is.na(quarterly_data[,i]))
}
colnames(quarterly_data)=make.names(colnames(quarterly_data))

# Excess return for monthly data
excess_returns<-(monthly_raw_data$Index - dplyr::lag(monthly_raw_data$Index))/dplyr::lag(monthly_raw_data$Index)-monthly_raw_data$`Risk-free Rate`
monthly_data<-as.data.frame(excess_returns)
# Add collumn vectors with lagged excess returns
for (i in 1:10) {
  monthly_data[,paste0("excess_returns_lag_",i)]<-dplyr::lag(monthly_data[,"excess_returns"],n=i)
}
# Add collumn vectors with lagged predictors
for (i in names(monthly_raw_data)) {
  monthly_data[,paste0("lagged_",i)]<-dplyr::lag(monthly_raw_data[,i])
}
# excluding CRSP_. and Date, Index and `Risk-free Rate`
monthly_data <- monthly_data[, !(names(monthly_data) %in% c("lagged_Stock Returns Including Dividends",
                                                            "lagged_Stock Returns Excluding Dividends",
                                                            "lagged_Date","lagged_Index",
                                                            "lagged_Risk-free Rate"))]
# Removing NA-Values
for (i in 1:ncol(monthly_data))
{
  monthly_data<-dplyr::filter(monthly_data, !is.na(monthly_data[,i]))
}
colnames(monthly_data)=make.names(colnames(monthly_data))

#======================================================================
# First Part
#======================================================================
#======================================================================
# a) For each of the models of the previous two projects and data frequency, create a one-step-ahead
# forecast ˆyt+1 for each time t+1 in the test data, based only on the lags of the excess returns. This
# time you must update (reestimating/refitting) the model to include the information available up
# to the forecasting point. The baseline should be a rolling window approach, but it is ok to work
# with expanding windows.
#======================================================================
# model with only lags of excess-returns
regressors_a<-paste0("excess_returns_lag_",1:10)
formula_a<-as.formula(paste("excess_returns ~ ", paste(regressors_a, collapse = "+")))

forecasts_annual_linear_lagged<-c()
forecasts_annual_tree_lagged<-c()
forecasts_annual_forest_lagged<-c()
for (i in 1:10) {
  linear_model <- arima(annual_data$excess_returns[1:(dim(annual_data)[1]-11+i)], order=c(2,0,0))
  tree_model <-  rpart(formula_a, data =annual_data[1:(dim(annual_data)[1]-11+i),]) 
  forest_model <- randomForest(formula_a, data =annual_data[1:(dim(annual_data)[1]-11+i),]) 
  forecasts_annual_linear_lagged[i]=forecast(linear_model,h=1)$fitted[dim(annual_data)[1]-11+i]
  forecasts_annual_tree_lagged[i]=predict(tree_model, newdata = annual_data[1:(dim(annual_data)[1]-10+i),])[dim(annual_data)[1]-10+i]
  forecasts_annual_forest_lagged[i]=predict(forest_model, newdata = annual_data[1:(dim(annual_data)[1]-10+i),])[dim(annual_data)[1]-10+i]
}

forecasts_quarterly_linear_lagged<-c()
forecasts_quarterly_tree_lagged<-c()
forecasts_quarterly_forest_lagged<-c()
for (i in 1:40) {
  linear_model <- arima(quarterly_data$excess_returns[1:(dim(quarterly_data)[1]-41+i)], order=c(8,0,0))
  tree_model <- rpart(formula_a, data=quarterly_data[1:(dim(quarterly_data)[1]-41+i),])
  forest_model <- randomForest(formula_a, data=quarterly_data[1:(dim(quarterly_data)[1]-41+i),])
  forecasts_quarterly_linear_lagged[i]=forecast(linear_model,h=1)$fitted[dim(quarterly_data)[1]-41+i]
  forecasts_quarterly_tree_lagged[i]=predict(tree_model, newdata = quarterly_data[1:(dim(quarterly_data)[1]-40+i),])[dim(quarterly_data)[1]-40+i]
  forecasts_quarterly_forest_lagged[i]=predict(forest_model, newdata = quarterly_data[1:(dim(quarterly_data)[1]-40+i),])[dim(quarterly_data)[1]-40+i]
}

forecasts_monthly_linear_lagged<-c()
forecasts_monthly_tree_lagged<-c()
forecasts_monthly_forest_lagged<-c()
for (i in 1:120) {
  linear_model <- arima(monthly_data$excess_returns[1:(dim(monthly_data)[1]-121+i)], order=c(1,0,0))
  tree_model <- rpart(formula_a,data=monthly_data[1:(dim(monthly_data)[1]-121+i),])
  forest_model <- randomForest(formula_a,data=monthly_data[1:(dim(monthly_data)[1]-121+i),])
  forecasts_monthly_linear_lagged[i]=forecast(linear_model,h=1)$fitted[dim(monthly_data)[1]-121+i]
  forecasts_monthly_tree_lagged[i]=predict(tree_model, newdata = monthly_data[1:(dim(monthly_data)[1]-120+i),])[dim(monthly_data)[1]-120+i]
  forecasts_monthly_forest_lagged[i]=predict(forest_model, newdata = monthly_data[1:(dim(monthly_data)[1]-120+i),])[dim(monthly_data)[1]-120+i]
}

#=============================================================================================
#Visualization a)
#=============================================================================================
#only With lagged excess return
#=============================================================================================
# Forecasts Monthly Data
#=================================================================
# Combine the data for plotting
plot_data <- data.frame(
  Month = 1:nrow(monthly_data),
  Excess_Returns = monthly_data$excess_returns,
  LM_Forecast = ifelse(1:nrow(monthly_data) > nrow(monthly_data) - 122, forecasts_monthly_linear_lagged, NA),
  RT_Forecast = ifelse(1:nrow(monthly_data) > nrow(monthly_data) - 122, forecasts_monthly_tree_lagged, NA),
  RF_Forecast = ifelse(1:nrow(monthly_data) > nrow(monthly_data) - 122, forecasts_monthly_forest_lagged, NA)
)

# Filter only the last 120 observations
plot_data <- plot_data[(nrow(plot_data) - 121):nrow(plot_data), ]

# Manually set x-axis labels
custom_x_labels <- c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)

# Create a new variable for the x-axis
plot_data$CustomMonth <- 1:nrow(plot_data)

# Create the ggplot
plot = ggplot(plot_data, aes(x = CustomMonth)) +
  geom_line(aes(y = Excess_Returns, color = "Actual Excess Return"), size = 1, alpha = 0.8) +
  geom_line(aes(y = LM_Forecast, color = "Linear Model Forecast"), size = 1, alpha = 0.5) +
  geom_line(aes(y = RT_Forecast, color = "Regression Tree Forecast"), size = 1, alpha = 0.5) +
  geom_line(aes(y = RF_Forecast, color = "Random Forest Forecast"), size = 1, alpha = 0.5) +
  labs(title = "Monthly Data: One-Step-Ahead Forecasts with Original Data",
       x = "Time Point",
       y = "Value") +
  scale_color_manual(
    name = " ",
    values = c("black", "red", "blue", "green")
  ) +
  scale_x_continuous(breaks = custom_x_labels, labels = custom_x_labels) +
  theme_minimal() +
  theme(
    text = element_text(size = 15),  # Adjust the font size
    title = element_text(size = 16),  # Adjust the title font size
    axis.title = element_text(size = 15),  # Adjust the axis title font size
    axis.text = element_text(size = 13),  # Adjust the axis tick font size
    legend.text = element_text(size = 13) )+ # Adjust the legend text font size
  guides(color = guide_legend(override.aes = list(alpha = c(0.8, 0.2, 0.2, 0.2)))) 
ggsave("output_plot_m.pdf", plot, width = 11, height = 4)
#=================================================================
# Forecasts Quarterly Data
#=================================================================
#With lagged excess return only
# Combine the data for plotting
plot_data <- data.frame(
  Quarter = 1:nrow(quarterly_data),
  Excess_Returns = quarterly_data$excess_returns,
  LM_Forecast = ifelse(1:nrow(quarterly_data) > nrow(quarterly_data) - 41, forecasts_quarterly_linear_lagged, NA),
  RT_Forecast = ifelse(1:nrow(quarterly_data) > nrow(quarterly_data) - 41, forecasts_quarterly_tree_lagged, NA),
  RF_Forecast = ifelse(1:nrow(quarterly_data) > nrow(quarterly_data) - 41, forecasts_quarterly_forest_lagged, NA)
)

# Filter only the last 40 observations
plot_data <- plot_data[(nrow(plot_data) - 40):nrow(plot_data), ]


# Manually set x-axis labels
custom_x_labels <- c(1, 5, 10, 15, 20, 25, 30, 35, 40)

# Create a new variable for the x-axis
plot_data$CustomQuarter <- 1:nrow(plot_data)

# Create the ggplot
plot_quarterly = ggplot(plot_data, aes(x = CustomQuarter)) +
  geom_line(aes(y = Excess_Returns, color = "Actual Excess Return"), size = 1, alpha = 0.8) +
  geom_line(aes(y = LM_Forecast, color = "Linear Model Forecast"), size = 1, alpha = 0.5) +
  geom_line(aes(y = RT_Forecast, color = "Regression Tree Forecast"), size = 1, alpha = 0.5) +
  geom_line(aes(y = RF_Forecast, color = "Random Forest Forecast"), size = 1, alpha = 0.5) +
  scale_color_manual(
    name = " ",
    values = c("black", "red", "blue", "green")
  ) +
  scale_x_continuous(breaks = custom_x_labels, labels = custom_x_labels) +
  
  labs(title = "Quarterly Data: One-Step-Ahead Forecasts with Original Data",
       x = "Time Point",
       y = "Value") +
  theme_minimal() +
  theme(
    text = element_text(size = 15),  # Adjust the font size
    title = element_text(size = 16),  # Adjust the title font size
    axis.title = element_text(size = 15),  # Adjust the axis title font size
    axis.text = element_text(size = 13),  # Adjust the axis tick font size
    legend.text = element_text(size = 13)  # Adjust the legend text font size
  ) +
  guides(color = guide_legend(override.aes = list(alpha = c(0.8, 0.2, 0.2, 0.2)))) 

# Save the plot as a PDF file
ggsave("output_plot_q.pdf", plot_quarterly, width = 11, height = 4)  

#=================================================================
# Forecasts Yearly Data
#=================================================================
#with lagged excess return
# Combine the data for plotting
plot_data <- data.frame(
  Year = 1:nrow(annual_data),
  Excess_Returns = annual_data$excess_returns,
  LM_Forecast = ifelse(1:nrow(annual_data) > nrow(annual_data) - 10, forecasts_annual_linear_lagged, NA),
  RT_Forecast = ifelse(1:nrow(annual_data) > nrow(annual_data) - 10, forecasts_annual_tree_lagged, NA),
  RF_Forecast = ifelse(1:nrow(annual_data) > nrow(annual_data) - 10, forecasts_annual_forest_lagged, NA)
)

# Filter only the last 10 observations
plot_data <- plot_data[(nrow(plot_data) - 9):nrow(plot_data), ]

# Manually set x-axis labels
custom_x_labels <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# Create a new variable for the x-axis
plot_data$CustomYear <- 1:nrow(plot_data)

# Create the ggplot
plot_a = ggplot(plot_data, aes(x = CustomYear)) +
  geom_line(aes(y = Excess_Returns, color = "Actual Excess Return"), size = 1, alpha = 0.8) +
  geom_line(aes(y = LM_Forecast, color = "Linear Model Forecast"), size = 1, alpha = 0.5) +
  geom_line(aes(y = RT_Forecast, color = "Regression Tree Forecast"), size = 1, alpha = 0.5) +
  geom_line(aes(y = RF_Forecast, color = "Random Forest Forecast"), size = 1, alpha = 0.5) +
  scale_color_manual(
    name = " ",
    values = c("black", "red", "blue", "green")
  ) +
  scale_x_continuous(breaks = custom_x_labels, labels = custom_x_labels) +
  labs(title = "Annual Data: One-Step-Ahead Forecasts with Original Data",
       x = "Time Point",
       y = "Value") +
  theme_minimal() +
  theme(
    text = element_text(size = 15),  # Adjust the font size
    title = element_text(size = 16),  # Adjust the title font size
    axis.title = element_text(size = 15),  # Adjust the axis title font size
    axis.text = element_text(size = 13),  # Adjust the axis tick font size
    legend.text = element_text(size = 13)) +  # Adjust the legend text font size
  
  guides(color = guide_legend(override.aes = list(alpha = c(0.8, 0.2, 0.2, 0.2))))  

ggsave("output_plot_a.pdf", plot_a, width = 11, height = 4)

#=================================================================
# Root Mean Squared Forecast Error (RMSFE) Comparison Bar graph
#=================================================================

# Calculate RMSFE for each model and frequency
rmsfe_annual_linear_lagged <- sqrt(mean((annual_data$excess_returns[(length(annual_data$excess_returns) - 9):length(annual_data$excess_returns)] - forecasts_annual_linear_lagged)^2))
rmsfe_annual_tree_lagged <- sqrt(mean((annual_data$excess_returns[(length(annual_data$excess_returns) - 9):length(annual_data$excess_returns)] - forecasts_annual_tree_lagged)^2))
rmsfe_annual_forest_lagged <- sqrt(mean((annual_data$excess_returns[(length(annual_data$excess_returns) - 9):length(annual_data$excess_returns)] - forecasts_annual_forest_lagged)^2))

rmsfe_quarterly_linear_lagged <- sqrt(mean((quarterly_data$excess_returns[(length(quarterly_data$excess_returns) - 39):length(quarterly_data$excess_returns)] - forecasts_quarterly_linear_lagged)^2))
rmsfe_quarterly_tree_lagged <- sqrt(mean((quarterly_data$excess_returns[(length(quarterly_data$excess_returns) - 39):length(quarterly_data$excess_returns)] - forecasts_quarterly_tree_lagged)^2))
rmsfe_quarterly_forest_lagged <- sqrt(mean((quarterly_data$excess_returns[(length(quarterly_data$excess_returns) - 39):length(quarterly_data$excess_returns)] - forecasts_quarterly_forest_lagged)^2))

rmsfe_monthly_linear_lagged <- sqrt(mean((monthly_data$excess_returns[(length(monthly_data$excess_returns) - 119):length(monthly_data$excess_returns)] - forecasts_monthly_linear_lagged)^2))
rmsfe_monthly_tree_lagged <- sqrt(mean((monthly_data$excess_returns[(length(monthly_data$excess_returns) - 119):length(monthly_data$excess_returns)] - forecasts_monthly_tree_lagged)^2))
rmsfe_monthly_forest_lagged <- sqrt(mean((monthly_data$excess_returns[(length(monthly_data$excess_returns) - 119):length(monthly_data$excess_returns)] - forecasts_monthly_forest_lagged)^2))

# Create a data frame for plotting
rmsfe_data <- data.frame(
  Frequency = rep(c("Annual", "Quarterly", "Monthly"), each = 3),
  Model = rep(c("Linear Model", "Regression Tree", "Random Forest"), times = 3),
  RMSFE = c(rmsfe_annual_linear_lagged, rmsfe_annual_tree_lagged, rmsfe_annual_forest_lagged,
            rmsfe_quarterly_linear_lagged, rmsfe_quarterly_tree_lagged, rmsfe_quarterly_forest_lagged,
            rmsfe_monthly_linear_lagged, rmsfe_monthly_tree_lagged, rmsfe_monthly_forest_lagged)
)

# Define beautiful colors
beautiful_colors <- c("#e31a1c", "#1f78b4", "#33a02c", "#ff7f00", "#6a3d9a", "#b15928", "#a6cee3", "#fb9a99", "#fdbf6f")

# Plot RMSFE comparison chart
rmsfe_lag = ggplot(rmsfe_data, aes(x = Frequency, y = RMSFE, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(RMSFE, 3)), position = position_dodge(width = 0.9), vjust = -0.5, size = 4, color = "black") +
  scale_fill_manual(values = beautiful_colors) +
  theme_minimal() +
  labs(
    title = "Root Mean Squared Forecast Error (RMSFE) Comparison",
    x = "Frequency",
    y = "RMSFE"
  ) +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 15),  # Adjust the font size
        title = element_text(size = 16),  # Adjust the title font size
        axis.title = element_text(size = 15),  # Adjust the axis title font size
        axis.text = element_text(size = 13),  # Adjust the axis tick font size
        legend.text = element_text(size = 13))
ggsave("rmsfe_lag.pdf", rmsfe_lag, width = 11, height = 5)

#======================================================================
# b) Create, again for each method, a one-step-ahead forecast ˆyt+1, but now using the covariates
# additionally to the lags of the excess returns, updating your forecast as in the previous step.
#======================================================================

forecasts_annual_linear_predictors<-c()
forecasts_annual_backward_predictors<-c()
forecasts_annual_forward_predictors<-c()
forecasts_annual_tree_predictors<-c()
forecasts_annual_forest_predictors<-c()
forecasts_annual_single_predictors<-data.frame()
for (i in 1:10) {
  linear_model=lm(excess_returns ~ .,data=annual_data[1:(dim(annual_data)[1]-11+i),])
  backward_model=lm(excess_returns ~ lagged_Earnings                                  
                    +lagged_Treasury.Bills                           +lagged_Corporate.Bond.Yields.on.BAA.rated.Bonds
                    +lagged_Long.Term.Yield                          + lagged_Inflation                                 
                    +lagged_Long.term.Corporate.Bond.Returns         + lagged_Cross.Sectional.beta.Premium            
                    +lagged_Investment.to.Capital.Ratio 
                    ,data=annual_data[1:(dim(annual_data)[1]-11+i),])
  forward_model=lm(excess_returns ~ lagged_Inflation
                   + lagged_Cross.Sectional.beta.Premium +lagged_Treasury.Bills               
                   + lagged_Investment.to.Capital.Ratio
                   ,data=annual_data[1:(dim(annual_data)[1]-11+i),])
  tree_model=rpart(excess_returns ~ ., data =annual_data[1:(dim(annual_data)[1]-11+i),]) 
  forest_model=randomForest(excess_returns ~ ., data =annual_data[1:(dim(annual_data)[1]-11+i),])
  forecasts_annual_linear_predictors[i]=predict(linear_model, newdata = annual_data[1:(dim(annual_data)[1]-10+i),])[dim(annual_data)[1]-10+i]
  forecasts_annual_backward_predictors[i]=predict(backward_model, newdata = annual_data[1:(dim(annual_data)[1]-10+i),])[dim(annual_data)[1]-10+i]
  forecasts_annual_forward_predictors[i]=predict(forward_model, newdata = annual_data[1:(dim(annual_data)[1]-10+i),])[dim(annual_data)[1]-10+i]
  forecasts_annual_tree_predictors[i]=predict(tree_model, newdata = annual_data[1:(dim(annual_data)[1]-10+i),])[dim(annual_data)[1]-10+i]
  forecasts_annual_forest_predictors[i]=predict(forest_model, newdata = annual_data[1:(dim(annual_data)[1]-10+i),])[dim(annual_data)[1]-10+i]
  
  for (j in (names(annual_data))[-1]) {
    formula=as.formula(paste("excess_returns ~ ",j))
    single_model=lm(formula,data=annual_data[1:(dim(annual_data)[1]-11+i),])
    forecasts_annual_single_predictors[i,j]=predict(single_model, newdata = annual_data[1:(dim(annual_data)[1]-10+i),])[dim(annual_data)[1]-10+i]
  }
}

forecasts_quarterly_linear_predictors<-c()
forecasts_quarterly_single_predictors<-c()
forecasts_quarterly_backward_predictors<-c()
forecasts_quarterly_forward_predictors<-c()
forecasts_quarterly_tree_predictors<-c()
forecasts_quarterly_forest_predictors<-c()
forecasts_quarterly_single_predictors<-data.frame()
for (i in 1:40) {
  linear_model=lm(excess_returns ~ .,data=quarterly_data[1:(dim(quarterly_data)[1]-41+i),])
  backward_model=lm(excess_returns ~ lagged_Dividends                                 
                    +lagged_Treasury.Bills+lagged_Corporate.Bond.Yields.on.AAA.rated.Bonds
                    +lagged_Long.Term.Yield +lagged_Inflation                                 
                    +lagged_Long.Term.Rate.of.Returns+lagged_Long.term.Corporate.Bond.Returns        
                    +lagged_Stock.Variance+lagged_Quarterly.Dividends  
                    ,data=quarterly_data[1:(dim(quarterly_data)[1]-41+i),])
  forward_model=lm(excess_returns ~ lagged_Stock.Variance                          
                   +lagged_Inflation +lagged_Treasury.Bills                          
                   +lagged_Corporate.Bond.Yields.on.AAA.rated.Bonds
                   ,data=quarterly_data[1:(dim(quarterly_data)[1]-41+i),]
  )
  tree_model=rpart(excess_returns ~ .,data=quarterly_data[1:(dim(quarterly_data)[1]-41+i),])
  forest_model=randomForest(excess_returns ~ .,data=quarterly_data[1:(dim(quarterly_data)[1]-41+i),])
  forecasts_quarterly_linear_predictors[i]=predict(linear_model, newdata = quarterly_data[1:(dim(quarterly_data)[1]-40+i),])[dim(quarterly_data)[1]-40+i]
  forecasts_quarterly_backward_predictors[i]=predict(backward_model, newdata = quarterly_data[1:(dim(quarterly_data)[1]-40+i),])[dim(quarterly_data)[1]-40+i]
  forecasts_quarterly_forward_predictors[i]=predict(forward_model, newdata = quarterly_data[1:(dim(quarterly_data)[1]-40+i),])[dim(quarterly_data)[1]-40+i]
  forecasts_quarterly_tree_predictors[i]=predict(tree_model, newdata = quarterly_data[1:(dim(quarterly_data)[1]-40+i),])[dim(quarterly_data)[1]-40+i]
  forecasts_quarterly_forest_predictors[i]=predict(forest_model, newdata = quarterly_data[1:(dim(quarterly_data)[1]-40+i),])[dim(quarterly_data)[1]-40+i]
  
  for (j in names(quarterly_data)[-1]) {
    formula=as.formula(paste("excess_returns ~ ",j))
    single_model=lm(formula,data=quarterly_data[1:(dim(quarterly_data)[1]-41+i),])
    forecasts_quarterly_single_predictors[i,j]=predict(single_model, newdata = quarterly_data[1:(dim(quarterly_data)[1]-40+i),])[dim(quarterly_data)[1]-40+i]
  }
}

forecasts_monthly_linear_predictors<-c()
forecasts_monthly_single_predictors<-c()
forecasts_monthly_backward_predictors<-c()
forecasts_monthly_forward_predictors<-c()
forecasts_monthly_tree_predictors<-c()
forecasts_monthly_forest_predictors<-c()
forecasts_monthly_single_predictors<-data.frame()
for (i in 1:120) {
  linear_model=lm(excess_returns ~ .,data=monthly_data[1:(dim(monthly_data)[1]-121+i),])
  backward_model=lm(excess_returns ~ lagged_Book.to.Market.Ratio                   
                    +lagged_Corporate.Bond.Yields.on.AAA.rated.Bonds + lagged_Corporate.Bond.Yields.on.BAA.rated.Bonds
                    +lagged_Long.Term.Yield                   + lagged_Net.Equity.Expansion                   
                    +lagged_Long.Term.Rate.of.Returns               +lagged_Long.term.Corporate.Bond.Returns       
                    +lagged_Stock.Variance                          +lagged_Cross.Sectional.beta.Premium,
                    data=monthly_data[1:(dim(monthly_data)[1]-121+i),])
  forward_model=lm(excess_returns ~ lagged_Stock.Variance                  
                   +lagged_Long.term.Corporate.Bond.Returns + lagged_Cross.Sectional.beta.Premium    
                   +lagged_Book.to.Market.Ratio            + lagged_Net.Equity.Expansion            
                   +lagged_Long.Term.Rate.of.Returns, 
                   data=monthly_data[1:(dim(monthly_data)[1]-121+i),])
  tree_model=rpart(excess_returns ~ .,data=monthly_data[1:(dim(monthly_data)[1]-121+i),])
  forest_model=randomForest(excess_returns ~ .,data=monthly_data[1:(dim(monthly_data)[1]-121+i),])
  forecasts_monthly_linear_predictors[i]=predict(linear_model, newdata = monthly_data[1:(dim(monthly_data)[1]-120+i),])[dim(monthly_data)[1]-120+i]
  forecasts_monthly_backward_predictors[i]=predict(backward_model, newdata = monthly_data[1:(dim(monthly_data)[1]-120+i),])[dim(monthly_data)[1]-120+i]
  forecasts_monthly_forward_predictors[i]=predict(forward_model, newdata = monthly_data[1:(dim(monthly_data)[1]-120+i),])[dim(monthly_data)[1]-120+i]
  forecasts_monthly_tree_predictors[i]=predict(tree_model, newdata = monthly_data[1:(dim(monthly_data)[1]-120+i),])[dim(monthly_data)[1]-120+i]
  forecasts_monthly_forest_predictors[i]=predict(forest_model, newdata = monthly_data[1:(dim(monthly_data)[1]-120+i),])[dim(monthly_data)[1]-120+i]
  
  for (j in names(monthly_data)[-1]) {
    formula=as.formula(paste("excess_returns ~ ",j))
    single_model=lm(formula,data=monthly_data[1:(dim(monthly_data)[1]-121+i),])
    forecasts_monthly_single_predictors[i,j]=predict(single_model, newdata = monthly_data[1:(dim(monthly_data)[1]-120+i),])[dim(monthly_data)[1]-120+i]
  }
}

#=================================================================
# RMSFE Comparison of Forecasting Models - Annual Data
#=================================================================

# Calculate RMSFE for lagged predictors and covariates forecasts
rmsfe_linear_predictors_annual <- sqrt(mean((annual_data$excess_returns[(length(annual_data$excess_returns) - 9):length(annual_data$excess_returns)] - forecasts_annual_linear_predictors)^2))
rmsfe_backward_predictors_annual <- sqrt(mean((annual_data$excess_returns[(length(annual_data$excess_returns) - 9):length(annual_data$excess_returns)] - forecasts_annual_backward_predictors)^2))
rmsfe_forward_predictors_annual <- sqrt(mean((annual_data$excess_returns[(length(annual_data$excess_returns) - 9):length(annual_data$excess_returns)] - forecasts_annual_forward_predictors)^2))
rmsfe_tree_predictors_annual <- sqrt(mean((annual_data$excess_returns[(length(annual_data$excess_returns) - 9):length(annual_data$excess_returns)] - forecasts_annual_tree_predictors)^2))
rmsfe_forest_predictors_annual <- sqrt(mean((annual_data$excess_returns[(length(annual_data$excess_returns) - 9):length(annual_data$excess_returns)] - forecasts_annual_forest_predictors)^2))

# Calculate RMSFE for each single predictor (for the last 10 years)
rmsfe_annual_single_predictors <- sapply(colnames(forecasts_annual_single_predictors), function(col) {
  sqrt(mean((annual_data$excess_returns[(length(annual_data$excess_returns) - 9):length(annual_data$excess_returns)] - forecasts_annual_single_predictors[, col])^2))
})


# Create a data frame for plotting
rmsfe_data_annual_updated <- data.frame(
  Model = c("Linear Predictors", "Backward Predictors", "Forward Predictors", "Tree Predictors", "Forest Predictors", colnames(forecasts_annual_single_predictors)),
  RMSFE = c(rmsfe_linear_predictors_annual, rmsfe_backward_predictors_annual, rmsfe_forward_predictors_annual, rmsfe_tree_predictors_annual, rmsfe_forest_predictors_annual, rmsfe_annual_single_predictors)
)


# Plot RMSFE comparison chart

annual_rmsfe = ggplot(rmsfe_data_annual_updated, aes(x=Model, y=RMSFE, fill=Model)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=round(RMSFE, 4)), position=position_dodge(width=0.9), angle=90, hjust=1.2, vjust=0.5, size=3.5) +
  theme_minimal() +
  labs(title = "RMSFE Comparison of Forecasting Models - Annual Data",
       x = "Model",
       y = "RMSFE") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none", # this removes the legend
        axis.text.x = element_text(angle = 90, hjust = 1)) # this rotates the x-axis labels
ggsave("annual_rmsfe_last_10_years.pdf", annual_rmsfe, width = 11, height = 6)

#=================================================================
# RMSFE Comparison of Forecasting Models - Quarterly Data
#=================================================================


# Calculate RMSFE for lagged predictors and covariates forecasts
rmsfe_linear_predictors_quarterly <- sqrt(mean((quarterly_data$excess_returns[(length(quarterly_data$excess_returns) - 39):length(quarterly_data$excess_returns)] - forecasts_quarterly_linear_predictors)^2))
rmsfe_backward_predictors_quarterly <- sqrt(mean((quarterly_data$excess_returns[(length(quarterly_data$excess_returns) - 39):length(quarterly_data$excess_returns)] - forecasts_quarterly_backward_predictors)^2))
rmsfe_forward_predictors_quarterly <- sqrt(mean((quarterly_data$excess_returns[(length(quarterly_data$excess_returns) - 39):length(quarterly_data$excess_returns)] - forecasts_quarterly_forward_predictors)^2))
rmsfe_tree_predictors_quarterly <- sqrt(mean((quarterly_data$excess_returns[(length(quarterly_data$excess_returns) - 39):length(quarterly_data$excess_returns)] - forecasts_quarterly_tree_predictors)^2))
rmsfe_forest_predictors_quarterly <- sqrt(mean((quarterly_data$excess_returns[(length(quarterly_data$excess_returns) - 39):length(quarterly_data$excess_returns)] - forecasts_quarterly_forest_predictors)^2))

# Calculate RMSFE for each single predictor (for the last 10 years)
rmsfe_quarterly_single_predictors <- sapply(colnames(forecasts_quarterly_single_predictors), function(col) {
  sqrt(mean((quarterly_data$excess_returns[(length(quarterly_data$excess_returns) - 39):length(quarterly_data$excess_returns)] - forecasts_quarterly_single_predictors[, col])^2))
})

# Create a data frame for plotting
rmsfe_data_quarterly_updated <- data.frame(
  Model = c("Linear Predictors", "Backward Predictors", "Forward Predictors", "Tree Predictors", "Forest Predictors", colnames(forecasts_quarterly_single_predictors)),
  RMSFE = c(rmsfe_linear_predictors_quarterly, rmsfe_backward_predictors_quarterly, rmsfe_forward_predictors_quarterly, rmsfe_tree_predictors_quarterly, rmsfe_forest_predictors_quarterly, rmsfe_quarterly_single_predictors)
)

# Plot RMSFE comparison chart

quarterly_rmsfe = ggplot(rmsfe_data_quarterly_updated, aes(x=Model, y=RMSFE, fill=Model)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=round(RMSFE, 4)), position=position_dodge(width=0.9), angle=90, hjust=1.08, vjust=0.5, size=3.5) +
  theme_minimal() +
  labs(title = "RMSFE Comparison of Forecasting Models - Quarterly Data",
       x = "Model",
       y = "RMSFE") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none", # this removes the legend
        axis.text.x = element_text(angle = 90, hjust = 1)) # this rotates the x-axis labels
ggsave("quarterly_rmsfe_last_10_years.pdf", quarterly_rmsfe, width = 11, height = 6)

#=================================================================
# RMSFE Comparison of Forecasting Models - Monthly Data
#=================================================================

# Calculate RMSFE for lagged predictors and covariates forecasts
rmsfe_linear_predictors_monthly <- sqrt(mean((monthly_data$excess_returns[(length(monthly_data$excess_returns) - 119):length(monthly_data$excess_returns)] - forecasts_monthly_linear_predictors)^2))
rmsfe_backward_predictors_monthly <- sqrt(mean((monthly_data$excess_returns[(length(monthly_data$excess_returns) - 119):length(monthly_data$excess_returns)] - forecasts_monthly_backward_predictors)^2))
rmsfe_forward_predictors_monthly <- sqrt(mean((monthly_data$excess_returns[(length(monthly_data$excess_returns) - 119):length(monthly_data$excess_returns)] - forecasts_monthly_forward_predictors)^2))
rmsfe_tree_predictors_monthly <- sqrt(mean((monthly_data$excess_returns[(length(monthly_data$excess_returns) - 119):length(monthly_data$excess_returns)] - forecasts_monthly_tree_predictors)^2))
rmsfe_forest_predictors_monthly <- sqrt(mean((monthly_data$excess_returns[(length(monthly_data$excess_returns) - 119):length(monthly_data$excess_returns)] - forecasts_monthly_forest_predictors)^2))

# Calculate RMSFE for each single predictor (for the last 10 years)
rmsfe_monthly_single_predictors <- sapply(colnames(forecasts_monthly_single_predictors), function(col) {
  sqrt(mean((monthly_data$excess_returns[(length(monthly_data$excess_returns) - 119):length(monthly_data$excess_returns)] - forecasts_monthly_single_predictors[, col])^2))
})

# Create a data frame for plotting
rmsfe_data_monthly_updated <- data.frame(
  Model = c("Linear Predictors", "Backward Predictors", "Forward Predictors", "Tree Predictors", "Forest Predictors", colnames(forecasts_monthly_single_predictors)),
  RMSFE = c(rmsfe_linear_predictors_monthly, rmsfe_backward_predictors_monthly, rmsfe_forward_predictors_monthly, rmsfe_tree_predictors_monthly, rmsfe_forest_predictors_monthly, rmsfe_monthly_single_predictors)
)

# Plot RMSFE comparison chart
monthly_rmsfe = ggplot(rmsfe_data_monthly_updated, aes(x=Model, y=RMSFE, fill=Model)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=round(RMSFE, 4)), position=position_dodge(width=0.9), angle=90, hjust=1.2, vjust=0.5, size=3.5) +
  theme_minimal() +
  labs(title = "RMSFE Comparison of Forecasting Models - Monthly Data",
       x = "Model",
       y = "RMSFE") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none", # this removes the legend
        axis.text.x = element_text(angle = 90, hjust = 1)) # this rotates the x-axis labels
ggsave("monthly_rmsfe_last_10_years.pdf", monthly_rmsfe, width = 11, height = 6)



#======================================================================
# Second Part
#======================================================================
#======================================================================
# b) Once you have understood the test procedure, you will perform a Diebold-Mariano Test for each
# pair of forecasts available from the first part of the project and choose the models that perform
# best according to this procedure.
#======================================================================

forecasts_annual=cbind(
  forecasts_annual_linear_lagged,
  forecasts_annual_tree_lagged,
  forecasts_annual_forest_lagged,
  forecasts_annual_linear_predictors,
  forecasts_annual_backward_predictors,
  forecasts_annual_forward_predictors,
  forecasts_annual_tree_predictors,
  forecasts_annual_forest_predictors,
  forecasts_annual_single_predictors
)
dm_annual=matrix(nrow=ncol(forecasts_annual),ncol=ncol(forecasts_annual))
for (i in 1:ncol(forecasts_annual)) {
  for (j in 1:ncol(forecasts_annual)) {
    if(i!=j)
      dm_annual[i,j]=dm.test(forecasts_annual[,i]-
                               annual_data$excess_returns[(length(annual_data$excess_returns)-9):
                                                            length(annual_data$excess_returns)],
                             forecasts_annual[,j]-
                               annual_data$excess_returns[(length(annual_data$excess_returns)-9):
                                                            length(annual_data$excess_returns)]
                             ,h=1,alternative = "less")$p.value
  }
}
colnames(dm_annual)=colnames(forecasts_annual)
rownames(dm_annual)=colnames(forecasts_annual)

forecasts_quarterly=cbind(
  forecasts_quarterly_linear_lagged,
  forecasts_quarterly_tree_lagged,
  forecasts_quarterly_forest_lagged,
  forecasts_quarterly_linear_predictors,
  forecasts_quarterly_backward_predictors,
  forecasts_quarterly_forward_predictors,
  forecasts_quarterly_tree_predictors,
  forecasts_quarterly_forest_predictors,
  forecasts_quarterly_single_predictors
)
dm_quarterly=matrix(nrow=ncol(forecasts_quarterly),ncol=ncol(forecasts_quarterly))
for (i in 1:ncol(forecasts_quarterly)) {
  for (j in 1:ncol(forecasts_quarterly)) {
    if(i!=j)
      dm_quarterly[i,j]=dm.test(forecasts_quarterly[,i]-quarterly_data$excess_returns[(length(quarterly_data$excess_returns)-39):
                                                                                        length(quarterly_data$excess_returns)],
                                forecasts_quarterly[,j]-quarterly_data$excess_returns[(length(quarterly_data$excess_returns)-39):
                                                                                        length(quarterly_data$excess_returns)]
                                ,h=1,alternative = "less")$p.value
  }
}
colnames(dm_quarterly)=colnames(forecasts_quarterly)
rownames(dm_quarterly)=colnames(forecasts_quarterly)

forecasts_monthly=cbind(
  forecasts_monthly_linear_lagged,
  forecasts_monthly_tree_lagged,
  forecasts_monthly_forest_lagged,
  forecasts_monthly_linear_predictors,
  forecasts_monthly_backward_predictors,
  forecasts_monthly_forward_predictors,
  forecasts_monthly_tree_predictors,
  forecasts_monthly_forest_predictors,
  forecasts_monthly_single_predictors
)
dm_monthly=matrix(nrow=ncol(forecasts_monthly),ncol=ncol(forecasts_monthly))
for (i in 1:ncol(forecasts_monthly)) {
  for (j in 1:ncol(forecasts_monthly)) {
    if(i!=j)
      dm_monthly[i,j]=dm.test(forecasts_monthly[,i]-monthly_data$excess_returns[(length(monthly_data$excess_returns)-119):
                                                                                  length(monthly_data$excess_returns)],
                              forecasts_monthly[,j]-monthly_data$excess_returns[(length(monthly_data$excess_returns)-119):
                                                                                  length(monthly_data$excess_returns)],
                              h=1,alternative = "less")$p.value
  }
}
colnames(dm_monthly)=colnames(forecasts_monthly)

rownames(dm_monthly)=colnames(forecasts_monthly)


#======================================================
# Diebold-Mariano Test: Heatmap of P-values - Annual Data
#======================================================


# Your p-values matrix
p_values_matrix <- dm_annual # Assuming dm_annual is your matrix of p-values

# Significance threshold (adjust based on your significance level)
significance_threshold <- 0.05

# Create a binary matrix for conditional coloring
color_matrix <- ifelse(p_values_matrix <= significance_threshold, "significant", "non-significant")

# Convert the binary matrix to a matrix for color mapping
color_matrix <- matrix(as.numeric(factor(color_matrix, levels = c("non-significant", "significant"))), nrow = nrow(p_values_matrix), ncol = ncol(p_values_matrix))

# Define color palette for the heatmap
heatmap_colors <- c("white", "skyblue")

# Define legend labels
legend_labels <- c("Non-significant", "Significant")

# Open a PDF file for plotting
pdf("pheatmap_dm_annual.pdf", width = 11, height = 8)

# Plot the heatmap with legend
pheatmap::pheatmap(
  p_values_matrix,
  color = heatmap_colors,
  annotation_legend = FALSE,
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  clustering_method = "complete",
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  main = "Diebold-Mariano Test: Heatmap of P-values",  # Add your title here
  show_colnames = TRUE,  # Show column names
  show_rownames = TRUE,  # Show row names
  legend = TRUE,  # Add color legend
  legend_position = "top",  # Set legend position
  legend_labels = legend_labels,  # Set legend labels
  legend_border = TRUE
  
)

# Close the PDF file
dev.off()

#======================================================
# Diebold-Mariano Test: Heatmap of P-values - Quarterly Data
#======================================================



# Your p-values matrix
p_values_matrix <- dm_quarterly # Assuming dm_annual is your matrix of p-values

# Significance threshold (adjust based on your significance level)
significance_threshold <- 0.05

# Create a binary matrix for conditional coloring
color_matrix <- ifelse(p_values_matrix <= significance_threshold, "significant", "non-significant")

# Convert the binary matrix to a matrix for color mapping
color_matrix <- matrix(as.numeric(factor(color_matrix, levels = c("non-significant", "significant"))), nrow = nrow(p_values_matrix), ncol = ncol(p_values_matrix))

# Define color palette for the heatmap
heatmap_colors <- c("white", "lightgreen")

# Define legend labels
legend_labels <- c("Non-significant", "Significant")

# Open a PDF file for plotting
pdf("pheatmap_dm_quarterly.pdf", width = 11, height = 8)

# Plot the heatmap with legend
pheatmap::pheatmap(
  p_values_matrix,
  color = heatmap_colors,
  annotation_legend = FALSE,
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  clustering_method = "complete",
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  main = "Diebold-Mariano Test: Heatmap of P-values - Quarterly Data",  # Add your title here
  show_colnames = TRUE,  # Show column names
  show_rownames = TRUE,  # Show row names
  legend = TRUE,  # Add color legend
  legend_position = "top",  # Set legend position
  legend_labels = legend_labels,  # Set legend labels
  legend_border = TRUE
  
)

# Close the PDF file
dev.off()

#======================================================
# Diebold-Mariano Test: Heatmap of P-values - Monthly Data"
#======================================================


# Your p-values matrix
p_values_matrix <- dm_monthly # Assuming dm_annual is your matrix of p-values

# Significance threshold (adjust based on your significance level)
significance_threshold <- 0.05

# Create a binary matrix for conditional coloring
color_matrix <- ifelse(p_values_matrix <= significance_threshold, "significant", "non-significant")

# Convert the binary matrix to a matrix for color mapping
color_matrix <- matrix(as.numeric(factor(color_matrix, levels = c("non-significant", "significant"))), nrow = nrow(p_values_matrix), ncol = ncol(p_values_matrix))

# Define color palette for the heatmap
heatmap_colors <- c("white", "pink")

# Define legend labels
legend_labels <- c("Non-significant", "Significant")

# Open a PDF file for plotting
pdf("pheatmap_dm_monthly.pdf", width = 11, height = 8)

# Plot the heatmap with legend
pheatmap::pheatmap(
  p_values_matrix,
  color = heatmap_colors,
  annotation_legend = FALSE,
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  clustering_method = "complete",
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  main = "Diebold-Mariano Test: Heatmap of P-values - Monthly Data",  # Add your title here
  show_colnames = TRUE,  # Show column names
  show_rownames = TRUE,  # Show row names
  legend = TRUE,  # Add color legend
  legend_position = "top",  # Set legend position
  legend_labels = legend_labels,  # Set legend labels
  legend_border = TRUE
  
)

# Close the PDF file
dev.off()

#======================================================================
# c) Based on the results from the previous item and on the information you gathered from both
# papers and your work on the two previous projects, build conclusions on the ability of the
# models with which you have worked to forecast the returns and also on their shortcomings and
# potential improvements and further outlook.
#======================================================================
