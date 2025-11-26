#Import necessary libraries
library(readr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(ggplot2)

citation(rpart)

# set working directory to source file location (only in rstudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# set seed
set.seed(4)

#======================================================================
# Import the series as a data frame in R
#======================================================================

raw_data <- read_csv("PredictorData2022.xlsx - Monthly.csv")

colnames(raw_data)[1:18]<-c("Date","Index","Dividends","Earnings","Book-to-Market Ratio",
                            "Treasury Bills","Corporate Bond Yields on AAA-rated Bonds",
                            "Corporate Bond Yields on BAA-rated Bonds",
                            "Long Term Yield", "Net Equity Expansion", "Risk-free Rate",
                            "Inflation", "Long Term Rate of Returns", 
                            "Long-term Corporate Bond Returns",
                            "Stock Variance","Cross-Sectional beta Premium",
                            "Stock Returns Including Dividends",
                            "Stock Returns Excluding Dividends")

#======================================================================
# Generate the excess returns series (stock returns minus Risk-free Rate, where the stock returns
#                                       are the growth rates of the series Index)
#======================================================================

excess_returns<-(raw_data$Index - dplyr::lag(raw_data$Index))/dplyr::lag(raw_data$Index)-raw_data$`Risk-free Rate`

################################Declaring fuction to calculate MSFE#####################
# Evaluate models using MSFE
# Define a function to calculate MSFE
msfe <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}
#########################################################################################

#======================================================================
# a) For each of the two methods you choose, create a one-step-ahead forecast ˆyt+1 based only on the
# lags of the excess returns;
#======================================================================

#(Index and `Risk-free Rate` is removed because the dependent variable is calculated based on them)

data = as.data.frame(excess_returns)
# Add collumn vectors with lagged excess returns
for (i in 1:10) {
  data[,paste0("excess_returns_lag_",i)]<-dplyr::lag(data[,"excess_returns"],n=i)
}
# Add collumn vectors with lagged predictors
for (i in names(raw_data)) {
  data[,paste0("lagged_",i)]<-dplyr::lag(raw_data[,i])
}
# excluding CRSP_. and Date, Index and `Risk-free Rate`
data <- data[, !(names(data) %in% c("lagged_Stock Returns Including Dividends",
                                    "lagged_Stock Returns Excluding Dividends",
                                    "lagged_Date","lagged_Index",
                                    "lagged_Risk-free Rate"))]

# Removing NA-Values
for (i in 1:ncol(data))
{
  data<-dplyr::filter(data, !is.na(data[,i]))
}

#Splitting the dataset into training and testiong data ( 8:2 )
train_index <- sample(1:nrow(data), 0.8 * nrow(data))  # 80% for training
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Clean column names 
# renaming the columns exclusing the spaces because it will give error otherwise while fitting Random forest model
colnames(train_data) <- make.names(colnames(train_data))
colnames(test_data) <- make.names(colnames(test_data))

# model with only lags of excess-returns
regressors_a<-paste0("excess_returns_lag_",1:10)
formula_a<-as.formula(paste("excess_returns ~ ", paste(regressors_a, collapse = "+")))

#############################################################################
#############Cross Validation on training data to fit the model#################

n<-nrow(train_data)
lag_trees<-list()
lag_forests<-list()
lag_lms<-list()#
lag_trees_msfe<-c()
lag_forests_msfe<-c()
lag_lms_msfe<-c()#
for (i in 1:5) {
  lag_trees[[i]]<-rpart(formula_a, data=train_data[((i*n/5)-(n/5)+1):(i*n/5),])
  lag_trees_msfe[i]<-msfe(test_data$excess_returns, predict(lag_trees[[i]], newdata = test_data))
  lag_forests[[i]]<-randomForest(formula_a, data=train_data[((i*n/5)-(n/5)+1):(i*n/5),])
  lag_forests_msfe[i]<-msfe(test_data$excess_returns,predict(lag_forests[[i]], newdata = test_data) )
}

lag_lms <- lm(excess_returns ~ ., data = train_data)#new addition
lag_lms_msfe<-msfe(test_data$excess_returns,predict(lag_lms, newdata = test_data) )#new addition

lag_tree<-lag_trees[which.min(lag_trees_msfe)]
lag_forest<-lag_forests[which.min(lag_forests_msfe)]


# Make predictions on test data using the models obtained from cross-validation
#Regression Tree
tree_pred_lag <- predict(lag_tree, newdata = test_data)

#Random Forest
rf_pred_lag <- predict(lag_forest, newdata = test_data)

#linear model
lm_pred_lag <- predict(lag_lms, newdata = test_data)#new addition

# Calculate MSFE for both models (on test data)
msfe_tree_lagged_return <- msfe(test_data$excess_returns, tree_pred_lag[[1]])
msfe_rf_lagged_return <- msfe(test_data$excess_returns, rf_pred_lag[[1]])
msfe_lm_lagged_return <- msfe(test_data$excess_returns, lm_pred_lag[[1]])#new addition

# Display MSFE for comparison
print(paste("MSFE for Regression Trees considering only lagged excess return:", round(msfe_tree_lagged_return, 4)))

print(paste("MSFE for Random Forests considering only lagged excess return:", round(msfe_rf_lagged_return, 4)))

print(paste("MSFE for linear model considering only lagged excess return:", round(msfe_lm_lagged_return, 4)))#new addition

#======================================================================
# b) Create, again for each method, a one-step-ahead forecast ˆyt+1, but now using the covariates
# additionally to the lags of the excess returns;
#======================================================================
#############################################################################
#############Cross Validation on training data to fit the model#################
n<-nrow(train_data)
predictor_trees<-list()
predictor_forests<-list()
predictor_trees_msfe<-c()
predictor_forests_msfe<-c()
for (i in 1:5) {
  predictor_trees[[i]]<-rpart(excess_returns ~ ., 
                              data = train_data[((i*n/5)-(n/5)+1):(i*n/5),])
  predictor_trees_msfe[i]<-msfe(test_data$excess_returns, predict(predictor_trees[[i]], newdata = test_data))
  predictor_forests[[i]]<-randomForest(excess_returns ~ ., 
                                       data =train_data[((i*n/5)-(n/5)+1):(i*n/5),])
  predictor_forests_msfe[i]<-msfe(test_data$excess_returns,predict(predictor_forests[[i]], newdata = test_data))
  
}
predictor_tree<-predictor_trees[which.min(predictor_trees_msfe)]
predictor_forest<-predictor_forests[which.min(predictor_forests_msfe)]

print(predictor_tree)

#selected trees from the training data is of index of 4 for RT and is of index 2 for RF ####

# Make predictions on test data
# Regression Trees predictions
tree_pred_all_covariates <- predict(predictor_tree, newdata = test_data)
# Random Forests predictions
rf_pred_all_covariates <- predict(predictor_forest, newdata = test_data)

# Calculate MSFE for both models (on test data)
msfe_tree_all_covariates <- msfe(test_data$excess_returns, tree_pred_all_covariates[[1]])
msfe_rf_all_covariates <- msfe(test_data$excess_returns, rf_pred_all_covariates[[1]])

# Display MSFE for comparison
print(paste("MSFE for Regression Trees considering all the covariates and lagged excess return:", round(msfe_tree_all_covariates, 4)))
print(paste("MSFE for Random Forests considering all the covariates and lagged excess return:", round(msfe_rf_all_covariates, 4)))

# Loop through and plot all Regression trees
for (i in 1:length(predictor_trees)) {
  cat("Plotting Tree", i, "\n")
  rpart.plot(predictor_trees[[i]], main = paste("Tree", i))
}

#======================================================================
# c) pick one importance measure of your preference and write code to compute it for the case of the
# forecasts under item b) above.
#======================================================================
importance_tree<-c()
importance_forest<-c()

for (i in (names(train_data)[-1])) {
  imp_data<-train_data
  imp_data[[i]]<-sample(train_data[[i]])
  tree_imp_model<-rpart(excess_returns ~ .,  data = imp_data)
  tree_imp_predict<-predict(tree_imp_model, newdata = test_data)
  forest_imp_model<-randomForest(excess_returns ~ ., data = imp_data)
  forest_imp_predict<-predict(forest_imp_model, newdata = test_data)
  importance_tree[i]<-msfe(test_data$excess_returns,tree_imp_predict)-msfe_tree_all_covariates
  importance_forest[i]<-msfe(test_data$excess_returns,forest_imp_predict)-msfe_rf_all_covariates
}

#Visualization of variable importance
#Regression Tree
# Convert the importance_tree into a data frame for easier plotting
importance_tree_df <- data.frame(
  Variable = names(importance_tree),
  Importance = importance_tree
)

# Sorting the data frame by importance values 
importance_tree_df <- importance_tree_df[order(importance_tree_df$Importance, decreasing = TRUE), ]

# Plotting the Decision Tree variable importance using ggplot2
plot_tree<- ggplot(importance_tree_df, aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "Variables", y = "Importance") +
  ggtitle("Variable Importance in Regression Tree") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size=12))
ggsave("tree_Var_Importance.pdf", plot_tree, width = 11, height = 6)


# Visualization of variable importance for Random Forest
# Convert the importance_forest into a data frame 
importance_forest_df <- data.frame(
  Variable = names(importance_forest),
  Importance = importance_forest
)
print(importance_tree_df)
# Format the Importance values to display without scientific notation and control decimal places
importance_forest_df$Importance <- format(importance_forest_df$Importance, scientific = FALSE, digits = 4)

# Ensure Importance column is numeric
importance_forest_df$Importance <- as.numeric(as.character(importance_forest_df$Importance), scientific = FALSE)

# Sort the data frame by importance values 
importance_forest_df <- importance_forest_df[order(importance_forest_df$Importance, decreasing = TRUE), ]

plot_rf <- ggplot(importance_forest_df, aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", labels = function(x) format(x, scientific = FALSE)) +  # Format legend labels
  labs(x = "Variables", y = "Importance") +
  ggtitle("Variable Importance in Random Forest") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))  # Control y-axis label formatting

ggsave("rf_Var_Importance.pdf", plot_rf, width = 11, height = 6)



#======================================================================
# MSFE Plot
#======================================================================

# Assuming you have calculated MSFEs for all scenarios
msfe_values <- c(msfe_tree_Lagged_return, msfe_rf_Lagged_return, msfe_tree_all_covariates, msfe_rf_all_covariates)
task_names <- c("Regression Trees (Lagged)", "Random Forests (Lagged)", "Regression Trees (Covariates)", "Random Forests (Covariates)")

# Creating a data frame with task names and their corresponding MSFE values
msfe_data_combined <- data.frame(Model = task_names, MSFE = msfe_values)

# Create the bar plot
bar_plot <- ggplot(msfe_data_combined, aes(x = Model, y = MSFE, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8) +
  labs(title = "Comparison of MSFEs for Regression Trees and Random Forests",
       x = "Models", y = "Mean Squared Forecast Error (MSFE)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
        axis.text.y = element_text(size = 13),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18)) +  # Center the title
  geom_text(aes(size= 13 , label = round(MSFE, 4)), position = position_dodge(width = 0.5), vjust = -0.2)


# Save the plot as a PDF with specified dimensions
ggsave("msfe_comparison.pdf", plot = bar_plot, width = 10, height = 6)
