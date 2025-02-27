# ---------------------------------------------------------
# Script for Random Forest Modeling and Evaluation
# ---------------------------------------------------------
# Description:
# This script processes environmental data, builds a random forest model,
# evaluates its performance, and generates important visualizations and plots.
# The steps are as follows:
# 1. Data loading and preparation (handling missing values and factors).
# 2. Stratified data splitting into training and testing sets.
# 3. Exploratory data analysis through correlation matrices and feature plots.
# 4. Training a random forest model using cross-validation.
# 5. Variable importance analysis and visualization.
# 6. Model performance evaluation with RMSE and R-squared.
# 7. Visualization of actual vs. predicted values and correlation vs. predictors.
#
# ----------------------

# Load necessary libraries
library(terra)   # For raster and spatial operations
library(caret)   # For machine learning and cross-validation
library(iml)     # For model interpretation and feature importance
library(dplyr)   # For data manipulation
library(tidyr)   # For reshaping data
library(ggplot2) # For data visualization

# -------------------------------------------------------------------
# 1. Load Data
# -------------------------------------------------------------------
# Read the full CSV to ensure the research area (aoi) is included
model_data <- na.omit(read.csv("data/model_data/model_data.csv"))[,-6]

# (Optional) Subset the data to only include necessary variables, including the research area (aoi)
# Example: model_data <- model_data[, c("correlation", "var1", "var2", "var3", "tree_cover_density", "aridity_index", "research_area")]

# Ensure the research_area (aoi) column is treated as a factor (categorical variable)
model_data$aoi <- as.factor(model_data$aoi)

# -------------------------------------------------------------------
# 2. Create Training and Testing Datasets (Stratified by Study Area)
# -------------------------------------------------------------------
# Set a random seed for reproducibility
set.seed(42)

# Create a stratified partition using the research area (aoi) to ensure each study area is represented in both training and testing datasets
train_indices <- createDataPartition(model_data$aoi, p = 0.7, list = FALSE)
train_data <- model_data[train_indices, ]  # Training data (70%)
test_data  <- model_data[-train_indices, ] # Testing data (30%)

# -------------------------------------------------------------------
# 3. Exploratory Plots and Correlation Matrix
# -------------------------------------------------------------------
# Calculate the correlation matrix for numeric variables in the training data (exclude 'aoi')
num_data <- train_data %>% select_if(is.numeric)
correlation_matrix <- cor(num_data, use = "complete.obs")

# Plot feature relationships (scatter and smooth) between the predictors and the target variable ('correlation')
# Exclude 'correlation' and 'aoi' from predictors
featurePlot(
  x = model_data[, setdiff(names(train_data), c("correlation", "aoi"))], 
  y = model_data$correlation, 
  type = c("p", "smooth"),
  plot = "scatter"
)

# -------------------------------------------------------------------
# 4. Train the Random Forest Model
# -------------------------------------------------------------------
# Set up cross-validation parameters (5-fold cross-validation)
train_control <- trainControl(
  method = "cv",       # Cross-validation
  number = 5,          # 5-fold
  verboseIter = FALSE   # Don't print training progress
)

# Train a Random Forest model using all predictors (excluding 'aoi')
rf_model <- train(
  correlation ~ .,                      # Model the correlation (target variable)
  data = train_data[, !names(train_data) %in% c("aoi")],  # Exclude 'aoi' as predictor
  method = "rf",                        # Use Random Forest algorithm
  trControl = train_control,            # Cross-validation control
  importance = TRUE,                    # Get feature importance
  tuneLength = 5                        # Test 5 different hyperparameter settings
)

# Print model details (e.g., hyperparameters and performance)
print(rf_model)

# -------------------------------------------------------------------
# 5. Variable Importance
# -------------------------------------------------------------------
# Define variable names for better interpretation of results
var_names <- c("Tree Cover Density", "TPI", "TWI", "Slope", "Aspect", "Aridity Index", "DEM")

# Get and sort the importance values of the variables (features)
importance_values <- sort(varImp(rf_model)$importance$Overall, decreasing = FALSE)

# Adjust plot margins for better visibility and create a horizontal bar plot for variable importance
par(mar = c(5, 8, 4, 2) + 0.1)
barplot(importance_values, names.arg = var_names, horiz = TRUE, las = 1, 
        main = "Variable Importance", col = "steelblue")

# Use the iml package to analyze feature importance through permutation method
# Wrap the trained model as a Predictor object (exclude 'correlation' and 'aoi')
predictor <- Predictor$new(
  model = rf_model$finalModel,
  data = train_data[, !names(train_data) %in% c("correlation", "aoi")],
  y = train_data$correlation
)

# Calculate permutation-based feature importance using RMSE as the loss metric
feature_importance <- FeatureImp$new(predictor, loss = "rmse")

# Plot the feature importance values
plot(feature_importance)

# -------------------------------------------------------------------
# 6. Model Performance and Plotting Predicted vs. Actual
# -------------------------------------------------------------------
# Make predictions on the testing dataset using the trained model
predictions <- predict(rf_model, newdata = test_data)

# Calculate performance metrics (RMSE and R-squared) to evaluate model accuracy
rmse <- sqrt(mean((test_data$correlation - predictions)^2))
r_squared <- cor(test_data$correlation, predictions)^2

# Print RMSE and R-squared values
cat("Root Mean Squared Error:", rmse, "\n")
cat("R-squared:", r_squared, "\n")

# Prepare data for plotting Actual vs Predicted values, including study area for coloring
plot_df <- data.frame(
  Actual = test_data$correlation,
  Predicted = predictions,
  research_area = test_data$aoi
)

# Determine the axis limits based on the range of actual and predicted values
axis_limits <- range(c(plot_df$Actual, plot_df$Predicted), na.rm = TRUE)

# Plot the actual vs predicted values with points colored by study area (research area)
ggplot(plot_df, aes(x = Actual, y = Predicted, color = research_area)) +
  geom_point(alpha = 0.6) +                            # Scatter plot
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Reference line (perfect prediction)
  annotate("text", x = Inf, y = 0.2, 
           label = paste("R² =", round(r_squared, 2), "\nRMSE =", round(rmse, 2)), 
           hjust = 1.1, vjust = 1.1, size = 4) +        # Annotation with RMSE and R² values
  labs(title = "Actual vs. Predicted Correlations", 
       x = "Actual Correlation", 
       y = "Predicted Correlation", 
       color = "Study Area") +
  coord_equal(xlim = axis_limits, ylim = axis_limits, expand = FALSE) +
  theme_minimal()

# -------------------------------------------------------------------
# 7. Correlation vs. Predictors Visualization
# -------------------------------------------------------------------
# Reshape the data into long format for visualization
df_long <- model_data %>%
  pivot_longer(
    cols = -c(correlation, aoi),  # Exclude 'correlation' and 'aoi'
    names_to = "Predictor",
    values_to = "Value"
  )

# Map original predictor names to more descriptive names
original_predictors <- names(train_data)[!names(train_data) %in% c("correlation", "aoi")]
var_names <- c("DEM", "Slope", "Aspect", "TPI", "TWI", "Tree Cover Density", "Aridity Index")
names_map <- setNames(var_names, original_predictors)
df_long$Predictor <- names_map[df_long$Predictor]

# Create faceted plot to visualize correlation with each predictor, colored by research area
ggplot(df_long, aes(x = Value, y = correlation, color = aoi)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "black", se = FALSE, size = 0.8) +
  facet_wrap(~ Predictor, scales = "free_x", ncol = 2) +
  labs(
    x = "Predictor Value",
    y = "Correlation",
    title = "Correlation vs. Predictors",
    color = "Study Area"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.95, 0.05),  # Bottom-right corner for legend
    legend.justification = c(1, 0),   # Anchor point for legend
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.margin = ggplot2::margin(t = 50)
  ) +
  guides(color = guide_legend(
    nrow = 4,  # Arrange legend items in multiple rows if necessary
    title.position = "top"
  ))

