# Install necessary package (uncomment if not installed)
# install.packages("randomForest")

# Load necessary libraries
library(randomForest)
library(caTools)

# Function to run the Random Forest model
run_random_forest <- function(user_input, updated_data) {
  # Load and preprocess the data
  data <- read.csv("Student Stress Factors.csv")
  colnames(data) <- c(
    "Sleep_Quality", 
    "Headaches_Per_Week",
    "Academic_Performance", 
    "Study_Load", 
    "Extracurricular_Activities", 
    "Stress_Levels"
  )
  data$Headaches_Per_Week <- data$Headaches_Per_Week - 1
  
  # Update data with user input if provided
  if (!is.null(user_input) && !any(is.na(user_input))) {
    colnames(user_input) <- c(
      "Sleep_Quality", 
      "Academic_Performance", 
      "Study_Load", 
      "Extracurricular_Activities", 
      "Stress_Levels", 
      "Headaches_Per_Week"
    )
    updated_data <- rbind(updated_data, as.data.frame(user_input))
  }
  
  # Split the data into training and testing sets
  set.seed(123)  # For reproducibility
  split <- sample.split(updated_data$Stress_Levels, SplitRatio = 0.75)
  train_data <- subset(updated_data, split == TRUE)
  test_data <- subset(updated_data, split == FALSE)
  
  # Prepare training and testing matrices
  X_train <- as.matrix(train_data[, c("Sleep_Quality", "Academic_Performance", "Study_Load", "Extracurricular_Activities")])
  Y_train <- train_data[, c("Stress_Levels", "Headaches_Per_Week")]
  X_test <- as.matrix(test_data[, c("Sleep_Quality", "Academic_Performance", "Study_Load", "Extracurricular_Activities")])
  Y_test <- test_data[, c("Stress_Levels", "Headaches_Per_Week")]
  
  # Train Random Forest models
  rf_model_Stress_Levels <- randomForest(X_train, Y_train$Stress_Levels, ntree = 100, importance = TRUE)
  rf_model_Headaches_Per_Week <- randomForest(X_train, Y_train$Headaches_Per_Week, ntree = 100, importance = TRUE)
  
  # Make predictions on the test set
  Stress_Levels_pred <- predict(rf_model_Stress_Levels, X_test)
  Headaches_Per_Week_pred <- predict(rf_model_Headaches_Per_Week, X_test)
  
  # Calculate RMSE
  rmse_Stress_Levels <- sqrt(mean((Y_test$Stress_Levels - Stress_Levels_pred)^2))
  rmse_Headaches_Per_Week <- sqrt(mean((Y_test$Headaches_Per_Week - Headaches_Per_Week_pred)^2))
  
  # Return results
  return(list(
    RMSE_Stress_Levels = rmse_Stress_Levels,
    RMSE_Headaches_Per_Week = rmse_Headaches_Per_Week,
    rf_model_Stress_Levels = rf_model_Stress_Levels,
    rf_model_Headaches_Per_Week = rf_model_Headaches_Per_Week,
    updated_data = updated_data
  ))
}

# Load and preprocess the existing data
original_data <- read.csv("Student Stress Factors.csv")
colnames(original_data) <- c(
  "Sleep_Quality", "Headaches_Per_Week", "Academic_Performance", 
  "Study_Load", "Extracurricular_Activities", "Stress_Levels"
)
original_data <- original_data[, c("Sleep_Quality", "Academic_Performance", 
                                   "Study_Load", "Extracurricular_Activities", 
                                   "Stress_Levels", "Headaches_Per_Week")]
original_data$Headaches_Per_Week <- original_data$Headaches_Per_Week - 1
updated_data <- original_data

# Example user input
user_input <- matrix(c(2, 3, 4, 5, 4, 0), nrow = 1, ncol = 6)

# Run the Random Forest model
result <- run_random_forest(user_input, updated_data)

# Print RMSE values
cat("RMSE for Stress Levels:", result$RMSE_Stress_Levels, "\n")
cat("RMSE for Headaches per Week:", result$RMSE_Headaches_Per_Week, "\n")

# Predictions for new data
new_data <- data.frame(
  Sleep_Quality = 3,
  Academic_Performance = 4,
  Study_Load = 2,
  Extracurricular_Activities = 3
)
Stress_Levels_pred_new <- predict(result$rf_model_Stress_Levels, new_data)
Headaches_Per_Week_pred_new <- predict(result$rf_model_Headaches_Per_Week, new_data)

# Print predictions
cat("Predicted Stress Level:", Stress_Levels_pred_new, "\n")
cat("Predicted Headaches per Week Probability:", Headaches_Per_Week_pred_new / 4 * 100, "%\n")





