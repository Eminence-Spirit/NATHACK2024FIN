# Install necessary packages (uncomment if needed)
# install.packages("randomForest")
# install.packages("caTools")

# Load necessary libraries
library(randomForest)
library(caTools)

# Function to run the Random Forest model
run_random_forest <- function(user_input, updated_data) {
  # Load and preprocess the data
  data <- read.csv("/Users/gyurim/Desktop/NatHack2024/4_17/Proof_by_unpublished_paper/project/Student Stress Factors.csv")
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
    user_input <- as.data.frame(matrix(user_input, ncol = 6)) # Convert to data frame
    colnames(user_input) <- c(
      "Sleep_Quality", 
      "Academic_Performance", 
      "Study_Load", 
      "Extracurricular_Activities", 
      "Stress_Levels", 
      "Headaches_Per_Week"
    )
    updated_data <- rbind(updated_data, user_input)
  }
  
  # Split the data into training and testing sets
  set.seed(123)  # For reproducibility
  split <- sample.split(updated_data$Stress_Levels, SplitRatio = 0.75)
  train_data <- subset(updated_data, split == TRUE)
  test_data <- subset(updated_data, split == FALSE)
  
  # Train Random Forest models
  rf_model_Stress_Levels <- randomForest(
    Stress_Levels ~ Sleep_Quality + Academic_Performance + Study_Load + Extracurricular_Activities, 
    data = train_data, 
    ntree = 100, 
    importance = TRUE
  )
  rf_model_Headaches_Per_Week <- randomForest(
    Headaches_Per_Week ~ Sleep_Quality + Academic_Performance + Study_Load + Extracurricular_Activities, 
    data = train_data, 
    ntree = 100, 
    importance = TRUE
  )
  
  # Make predictions on the test set
  Stress_Levels_pred <- predict(rf_model_Stress_Levels, test_data)
  Headaches_Per_Week_pred <- predict(rf_model_Headaches_Per_Week, test_data)
  
  # Calculate RMSE
  rmse_Stress_Levels <- sqrt(mean((test_data$Stress_Levels - Stress_Levels_pred)^2))
  rmse_Headaches_Per_Week <- sqrt(mean((test_data$Headaches_Per_Week - Headaches_Per_Week_pred)^2))
  
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
original_data <- read.csv("/Users/gyurim/Desktop/NatHack2024/4_17/Proof_by_unpublished_paper/project/Student Stress Factors.csv")
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
user_input <- NA

# Run the Random Forest model
result <- run_random_forest(user_input, updated_data)

# Print RMSE values
cat("RMSE for Stress Levels:", result$RMSE_Stress_Levels, "\n")
cat("RMSE for Headaches per Week:", result$RMSE_Headaches_Per_Week, "\n")

# Predictions for new data
new_data <- data.frame(
  Sleep_Quality = 1,
  Academic_Performance = 1,
  Study_Load = 1,
  Extracurricular_Activities = 1
)
Stress_Levels_pred_new <- predict(result$rf_model_Stress_Levels, new_data)
Headaches_Per_Week_pred_new <- predict(result$rf_model_Headaches_Per_Week, new_data)

# Print predictions
cat("Predicted Stress Level:", Stress_Levels_pred_new, "\n")
cat("Predicted Headaches per Week Probability:", Headaches_Per_Week_pred_new / 3 * 100, "%\n")

# Write predictions to CSV file
prediction_output <- data.frame(
  Sleep_Quality = new_data$Sleep_Quality,
  Academic_Performance = new_data$Academic_Performance,
  Study_Load = new_data$Study_Load,
  Extracurricular_Activities = new_data$Extracurricular_Activities,
  Predicted_Stress_Level = Stress_Levels_pred_new,
  Predicted_Headaches_Per_Week_Probability = Headaches_Per_Week_pred_new / 3 * 100
)

output_file <- "/Users/gyurim/Desktop/NatHack2024/4_17/Proof_by_unpublished_paper/project/Prediction_Output.csv"
write.csv(prediction_output, output_file, row.names = FALSE)

# Confirm output
cat("Predictions written to:", output_file, "\n")
cat("Hi")
