random.forest <- function(input_vector) {
  data <- read.csv("Student Stress Factors.csv")
  
  library(randomForest)
  library(glmnet)
  library(caTools)
  
  # Rename columns for clarity
  colnames(data) <- c("Sleep_Quality", "Headaches_Per_Week", "Academic_Performance", 
                      "Study_Load", "Extracurricular_Activities", "Stress_Levels")
  
  # Adjust 'Headaches_Per_Week' by subtracting 1
  data$Headaches_Per_Week <- data$Headaches_Per_Week - 1
  
  # Set random seed for reproducibility and split the data
  set.seed(123)
  split <- sample.split(data$Stress_Levels, SplitRatio = 0.75)
  train_data <- subset(data, split == TRUE)
  test_data <- subset(data, split == FALSE)
  
  # Prepare training and testing matrices
  X_train <- as.matrix(train_data[, c("Sleep_Quality", "Academic_Performance", "Study_Load", "Extracurricular_Activities")])
  Y_train_Stress_Levels <- train_data$Stress_Levels
  Y_train_Headaches_Per_Week <- train_data$Headaches_Per_Week
  X_test <- as.matrix(test_data[, c("Sleep_Quality", "Academic_Performance", "Study_Load", "Extracurricular_Activities")])
  Y_test_Stress_Levels <- test_data$Stress_Levels
  Y_test_Headaches_Per_Week <- test_data$Headaches_Per_Week
  
  # Train Random Forest models
  rf_model_Stress_Levels <- randomForest(X_train, Y_train_Stress_Levels, ntree = 100, importance = TRUE)
  rf_model_Headaches_Per_Week <- randomForest(X_train, Y_train_Headaches_Per_Week, ntree = 100, importance = TRUE)
  
  # Make predictions on test data
  Stress_Levels_pred <- predict(rf_model_Stress_Levels, X_test)
  Headaches_Per_Week_pred <- predict(rf_model_Headaches_Per_Week, X_test)
  
  # Evaluate performance
  rmse_Stress_Levels <- sqrt(mean((Y_test_Stress_Levels - Stress_Levels_pred)^2))
  rmse_Headaches_Per_Week <- sqrt(mean((Y_test_Headaches_Per_Week - Headaches_Per_Week_pred)^2))
  
  # Map input_vector to features
  new_data <- data.frame(
    Sleep_Quality = input_vector[1],
    Academic_Performance = input_vector[2],
    Study_Load = input_vector[3],
    Extracurricular_Activities = input_vector[4]
  )
  
  # Make predictions for new data
  Stress_Levels_pred_new <- predict(rf_model_Stress_Levels, new_data)
  Headaches_Per_Week_pred_new <- predict(rf_model_Headaches_Per_Week, new_data)
  
  # Return predictions as a list
return(list(Predicted_Stress_Levels = results$Stress_Levels,
    Predicted_Headaches_Per_Week_Probability = results$Headaches_Per_Week_Probability))}



