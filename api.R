# Load necessary libraries
library(plumber)
library(randomForest)
library(jsonlite)

# Source the model function from the file random.forest.R
source("random.forest.R")  # Ensure random.forest.R contains the function `function_name`

#* @apiTitle Stress Prediction API
#* @apiDescription API to predict stress levels and headaches per week based on user input.

#* Enable CORS
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "POST, GET, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  plumber::forward()
}

#* Predict Stress Levels and Headaches
#* @param Sleep_Quality 
#* @param Academic_Performance 
#* @param Study_Load 
#* @param Extracurricular_Activities 
#* @post /predict
function(Sleep_Quality, Academic_Performance, Study_Load, Extracurricular_Activities) {
  # Convert inputs to numeric values
  input_vector <- as.numeric(c(Sleep_Quality, Academic_Performance, Study_Load, Extracurricular_Activities))
  
  # Validate input: Check if all inputs are numeric
  if (any(is.na(input_vector))) {
    stop("Invalid input: all parameters must be numeric and not empty.")
  }
  
  # Call the model function with the input vector
  results <- function_name(input_vector)
  
  # Ensure the result has the expected fields
  if (!all(c("Stress_Levels", "Headaches_Per_Week_Probability") %in% names(results))) {
    stop("Model function returned invalid results. Ensure it outputs `Stress_Levels` and `Headaches_Per_Week_Probability`.")
  }
  
  # Return the results as JSON
  list(
    Predicted_Stress_Levels = results$Stress_Levels[1],
    Predicted_Headaches_Per_Week_Probability = results$Headaches_Per_Week_Probability[1]
  )
}

