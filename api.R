# Load necessary libraries
library(plumber)
library(randomForest)
library(jsonlite)

# Source the model function from the file random.forest.R
source("random.forest.R")  # Ensure random.forest.R contains the function `random.forest`

#* @apiTitle Stress Prediction API
#* @apiDescription API to predict stress levels and headaches per week based on user input.

#* Enable CORS
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")  # Allow requests from any origin
  res$setHeader("Access-Control-Allow-Methods", "POST, GET, OPTIONS")  # Allow specified HTTP methods
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")  # Allow specified headers
  
  # Handle preflight requests (OPTIONS method)
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200  # Respond with HTTP 200 status for preflight requests
    return(list())  # Return an empty response for preflight requests
  }
  
  plumber::forward()  # Forward the request to the next handler if it's not a preflight request
}

#* Predict Stress Levels and Headaches
#* @param Sleep_Quality Sleep quality score (1–5)
#* @param Academic_Performance Academic performance score (1–5)
#* @param Study_Load Study load in hours per day (1–5)
#* @param Extracurricular_Activities Hours of extracurricular activities per week (1–5)
#* @post /predict
function(Sleep_Quality, Academic_Performance, Study_Load, Extracurricular_Activities) {
  # Convert inputs to numeric values
  input_vector <- as.numeric(c(Sleep_Quality, Academic_Performance, Study_Load, Extracurricular_Activities))
  
  # Validate input: Check if all inputs are numeric
  if (any(is.na(input_vector))) {
    stop("Invalid input: all parameters must be numeric and not empty.")
  }
  
  # Call the model function with the input vector
  results <- random.forest(input_vector)
  
  # Ensure the result has the expected fields
  if (!all(c("Predicted_Stress_Levels", "Predicted_Headaches_Per_Week_Probability") %in% names(results))) {
    stop("Model function returned invalid results. Ensure it outputs `Predicted_Stress_Levels` and `Predicted_Headaches_Per_Week_Probability`.")
  }
  
  # Return the results as JSON
  list(
    Predicted_Stress_Levels = as.numeric(results$Predicted_Stress_Levels),
    Predicted_Headaches_Per_Week_Probability = as.numeric(results$Predicted_Headaches_Per_Week_Probability)
  )
}




