#function to calculate rmse from a sitar model
calculate_rmse <- 
  function(full_model) {
    data <- full_model$data
    outcome_variable <- as.character(full_model$call.sitar$y)
    predictions <- predict(full_model, newdata = data, type = "response")
    actuals <- data[[outcome_variable]]
    sqrt(mean((predictions - actuals)^2))
  }

# Function to calculate Pseudo R²
calculate_pseudo_r2 <- 
  function(full_model) {
    # Extract the data from the full model that we need to run a null model
    frame_to_run_null_model <-
      data.frame(
        id=full_model$data[[full_model$call.sitar$id]],
        x=full_model$data[[full_model$call.sitar$x]],
        y=full_model$data[[full_model$call.sitar$y]]
      )
    #run a null model
    null_model <- 
      sitar(x = x, 
            y = y, 
            id = id, 
            data = frame_to_run_null_model, 
            df = 0)
    
    # Extract log-likelihoods of both models
    log_likelihood_full <- logLik(full_model)
    log_likelihood_null <- logLik(null_model)
    
    #compare the log likelihoods to derive McFadden's Pseudo R2
    pseudo_r2 <- 
      1 - (as.numeric(-2 * log_likelihood_full) / as.numeric(-2 * log_likelihood_null))
    
    return(pseudo_r2)
  }



calculate_r2_nagelkerke <- function(full_model) {
  frame_to_run_null_model <- data.frame(
    id = full_model$data[[as.character(full_model$call.sitar$id)]],
    x = full_model$data[[as.character(full_model$call.sitar$x)]],
    y = full_model$data[[as.character(full_model$call.sitar$y)]]
  ) |> na.omit()
  
  null_model <- sitar(x = x, y = y, id = id, data = frame_to_run_null_model, df = 0)
  
  log_likelihood_full <- as.numeric(logLik(full_model))
  log_likelihood_null <- as.numeric(logLik(null_model))
  
  n <- nrow(full_model$data)
  
  # Nagelkerke's R² adjusts McFadden’s R² to range between 0 and 1
  r2_nagelkerke <- (1 - exp((2/n) * (log_likelihood_null - log_likelihood_full))) / 
    (1 - exp((2/n) * log_likelihood_null))
  
  return(r2_nagelkerke)
}
