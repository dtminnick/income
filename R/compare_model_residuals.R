compare_model_residuals <- function(...) {
  models <- list(...)
  model_names <- names(models)
  
  if (is.null(model_names)) {
    model_names <- paste0("Model_", seq_along(models))
  }
  
  extract_residuals <- function(mod, name) {
    data.frame(
      residual = residuals(mod),
      fitted = fitted(mod),
      model = name,
      obs = seq_along(residuals(mod))
    )
  }
  
  results <- Map(extract_residuals, models, model_names)
  do.call(rbind, results)
}
