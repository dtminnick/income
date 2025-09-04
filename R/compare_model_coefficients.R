compare_model_coefficients <- function(...) {
  models <- list(...)
  model_names <- names(models)
  
  if (is.null(model_names)) {
    model_names <- paste0("Model_", seq_along(models))
  }
  
  extract_summary <- function(mod, name) {
    if (inherits(mod, "lm") || inherits(mod, "glm")) {
      s <- summary(mod)
      coefs <- as.data.frame(s$coefficients)
      coefs$predictor <- rownames(coefs)
      coefs$model <- name
      
      names(coefs) <- sub("Pr\\(>.*\\)", "p_value", names(coefs))
      names(coefs) <- sub("t value|z value", "statistic", names(coefs))
      
      coefs[, c("model", "predictor", "Estimate", "Std. Error", "statistic", "p_value")]
      
    } else if (is.numeric(mod) && !is.null(names(mod))) {
      # Handle named coefficient vector (e.g., from indicator matrix)
      coefs <- data.frame(
        model = name,
        predictor = names(mod),
        Estimate = mod,
        `Std. Error` = NA,
        statistic = NA,
        p_value = NA
      )
      coefs
    } else {
      stop(paste("Unsupported model type for:", name))
    }
  }
  
  results <- Map(extract_summary, models, model_names)
  do.call(rbind, results)
}
