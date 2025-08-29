
library("caret")
library("pROC")

evaluate_model <- function(model, test_data, threshold = 0.5, positive_class = "1", model_name = "Model") {
  
  # Predict probabilities.
  
  predicted_probs <- predict(model, newdata = test_data, type = "response")
  
  # Class predictions based on threshold.
  
  predicted_class <- ifelse(predicted_probs > threshold, 1, 0)
  
  # Confusion matrix.
  
  cm <- confusionMatrix(factor(predicted_class, levels = c(0, 1)),
                        factor(test_data$income_binary, levels = c(0, 1)),
                        positive = positive_class)
  
  # ROC & AUC.
  
  roc_obj <- roc(test_data$income_binary, predicted_probs)
  
  auc_val <- auc(roc_obj)
  
  # AIC.
  
  model_aic <- AIC(model)
  
  # Create single-row data frame of key metrics.
  
  data.frame(Model = model_name,
             Threshold = threshold,
             Accuracy = cm$overall["Accuracy"],
             Kappa = cm$overall["Kappa"],
             Sensitivity = cm$byClass["Sensitivity"],
             Specificity = cm$byClass["Specificity"],
             Precision = cm$byClass["Precision"],
             Recall = cm$byClass["Sensitivity"],  # same as sensitivity
             F1 = 2 * (cm$byClass["Precision"] * cm$byClass["Sensitivity"]) / (cm$byClass["Precision"] + cm$byClass["Sensitivity"]),
             BalancedAccuracy = cm$byClass["Balanced Accuracy"],
             AUC = auc_val,
             AIC = model_aic)
}
