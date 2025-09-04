
income <- readRDS("./data/income_final.rds") %>% tidyr::drop_na()

model <- glm(income_num ~ 
               education_group + 
               poly(career_stage_num, 2) + 
               has_investment_activity + 
               occupation_group,
               data = income, family = "binomial")

summary(model)

model_poly <- glm(income_num ~ education_group, data = income, family = binomial)

summary(model_poly)

# Load required package
library(pROC)

# Get predicted probabilities from both models
prob_model      <- predict(model, type = "response")
prob_model_poly <- predict(model_poly, type = "response")

# Compute ROC curves
roc_model      <- roc(income$income_num, prob_model)
roc_model_poly <- roc(income$income_num, prob_model_poly)

# Plot both ROC curves
plot(roc_model, col = "blue", lwd = 2, main = "ROC Comparison: Linear vs Polynomial")
lines(roc_model_poly, col = "red", lwd = 2, lty = 2)
legend("bottomright", legend = c("Linear", "Polynomial"),
       col = c("blue", "red"), lwd = 2, lty = c(1, 2))

# Print AUCs
cat("AUC ():", auc(roc_model), "\n")
cat("AUC (Polynomial):", auc(roc_model_poly), "\n")

roc.test(roc_model, roc_model_poly)

income$predicted_prob <- predict(model, type = "response")

income_by_cs <- income %>%
  group_by(career_stage) %>%
  summarise(mean_prob = mean(predicted_prob))

ggplot(aes(x = career_stage, y = mean_prob)) +
  geom_col(fill = "steelblue") +
  labs(title = "Predicted Probability of High Income by Career Stage",
       x = "Career Stage", y = "Predicted Probability") +
  theme_minimal()

ggplot(income, aes(x = career_stage_num, y = prob_model_poly)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, color = "darkred") +
  scale_x_continuous(breaks = 1:4, labels = c("Early", "Mid", "Peak", "Transition")) +
  labs(title = "Predicted Probability of High Income by Career Stage",
       x = "Career Stage", y = "Predicted Probability") +
  theme_minimal()
