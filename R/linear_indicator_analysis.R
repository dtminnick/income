
income <- readRDS("./data/income.rds")

model_below <- lm(income_below_num ~
              age_num + 
              workclass_num + 
              education_num + 
              marital_status_num + 
              hours_per_week_num + 
              relationship_num + 
              race_num + 
              gender_num + 
              capital_gain_num + 
              capital_loss_num, 
            data = income)

summary(model_below)

car::crPlots(model_below) 

car::durbinWatsonTest(model_below)

car::vif(model_below) 

plot(model_below, which = 1, main = "Residuals vs Fitted")

plot(model_below, which = 2, main = "Normal Q-Q")

plot(model_below, which = 3, main = "Scale-Location")

plot(model_below, which = 5, main = "Residuals vs Leverage")

model_above <- lm(income_above_num ~
              age_num + 
              workclass_num + 
              education_num + 
              marital_status_num + 
              hours_per_week_num + 
              relationship_num + 
              race_num + 
              gender_num + 
              capital_gain_num + 
              capital_loss_num, 
            data = income)

summary(model_above)



plot(model_above, which = 1, main = "Residuals vs Fitted")

plot(model_above, which = 2, main = "Normal Q-Q")

plot(model_above, which = 3, main = "Scale-Location")

plot(model_above, which = 5, main = "Residuals vs Leverage")

              