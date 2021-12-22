# load libraries
library(glmnetUtils)
library(tidyverse)

# load test data
covid_test = read_tsv("data/clean/covid_test.tsv")

# load ridge fit object
load("results/ridge_fit.Rda")

# load lasso fit object
load("results/lasso_fit.Rda")

# evaluate ridge RMSE
ridge_predictions = predict(ridge_fit, 
                            newdata = covid_test, 
                            s = "lambda.1se") %>%
  as.numeric()
ridge_RMSE = sqrt(mean((ridge_predictions-covid_test$case_fatality_rate)^2))

# evaluate lasso RMSE
lasso_predictions = predict(lasso_fit, 
                            newdata = covid_test, 
                            s = "lambda.1se") %>%
  as.numeric()
lasso_RMSE = sqrt(mean((lasso_predictions-covid_test$case_fatality_rate)^2))

# evaluate tree
pred_1 = predict(tree_fit, newdata = schools_test)
pred_2 = predict(tree_fit_2, newdata = schools_test)
results = tibble(Y = schools_test$aggregate_score, Y_hat_1 = pred_1, Y_hat_2 = pred_2)
results %>% summarise(RMSE_1 = sqrt(mean((Y - Y_hat_1)^2)), 
                      RMSE_2 = sqrt(mean((Y-Y_hat_2)^2)))

# print nice table
tibble(Method = c("Ridge", "Lasso"), `Test RMSE` = c(ridge_RMSE, lasso_RMSE)) %>%
  write_tsv("results/model-evaluation.tsv")

# evaluate optimal tree RMSE
pred = predict(optimal_tree, newdata = schools_test)
pred

mean((pred-schools_test$aggregate_score)^2)
