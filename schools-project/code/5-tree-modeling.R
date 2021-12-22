
library(rpart)             
library(rpart.plot)        
library(tidyverse)

# read in the training data
schools_train = read_tsv("/Users/rmtf1111/schools-project/data/clean/schools_train.tsv")

# fit the tree
tree_fit = rpart(aggregate_score ~ percent_of_housing_crowded +
                   percent_households_below_poverty +
                   percent_aged_16_unemployed + 
                   percent_aged_25_without_high_school_diploma +
                   percent_aged_under_18_or_over_64 + 
                   per_capita_income +
                   hardship_index +
                   average_student_attendance +
                   rate_of_misconducts_per_100_students +
                   average_teacher_attendance +
                   prenatal_care_beginning_in_first_trimester +
                   teen_birth_rate +
                   assault_homicide +
                   unemployment,
                 data = schools_train)

# create tree
png(width = 10, 
    height = 8,
    res = 300,
    units = "in", 
    filename = "/Users/rmtf1111/schools-project/results/simple_tree.png")
rpart.plot(tree_fit)
dev.off()

# get the variable importance
importance <- tree_fit$variable.importance

# save variable importance
save(importance, file = "/Users/rmtf1111/schools-project/results/ridge_fit.Rda")

# we use minimum split 100
tree_fit_2 = rpart(aggregate_score ~ percent_of_housing_crowded +
                     percent_households_below_poverty +
                     percent_aged_16_unemployed + 
                     percent_aged_25_without_high_school_diploma +
                     percent_aged_under_18_or_over_64 + 
                     per_capita_income +
                     hardship_index +
                     average_student_attendance +
                     rate_of_misconducts_per_100_students +
                     individualized_education_program_compliance_rate +
                     average_teacher_attendance +
                     prenatal_care_beginning_in_first_trimester +
                     teen_birth_rate +
                     assault_homicide +
                     unemployment, 
                   control = rpart.control(minsplit = 100),
                   data = schools_train)
# create split tree
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "/Users/rmtf1111/schools-project/results/split100_tree.png")
rpart.plot(tree_fit_2)
dev.off()

# terminal nodes vs cv error
cp_table = printcp(tree_fit) %>% as_tibble()
cp_table
cp_graph <- cp_table %>% 
  ggplot(aes(x = nsplit+1, y = xerror, 
             ymin = xerror - xstd, ymax = xerror + xstd)) + 
  geom_point() + geom_line() +
  geom_errorbar(width = 0.2) +
  xlab("Number of terminal nodes") + ylab("CV error") + 
  geom_hline(aes(yintercept = min(xerror)), linetype = "dashed") + 
  theme_bw()

# save nodes vs cv error
ggsave(filename = "/Users/rmtf1111/schools-project/results/nodescv.png", 
       plot = cp_graph, 
       device = "png", 
       width = 6, 
       height = 4)

pred_1 = predict(tree_fit, newdata = schools_test)
pred_2 = predict(tree_fit_2, newdata = schools_test)
results = tibble(Y = schools_test$aggregate_score, Y_hat_1 = pred_1, Y_hat_2 = pred_2)
RMSE_tree_1 <- results %>% summarise(RMSE_tree_1 = sqrt(mean((Y - Y_hat_1)^2)))
RMSE_tree_2 <- results %>% summarise(RMSE_tree_2 = sqrt(mean((Y - Y_hat_2)^2)))

# training a random forest
rf_fit = randomForest(aggregate_score ~ percent_of_housing_crowded +
                        percent_households_below_poverty +
                        percent_aged_16_unemployed + 
                        percent_aged_25_without_high_school_diploma +
                        percent_aged_under_18_or_over_64 + 
                        per_capita_income +
                        hardship_index +
                        average_student_attendance +
                        rate_of_misconducts_per_100_students +
                        individualized_education_program_compliance_rate +
                        average_teacher_attendance +
                        prenatal_care_beginning_in_first_trimester +
                        teen_birth_rate +
                        assault_homicide +
                        unemployment,
                      mtry = 11,
                      data = schools_train)
plot(rf_fit)
# save error vs trees
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "/Users/rmtf1111/schools-project/results/bagging.png")
plot(rf_fit)
dev.off()


# importance
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "/Users/rmtf1111/schools-project/results/importance.png")
varImpPlot(rf_fit)
dev.off()

# OOB error againsts more values of mtry
mvalues = seq(1,15, by = 2)
oob_errors = numeric(length(mvalues))
ntree = 500
for(idx in 1:length(mvalues)){
  m = mvalues[idx]
  rf_fit = randomForest(aggregate_score ~ percent_of_housing_crowded +
                          percent_households_below_poverty +
                          percent_aged_16_unemployed + 
                          percent_aged_25_without_high_school_diploma +
                          percent_aged_under_18_or_over_64 + 
                          per_capita_income +
                          hardship_index +
                          average_student_attendance +
                          rate_of_misconducts_per_100_students +
                          individualized_education_program_compliance_rate +
                          average_teacher_attendance +
                          prenatal_care_beginning_in_first_trimester +
                          teen_birth_rate +
                          assault_homicide +
                          unemployment,
                          mtry = m, data = schools_train)
  oob_errors[idx] = rf_fit$mse[ntree]
}

oobvsm <- tibble(m = mvalues, oob_err = oob_errors) %>%
  ggplot(aes(x = m, y = oob_err)) + 
  geom_line() + geom_point() + 
  scale_x_continuous(breaks = mvalues) +
  theme_bw()

ggsave(filename = "/Users/rmtf1111/schools-project/results/m.png", 
       plot = oobvsm, 
       device = "png", 
       width = 6, 
       height = 4)

# compute rf error
rf_predictions = predict(rf_fit, newdata = schools_test)
RMSE_rf <- mean((rf_predictions - schools_test$aggregate_score)^2)
RMSE_rf

# make final RMSE tabel 
RMSE_table <- 
  tibble(`Model` = c("OLS", "LASSO", "Ridge", "Decision Tree",
         "Split decision tree", "Random Forest"),
         `Test error` = c(RMSE_lm, RMSE_lasso, RMSE_ridge, 0.157, 0.153, RMSE_rf))
write_tsv(RMSE_table, file = "/Users/rmtf1111/schools-project/results/RMSE-table.png")




