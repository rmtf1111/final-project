# load libraries
library(glmnetUtils)                    # to run ridge and lasso
source("/Users/rmtf1111/schools-project/code/functions/plot_glmnet.R")            # for lasso/ridge trace plots

# read in the training data
schools_train = read_tsv("/Users/rmtf1111/schools-project/data/clean/schools_train.tsv")

schools_train
# run ridge regression
set.seed(1)
lm_fit = lm(aggregate_score ~ percent_of_housing_crowded +
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
                      data = schools_train)

# save the ridge fit object
save(lm_fit, file = "/Users/rmtf1111/schools-project/results/OLS_fit.Rda")
summary(lm_fit)


# compute OLS errors
lm_predictions = predict(lm_fit, 
                            newdata = schools_test) %>% as.numeric()
RMSE_lm = sqrt(mean((lm_predictions - schools_test$aggregate_score)^2))
RMSE_lm

# create ridge trace plot
p = plot_glmnet(ridge_fit, schools_train, features_to_plot = 6)
ggsave(filename = "/Users/rmtf1111/schools-project/results/ridge-trace-plot.png", 
       plot = p, 
       device = "png", 
       width = 6, 
       height = 4)

# run ridge regression
set.seed(1)
ridge_fit = cv.glmnet(aggregate_score ~ percent_of_housing_crowded +
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
                      alpha = 0,                 
                      nfolds = 10,               
                      data = schools_train)

# save the ridge fit object
save(ridge_fit, file = "/Users/rmtf1111/schools-project/results/ridge_fit.Rda")

# create ridge CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "/Users/rmtf1111/schools-project/results/ridge-cv-plot.png")
plot(ridge_fit)
dev.off()

# create ridge trace plot
p = plot_glmnet(ridge_fit, schools_train, features_to_plot = 6)
ggsave(filename = "/Users/rmtf1111/schools-project/results/ridge-trace-plot.png", 
       plot = p, 
       device = "png", 
       width = 6, 
       height = 4)

# extract features selected by ridge and their coefficients
beta_hat_std = extract_std_coefs(ridge_fit, schools_train)
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>% 
  write_tsv("/Users/rmtf1111/schools-project/results/ridge-features-table.tsv")

# compute ridge errors
ridge_predictions = predict(ridge_fit, 
                            newdata = schools_test,
                            s = "lambda.1se") %>% as.numeric()
RMSE_ridge = sqrt(mean((ridge_predictions - schools_test$aggregate_score)^2))
RMSE_ridge

# run lasso regression
set.seed(1)
lasso_fit = cv.glmnet(aggregate_score ~ percent_of_housing_crowded +
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
                      alpha = 1,                 
                      nfolds = 10,               
                      data = schools_train)

# save the lasso fit object
save(lasso_fit, file = "/Users/rmtf1111/schools-project/results/lasso_fit.Rda")

# create lasso CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "/Users/rmtf1111/schools-project/results/lasso-cv-plot.png")
plot(lasso_fit)
dev.off()

# create lasso trace plot
p = plot_glmnet(lasso_fit, schools_train, features_to_plot = 6)
ggsave(filename = "/Users/rmtf1111/schools-project/results/lasso-trace-plot.png", 
       plot = p, 
       device = "png", 
       width = 6, 
       height = 4)

# extract features selected by lasso and their coefficients
beta_hat_std = extract_std_coefs(lasso_fit, schools_train)
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>% 
  write_tsv("/Users/rmtf1111/schools-project/results/lasso-features-table.tsv")

# compute lasso errors
lasso_predictions = predict(lasso_fit, 
                            newdata = schools_test,
                            s = "lambda.1se") %>% as.numeric()
RMSE_lasso = sqrt(mean((lasso_predictions - schools_test$aggregate_score)^2))
RMSE_lasso

