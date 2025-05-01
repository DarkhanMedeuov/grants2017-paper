library(tidyverse)
library(tidymodels)
library(glmnet)
tidymodels_prefer()
grants_def <- 
  read_rds(file = "out/data/replication_dataset_negotiating_academic_funding.rds")
logistic_full_fit <- read_rds("out/data/models_win_full.rds")

X <- model.matrix(logistic_full_fit$formula, 
                  data = grants_def)[, -1]
Y <- grants_def %>% .$win

logfit <- glmnet(X, Y, family = "binomial")
cvfit <- cv.glmnet(X, Y, family = "binomial", type.measure = "class")

pdf("out/pic/sup_lasso_lambda_selection.pdf")
plot(cvfit)
dev.off()

binomial_lasso_spec <- 
  logistic_reg(penalty = 0.1, mixture = 1) %>%
  set_engine("glmnet")

set.seed(1)
binomial_lasso_fit <- 
  binomial_lasso_spec %>% 
  fit(full_formula, 
      data = grants_def)

tidy(logistic_full_fit) %>% 
  left_join(tidy(binomial_lasso_fit, 
                 penalty = cvfit$lambda.min) %>%
              rename(`min_estimate` = estimate,
                     `min_lambda` = penalty), by = "term") %>%
  left_join(tidy(binomial_lasso_fit, 
                 penalty = cvfit$lambda.1se) %>%
              rename(`1se_estimate` = estimate, 
                     `1se_lamda` = penalty), by = "term") %>%
  select(term, estimate, min_estimate, `1se_estimate`, min_lambda, `1se_lamda`) %>%
  datasummary_df(output = "out/tables/lasso_comparison.docx", fmt = 4)

