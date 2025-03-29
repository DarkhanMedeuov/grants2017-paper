library(ranger)
library(xgboost)
#library(keras)
#library(tensorflow)
#use_condaenv("r-tensorflow")
# forest
rf_spec <- 
  rand_forest(trees = 1000,  
              mtry = tune(), 
              min_n = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")
rf_wflow <- 
  workflow() %>% 
  add_recipe(full_rec) %>% 
  add_model(rf_spec) 
rf_param <-
  rf_wflow %>%
  extract_parameter_set_dials() %>% 
  update(
    mtry = mtry(c(1, 27)),
    min_n = min_n(c(2, 100))
  )

my_metrics <- 
  metric_set(roc_auc, mn_log_loss)
  


library(finetune)
set.seed(1308)
#rf_sfd_race <-
#  rf_wflow %>%
#  tune_race_anova(
#    grants_folds,
#    grid = 50,
#    param_info = rf_param,
#    metrics = my_metrics,
#    control = control_race(verbose_elim = TRUE, 
#                           verbose = TRUE, 
#                           save_pred = TRUE)
#  )

#write_rds(rf_sfd_race, 
#          file = "data/processed/random_forest_tuned_models.rds")

rf_sfd_race <- 
  read_rds(file = "data/processed/random_forest_tuned_models.rds")

select_best(rf_sfd_race, metric = "roc_auc")
select_best(rf_sfd_race, metric = "mn_log_loss")

rf_sfd_race %>%
  collect_metrics() %>% 
  arrange(desc(.metric), desc(mean), std_err)
plot_race(rf_sfd_race)


rf_sfd_race %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")
best_auc <- select_best(rf_sfd_race, metric = "roc_auc")
best_log_loss <- select_best(rf_sfd_race, metric = "mn_log_loss")

final_rf_wflow1 <-
  rf_wflow %>% 
  finalize_workflow(best_auc)

final_rf_wflow2 <-
  rf_wflow %>% 
  finalize_workflow(best_log_loss)

set.seed(1003)
rf_res1 <- 
  final_rf_wflow1 %>% 
  fit_resamples(resamples = grants_folds, 
                metrics = metric_set(precision, f_meas, 
                  accuracy, kap,
                  roc_auc, sens, spec),
                control = keep_pred)
rf_res2 <-
  final_rf_wflow2 %>% 
  fit_resamples(resamples = grants_folds, 
                metrics = metric_set(precision, f_meas, 
                                     accuracy, kap,
                                     roc_auc, sens, spec),
                control = keep_pred)

