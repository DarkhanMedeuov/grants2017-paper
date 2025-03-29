library(usemodels)

xgboost_recipe <- 
  full_rec %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_zv(all_predictors()) 


# xgboost specification
xgboost_spec <- 
  boost_tree(trees = 1000, 
             min_n = tune(), 
             tree_depth = tune(), 
             learn_rate = tune(), 
             mtry = tune(),
             loss_reduction = tune(), 
             sample_size = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost") 

xgboost_workflow <- 
  workflow() %>% 
  add_recipe(xgboost_recipe) %>% 
  add_model(xgboost_spec)

set.seed(51416)
#xgboost_tune <- # takes time
#  xgboost_workflow %>%
#  tune_race_anova(
#    resamples = grants_folds,
#    grid = 40,
#    metrics = my_metrics,
#    control = control_race(verbose_elim = TRUE, 
#                           verbose = TRUE,
#                           save_pred = TRUE))
# write_rds(xgboost_tune, file = "data/processed/xbgoost_tune.rds")

xgboost_tune <- read_rds(file = "data/processed/xbgoost_tune.rds")

plot_race(xgboost_tune)
show_best(xgboost_tune, metric = "roc_auc")
show_best(xgboost_tune, metric = "mn_log_loss")
collect_metrics(xgboost_tune)

xgb_last_wf_roc <- 
  xgboost_workflow %>%
  finalize_workflow(select_best(xgboost_tune, metric = "roc_auc"))

xgb_last_wf_log_loss <- 
  xgboost_workflow %>%
  finalize_workflow(select_best(xgboost_tune, metric = "mn_log_loss"))

xgb_res_roc <- xgb_last_wf_roc %>% 
  fit_resamples(resamples = grants_folds, 
                metrics = metric_set(precision, f_meas, 
                                     accuracy, kap,
                                     roc_auc, sens, spec),
                control = keep_pred)
xgb_res_log_loss <- xgb_last_wf_log_loss %>% 
  fit_resamples(resamples = grants_folds, 
                metrics = metric_set(precision, f_meas, 
                                     accuracy, kap,
                                     roc_auc, sens, spec),
                control = keep_pred)



# neural
#nnet_spec <-
#  mlp() %>%
#  set_mode("classification") %>% 
#  set_engine("keras", verbose = 0)


#nnet_wflow <-
#  workflow() %>%
#  add_recipe(full) %>% 
#  add_model(nnet_spec)

#nnet_res <- nnet_wflow %>% 
#  fit_resamples(resamples = grants_folds, 
#                metrics = metric_set(precision, f_meas, 
#                                     accuracy, kap,
#                                     roc_auc, sens, spec),
#                control = keep_pred)
