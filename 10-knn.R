#k-neighbors
knn_spec <- 
  nearest_neighbor(neighbors = 4) %>% # we can adjust the number of neighbors 
  set_engine("kknn") %>% 
  set_mode("classification")

knn_wflow <-
  workflow() %>%
  add_recipe(full) %>% 
  add_model(knn_spec)


knn_res <- knn_wflow %>% 
  fit_resamples(resamples = grants_folds, 
                metrics = metric_set(precision, f_meas, 
                                     accuracy, kap,
                                     roc_auc, sens, spec),
                control = keep_pred)