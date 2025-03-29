log_metrics <- 
  logit_models %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Logistic Regression") # add the name of the model to every row

rf_metrics <- 
  as_workflow_set(rf1 = rf_res1, 
                  rf2 = rf_res2) %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Random Forest")

xgb_metrics <- 
  as_workflow_set(xgb1 = xgb_res_roc, 
                  xgb2 = xgb_res_log_loss) %>%
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "XGBoost")



# create dataframe with all models
model_compare <- bind_rows(
  log_metrics,
  rf_metrics,
  xgb_metrics) 

all_models <- 
  as_workflow_set(random_forest_roc = rf_res1,
                  random_forest_log_loss = rf_res2,
                  xgboost_roc = xgb_res_roc,
                  xgboost_log_loss = xgb_res_log_loss) %>% 
  bind_rows(logit_models %>% 
              filter(wflow_id %in% 
                       c("full_log", 
                         "full_scoreXdomain_log", 
                         "full_scoreXsex_log")))

(f_meas_plot <-
    autoplot(all_models, 
             metric = "f_meas") + 
    geom_text_repel(aes(label = wflow_id), nudge_x = 1/8, nudge_y = 1/100) +
    labs(title = "f_meas") +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)))



all_pred <- 
  all_models %>%
  collect_predictions()

all_models %>% collect_metrics() %>% 
  filter(.metric == "accuracy") %>% 
  ggplot(aes(y = wflow_id, x = mean)) + 
  geom_point() + 
  scale_x_continuous(limits = c(0, 1))

(roc_curves <-
    all_pred %>%
    group_by(wflow_id) %>% # id contains our folds
    roc_curve(win, .pred_Yes) %>% 
    autoplot() + 
    scale_color_discrete(labels = 
                           c("Logistic Full", 
                             "Logistic: Score X Domain", 
                             "Logistic: Score X Gender", 
                             "Random Forest: Log Loss", 
                             "Random Forest: ROC", 
                             "XGBoost: Log Loss", 
                             "XGBoost: ROC")) + 
    labs(color = NULL)) 
ggsave(filename = "out/pic/fig3_all_roc_curves.tiff",
       device = "tiff",
       roc_curves, 
       dpi = 300)
# All models
labels <- c("Logistic Full", 
            "Logistic: Score X Domain", 
            "Logistic: Score X Gender", 
            "Random Forest: Log Loss", 
            "Random Forest: ROC", 
            "XGBoost: Log Loss", 
            "XGBoost: ROC")
sens_plot <- 
  all_models %>% collect_metrics() %>% 
  filter(.metric == "sens") %>% 
  ggplot(aes(y = fct_rev(wflow_id), x = mean)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = mean - 2*std_err, 
                    xmax = mean + 2*std_err), width = 0.2) + 
  geom_vline(xintercept = 0.245, linetype = "dotted") + 
  scale_y_discrete(labels = rev(labels)) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(y = NULL, x = "Sensitivity")

spec_plot <-
  all_models %>% collect_metrics() %>% 
  filter(.metric == "spec") %>% 
  ggplot(aes(y = fct_rev(wflow_id), x = mean)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = mean - 2*std_err, 
                    xmax = mean + 2*std_err), width = 0.2) + 
  geom_vline(xintercept = 1 - 0.245, linetype = "dotted") + 
  scale_y_discrete(labels = rev(labels)) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(y = NULL, x = "Specificity")

precision_plot <-
  all_models %>% collect_metrics() %>% 
  filter(.metric == "precision") %>% 
  ggplot(aes(y = fct_rev(wflow_id), x = mean)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = mean - 2*std_err, 
                    xmax = mean + 2*std_err), width = 0.2) + 
#  geom_vline(xintercept = 1 - 0.245, linetype = "dotted") + 
  scale_y_discrete(labels = rev(labels)) +
  scale_x_continuous(limits = c(0, 1)) +
  labs(y = NULL, x = "Precision")

(panel1 <- (sens_plot + spec_plot)/precision_plot)
ggsave(filename = "out/pic/fig4_model_metrics1.tiff", 
       device = "tiff",
       panel1,
       dpi = 300)

(f_meas_plot <-
    autoplot(all_models, 
             metric = "f_meas") + 
    geom_text_repel(aes(label = wflow_id), nudge_x = 1/8, nudge_y = 1/100) +
    labs(title = "f_meas") +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)))

(accuracy_plot <-
    autoplot(all_models, 
             metric = "accuracy") + 
    geom_text_repel(aes(label = wflow_id), nudge_x = 1/8, nudge_y = 1/100) +
    labs(title = "Accuracy") +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)))

(kap_plot <-
    autoplot(all_models, 
             metric = "kap") + 
    geom_text_repel(aes(label = wflow_id), nudge_x = 1/8, nudge_y = 1/100) +
    labs(title = "kap") +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)))

(roc_auc_plot <-
    autoplot(all_models, 
             metric = "roc_auc") + 
    geom_text_repel(aes(label = wflow_id), nudge_x = 1/8, nudge_y = 1/100) +
    labs(title = "roc_auc") +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5)))

(panel1 <- (sens_plot + spec_plot)/precision_plot)
ggsave(filename = "out/pic/fig4_model_metrics1.tiff", panel1, 
       width = 10, height = 7)

(panel2 <- (f_meas_plot + accuracy_plot + kap_plot + roc_auc_plot))
ggsave(filename = "out/pic/all_model_metrics2.png", panel2,
       width = 10, height = 7)


last_fit_rf1 <- last_fit(final_rf_wflow1, 
                        split = grants_split,
                        metrics = metric_set(precision, f_meas,
                                             accuracy, kap,
                                             roc_auc, sens, spec)
                        )
last_fit_rf2 <- last_fit(final_rf_wflow2, 
                         split = grants_split,
                         metrics = metric_set(precision, f_meas,
                                              accuracy, kap,
                                              roc_auc, sens, spec)
                         )
last_fit_xgb_roc <- last_fit(xgb_last_wf_roc, 
                        split = grants_split,
                        metrics = metric_set(precision, f_meas, 
                                             accuracy, kap,
                                             roc_auc, sens, spec)
                        )

last_fit_xgb_log_loss <- last_fit(xgb_last_wf_log_loss, 
                             split = grants_split,
                             metrics = metric_set(precision, f_meas, 
                                                  accuracy, kap,
                                                  roc_auc, sens, spec)
                             )

final_log_wflow <-
  workflow() %>% 
  add_recipe(full_scoreXdomain) %>% 
  add_model(logit_model) 

last_fit_log <- last_fit(final_log_wflow, 
                        split = grants_split,
                        metrics = metric_set(precision, f_meas, 
                                             accuracy, kap,
                                             roc_auc, sens, spec))


final_models <- bind_rows(rf_roc = last_fit_rf1, 
                          rf_log_loss = last_fit_rf2,
                          xgb_roc = last_fit_xgb_roc,
                          xgb_log_loss = last_fit_xgb_log_loss,
                          log = last_fit_log, 
                          .id = "model") 
final_models %>% 
  unnest(.metrics) %>% 
  ggplot(aes(x = model, y = .estimate)) + 
  geom_point() + 
  facet_wrap(~.metric)

library(vip)
p1 <- 
  last_fit_rf1 %>% 
  pluck(".workflow", 1) %>%   
  extract_fit_parsnip() %>% 
  vip(num_features = 15) + 
  labs(title = "Random Forest (roc optimised)")

p2 <- 
  last_fit_xgb_roc %>% 
  pluck(".workflow", 1) %>%   
  extract_fit_parsnip() %>% 
  vip(num_features = 15) + 
  labs(title = "XG boost (roc optimised)")
p1/p2


rf_conf <-
  last_fit_rf1 %>%
  collect_predictions() %>% 
  conf_mat(win, .pred_class) %>% 
  autoplot(type = "heatmap") + 
  labs(title = "RF") + 
  theme(plot.title = element_text(hjust = 0.5))
log_conf <-
  last_fit_log %>%
  collect_predictions() %>% 
  conf_mat(win, .pred_class) %>% 
  autoplot(type = "heatmap") + 
  labs(title = "Logit") + 
  theme(plot.title = element_text(hjust = 0.5))
xgb_conf <-
  last_fit_xgb_roc %>%
  collect_predictions() %>% 
  conf_mat(win, .pred_class) %>% 
  autoplot(type = "heatmap") + 
  labs(title = "XG Boost") + 
  theme(plot.title = element_text(hjust = 0.5))
(rf_conf + log_conf + xgb_conf)


