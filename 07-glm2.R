library(tidymodels)
library(patchwork)
library(ggrepel)
tidymodels_prefer()
rm(list = ls()[!(ls() %>% grepl("grants_def", .))])

grants_def <-
  grants_def %>% 
  mutate(win = fct_relevel(win, "Yes"))
set.seed(502)
grants_split <- initial_split(grants_def, prop = 0.8, strata = win)
grants_train <- training(grants_split)
grants_test  <- testing(grants_split)


set.seed(1001)
grants_folds <- vfold_cv(grants_train, v = 10, repeats = 2)

## Logistic regression models
baseline_rec <- # the most simple one
  recipe(win ~ 1, 
         data = grants_train)

meritocracy_rec <-
  recipe(win ~ 
           score + hirsh + rints + scopus + fake, # quality indicators
         data = grants_train) %>% 
  step_dummy(all_nominal_predictors())

meritocracy_memory_rec <-
  recipe(win ~
           score + hirsh + rints + scopus + fake + # quality indicators
           win_2014 + degree, # institutional memory aka Mathew effect
         data = grants_train) %>% 
  step_dummy(all_nominal_predictors())

meritocracy_memory_demo_rec <-
  recipe(win ~
           score + hirsh + rints + scopus + fake + # quality indicators
           win_2014 + degree + # institutional memory aka Mathew effect
           domain + sex + region, # demographics
         data = grants_train) %>% 
  step_dummy(all_nominal_predictors())

full_no_inst_rec <-
  recipe(win ~
           score + hirsh + rints + scopus + fake + # quality indicators
           win_2014 + degree + # institutional memory aka Mathew effect
           domain + sex + region + # demographics
           org_prestige + pr_rank,
         data = grants_train) %>% 
  step_dummy(all_nominal_predictors())

full_rec <-
  recipe(win ~ 
           score + hirsh + rints + scopus + fake + # quality indicators
           win_2014 + degree + # institutional memory aka Mathew effect
           domain + sex + region + # demographics
           org_prestige + pr_rank + 
           inst_cap, 
         data = grants_train) %>% 
  step_dummy(all_nominal_predictors())

full_scoreXdomain <-
  full_rec %>% 
  step_interact(~ score:starts_with("domain_"))

full_scoreXsex <-
  full_rec %>% 
  step_interact(~ score:starts_with("sex_"))

full_hirshXsex <-
  full_rec %>% 
  step_interact(~ hirsh:starts_with("sex_"))


preproc <-
  list(
    baseline = baseline_rec,
    meritocracy = meritocracy_rec,
    meritocracy_memory = meritocracy_memory_rec,
    meritocracy_memory_demo = meritocracy_memory_demo_rec,
    full_no_inst = full_no_inst_rec,
    full = full_rec,
    full_scoreXdomain = full_scoreXdomain,
    full_scoreXsex = full_scoreXsex,
    full_hirshXsex = full_hirshXsex
  )


logit_model <- 
  logistic_reg() %>% 
  set_engine("glm", 
             family = binomial())

logit_models <- 
  workflow_set(preproc, 
               list(log = logit_model), cross = FALSE)
keep_pred <- control_resamples(save_pred = TRUE, 
                               save_workflow = TRUE,
                               verbose = TRUE)


logit_models <-
  logit_models %>% 
  workflow_map("fit_resamples", 
               metrics = metric_set(
                 precision, f_meas, 
                 accuracy, kap,
                 roc_auc, sens, spec),
               seed = 1101, verbose = TRUE, 
               resamples = grants_folds, 
               control = keep_pred)

# Groups are respected on the new metric function

model_metrics <-
  collect_metrics(logit_models, summarize = FALSE)
  
log_pred <- 
  logit_models %>%
  collect_predictions()

(roc_curves <-
  log_pred %>%
  group_by(wflow_id) %>% # id contains our folds
  roc_curve(win, .pred_Yes) %>% 
  autoplot() + 
  scale_color_discrete(labels = c("Baseline", "Full, Hirsh X Sex", 
                                    "Full", "Full, no Inst. Cap.", 
                                    "Full, Score X Domain", "Full, Score X Sex", 
                                    "Merit Only", "Merit, Matthew, and Demo", 
                                    "Merit and Memo"))
  )

