library(tidyverse)
library(modelsummary)
library(lmtest)
library(sandwich)
library(lme4)
grants_def <- 
  read_rds(file = "out/data/replication_dataset_negotiating_academic_funding.rds")
models_win <-
  list(
    `merit` = 
      formula(
        win ~ 
          score 
      ),
    `merit+memory+demo` = 
      formula(
        win ~
          score + hirsh + rints + scopus + delisted
      ),
    `full-inst_cap` = 
      formula(
        win ~
          score + hirsh + rints + scopus + delisted + # quality indicators
          win_2014 + degree + # institutional memory aka Mathew effect
          domain + sex + region + # demographics
          org_prestige + pr_rank
      ),
    `full` = 
      formula(
        win ~ 
          score + hirsh + rints + scopus + delisted + # quality indicators
          win_2014 + degree + # institutional memory aka Mathew effect
          domain + sex + region + # demographics
          org_prestige + pr_rank + 
          inst_cap
      ),
    `full, scoreXsex` =
      formula(
        win ~ 
          score + hirsh + rints + scopus + delisted + # quality indicators
          win_2014 + degree + # institutional memory aka Mathew effect
          domain + sex + region + # demographics
          org_prestige + pr_rank + 
          inst_cap + score:sex
      ),
    `full, scoreXdomain` = 
      formula(
        win ~ 
          score + hirsh + rints + scopus + delisted + # quality indicators
          win_2014 + degree + # institutional memory aka Mathew effect
          domain + sex + region + # demographics
          org_prestige + pr_rank + 
          inst_cap + score:domain
      ), 
    `full, hirshXsex` =
      formula(
        win ~ 
          score + hirsh + rints + scopus + delisted + 
          win_2014 + degree + 
          domain + sex + region + 
          org_prestige + pr_rank + 
          inst_cap + hirsh:sex
      )
    )

models_win_fit <-
  lapply(models_win, 
         function(formula) {
           glm(data = grants_def, 
              formula = formula, 
              family = "binomial")
         })
write_rds(models_win_fit$full, "out/data/models_win_full.rds")
modelsummary(models_win_fit[c(1:5, 7)], 
             output = "out/tables/tab5_model_win_all.html", 
             estimate  = "{estimate}({std.error}){stars}",
             statistic = NULL, 
             coef_rename = coef_rename)
modelsummary(models_win_fit[c(1:5, 7)], 
             output = "out/tables/tab5_model_win_all.tex", 
             coef_omit = "domain|org|region",
             gof_omit = "^(?!.*Num.Obs.|AIC)",
             estimate  = "{estimate}{stars}",
             statistic = "({std.error})", 
             vcov  = "HC0",
             coef_rename = coef_rename)


tt <- coeftest(models_win_fit$full, 
         vcov. = vcovCL(models_win_fit$full, 
                        cluster = grants_def$pi_id, type = "HC0"))
ttt <- coeftest(models_win_fit$full, 
               vcov. = vcovCL(models_win_fit$full, 
                              cluster = ~pi_id + domain, type = "HC0"))

win_random_effects <- 
  glmer(win ~ 
         score + hirsh + rints + scopus + delisted +
          win_2014 + degree + 
          sex + 
          org_prestige + pr_rank + 
          inst_cap + (1|region) + (1|domain), 
       data = grants_def, family = "binomial")


modelsummary(list(`Robust SE` = models_win_fit$full,
                  `Clustered (PI)` = tt,
                  `Clustered (PI + Domain)` = ttt, 
                  `RE(Region + Domain)`= win_random_effects), 
             output = "out/tables/S4_table.docx",
             estimate  = "{estimate}{stars}",
             statistic = "({std.error})", 
             gof_omit = "R2 Marg.|R2 Cond.|BIC|ICC|Log.Lik.|F|RMSE")

modelsummary(list(`Robust SE` = models_win_fit$full,
                  `Clustered (PI)` = tt,
                  `Clustered (PI + Domain)` = ttt, 
                  `RE(Region + Domain)`= win_random_effects),
             estimate  = "{estimate}{stars}",
             statistic = "({std.error})",
             gof_omit = "R2 Marg.|R2 Cond.|BIC|ICC|Log.Lik.|F|RMSE")


# Taking the full model

full_formula <- 
  formula(win ~ score + sex + win_2014 + rints + 
            scopus + hirsh + delisted +
            region + degree + inst_cap + pr_rank + org_prestige)
my_model <-
  function(df) {
    glm(win ~ score + sex + win_2014 + rints + 
          scopus + hirsh + delisted +
          region + degree + inst_cap + pr_rank + org_prestige,
       data = df,
       family = "binomial")
  }

data <- 
  grants_def %>% 
  group_by(domain) %>% 
  nest()

data <- 
  data %>% 
  mutate(model = map(data, my_model))

win_models <- 
  data %>%
  filter(domain != "Security") %>%
  pull(model, name = domain)

modelsummary(win_models, 
             output = "out/tables/tab6_model_win_all_by_domains.tex",
             estimate  = "{estimate}{stars}",
             statistic = "({std.error})", 
             vcov  = "HC0",
             gof_omit = "BIC|Log.Lik|RMSE",
             coef_rename = coef_rename)
modelsummary(win_models,
             estimate  = "{estimate}{stars}",
             statistic = "{std.error}", 
             vcov  = "HC0",
             gof_omit = "BIC|Log.Lik|RMSE",
             coef_rename = coef_rename)

data %>% 
  mutate(tidy_models = map(model, broom::tidy)) %>% 
  unnest(tidy_models, .drop = TRUE) %>% 
  filter(term != "(Intercept)") %>%
  filter(abs(std.error) < 2) %>%
  ggplot(aes(x = term, y = estimate, ymin = estimate - 2*std.error, 
             ymax = estimate + 2*std.error, color = term)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_pointrange(show.legend = FALSE, size = 0.2) +
  coord_flip() + 
  facet_wrap(~domain) + 
  labs(title = "Modelling winning by domain") + 
  theme(text = element_text(size = 8))
ggsave("out/pic/win_models_by_domain.png", width = 10, height = 6)



