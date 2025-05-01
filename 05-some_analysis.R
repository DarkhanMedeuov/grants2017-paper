library(tidyverse)
library(modelr)
library(modelsummary)
library(RColorBrewer)
library(cowplot)
library(ggpubr)
library(lmtest)
library(sandwich)
library(VGAM)
library(MASS)
library(lme4)
grants_def <- 
  read_rds(file = "out/data/replication_dataset_negotiating_academic_funding.rds")

models_score <-
  list(
    sex = 
      formula(score ~ sex),
    demo =
      formula(score ~ 
                sex + region
              ),
    `demo+domain` = 
      formula(score ~
                sex + region + domain
              ),
    `demo+domain+delisted+qual` = 
      formula(score ~
                sex + region + domain +
                rints + scopus + hirsh + delisted
              ),
    full = 
      formula(score ~ 
                 sex + region + domain + # demographics
                 rints + scopus + hirsh + delisted + # quality indicators
                 win_2014 + degree + # institutional memory aka Mathew effect
                 inst_cap + org_prestige # institutional capital)
               )
    )

models_score <-
  lapply(models_score, 
         function(formula) {
           lm(data = grants_def, 
              formula = formula)
         })
models_score$`full, robust SE` <-
  coeftest(models_score$full, 
           vcov = vcovCL, 
           type = "HC1",
           cluster = ~domain)
  
modelsummary(models_score, 
             output = "out/tables/tab4_model_score_all.tex", 
             estimate  = "{estimate}{stars}",
             statistic = "({std.error})", 
             coef_rename = coef_rename)
modelsummary(models_score, estimate  = "{estimate}{stars}", 
             output = "out/tables/tab4_model_score_all.html",
             statistic = "{std.error}", 
             coef_rename = coef_rename)

# QQ plot show that theoretical and empirical quantiles diverge in the upper
# part of the range
plot(models_score$full, 2)
write_rds(models_score$full, file = "out/data/models_score.rds")
## Let's take a look at data by domains
my_model <-
  function(df) {
    lm(score ~ 
         sex + region + 
         rints + scopus + hirsh + delisted +
         win_2014 + degree + 
         inst_cap + org_prestige,
       data = df)
  }

my_model2 <-
  function(df) {
    lm(score ~ sex,
       data = df)
  }

grants_out <-
  grants_def %>% 
  group_by(domain) %>% 
  nest() %>% 
  mutate(model = map(data, my_model), 
         model2 = map(data, my_model2)) %>%
  mutate(data = map2(data, model, add_predictions)) %>% 
  mutate(data = map2(data, model, add_residuals))

# Turning them into regression tables

score_models <- 
  grants_out %>%
  filter(domain != "Security") %>% # Domain 'security' doesn't fare well
  pull(model, name = domain)

modelsummary(score_models, 
             output = "out/tables/table2.tex",
             vcov = "HC1", # robust se
             estimate  = "{estimate}{stars}",
             statistic = "({std.error})",
             coef_rename = coef_rename)
modelsummary(score_models, 
             vcov = "HC1", # robust se
             estimate  = "{estimate}{stars}",
             statistic = "({std.error})",
             coef_rename = coef_rename)

# just to see how the coef varies across domains
score_models2 <- 
  grants_out %>%
  filter(domain != "Security") %>%
  pull(model2, name = domain)

modelsummary(score_models2, 
             estimate  = "{estimate}{stars}",
             statistic = "{std.error}",
             coef_omit = "Intercept", 
             coef_rename = coef_rename)


## Testing model assumptions and auxiliary hypotheses

score_robust_errors <-
  coeftest(models_score$full, 
         vcov = vcovCL, 
         type = "HC1",
         cluster = ~domain)

score_random_effects <- 
  lmer(score ~ 
         sex + 
         rints + scopus + hirsh + delisted + 
         win_2014 + degree + 
         inst_cap + org_prestige + (1|pi_id) + (1|region) + (1|domain), 
       data = grants_def)

final_formula <- formula(
  score ~ 
    sex + region + domain + 
    rints + scopus + hirsh + delisted + 
    win_2014 + degree + 
    inst_cap + org_prestige)

score_tobit_reg <- 
  vglm(final_formula, tobit(Upper = 36), data = grants_def)

# collecting
score_auxiliary <- 
  list(
    `Full OLS` = models_score$full,
    `Clustered SE` = score_robust_errors, 
    `Random Effects` = score_random_effects,
    `Tobit` = score_tobit_reg
  )

write_rds(score_auxiliary, file = "out/data/score_auxiliary.rds")
modelsummary(score_auxiliary,
             output = "out/tables/score_check_tables.tex",
             estimate  = "{estimate}{stars}",
             statistic = "({std.error})")

## Treating score as an ordinal variable
### Using existing values as levels
score_polr_reg <- 
  polr(final_formula, data = grants_def %>% 
         mutate(score = as.ordered(score)))
score_polr_reg2 <-
  polr(final_formula, data = grants_def %>%
         mutate(score = cut(score, breaks = seq(0, 40, by = 5))) %>% 
         mutate(score = fct_collapse(score, 
                                     `30+` = c("(30,35]", "(35,40]"))))

score_auxiliary2 <-
  list(`POLR` = score_polr_reg, 
       `POLR, intervals` = score_polr_reg2)
write_rds(score_auxiliary2, file = "out/data/score_auxiliary2.rds")
modelsummary(score_auxiliary2, 
             estimate  = "{estimate}{stars}",
             statistic = "({std.error})", 
             coef_omit = "\\|")



(strange_plot3 <- 
  grants_out %>%
  unnest(data) %>%
  ggplot(aes(x = pred, y = score)) +
  geom_point(aes(color = win, shape = inst_cap), alpha = 1/3) + 
  geom_smooth(se = FALSE) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  facet_wrap(~domain) + 
  labs(x = "Predicted", y = "Observed") +
  scale_x_continuous(limits = c(6, 36)) + 
  scale_y_continuous(limits = c(6, 36)))
ggsave(filename = "out/pic/S3_fig_score_vs_success.tiff",
       device = "tiff",
       plot = strange_plot3, 
       dpi = 300)


(culture_plot <-
  grants_out %>% 
  unnest(data) %>% 
  filter(domain == "Culture") %>%
  ggplot(aes(x = pred, y = score)) + 
  geom_point(aes(color = region), alpha = 2/3) +
  geom_smooth(se = FALSE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  scale_x_continuous(limits = c(6, 36)) +
  scale_y_continuous(limits = c(6, 36)) +
  color_palette("Accent") + 
  theme(legend.title = element_blank()) +
  labs(x = "Predicted", y = "Observed", 
         caption = "The second mode in predicted scores associated with Shymkent"))


grants_out %>% 
  unnest (data) %>% 
  filter(domain == "Culture") %>% 
  group_by(region) %>% 
  summarise(mean_score_pred = mean(pred), 
            sd__score_pred = sd(pred), 
            n = n())
## Fraq of candidates and Shymkent correlate in Culture
grants_out %>% 
  unnest (data) %>% 
  filter(domain == "Culture") %>%
  group_by(region) %>%
  summarise(candidate_count = mean(degree == "Candidate"), 
            n = n(), 
            .groups = "drop") %>% 
  arrange(desc(candidate_count))

## 
grants_out %>% 
  unnest (data) %>% 
#  filter(domain == "Culture") %>%
  group_by(region) %>%
  summarise(win_2014_count = mean(win_2014 == "Yes"), 
            n = n(), 
            .groups = "drop") %>% 
  arrange(desc(win_2014_count))

# 
reg_tables <-
  grants_out %>% 
  unnest(data) %>%
  mutate(tidy_models = map(model, broom::tidy)) %>% 
  mutate(model_stats = map(model, broom::glance)) %>%
  unnest(tidy_models, .drop = TRUE) %>% 
  unnest(model_stats, .drop = TRUE)

anno <- 
  tibble(term = 15, estimate = 8, 
         label = paste(1:7))
plot <-
  reg_tables %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = term, y = estimate, 
             ymin = estimate - 2*std.error, 
             ymax = estimate + 2*std.error, 
             color = term)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_pointrange(show.legend = FALSE) +
  coord_flip() + 
  facet_wrap(~domain) + 
  labs(title = "Modelling score by domains")

anno <- 
  reg_tables %>% 
  group_by(domain) %>%
  summarise(r2 = adj.r.squared[1],
            n = nobs[1]) %>% 
  mutate(r2 = sprintf("italic(R^2)[adj] == %.2f", r2),
         n = sprintf("italic(N) == %.4s", n))
(plot + 
  geom_text(data = anno, aes(label = r2), 
            x = 15, y = 8, 
            inherit.aes = FALSE, parse = TRUE) + 
  geom_text(data = anno, aes(label = n),
            x = 12, y = 8, 
            inherit.aes = FALSE, parse = TRUE, 
            vjust = "inward"))

ggsave("out/pic/score_models_by_domains.png", 
       width = 10, height = 6)
rm(list = ls()[!(ls() %>% grepl("grants_def", .))])
