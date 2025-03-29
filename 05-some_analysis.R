library(modelr)
library(modelsummary)
library(RColorBrewer)
library(cowplot)
library(ggpubr)
grants_def <- 
  read_rds(file = "data/processed/grants_def.rds")

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
    `demo+domain+fake+qual` = 
      formula(score ~
                domain + sex + region + 
                fake + rints + scopus + hirsh 
              ),
    full = 
      formula(score ~ 
                 domain + sex + region + # demographics
                 rints + scopus + hirsh + fake + # quality indicators
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
modelsummary(models_score, 
             output = "out/tables/tab4_model_score_all.tex", 
             estimate  = "{estimate}{stars}",
             statistic = "({std.error})", 
             coef_rename = coef_rename)
modelsummary(models_score, estimate  = "{estimate}{stars}", 
             statistic = "{std.error} ({p.value})")

plot(models_score$full)

ggplot(models_score$full, aes(sample = .resid)) +
  geom_qq() +
  geom_qq_line()
plot(models_score$full$fitted.values, 
     models_score$full$residuals)

library(lmtest)
resettest(models_score$full)
bptest(models_score$full)

grants_def$score %>% mean()
grants_def$score %>% var()

tibble(x = rpois(n = 4488, lambda = 24)) %>% 
  mutate(x_mean = mean(x)) %>%
  ggplot(aes(x = x)) + 
  geom_histogram(color = "grey", bins = 30) + 
  geom_vline(xintercept = 24)
mean(models_score$full$residuals)

final_formula <- 
  formula(score ~ 
  domain + sex + region + # demographics
  rints + scopus + hirsh + fake + # quality indicators
  win_2014 + degree + # institutional memory aka Mathew effect
  inst_cap + org_prestige # institutional capital)
  )


pois_fit1 <- glm(data = grants_def, final_formula, family = "poisson")
summary(pois_fit1)
library(lme4)
grants_def
library(MASS)
lmodel <- glm(final_formula, data = grants_def, family = quasipoisson)
tidy(lmodel, exponentiate = FALSE) %>%
  mutate(stars = 
           case_when(p.value < 0.001 ~ "***", 
                     p.value >= 0.001 & p.value < 0.01 ~ "**", 
                     p.value >= 0.01 & p.value < 0.05 ~ "*", 
                     p.value >= 0.05 ~ "",
                     .default = as.character(p.value))) %>% View()


grants_def[1283, ] %>% View()

## Смотрим на данные с точки зрения доменов
my_model <-
  function(df) {
    lm(score ~ hirsh + win_2014 + rints + 
         scopus + fake + sex + 
         region + degree + inst_cap + org_prestige,
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

# Теперь сделаем таблицы регрессий

score_models <- 
  grants_out %>%
  filter(domain != "Security") %>%
  pull(model, name = domain)

modelsummary(score_models, 
             output = "out/tables/table2.tex",
             estimate  = "{estimate}{stars}",
             statistic = "({std.error})",
             coef_rename = coef_rename)
modelsummary(score_models, 
             output = "out/tables/table2.html",
             estimate  = "{estimate}{stars}",
             statistic = "({std.error})",
             coef_rename = coef_rename)

score_models2 <- 
  grants_out %>%
  filter(domain != "Security") %>%
  pull(model2, name = domain)

modelsummary(score_models2, 
             estimate  = "{estimate}{stars}",
             statistic = "{std.error}",
             coef_omit = "Intercept", 
             coef_rename = coef_rename)

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
