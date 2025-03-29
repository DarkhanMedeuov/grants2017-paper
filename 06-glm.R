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
          score + hirsh + rints + scopus + fake
      ),
    `full-inst_cap` = 
      formula(
        win ~
          score + hirsh + rints + scopus + fake + # quality indicators
          win_2014 + degree + # institutional memory aka Mathew effect
          domain + sex + region + # demographics
          org_prestige + pr_rank
      ),
    `full` = 
      formula(
        win ~ 
          score + hirsh + rints + scopus + fake + # quality indicators
          win_2014 + degree + # institutional memory aka Mathew effect
          domain + sex + region + # demographics
          org_prestige + pr_rank + 
          inst_cap
      ),
    `full, scoreXsex` =
      formula(
        win ~ 
          score + hirsh + rints + scopus + fake + # quality indicators
          win_2014 + degree + # institutional memory aka Mathew effect
          domain + sex + region + # demographics
          org_prestige + pr_rank + 
          inst_cap + score:sex
      ),
    `full, scoreXdomain` = 
      formula(
        win ~ 
          score + hirsh + rints + scopus + fake + # quality indicators
          win_2014 + degree + # institutional memory aka Mathew effect
          domain + sex + region + # demographics
          org_prestige + pr_rank + 
          inst_cap + score:domain
      ), 
    `full, hirshXsex` =
      formula(
        win ~ 
          score + hirsh + rints + scopus + fake + 
          win_2014 + degree + 
          domain + sex + region + 
          org_prestige + pr_rank + 
          inst_cap + hirsh:sex
      )
    )


lm(cbind(degreePhD, degreeDoctor) ~ if_else(hirsh > 1, 1, 0), data = tt) %>%
  summary()
lm(hirsh ~ degreePhD + degreeDoctor, data = tt) %>%
  summary()
lm(cbind(domainAgriculture, domainScience, domainLife, domainSecurity, 
         domainNatural_rm, domainEnergy) ~ if_else(hirsh > 1, 1, 0), 
   data = tt) %>% summary()

lm(cbind(inst_capMember) ~ if_else(hirsh > 1, 1, 0), 
   data = tt) %>% summary()
lm(inst_capMember ~ 1, 
   data = tt) %>% summary()

grants_def %>% group_by(inst_cap) %>% summarise(mean = mean(hirsh))

models_win_fit <-
  lapply(models_win, 
         function(formula) {
           glm(data = grants_def, 
              formula = formula, 
              family = "binomial")
         })
summary(models_win_fit$full)
write_rds(x = models_win_fit, 
          file = "/Users/dmedeuov/Documents/courses/soc203_final_ksl/data/models_win.rds")
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
             coef_rename = coef_rename)
?modelsummary

modelsummary(models_win_fit[1:5], 
             estimate  = "{estimate}({std.error}){stars}",
             statistic = NULL, 
             coef_rename = coef_rename)




(anova_fit1 <-
  anova(models_win_fit$`full-inst_cap`, 
        models_win_fit$full))
anova(models_win_fit$`full-inst_cap`)
anova(models_win_fit$`full, scoreXdomain`, test = "Rao")

anova_fit2 <-
  anova(models_win_fit$full,
      models_win_fit$`full, scoreXsex`)


library(sandwich)
library("lmtest")
fit <- models_win_fit$full
grants_def %>% 
  count(name, sort = TRUE)
tt <- coeftest(fit, 
         vcov. = vcovCL(fit, cluster = grants_def$name, type = "HC0"))
modelsummary(list(clustered_logit = tt, 
                  vanila_logit = fit), 
             estimate  = "{estimate}{stars}",
             statistic = "({std.error})")
summary(fit)

# Здесь будем анализировать финальную модель, но к ней еще надо будет прийти. 
my_model <-
  function(df) {
    glm(win ~ score + sex + win_2014 + rints + 
          scopus + hirsh + fake +
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
             output = "out/tables/tab5_model_win_all_by_domains.docx",
             estimate  = "{estimate}({std.error}){stars}",
             statistic = NULL, 
             coef_rename = coef_rename)
modelsummary(win_models, 
             output = "out/tables/tab6_model_win_all_by_domains.tex",
             estimate  = "{estimate}{stars}",
             statistic = "({std.error})", 
             coef_rename = coef_rename)
modelsummary(win_models,
             estimate  = "{estimate}{stars}",
             statistic = "{std.error}", 
             coef_rename = coef_rename)

grants_def %>% 
  group_by(domain, member) %>% 
  summarise(n = n(), 
            win_rate = mean(win == "Yes"))

grants_def %>% 
  group_by(domain, sex) %>% 
  summarise(n = n(), 
            win_rate = mean(win == "Yes"))

grants_def %>% 
  group_by(domain) %>% 
  summarise(male_rate = mean(sex == "Male"), 
            female_rate = mean(sex == "Female"), 
            difference = male_rate - female_rate)

grants_def %>% 
  group_by(domain, sex) %>%
  summarise(mean_score = mean(score), 
            mean_hirsh = mean(hirsh))

summary(data$model[[1]])
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
  labs(title = "Modelling win var by domain") + 
  theme(text = element_text(size = 8))
ggsave("out/pic/win_models_by_domain.png", width = 10, height = 6)



