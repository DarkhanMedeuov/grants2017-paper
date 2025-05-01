library(patchwork)

full_rec <-
  recipe(win ~ 
           score + hirsh + rints + scopus + fake + # quality indicators
           win_2014 + degree + # institutional memory aka Mathew effect
           domain + sex + region + # demographics
           org_prestige + pr_rank + 
           inst_cap, 
         data = grants_train) %>% 
  step_relevel(domain, ref_level = "Culture") %>%
  step_dummy(all_nominal_predictors())
full_scoreXdomain <-
  full_rec %>% 
  step_interact(~ score:starts_with("domain_"))

logit_model <- 
  logistic_reg() %>% 
  set_engine("glm", family = binomial())

final_log_wflow1 <-
  workflow() %>% 
  add_recipe(full_rec) %>% 
  add_model(logit_model) 
final_log_wflow2 <-
  workflow() %>% 
  add_recipe(full_scoreXdomain) %>% 
  add_model(logit_model) 

grants_def <-
  grants_def %>% 
  mutate(win = fct_relevel(win, "No"))

last_fit_log1 <- fit(final_log_wflow1, 
                         data = grants_def)
last_fit_log2 <- fit(final_log_wflow2, 
                     data = grants_def)

summary(extract_fit_engine(last_fit_log1))
summary(extract_fit_engine(last_fit_log2))


new_grants_list <- 
  with(data = grants_def, 
       list(score = 6:36,
              domain = levels(domain), 
              sex = levels(sex),
              hirsh = median(hirsh),
              rints = fct_infreq(rints) %>% levels() %>% .[1],
              scopus = fct_infreq(scopus) %>% levels() %>% .[1],
              fake = fct_infreq(fake) %>% levels() %>% .[1],
              win_2014 = fct_infreq(win_2014) %>% levels() %>% .[1],
              region = fct_infreq(region) %>% levels() %>% .[1], 
              org_prestige = fct_infreq(org_prestige) %>% levels() %>% .[1],
              pr_rank = fct_infreq(pr_rank) %>% levels() %>% .[1],
              inst_cap = fct_infreq(inst_cap) %>% levels() %>% .[1],
              degree = fct_infreq(degree) %>% levels() %>% .[1]))
new_grants_data <-
  expand_grid(!!!new_grants_list)

grants_preds <-
  augment(last_fit_log1, new_grants_data) %>% 
  bind_cols(
    predict(last_fit_log2, new_grants_data, type = "prob") %>% 
      transmute(.pred_Yes_int = .pred_Yes)) %>%
  bind_cols(
    predict(last_fit_log1, new_grants_data, type = "conf_int", level = 0.90)
  ) %>% 
  bind_cols(
    predict(last_fit_log2, new_grants_data, type = "conf_int", level = 0.90) %>%
      transmute(.pred_lower_Yes_int = .pred_lower_Yes, 
                .pred_upper_Yes_int = .pred_upper_Yes)
  ) %>% 
  mutate(
    across(where(is.character), as_factor)
  )
  

p1 <-
  grants_preds %>% 
  filter(domain %in% c("Culture", "Life", "Natural_rm")) %>%
  ggplot(aes(score, .pred_Yes, color = domain), show.legend = FALSE) +
  geom_ribbon(
    aes(ymin = .pred_lower_Yes,
        ymax = .pred_upper_Yes,
        fill = domain),
    alpha = 0.5, show.legend = FALSE
  ) + 
  geom_line() +
  facet_wrap(~sex) +
  labs(y = "Predicted probability of funding", 
       x = "Score", color = NULL,
       title = "No interaction") + 
  scale_x_continuous(breaks = seq(6, 36, by = 2)) + 
  theme_minimal()
p1
p2 <-
  grants_preds %>% 
  ggplot(aes(score, .pred_Yes, color = sex)) +
  geom_ribbon(aes(
    ymin = .pred_lower_Yes,
    ymax = .pred_upper_Yes),
    alpha = 0.5
  ) +
  geom_line() +
  facet_wrap(~domain) +
  labs(y = "Predicted probability of funding", 
       x = "Score", color = NULL,
       title = "No interaction") 
p2

### Interaction

p3 <-
  grants_preds %>% 
  filter(domain %in% c("Culture", "Life", "Natural_rm")) %>%
  ggplot(aes(score, .pred_Yes_int, color = domain)) +
  geom_ribbon(
    aes(ymin = .pred_lower_Yes_int,
        ymax = .pred_upper_Yes_int,
        fill = domain),
    alpha = 0.5, show.legend = FALSE
  ) + 
  geom_line() +
  facet_wrap(~sex) +
  labs(y = "Predicted probability of funding", 
       x = "Score", color = NULL, 
       title = "Score interacts with domain") + 
  scale_x_continuous(breaks = seq(6, 36, by = 2)) + 
  theme_minimal()
p3
p4 <-
  grants_preds %>% 
  ggplot(aes(score, .pred_Yes_int, color = sex)) +
  geom_ribbon(aes(
    ymin = .pred_lower_Yes_int,
    ymax = .pred_upper_Yes_int),
    alpha = 0.5
  ) +
  geom_line() +
  facet_wrap(~domain) +
  labs(y = "Predicted probability of funding", 
       x = "Score", color = NULL,
       title = "Score interacts with domain") 

library(patchwork)
(p1/p3)
ggsave(filename = "out/pic/final_models_predictions1.png",
       width = 10, height = 6)
(p2+theme_minimal())+(p4+theme_minimal())
ggsave(filename = "out/pic/final_models_predictions2.png",
       width = 10, height = 6)


illus_rec_bs <-
  recipe(win ~ domain + sex, 
         data = grants_train) %>% 
  step_relevel(domain, ref_level = "Mangilik") %>% 
  step_dummy(all_nominal_predictors())

illus_fit <-
  workflow() %>% 
  add_recipe(illus_rec_bs) %>% 
  add_model(logit_model) %>%
  fit(grants_def)
summary(extract_fit_engine(illus_fit))

## some explorations

grants_parsed <-
  grants_def %>%
  group_by(domain, sex) %>%
  summarise(grants_count = mean(win == "Yes"), 
            n = n(), .groups = "drop")


p1 <-
  grants_parsed %>%
  ggplot(aes(grants_count, domain)) +
  geom_segment(
    data = grants_parsed %>%
      pivot_wider(domain,
        names_from = sex,
        values_from = grants_count
      ),
    aes(x = Male, xend = Female, y = domain, yend = domain),
    alpha = 0.7, color = "gray70", size = 1.5
  ) +
  geom_point(aes(color = sex), size = 3) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Probability of getting funded", y = NULL, color = NULL)

p1  

new_data <-
  tibble(domain = levels(grants_def$domain)) %>%
  crossing(sex = c("Male", "Female"))

grant_preds <-
  augment(illus_fit, new_data) %>%
  bind_cols(
    predict(illus_fit, new_data, type = "conf_int")
  ) %>% 
  mutate(sex = fct_relevel(sex, "Male"))

grant_preds  


p2 <-
  grant_preds %>%
  ggplot(aes(.pred_Yes, domain, color = sex)) +
  geom_errorbar(aes(
    xmin = .pred_lower_Yes,
    xmax = .pred_upper_Yes
  ),
  width = .2, size = 1.2, alpha = 0.5, show.legend = FALSE) +
  geom_point(size = 2.5) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Predicted probability of getting funded", y = NULL, color = NULL)

p1+p2
ggsave(file = "out/pic/illustration.png", width = 10, height = 6.18)






