counter_factual_test <- grants_def[1, ]

list_of_vars <- as.list(counter_factual_test)
list_of_vars$score <- 23:24
haha <- expand_grid(!!!list_of_vars)
hypothetical_arslanov <- augment(last_fit_log, new_data = haha)
hypothetical_arslanov %>% 
  filter(score == 23) %>%
  select(score, .pred_Yes)


p1 = 0.3653935
odds1 = p1/(1-p1)
odds2 = exp(beta)*odds1
beta = coef(extract_fit_engine(last_fit_log))[c("score", "score_x_domain_Science")]
p2 = odds2/(1+odds2)
p2

hypothetical_arslanov %>% 
  select(score, .pred_Yes) %>% View()

exp(hypothetical_arslanov)/(1+exp(hypothetical_arslanov))
hypothetical_arslanov[2] - hypothetical_arslanov[1]