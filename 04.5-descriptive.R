library(tidyverse)
library(modelsummary)
grants_def <- 
  read_rds(file = "out/data/replication_dataset_negotiating_academic_funding.rds")
tab1 <-
  grants_def %>% 
  group_by(domain) %>% 
  summarise(n = n(), 
            winners = sum(win == "Yes"), 
            proportion = winners/n*100, 
            `mean score` = mean(score), 
            `median score` = median(score), 
            sd = sd(score), 
            q1 = quantile(score, 0.25), 
            q3 = quantile(score, 0.75),) %>%
  arrange(domain)
#datasummary_df(tab1, output = "out/tables/tab1.docx")
datasummary_df(tab1, output = "out/tables/tab1.tex")

tab2 <- 
  grants_def %>% 
  mutate(score_ints = cut(score, breaks = seq(0, 40, by = 5))) %>% 
  mutate(score_ints = fct_collapse(score_ints, 
                                   `30+` = c("(30,35]", "(35,40]"))) %>%
  group_by(domain, score_ints) %>% 
  summarise(n = n(), 
            winners = sum(win == "Yes"), 
            proportion = winners/n*100) %>%
  ungroup() %>%
  arrange(domain)
tab2
(fig1 <-
  tab2 %>%
  ggplot(aes(x = score_ints, y = proportion)) + 
  geom_col(width = 0.1) +
  geom_point() +
  facet_wrap(~domain) +
  labs(y = "Success rate, %", x = "Score") + 
  theme(axis.text.x = element_text(size = 7)))
ggsave(filename = "out/pic/fig1_score_vs_success.pdf", 
       plot = fig1)
ggsave(filename = "out/pic/fig1_score_vs_success.tiff", 
       device = "tiff",
       plot = fig1, 
       dpi = 300)
ggsave(filename = "out/pic/fig1_score_vs_success.eps", 
       device = "eps",
       plot = fig1)

tab3 <- 
  grants_def %>% 
  group_by(domain) %>%
  summarise(prev_success_rate = mean(win_2014 == "Yes"), 
            Rints_rate = mean(rints == "Yes"), 
            Scopus_rate = mean(scopus == "Yes"), 
            Delisted_rate = mean(delisted == "Yes"), 
            Female_prop = mean(sex == "Female"), 
            hirsh0 = mean(hirsh == 0), 
            hirsh1 = mean(hirsh == 1), 
            hirsh2 = mean(hirsh == 2), 
            `hirsh3+` = mean(hirsh > 2), 
            candidate_rate = mean(degree == "Candidate"),
            doctor_rate = mean(degree == "Doctor"), 
            phd_rate = mean(degree == "PhD")) %>% 
  pivot_longer(-domain, names_to = "metric") %>% 
  pivot_wider(values_from = value, names_from = domain)

datasummary_df(tab3, output = "out/tables/S1_table.docx")


rm(fig1, tab1, tab2, tab3)
