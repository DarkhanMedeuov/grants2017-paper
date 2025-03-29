source("funs/my_functions.R")
grants_tidy <- 
  read_rds("data/processed/grants_tidy.rds") %>% 
  mutate(win = 
           ifelse(win == 1, "Yes", "No") %>% 
           fct_relevel("No"))

grants_temp <- 
  grants_tidy %>%
  mutate(name = str_squish(name)) %>%
  mutate(score = round(score, 2)) %>%
  select(name, score, everything())


grants_super_fin <- 
  grants_temp %>%
  count(name) %>%
  left_join(grants_temp) %>%
  filter(n == 2) %>%
  group_by(name) %>%
  mutate(pr_rank = (max(score) == score) + 1) %>%
  mutate(pr_rank2 = (max(pr_rank) == min(pr_rank)) + 0) %>%
  mutate(pr_rank = pr_rank + pr_rank2) %>%
  select(-pr_rank2) %>%
  ungroup() %>%
  right_join(grants_temp) %>%
  select(-n) %>%
  mutate(across(contains("pr_rank"), ~replace(., is.na(.), 0))) %>%
  mutate(pr_rank = 
           case_when(pr_rank == 0 ~ "only",
                     pr_rank == 1 ~ "second", 
                     pr_rank == 2 ~ "best", 
                     pr_rank == 3 ~ "tie",
                     TRUE ~ as.character(pr_rank)) %>%
           fct_relevel(c("only", "second", "best", "tie")))

connections_files <-
  str_c("data/raw/connections", list.files("data/raw/connections"), sep = "/")
names(connections_files) <- 
  list.files("data/raw/connections") %>% 
  str_remove(".csv") %>% str_to_sentence()
connections <- 
  lapply(connections_files, 
         read_connections) %>% 
  bind_rows(.id = "domain") %>% 
  mutate(domain = fct_recode(domain, Culture = "Mangilik")) %>% 
  as_tibble()

if (nrow(connections) == nrow(grants_super_fin)) {
  print("script 02 is alright")
}


