install.packages("anonymizer")
devtools::install_github("paulhendricks/anonymizer")
library(anonymizer)
letters %>% head %>% table()
#> [1] "a" "b" "c" "d" "e" "f"
letters %>% head %>% salt(.seed = 1) %>% table()

grants_def_anonrepo <-
  grants_def %>% 
  select(-project_name, -log_score, - league, -connection) %>% 
  rename(delisted = fake, 
         pi_id = name) %>%
  mutate(pi_id = hash(pi_id, .seed = 12))
write_csv(grants_def_anonrepo, 
          "out/data/replication_dataset_negotiating_academic_funding.csv")
