temp3 <- 
  read_rds("data/processed/super_fin.rds") %>%
  mutate(work_place = as.character(work_place) %>% 
           ifelse(. == "", "(Missing)", .)) %>%
  mutate(work_place = 
           replace(work_place, 
                   str_detect(work_place, "Евразийский национальный университет|ЕНУ"), 
                   "ЕНУ")) %>%
  mutate(work_place = 
           replace(work_place, 
                   str_detect(work_place, "Фараби"),
                   "КазНУ")) %>%
  mutate(work_place = 
           replace(work_place, 
                   str_detect(work_place, 'Сатпаев|АО "Институт металлургии и обогащения"') & 
                     !str_detect(work_place, "Институт геологических наук"),
                   "КазНТУ")) %>%
  mutate(work_place =
           work_place %>% 
           replace(., str_detect(., "Южно-Казахстанский госуд|ЮКГУ|Ауэзов") &
                     !str_detect(., "педагог|Институт литературы и искусства"),
                   "ЮКГУ")) %>%
  mutate(work_place = 
           work_place %>% 
           replace(., str_detect(., "аграр") &
                     !str_detect(., "Жангир"), 
                   "КазНАУ")) %>%
  mutate(work_place = 
           work_place %>%
           replace(., str_detect(str_to_lower(.), "агротех"), "КазАТУ")) %>%
  mutate(work_place = 
           work_place %>%
           replace(., str_detect(., "Караг") & str_detect(., "Букет|КГУ"),
                   "КГУ")) %>%
  mutate(work_place = 
           work_place %>%
           replace(., str_detect(., "Караг") & str_detect(., "медиц"), 
                   "КарГМУ")) %>%
  mutate(work_place = 
           work_place %>%
           replace(., str_detect(., "Кокше") & str_detect(., "Уали"), 
                   "Кокшетауский ГУ")) %>%
  mutate(work_place = 
           work_place %>%
           replace(., str_detect(., "Кокше") & str_detect(., "Мырза"), 
                   "Кокшетауский У им. Мырзахметова")) %>%
  mutate(work_place = 
           work_place %>%
           replace(., str_detect(., "Южно") & str_detect(str_to_lower(.), "пед"), 
                   "ЮКГПУ")) %>%
  mutate(work_place = 
           work_place %>%
           replace(., str_detect(str_to_lower(.), "назар|nazar") &
                     str_detect(str_to_lower(.), "uni|уни") |
                     str_detect(str_to_lower(.), "national laboratory"), "NU")) %>%
  mutate(work_place = 
           work_place %>%
           replace(., str_detect(str_to_lower(.), "нархоз"), 
                   "Нархоз")) %>%
  distinct()

orgs <-
  read_csv("data/raw/orgs/a_organisations_adil.csv") %>%
  mutate(across(c(uni, national, international), 
                ~replace(.x, is.na(.x), 0))) %>%
  select(work_place, uni:international)

temp4 <- 
  temp3 %>% left_join(orgs) %>%
  mutate(across(c(uni, national, international), 
                ~replace(.x, is.na(.x), 0))) %>% 
  unite(org_prestige, uni, national, international) %>%
  mutate(org_prestige = case_when(org_prestige == "0_0_0" ~ "Other", 
                                   org_prestige == "1_0_0" ~ "Regional",
                                   org_prestige == "1_1_0" ~ "National",
                                   org_prestige == "1_0_1" ~ "International",
                                   TRUE ~ as.character(org_prestige))) %>%
  mutate(org_prestige = 
           fct_relevel(org_prestige, "Regional", "National", "International")) %>%
  select(-work_place) %>%
  distinct()


grants_def <-
  temp4 %>%
  mutate(log_score = log(score)) %>%
  mutate(league = 
           case_when(
             score < quantile(score, 0.1) ~ "lower 10%",
             score > quantile(score, 0.9) ~ "upper 10%",
             TRUE ~ "middle"
           ) %>% 
           fct_relevel("lower 10%", "middle")) 

grants_def <-
  grants_def %>% 
  mutate(domain = fct_relevel(domain, "Culture"))

rm(temp4, temp3)

####### test

member_log <-
  glm(member ~ 
        score + hirsh + rints + scopus + fake + # quality indicators
        win_2014 + degree + # institutional memory aka Mathew effect
        domain + sex + region + # demographics
        org_prestige + pr_rank,
      family = "binomial", data = grants_def)
summary(member_log)

grants_def %>% 
  group_by(domain) %>% 
  summarise(member_frac = mean(member == "yes"),
            member_count = sum(member == "yes"), 
            n = n()) %>%
  arrange(desc(member_frac))


grants_def %>% 
  group_by(org_prestige) %>% 
  summarise(member_frac = mean(member == "yes"),
            member_count = sum(member == "yes"), 
            n = n()) %>%
  arrange(desc(member_frac))


grants_def %>% 
  group_by(domain, work_place) %>% 
  summarise(n = n(), win = sum(win == "Yes")) %>%
  mutate(prop = round(win/n*100, 3)) %>%
  filter(n > 10) %>%
  arrange(domain, desc(n))
write_rds(grants_def, 
          file = "data/processed/grants_def.rds")
rm(list = ls())
