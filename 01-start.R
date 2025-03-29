rm(list = ls())
if(!dir.exists("data")){dir.create("data")}
if(!dir.exists("out")){dir.create("out")}
library(tidyverse)
library(mice)
source("funs/my_functions.R")

grants <- 
  read_csv("data/raw/grants.csv") %>%
  select(-...15)

usual_factors <- 
  c("domain", "win", "win_2014", 
    "rints", "scopus", "fake", 
    "sex", "member", "degree")

domain_names <- 
  c(Science = "1", 
    Culture = "2", 
    Life = "3", 
    Security = "4", 
    Natural_rm = "5",
    Agriculture = "6", 
    Energy = "7")

## Selecting variables

vars_set_win <- 
  c("win", # var of interest
    "score", # academic merit
    "win_2014", # experience
    "hirsh", "scopus", "rints", # academic merit 
    "member", # social capital
    "domain", # competition
    "fake", "sex", "region", "degree", # other control vars
    "project_name", "name", "proj_id"
  )

# Tidying
grants_tidy_na <- 
  grants %>%
  select(-discipline, -year) %>% # those two were poorly coded
  mutate(across(contains("degree"), ~replace(., . == 18, NA))) %>%
  mutate(across(contains("fake"), ~replace(., . == 2, NA))) %>%
  mutate(across(contains("sex"), ~replace(., is.na(.), 1))) %>%
  mutate(across(all_of(usual_factors), as_factor)) %>%
  mutate(hirsh    = # h-index were not always a number
           parse_number(
             as.character(hirsh)),
         domain   = # domains were coded as numbers
           as_factor(as.character(domain)) %>%
           fct_recode(!!!domain_names), # changing to more sensible names
         region   = # region names have lots of typos
           str_to_lower(region) %>% 
           fct_collapse(
             актобе = c("актюбинск"),
             алматы = c("алмата"), 
             семей  = c("семипалатинск"), 
             `усть-каменогорск` = c("усть-каменогрск")) %>%
           fct_infreq() %>% 
           fct_lump(n = 3) %>% # there are too many regions  
           fct_recode(
             Алматы = "алматы", 
             Астана = "астана", 
             Шымкент = "шымкент"),
         member = 
           fct_recode(member, yes = "1", no = "0"),
         score = replace(score, score == 37.67, 31.67)) %>% # a mistake
#  mutate(across(where(is.factor), fct_explicit_na)) %>%
  mutate(proj_id = 1:4488) %>% # for convenience
  mutate(degree = 
           case_when(degree == 1 ~ "Candidate", 
                     degree == 2 ~ "Doctor", 
                     degree == 3 ~ "PhD",
                     TRUE ~ as.character(degree)) %>%
           replace(., . == "магистр", NA) %>%
           fct_relevel("Candidate")) %>% # Candidates the reference cat
  mutate(sex =
           ifelse(sex == 1, "Male", "Female") %>% 
           fct_relevel("Male"), 
         fake = 
           fct_recode(fake, Yes = "1", No = "0"),
         win_2014 =  
           ifelse(win_2014 == 1, "Yes", "No") %>% 
           fct_relevel("No"), 
         rints = 
           ifelse(rints == 1, "Yes", "No") %>% 
           fct_relevel("No"), 
         scopus = 
           fct_recode(scopus, Yes = "1", No = "0") %>% 
           fct_relevel("No")) %>%
  arrange(proj_id) %>% 
  select(all_of(vars_set_win))

write_rds(grants_tidy_na, file = "data/processed/grants_tidy_na.rds")


## Часть где я вставляю патч с данными в изначальный датасет

# Колонки по которым будет проходить слияние
key_cols <- c("project_name", "name", "win", "score", "sex")

# Патч с данными для сельхозки
missing_aggri <-
  readxl::read_xlsx("data/raw/missing_agriculture.xlsx") %>%
  rename(name = ФИО,
         project_name = "название проекта",
         win = победа, 
         score = баллы, 
         scopus = Скопус,
         hirsh = Хирш,
         fake = Фейк, 
         sex = Мужской) %>% 
  mutate(sex = # приводим к общему виду
           ifelse(sex == 1, "Male", "Female") %>% 
           fct_relevel("Male"),
         win = as_factor(win),
         scopus = 
           ifelse(scopus == 1, "Yes", "No") %>% 
           fct_relevel("No"), 
         fake = 
           ifelse(fake == 1, "Yes", "No") %>% 
           fct_relevel("No")) %>% 
  left_join(grants_tidy_na %>% # сначала мерджим ключ из главного дф
              select(proj_id, all_of(key_cols)), 
            by = key_cols)

# на всякий случай результат слияния сохраняю в отдельном дф
grants_tidy_na2 <- 
  grants_tidy_na %>%
  left_join(missing_aggri %>% 
              select(-all_of(key_cols)), 
            by = "proj_id") %>% 
  mutate(scopus = coalesce(scopus.x, scopus.y), # склеиваю колонки
         hirsh = coalesce(hirsh.x, hirsh.y),
         fake = coalesce(fake.x, fake.y)) %>% 
  select(-ends_with(c(".x", ".y"))) %>%
  arrange(proj_id) %>% 
  select(all_of(vars_set_win))

# Сохраняем на диск
write_rds(grants_tidy_na2, file = "data/processed/grants_tidy_na2.rds")

## Шаг 3: импутируем пропущенные значения
grants_tidy <- 
  grants_tidy_na %>% 
  group_by(domain) %>% 
  nest() %>% # делаю лист дфов, чтобы импутация была локальной для домена
  mutate(data = 
           map(data, 
               function(df){
                 #' тут происходит импутация
                 #' mice() выдает дохуя варнингов, это норм, 
                 #' автор - душнила
                 df <- 
                   mice(df, method = "rf", print = FALSE) # случайный лес
                 complete(df)
               })) %>% 
  unnest(data) %>% 
  ungroup() %>%
  arrange(proj_id) %>%
  select(all_of(vars_set_win))


## A quick sanity check of missing values
ini <- mice(grants_tidy_na, maxit = 0)
ini$nmis
ini2 <- mice(grants_tidy_na2, maxit = 0)
ini2$nmis
ini3 <- mice(grants_tidy, maxit = 0)
if(all(ini3$nmis == 0)){
  "Missing values are successfully imputed"
  } else {"Something is wrong in missing values"}

## Now save the tidy df and delete everything else
write_rds(grants_tidy, file = "data/processed/grants_tidy.rds")
rm(list = ls())

## На выходе у нас три дфа, grants_tidy и grants_tidy_na/2

#' Может лучше импутировать данные перед самым анализом и после слияний
#' c прочими данными. 

