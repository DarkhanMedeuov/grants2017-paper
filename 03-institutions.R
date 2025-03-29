set1 <- setdiff(grants_super_fin$name, connections$name)
set2 <- setdiff(connections$name, grants_super_fin$name)

mat1 <- adist(set1, set2)
ind <- which(mat1 == 1, arr.ind = TRUE)
patterns <-
  set1[ind[ ,"row"]]
replacements <-
  set2[ind[ ,"col"]]
name_correction <- replacements
names(name_correction) <- patterns

temp <- 
  grants_super_fin %>%
  mutate(name = str_replace_all(name, name_correction)) %>%
  mutate(name = case_when(name == "Лежиев Сергей Николаевич" ~ 
                            "Лежнев Сергей Николаевич", 
                          name == "Наурызбаева Калдыбай Кулсинбаевич" ~ 
                            "Наурызбаев Калдыбай Кулсинбаевич",
                          TRUE ~ as.character(name)))

nrow(anti_join(temp, connections)) == 0
nrow(anti_join(connections, temp)) == 0

fin <- temp %>% 
  left_join(connections)

dup <- duplicated(fin)

super_fin <- 
  fin %>%
  distinct() %>%
  mutate(connection = 
           replace(connection, 
                   member == "yes" & connection == 0,
                   1)) %>%
  unite("inst_cap", 
          connection, member, sep = "_", remove = FALSE) %>% 
  mutate(inst_cap = 
           case_when(
             inst_cap == "0_no" ~ "No", 
             inst_cap == "1_no" ~ "Works with",
             inst_cap == "1_yes" ~ "Member",
             inst_cap == "NA_no" ~ "(Missing)",
             TRUE ~ as.character(inst_cap)
           ) %>%
           fct_relevel("No", "Works with", "Member")) %>%
  mutate(domain = fct_relevel(domain, "Agriculture"))

super_fin %>% count(inst_cap, member)
super_fin %>% count(connection, member)


write_rds(super_fin, file = "data/processed/super_fin.rds")

