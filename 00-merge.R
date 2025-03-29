source("01-start.R")
source("02-connections.R")
source("03-institutions.R")
source("04-workplaces.R")
source("05-some_analysis.R")


grants_def %>% 
  filter(name %in% c("Козлов Владиллен Александрович", 
                     "Самашев Зайнолла", 
                     "Еркинбаева Лаззат Калымбековна", 
                     "Кливлеева Наиля Галивеевна")) %>% 
  View()

grants_def %>% 
  group_by(name) %>% 
  summarise(member = sum(member == "yes")) %>% 
  arrange(desc(member)) %>% 
  filter(member == 1)
  

grants_def %>% 
  filter(name == "Babaa Moulay Rachid")
