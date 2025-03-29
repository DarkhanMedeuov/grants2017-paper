read_connections <-
  function(file){
    read.csv(file) %>%
      mutate(Баллы = 
                    str_replace_all(Баллы, ",", ".") %>% 
                    parse_number()) %>%
      mutate(ФИО = 
                  str_squish(ФИО)) %>%
      rename(name = 
               ФИО, 
             score = Баллы, 
             work_place = Место.работы,
             connection = совпадение.места.работы.с.членом.ННС) %>%
      select(name, score, connection, work_place) %>%
      mutate(score = round(score, 2))
  }