
base %>% group_by(pro)

base %>% select(950:956) %>% View()

base %>% group_by(id_upm, id_hogar) %>% summarise(n()) %>% View()


base <- base %>% 
  mutate(dom = ifelse(substr(id_upm, 1, 6) %in% v_ciudades_auto, 
                      substr(id_upm, 1, 6), substr(id_upm, 1, 2) )) 


a <- base %>% 
  group_by(dom, id_upm) %>% 
  summarise(n_t = n_distinct(id_hogar)) %>% 
  left_join( base %>% 
  filter(!is.na(domtotal)) %>% 
  group_by(dom, id_upm) %>% summarise(n_e = n_distinct(id_hogar)), 
  by = c("dom", "id_upm"))


base %>% 
  group_by(dom, id_upm) %>%
  mutate(n_t = n_distinct(id_hogar)) %>% 
  ungroup() %>% 
  filter(!is.na(domtotal)) %>% 
  group_by(dom) %>%
  mutate(n_e = n_distinct(id_hogar), 
         t_efec = n_e/n_t) %>% 
  ungroup() %>% View()

t_efect_1 <- base %>% 
  filter(!is.na(domtotal)) %>% 
  mutate(pro = substr(id_upm, 1, 2)) %>% 
  group_by(pro) %>% summarise(n_distinct(id_hogar)) %>% View("efec")

base %>% 
  #filter(!is.na(domtotal)) %>% 
  mutate(pro = substr(id_upm, 1, 2)) %>% 
  group_by(pro) %>% summarise(n_distinct(id_hogar)) %>% View("Total")
