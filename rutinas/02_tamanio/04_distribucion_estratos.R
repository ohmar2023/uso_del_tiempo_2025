
rm(list = ls())

#-------------------------------------------------------------------------------
# Lectura del marco a nivel de UPM
#-------------------------------------------------------------------------------

marco_upm_01 <- import("insumos/01_estimaciones/marco_upm_01.rds")

#-------------------------------------------------------------------------------
# Definiendo estrato RURAL para Guayaquil
#-------------------------------------------------------------------------------

marco_eut <- marco_upm_01 %>% 
  mutate(estrato_eut = case_when(substr(id_upm, 1, 4) == "0901" & area == "2" ~ "4229", 
                                 T ~ estrato),  
         dom = substr(id_upm, 1, 2), 
         dom = case_when(dom == "17" & substr(estrato_eut, 1, 2) == "33" ~ "1701", 
                         dom == "09" & substr(estrato_eut, 1, 2) == "42" ~ "0901", 
                         T ~ dom))

marco_eut <- marco_eut %>% 
  group_by(dom, estrato_eut) %>% 
  summarise( n_viv = sum(Mi)) %>% 
  mutate(p = n_viv/sum(n_viv) ) 

#-------------------------------------------------------------------------------
# Lectura tamaño EUT
#-------------------------------------------------------------------------------

tamanio_inicial <- import("productos/01_tamanio/07_final/resultado_DOMTOTAL_todo_0055PORC_002.xlsx") %>% 
  filter(dom != "Total")

#-------------------------------------------------------------------------------
# Juntando base del marco con tamaño
#-------------------------------------------------------------------------------

tam_distr_estratos <- marco_eut %>% 
  left_join(select(tamanio_inicial, dom, dominio, n_upm_muestra), by = "dom") %>%
  mutate(area = substr(estrato_eut, 3,3),
         n_upm_estrato = ceiling(n_upm_muestra * p),
         n_upm_estrato = ifelse(n_upm_estrato < 3, 3, n_upm_estrato), 
         n_upm_muestra_distr = sum(n_upm_estrato), 
         n_dif = n_upm_muestra_distr - n_upm_muestra) %>% 
  group_by(dom) %>% 
  arrange(dom, desc(n_upm_estrato)) %>% 
  mutate(orden = row_number()) %>% 
  ungroup() %>% 
  mutate(n_upm_estrato_final = ifelse(orden <= n_dif,n_upm_estrato - 1, n_upm_estrato))

#-------------------------------------------------------------------------------
# Control
#-------------------------------------------------------------------------------

tam_distr_estratos %>% group_by(dom, dominio) %>% 
  summarise(n_upm_distr = sum(n_upm_estrato_final)) %>% 
  left_join(select(tamanio_inicial, n_upm_muestra, dom), by = "dom") %>% 
  mutate(diferencia = n_upm_distr - n_upm_muestra) %>% 
  adorn_totals() %>% 
  View()

tam_distr_estratos %>% group_by(area) %>% summarise(sum(n_upm_estrato_final)) %>% View()

#-------------------------------------------------------------------------------
# Exportando
#-------------------------------------------------------------------------------

tamanio_estratos_final <- tam_distr_estratos %>% 
  select(dom, dominio, area, n_upm_estrato_final) %>% 
  group_by(dom, dominio, area) %>% 
  summarise(n_upm_distr_estr = sum(n_upm_estrato_final), 
            n_upm_distr_estr_viv = n_upm_distr_estr * 9) %>% 
  adorn_totals()

export(tamanio_estratos_final, "tamanio_estratos_final.xlsx")
export(tam_distr_estratos, "tam_distr_estratos.rds")








