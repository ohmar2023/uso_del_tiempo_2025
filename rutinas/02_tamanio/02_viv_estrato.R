#rm(list = ls())

#source("rutinas/99_librerias_funciones/librerias.R")

#-------------------------------------------------------------------------------
# Lectura marco 
#-------------------------------------------------------------------------------

#marco <- import("insumos/01_estimaciones/20211025_marco_upm.rds")
marco <- import("insumos/01_estimaciones/marco_upm_01.rds")

#-------------------------------------------------------------------------------
# Creacion de un nuevo estrato para guayaquil canton (0901)
# Esto porque la estratificacion no contempla una parte rural en el canton
# Tambien consolidamos la base con la creacion de los dom de Quito y Gye
#-------------------------------------------------------------------------------

marco <- marco %>% 
  mutate(estrato_eut = case_when(substr(id_upm, 1, 4) == "0901" & area == "2" ~ "2629", 
                                 T ~ estrato), 
         dom = substr(id_upm, 1, 2))

aux_1 <- marco %>% 
  mutate(dom = substr(id_upm, 1, 2)) %>% 
  group_by(dom,  estrato_eut) %>% 
  summarise(n_viv = sum(Mi))

aux_2 <- marco %>% 
  filter(dom %in% c("25","26")) %>% 
  group_by(dom,  estrato_eut) %>% 
  summarise(n_viv = sum(Mi)) %>% 
  mutate(dom = case_when(dom == "25"~ "1701", 
                         dom == "26"~ "0901", 
                         T ~ "Error"))

viv_estrato_eut <- rbind(aux_1, aux_2)

#-------------------------------------------------------------------------------
# Porporcion de viviendas
#-------------------------------------------------------------------------------

viv_estrato_eut <- viv_estrato_eut %>% 
  group_by(dom) %>% 
  mutate(prop = n_viv/sum(n_viv)) %>% 
  ungroup()












