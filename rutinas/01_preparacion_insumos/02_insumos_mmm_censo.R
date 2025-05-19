
#-------------------------------------------------------------------------------
# MMM: Numero de UPMS en la poblacion: M
#-------------------------------------------------------------------------------

# marco_borr <- import("insumos/01_estimaciones/20211025_marco_upm.rds") %>% 
#   mutate(cod_4 = substr(id_conglomerado,1,4))

marco <- import("insumos/01_estimaciones/marco_upm_01.rds")

upm_prov <- marco %>%  
  mutate( dom = substr(id_upm, 1, 2)) %>% 
  group_by(dom) %>% summarise(n_upm_pobl = n())

upm_ciudades_auto <- marco %>% 
  mutate(estrato_eut = case_when(substr(id_upm, 1, 4) == "0901" & area == "2" ~ "4229", 
                                 T ~ estrato),  
         dom = substr(id_upm, 1, 2), 
         dom = case_when(dom == "17" & substr(estrato_eut, 1, 2) == "33" ~ "1701", 
                         dom == "09" & substr(estrato_eut, 1, 2) == "42" ~ "0901", 
                         T ~ dom)) %>% 
  filter(dom %in% c("1701","0901")) %>%
  group_by(dom) %>% 
  summarise(n_upm_pobl = n())

upm_dom <- rbind(upm_prov,upm_ciudades_auto)

upm_dom <- rbind(upm_dom, c("010101",sum(upm_dom$n_upm_pobl)))

#-------------------------------------------------------------------------------
# Calculo N desde el CENSO: Personas mayores a 12 años.
#-------------------------------------------------------------------------------
info_censo_eut <- readRDS("intermedios/01_insumos_tamanio/info_censo_eut.rds")

N_pobl <- info_censo_eut %>% select(dom = dominio, N = tot_per_12)

#info_censo_eut %>% filter(substr(dominio,1,2) %in% c("17", "09")) %>% View()

#-------------------------------------------------------------------------------
# Calculo N-f_exp: N
#-------------------------------------------------------------------------------

# N_pobl <- base %>% group_by(dom) %>% 
#   filter( p03>=12) %>% 
#   summarise(N = sum(fexp))
# 
# N_pobl <- rbind(N_pobl, c("010101",sum(N_pobl$N))) %>% 
#   mutate(N = as.numeric(N))


#-------------------------------------------------------------------------------
# Estimacion SD
#-------------------------------------------------------------------------------

# # Diseño de la Encuesta
# d1 <- base %>%
#   as_survey_design(
#     ids = id_upm,
#     #strata = estrato,
#     weights = fexp,
#     nest = T)
# 
# options(survey.lonely.psu = "certainty")
# 
# DOMTOTAL_nacional <- d1 %>%  
#   filter( ocupado == 1 & p03 >= 12 & !is.na(domtotal) &  tresul == 2) %>%
#   group_by(dom) %>% 
#   summarise(tiempo_medio_var_2 = survey_sd(domtotal,deff= T, 
#                                            vartype = c("se", "ci", "cv", "var"),
#                                            na.rm = TRUE))






