
rm(list = ls())

source("rutinas/99_librerias_funciones/librerias.R")
source("rutinas/99_librerias_funciones/unzip.R")

#-------------------------------------------------------------------------------
# Identificadores de las ciudades autorepresentadas y Lectura EUT
#-------------------------------------------------------------------------------

v_ciudades_auto <- c("1701","0901")

base <- read_zip("insumos/01_estimaciones", "Base EUT 2012.zip", "Base EUT 2012.sav") %>% 
  clean_names() %>% mutate(dom = substr(id_upm, 1, 2))
aux <- base %>% mutate(dom = (substr(id_upm, 1, 4))) %>% 
  filter(dom %in% v_ciudades_auto)
base <- rbind(base, aux)

#-------------------------------------------------------------------------------
# Correspondencia identificadores de dominios
#-------------------------------------------------------------------------------

correspondencia_dom <- import("insumos/01_estimaciones/correspondencia_dom.xlsx") %>% 
  clean_names() 

#-------------------------------------------------------------------------------
# Promedio por dominio de cuantas personas > 12 hubieron por upm 
#-------------------------------------------------------------------------------

per_por_hogar <- base %>% 
  filter(!is.na(domtotal)) %>% 
  group_by(dom, id_hogar) %>% 
  summarise(n = n()) %>% 
  group_by(dom) %>% 
  summarise(b = mean(n))

#-------------------------------------------------------------------------------
# MM: Numero de UPMS en la poblacion: M & Calculo N desde el CENSO: Personas mayores a 12 años.
#-------------------------------------------------------------------------------

source("rutinas/01_preparacion_insumos/02_insumos_mmm_censo.R")

#-------------------------------------------------------------------------------
# Lectura estimaciones - DIES
#-------------------------------------------------------------------------------

# 1: resultado_DOMTOTAL
# 2: resultado_REMUNTOT
# 3: resultado_TTT

ind <- read_excel("insumos/01_estimaciones/Reporte_06_mayo_2025.xlsx", 
           sheet = "resultado_DOMTOTAL" ) %>% 
  clean_names() 

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

#-------------------------------------------------------------------------------
#Juntando todos los resultados en ind
#----------------------------------------------------------------------------

ind <- ind %>% 
  left_join(correspondencia_dom) %>% 
  left_join(N_pobl) %>% 
  #left_join(DOMTOTAL_nacional) %>% 
  left_join(upm_dom) %>% 
  left_join(per_por_hogar)

ind <- ind %>% select(dominio, 
                      "estimacion_media_mu"    = tiempo_medio, 
                      "error_estandar"         = tiempo_medio_se,
                      "estimaciion_sd_sigma"   = sd, #toca cambiar 
                      "deff"                   = tiempo_medio_deff,
                      "N_pobl"                 = N, 
                      "upm_pobl_M"             = n_upm_pobl, 
                      "b"                      = b) 

ind <- ind %>% mutate(mer_delta = error_estandar * 1.96/estimacion_media_mu, 
               rho = (deff - 1)/( b * 12 - 1), 
               conf = 0.95, 
               tnr = 0.2)

#-------------------------------------------------------------------------------
# Exportando
#----------------------------------------------------------------------------

ruta <- "intermedios/01_insumos_tamanio/"

export(ind, paste0(ruta, "base_insumos.rds"))
export(ind, paste0(ruta, "base_insumos.xlsx"))









