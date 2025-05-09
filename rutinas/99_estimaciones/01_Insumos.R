

rm(list = ls())

library(samplesize4surveys)
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
# Correspondencia codigos dominios
#-------------------------------------------------------------------------------

correspondencia_dom <- import("insumos/01_estimaciones/correspondencia_dom.xlsx") %>% 
  clean_names() 


#-------------------------------------------------------------------------------
# MMM: Numero de UPMS en la poblacion: M
#-------------------------------------------------------------------------------

marco <- import("insumos/01_estimaciones/20211025_marco_upm.rds") %>% 
  mutate(cod_4 = substr(id_conglomerado,1,4))

upm_prov <- marco %>%  
  #filter(!cod_4 %in% v_ciudades_auto) %>% 
  group_by(dom) %>% summarise(n_upm_pobl = n())

upm_ciudades_auto <- marco %>% 
  filter(cod_4 %in% v_ciudades_auto) %>% 
  group_by(cod_4) %>% 
  summarise(n_upm_pobl = n()) %>% rename("dom" = cod_4)

upm_dom <- rbind(upm_prov,upm_ciudades_auto)

upm_dom <- rbind(upm_dom, c("010101",sum(upm_dom$n_upm_pobl)))

#-------------------------------------------------------------------------------
# Calculo N-f_exp: N
#-------------------------------------------------------------------------------

N_pobl <- base %>% group_by(dom) %>% 
  filter( p03>=12) %>% 
  summarise(N = sum(fexp))

N_pobl <- rbind(N_pobl, c("010101",sum(N_pobl$N))) %>% 
  mutate(N = as.numeric(N))










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
# Lectura estimaciones - DIES
#-------------------------------------------------------------------------------

ind <- import("insumos/01_estimaciones/Reporte.xlsx") %>% 
  clean_names() 

#-------------------------------------------------------------------------------
#Juntando todos los resultados en ind
#----------------------------------------------------------------------------

ind <- ind %>% 
  left_join(correspondencia_dom) %>% 
  left_join(N_pobl) %>% 
  left_join(DOMTOTAL_nacional) %>% 
  left_join(upm_dom) %>% 
  left_join(per_por_hogar)

ind <- ind %>% select(dominio, 
                      "estimacion_media_mu"    = tiempo_medio, 
                      "error_estandar"         = tiempo_medio_se,
                      "estimaciion_sd_sigma"   = tiempo_medio_var_2,
                      "deff"                   = tiempo_medio_deff,
                      "N_pobl"                 = N, 
                      "upm_pobl_M"             = n_upm_pobl, 
                      "b"                      = b) 

ind <- ind %>% mutate(mer_delta = error_estandar * 1.96/estimacion_media_mu, 
                      rho = (deff - 1)/( b * 12 - 1), 
                      conf = 0.95, 
                      tnr = 0.2)

#-------------------------------------------------------------------------------
#------------------ CREANDO VARIABLES FALTANTES ----------------------------
#----------------------------------------------------------------------------


base_insumos <- ind

N = base_insumos$N_pobl
M = base_insumos$upm_pobl_M
rho = base_insumos$rho
mu = base_insumos$estimacion_media_mu
sigma = base_insumos$estimaciion_sd_sigma
delta = base_insumos$mer_delta
conf = base_insumos$conf
m = 12
tnr = 0.2
b = base_insumos$b

i = 2

for (i in 1:dim(base_insumos)[1]){
  
  n_per <- ss4m(N = N[i], 
            mu = mu[i], 
            sigma = sigma[i], 
            DEFF = 1 + (m * b[i] - 1) * rho[i], 
            conf = conf[i],  
            error = "rme", 
            delta = delta[i], 
            plot = FALSE)
  n_viv <- ceiling(n_per/b[i])
  n_upm <-  ceiling(n_viv/m)
  
  a <- c(dominio = base_insumos$dominio[i], n_per = n_per, n_viv = n_viv, n_upm = n_upm)
  
  if (i == 1){
    muestra1 <- (a)
  }else{
    muestra1 <- rbind(muestra1,a)
  }
}

muestra1 %>% adorn_totals() %>% View()

export(ind, "ind.xlsx")












