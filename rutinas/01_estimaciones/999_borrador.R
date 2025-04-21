
rm(list = ls())

library(samplesize4surveys)
source("rutinas/99_librerias_funciones/librerias.R")
source("rutinas/99_librerias_funciones/unzip.R")

#-------------------------------------------------------------------------------
# Identificadores de las ciudades autorepresentadas ----------------------------
#-------------------------------------------------------------------------------

v_ciudades_auto <- c("170150","090150")

#-------------------------------------------------------------------------------
# Lectura EUT
#-------------------------------------------------------------------------------

base <- read_zip("insumos/01_estimaciones", "Base EUT 2012.zip", "Base EUT 2012.sav") %>% 
  clean_names() %>% 
  mutate(dom = ifelse(substr(id_upm, 1, 6) %in% v_ciudades_auto, 
                      substr(id_upm, 1, 6), substr(id_upm, 1, 2)))

#-------------------------------------------------------------------------------
# 
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Lectura estimaciones - DIES
#-------------------------------------------------------------------------------

ind <- import("insumos/01_estimaciones/Reporte.xlsx") %>% 
  clean_names() 

#-------------------------------------------------------------------------------
# Correspondencia codigos dominios
#-------------------------------------------------------------------------------

correspondencia_dom <- import("insumos/01_estimaciones/correspondencia_dom.xlsx") %>% 
  clean_names() 


#-------------------------------------------------------------------------------
# Calculo de hogares prom por UPM 
#-------------------------------------------------------------------------------

prom_hogares_upm <- base %>% 
  group_by(dom, id_upm) %>% 
  summarise(n_t = n_distinct(id_hogar)) %>% 
  left_join( base %>% 
               filter(!is.na(domtotal)) %>% 
               group_by(dom, id_upm) %>% summarise(n_e = n_distinct(id_hogar)), 
             by = c("dom", "id_upm"))

prom_hogares_upm <- prom_hogares_upm %>% group_by(dom) %>% 
  summarise(n_prom_hog_upm = mean(n_e)) 

prom_hogares_upm <- rbind(prom_hogares_upm, 
                          c("010101",mean(prom_hogares_upm$n_prom_hog_upm))) %>% 
  mutate(n_prom_hog_upm = as.numeric(n_prom_hog_upm))


#-------------------------------------------------------------------------------
# Numero de hogares en la encuesta por dominio
#-------------------------------------------------------------------------------

hogares_dom <- base %>% 
  filter(!is.na(domtotal)) %>% 
  group_by(dom) %>% 
  summarise(n_hog_dom = n_distinct(id_hogar))

hogares_dom <- rbind(hogares_dom, c("010101",sum(hogares_dom$n_hog_dom)))

#-------------------------------------------------------------------------------
# MMM: Numero de UPMS en la poblacion ------------------------------------------
# LEctura del marco actual
#-------------------------------------------------------------------------------

marco <- import("insumos/01_estimaciones/20211025_marco_upm.rds") %>% 
  mutate(cod_6 = substr(id_conglomerado,1,6))

upm_prov <- marco %>%  
  filter(!cod_6 %in% v_ciudades_auto) %>% 
  group_by(dom) %>% summarise(n_upm_pobl = n())

upm_ciudades_auto <- marco %>% 
  filter(cod_6 %in% v_ciudades_auto) %>% 
  group_by(cod_6) %>% 
  summarise(n_upm_pobl = n()) %>% rename("dom" = cod_6)

upm_dom <- rbind(upm_prov,upm_ciudades_auto)

upm_dom <- rbind(upm_dom, c("010101",sum(upm_dom$n_upm_pobl)))


#-------------------------------------------------------------------------------
# Calculo N-f_exp
#----------------------------------------------------------------------------

N_pobl <- base %>% group_by(dom) %>% summarise(N = sum(fexp))

N_pobl <- rbind(N_pobl, c("010101",sum(N_pobl$N))) %>% 
  mutate(N = as.numeric(N))

#-------------------------------------------------------------------------------
#Juntando todos los resultados en ind
#----------------------------------------------------------------------------

ind <- ind %>% 
  left_join(correspondencia_dom) %>% 
  left_join(prom_hogares_upm) %>% 
  left_join(hogares_dom) %>% 
  left_join(N_pobl) %>% 
  left_join(DOMTOTAL_nacional) %>% 
  left_join(upm_dom)

#-------------------------------------------------------------------------------
#------------------ CREANDO VARIABLES FALTANTES ----------------------------
#----------------------------------------------------------------------------

ind$mer <-  (ind$tiempo_medio_se/ind$tiempo_medio)*1.96
ind$rho <- (ind$tiempo_medio_deff-1)/(ind$n_prom_hog_upm-1)
ind$conf <- 0.95
ind$tnr <- 0.2
ind$b <- 1
ind$upm_pobl <- ind$n_upm_pobl
ind$upm_efect <- ind$n_hog_dom
ind$prom_hogares_upm <- ind$n_prom_hog_upm

base_insumos <-ind

N = base_insumos$N
delta = base_insumos$mer
rho = base_insumos$rho
conf = base_insumos$conf
mu = base_insumos$tiempo_medio
sigma = base_insumos$tiempo_medio_var_2
M = as.numeric(base_insumos$n_upm_pobl)
m = c(11:11)


for (i in 1:dim(base_insumos)[1]){
  n <- ss4HHSm(N[[i]], M[[i]], rho[[i]], mu[[i]], sigma[[i]], delta[i], conf[[i]], m) 
  n$HouseholdsInSample <- ceiling(n$HouseholdsInSample*(1/(1-0.2)))
  n$PSUinSample <-  ceiling(n$HouseholdsInSample/11)
  n$nombre_dom <- base_insumos$nombre_dom[i]
  if (i ==1){
    muestra1 <- n
  }else{
    muestra1 <- rbind(muestra1,n)
  }
}


N_1 = 774770
M_1 = 2643
rho_1 = 0.1572909
mu_1 = 22.93438

sigma_1 = 24.46
#sigma_1 = 113.14790949778

delta_1 = (0.2384844/22.93438) * 1.96
#delta_1 = 0.059725

conf_1 = 0.95
m_1 = 11  

ss4HHSm(N_1, m_1, rho_1, mu_1, sigma_1, delta_1, conf_1, m_1) 

N_2 = 194352.677241704
M_2 = 2629
rho_2 = 0.090112563
mu_2 = 158.520311819301
sigma_2 = 113.14790949778
delta_2 = 0.059725
conf_2 = 0.95
m_2 = 11  

ss4HHSm(N_2, m_2, rho_2, mu_2, sigma_2, delta_2, conf_2, m_2) 












