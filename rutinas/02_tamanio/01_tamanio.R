
tam_eut_2025 <- function(m, tnr, delta){
#rm(list = ls())

#source("rutinas/99_librerias_funciones/librerias.R")
source("rutinas/99_librerias_funciones/unzip.R")

#-------------------------------------------------------------------------------
# Lectura de bases de insumos
#-------------------------------------------------------------------------------

ruta <- "intermedios/01_insumos_tamanio/"

base_insumos <- import(paste0(ruta, "base_insumos.rds"))
#base_insumos <- import(paste0(ruta, "base_insumos_mod.xlsx"))

#-------------------------------------------------------------------------------
# Correspondencia identificadores de dominios
#-------------------------------------------------------------------------------

correspondencia_dom <- import("insumos/01_estimaciones/correspondencia_dom.xlsx") %>% 
  clean_names() 

#-------------------------------------------------------------------------------
# DEfiniendo nombres de parametros para el calculo del tamanio
#-------------------------------------------------------------------------------

#base_insumos <- ind

N = base_insumos$N_pobl
M = base_insumos$upm_pobl_M
rho = base_insumos$rho
mu = base_insumos$estimacion_media_mu
sigma = base_insumos$estimaciion_sd_sigma
#delta = base_insumos$mer_delta
#delta = 0.06
conf = base_insumos$conf
#m = 12
#tnr = 0.2
b = base_insumos$b

n_per <- c()
n_per <- c()
n_viv <- c()
n_upm <- c()
dominio <- c()

for (i in 1:dim(base_insumos)[1]){
  
  n_per[i] <- ss4m(N = N[i], 
                   mu = mu[i], 
                   sigma = sigma[i], 
                   DEFF = 1 + (m * b[i] - 1) * rho[i], 
                   conf = conf[i],  
                   error = "rme", 
                   #delta = delta[i], 
                   delta = delta,
                   plot = FALSE)
  n_viv[i] <- ceiling(n_per[i]/b[i])
  n_viv[i] <- n_viv[i] * 1/(1-tnr)
  n_upm[i] <-  ceiling(n_viv[i]/m)
  dominio[i] <- base_insumos$dominio[i]
}

tamanio_muestra_1 <- data.frame(dominio, n_per, n_viv, n_upm) %>% 
  left_join(correspondencia_dom) %>% 
  relocate(dom)

#-------------------------------------------------------------------------------
# Distribuxion UIO y GYE
#-------------------------------------------------------------------------------

source("rutinas/02_tamanio/02_viv_estrato.R")

tamanio_muestra_2 <- viv_estrato_eut %>% 
  left_join(select( tamanio_muestra_1, dom, n_upm ), by = "dom") %>% 
  mutate(tam_1 = ceiling(n_upm * prop), 
         tam_1 = ifelse(tam_1 < 3, 3, tam_1)) %>% 
  group_by(estrato_eut) %>% 
  mutate(maxi = ifelse(max(tam_1) == tam_1, 1, 0), 
         tam_2 = max(tam_1)) %>% 
  ungroup() %>% 
  filter(!(dom %in% c("17", "09") & 
             !substr(estrato_eut,1 ,2) %in% c("17", "09")))

tamanio_muestra_3 <- tamanio_muestra_2 %>% 
  group_by(dom) %>% 
  summarise(n_upm_muestra = sum(tam_2)) 

#-------------------------------------------------------------------------------
# Comparando con el tamaño del 2012
#-------------------------------------------------------------------------------

correspondencia_dom <- import("insumos/01_estimaciones/correspondencia_dom.xlsx") %>% 
  clean_names() 

eut_tamanio_2012_upm <- import("insumos/02_tamanio_2012/eut_tamanio_2012_upm.xlsx") %>% 
  clean_names() %>% 
  left_join(correspondencia_dom, by = "dominio") %>% 
  relocate(dom) %>% 
  arrange(dom)

tamanio_muestra <- tamanio_muestra_3 %>% 
  left_join(correspondencia_dom) %>% 
  relocate(dominio, .after = "dom") %>% 
  left_join(select(eut_tamanio_2012_upm, dom, total_sectores)) %>% 
  mutate(n_viv_muestra = n_upm_muestra * m, 
         n_viv_sectores = total_sectores * 12, 
    diferencia = n_viv_muestra - n_viv_sectores) %>% 
  #filter(substr(dom,1,2) %in% c("17", "09")) %>% 
  adorn_totals() 

return(tamanio_muestra)
}


