rm(list = ls())
library(samplesize4surveys)
library(openxlsx)
library(readxl)
library(rio)

#-------------------------------------------------------------------------------
# Lectura insumos
#-------------------------------------------------------------------------------
base_final <- read_excel("d1_mod.xlsx", sheet = "d1_mod")
muestra_final <- read_excel("muestra_final.xlsx")
muestra_final <- muestra_final[-dim(muestra_final)[1],]
base_final <- muestra_final %>% select(n_FINAL = Tam_final,nombre_dom) %>% 
  left_join(base_final , by = "nombre_dom")

#-------------------------------------------------------------------------------
# Función e4m para el cálculo de los MER finales
#-------------------------------------------------------------------------------
N= base_final$N
n= base_final$n_FINAL
mu= base_final$d1
sigma= base_final$sd
DEFF= base_final$d1_deff

aux <- e4m (N, n, mu, sigma, DEFF, conf = 0.95, plot = FALSE)
aux_2 <- data.frame( "MER_finales" = aux[[1]]/1.96 )
aux_2$Dominios <- base_final$nombre_dom
aux_2 <- aux_2 %>% select(Dominios,MER_finales)
export(aux_2,"mer_final.xlsx")
