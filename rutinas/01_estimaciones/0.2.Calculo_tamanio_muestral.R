rm(list = ls())
library(samplesize4surveys)
library(readxl)
library(janitor)
library(haven)
library(rio)

#-------------------------------------------------------------------------------
# LECTURA DE INSUMOS -----------------------------------------------------------
#-------------------------------------------------------------------------------

d1 <- read_excel("PRODUCTOS/NUESTROS/d1_mod.xlsx", sheet = "d1_mod")

#-------------------------------------------------------------------------------
# CALCULO TAMAÑOS --------------------------------------------------------------
#-------------------------------------------------------------------------------

base_insumos <- d1
N=base_insumos$N
M=base_insumos$upm_pobl
rho=base_insumos$rho
mu=base_insumos$d1
sigma=base_insumos$sd
delta=base_insumos$mer
conf = base_insumos$conf
m=c(11:11)

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
muestra1 <- muestra1 %>% relocate(nombre_dom,.before = HouseholdsPerPSU)
muestra1 <- rbind(muestra1[1:30,],muestra1[33,],muestra1[32,],muestra1[31,]) %>% 
  as.data.frame()

export(muestra1, "muestra.xlsx")


