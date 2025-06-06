library(haven)
library(tidyverse)

library(stringr) # Para concatenar cadenas, str_c str_sub
library(tidyverse) # Procesamiento y manipulación de datos filter, select, mutate etc
library(rio) # Importación y exporación de datos
library(haven) # Importación y exporación de datos
library(survey) # Análisis de encuestas complejas
library(srvyr) # Funciones survey a la tidyverse
library(sjmisc) # Utilidades para encuestas con etiquetas# frequencias
library(sjPlot) # Gráficos y tablas "bonitos" con etiquetas
library(broom) # Convierte en tibbles los resultados de modelos
library(sjlabelled) # Etiquetas 
library(readxl)
library(openxlsx)



datos <- read_sav("Base EUT 2012.sav")


names(datos)

# Diseño de la Encuesta
d1 <- datos %>%
  as_survey_design(
    ids = id_upm,
    #strata = estrato,
    weights = fexp,
    nest = T
  )
options(survey.lonely.psu = "certainty")



resultado_DOMTOTAL_nacional <- d1 %>%  
  filter( ocupado == 1 & P03>=12 & !is.na(DOMTOTAL) &  TRESUL == 2) %>%
  mutate( t = 1)%>% 
  summarise(  
    tiempo_medio = survey_mean(DOMTOTAL, vartype = c("se", "ci", "cv"),na.rm = TRUE), 
    n_ponderado= survey_total(DOMTOTAL, na.rm = T),  
    d_ponderado = survey_total(t, na.rm  = T),
    n_muestral= sum(DOMTOTAL, na.rm = T),  
    d_muestral = sum(t, na.rm  = T)
  )


resultado_DOMTOTAL_dominio <- d1 %>% 
  group_by(dominio) %>% 
  filter( ocupado == 1 & P03>=12 & !is.na(DOMTOTAL) &  TRESUL == 2) %>%
  mutate( t = 1)%>% 
  summarise(  
    tiempo_medio = survey_mean(DOMTOTAL, vartype = c("se", "ci", "cv"),na.rm = TRUE), 
    n_ponderado= survey_total(DOMTOTAL, na.rm = T),  
    d_ponderado = survey_total(t, na.rm  = T),
    n_muestral= sum(DOMTOTAL, na.rm = T),  
    d_muestral = sum(t, na.rm  = T)
  )


resultado_DOMTOTAL <- bind_rows(resultado_DOMTOTAL_nacional, resultado_DOMTOTAL_dominio) %>% 
  select(dominio, everything())


resultado_REMUNTOT_nacional <- d1 %>%  
  filter(ocupado == 1 & P03>=12 & !is.na(REMUNTOT) &  TRESUL == 2 ) %>%
  mutate( t = 1)%>% 
  summarise(  
    tiempo_medio = survey_mean(REMUNTOT, vartype = c("se", "ci", "cv"),na.rm = TRUE), 
    n_ponderado= survey_total(REMUNTOT, na.rm = T),  
    d_ponderado = survey_total(t, na.rm  = T),
    n_muestral= sum(REMUNTOT, na.rm = T),  
    d_muestral = sum(t, na.rm  = T)
  )


resultado_REMUNTOT_dominio <- d1 %>%  
  group_by(dominio) %>% 
  filter(ocupado == 1 & P03>=12 & !is.na(REMUNTOT) &  TRESUL == 2 ) %>%
  mutate( t = 1)%>% 
  summarise(  
    tiempo_medio = survey_mean(REMUNTOT, vartype = c("se", "ci", "cv"),na.rm = TRUE), 
    n_ponderado= survey_total(REMUNTOT, na.rm = T),  
    d_ponderado = survey_total(t, na.rm  = T),
    n_muestral= sum(REMUNTOT, na.rm = T),  
    d_muestral = sum(t, na.rm  = T)
  )

resultado_REMUNTOT <- bind_rows(resultado_REMUNTOT_nacional, resultado_REMUNTOT_dominio) %>% 
  select(dominio, everything())





resultado_TTT_nacional <- d1 %>%  
  filter(ocupado == 1 & P03>=12 & !is.na(TTT) &  TRESUL == 2) %>%
  mutate( t = 1)%>% 
  summarise(  
    tiempo_medio = survey_mean(TTT, vartype = c("se", "ci", "cv"),na.rm = TRUE), 
    n_ponderado= survey_total(TTT, na.rm = T),  
    d_ponderado = survey_total(t, na.rm  = T),
    n_muestral= sum(TTT, na.rm = T),  
    d_muestral = sum(t, na.rm  = T)
  )

resultado_TTT_dominio <- d1 %>%  
  group_by(dominio) %>% 
  filter(ocupado == 1 & P03>=12 & !is.na(TTT) &  TRESUL == 2) %>%
  mutate( t = 1)%>% 
  summarise(  
    tiempo_medio = survey_mean(TTT, vartype = c("se", "ci", "cv"),na.rm = TRUE), 
    n_ponderado= survey_total(TTT, na.rm = T),  
    d_ponderado = survey_total(t, na.rm  = T),
    n_muestral= sum(TTT, na.rm = T),  
    d_muestral = sum(t, na.rm  = T)
  )

resultado_TTT <- bind_rows(resultado_TTT_nacional, resultado_TTT_dominio) %>% 
  select(dominio, everything())


rio::export(list(resultado_DOMTOTAL = resultado_DOMTOTAL, 
                 resultado_REMUNTOT = resultado_REMUNTOT, 
                 resultado_TTT = resultado_TTT), "Reporte.xlsx")

            