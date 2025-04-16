rm(list = ls())

source("rutinas/99_librerias_funciones/librerias.R")
source("rutinas/99_librerias_funciones/unzip.R")

#-------------------------------------------------------------------------------
# Lectura EUT- -----------------------------------------------------------------
#-------------------------------------------------------------------------------

base <- read_zip("insumos/01_estimaciones", "Base EUT 2012.zip", "Base EUT 2012.sav") %>% 
  clean_names() %>% 
  
#-------------------------------------------------------------------------------
# Identificadores de las ciudades autorepresentadas ----------------------------
#-------------------------------------------------------------------------------

v_ciudades_auto <- c("170150","090150")

#-------------------------------------------------------------------------------
# MMM: Numero de UPMS en la poblacion ------------------------------------------
# LEctura del marco actual
#-------------------------------------------------------------------------------

marco <- import("insumos/01_estimaciones/20211025_marco_upm.rds") %>% 
  mutate(cod_6 = substr(id_conglomerado,1,6))

upm_prov <- marco %>%  
  filter(!cod_6 %in% v_ciudades_auto) %>% 
  group_by(dom) %>% summarise(n=n())

upm_ciudades_auto <- marco %>% 
  filter(cod_6 %in% v_ciudades_auto) %>% 
  group_by(cod_6) %>% 
  summarise(n=n()) %>% rename("dom" = cod_6)

upm_dom <- rbind(upm_prov,upm_ciudades_auto)

#-------------------------------------------------------------------------------
#------------ TOTAL DE UPMS EFECTIVAS POR DOMINIO - ENIGHUR2012 ----------------
#-------------------------------------------------------------------------------

prom_hog_efec_upm <- base %>% 
  filter(!is.na(domtotal)) %>% 
  group_by(dominio,identif_2010) %>% 
  summarise(n=n()) %>% group_by(dominio) %>% 
  summarise(n=mean(n)) %>% rename(dom=dominio) 

prom_hogares_upm$dom <- c("1_p","2_p","3_p","4_p","5_p","6_p","7_p","8_p","9_p","10_p","11_p","12_p",
                 "13_p","14_p","15_p","16_p","17_p","18_p","19_p","20_p","21_p","22_p","23_p","24_p",
                 "1_c","2_c","3_c","4_c","5_c","6_c","7_c","8_c","9_c")


# -----------------------------------------------------------------------------
# Creamos la variiable estrato para poder identificar la ciudad auto, resto urb
# y resto rural
#-------------------------------------------------------------------------------

base_estrato <- base %>% mutate(id_6 = substr(identif_2010,1,6),
                estrato = ifelse(area==1 ,1,0),
                estrato = ifelse(area==2 ,2,estrato),
                estrato = ifelse(id_6 %in% v_ciudades_auto & area == 1,3,estrato),
                estrato_f = paste0(substr(identif_2010,1,2),estrato)
                )

#-------------------------------------------------------------------------------
#PLAN DISEÑO: Calculo de las estimaciones --------------------------------------
#-------------------------------------------------------------------------------

dis <- base_estrato %>% as_survey_design(ids = identif_2010,
                                         strata = estrato_f,
                                         weights = fexp_cen2010,
                                         nest = T)
options(survey.lonely.psu = "adjust")

# ------------------------------------------------------------------------------
# Enlistamos las variables de diseño posibles ----------------------------------
#-------------------------------------------------------------------------------

lista_variables <- paste0("d",c(1:12))

#Creamos un repositorio en el que exportaremos las estimaciones de la MEDIA

wb <- createWorkbook()
addWorksheet(wb,"d1")
addWorksheet(wb,"d2")
addWorksheet(wb,"d3")
addWorksheet(wb,"d4")
addWorksheet(wb,"d5")
addWorksheet(wb,"d6")
addWorksheet(wb,"d7")
addWorksheet(wb,"d8")
addWorksheet(wb,"d9")
addWorksheet(wb,"d10")
addWorksheet(wb,"d11")
addWorksheet(wb,"d12")

for (i in c(12:1)){
  #-------------------------------------------------------------------------------
  #---------------------- MEDIA DE LA VARIABLE -----------------------------
  #----------------------------------------------------------------------------
  ind_prov_mean <- dis %>%  group_by(provincia) %>% 
    summarise(d1 = survey_mean(.data[[lista_variables[i]]], 
                                          vartype=c("se","ci","cv","var"),
                               na.rm = T,deff = T),n=n(),N=sum(fexp_cen2010)) %>% 
    rename("dominio"=provincia) %>% mutate(dominio=paste0(dominio,"_p"))
  
  ind_ciudades_mean <- dis %>%  group_by(ciudad_auto) %>% 
    summarise(d1 = survey_mean(.data[[lista_variables[i]]], 
                               vartype=c("se","ci","cv","var"),
                               na.rm = T,deff = T),n=n(),N=sum(fexp_cen2010)) %>% 
    rename("dominio"=ciudad_auto) %>% na.omit() %>% 
    mutate(dominio=paste0(dominio,"_c"))
  
  ind_mean <- rbind(ind_prov_mean,ind_ciudades_mean)
  ind_mean <- na.omit(ind_mean)
  
  #-------------------------------------------------------------------------------
  #---------------------- DESVIACION DE LA VARIABLE -----------------------------
  #----------------------------------------------------------------------------
  ind_prov_sd <- dis %>%  group_by(provincia) %>% 
    summarise(d1 = survey_sd(.data[[lista_variables[i]]], 
                               vartype=c("se","ci","cv","var"),
                               na.rm = T,deff = T),n=n(),N=sum(fexp_cen2010)) %>% 
    rename("dominio"=provincia) %>% mutate(dominio=paste0(dominio,"_p"))
  
  ind_ciudades_sd <- dis %>%  group_by(ciudad_auto) %>% 
    summarise(d1 = survey_sd(.data[[lista_variables[i]]], 
                               vartype=c("se","ci","cv","var"),
                               na.rm = T,deff = T),n=n(),N=sum(fexp_cen2010)) %>% 
    rename("dominio"=ciudad_auto) %>% na.omit() %>% 
    mutate(dominio=paste0(dominio,"_c"))
  
  ind_sd <- rbind(ind_prov_sd,ind_ciudades_sd)
  ind_sd <- na.omit(ind_sd)
  
  #-----------------------------------------------------------------------------
  #---------------------- CONSOLIDANDO ESTIMADORES -----------------------------
  #-----------------------------------------------------------------------------
  
  ind <- ind_mean %>% left_join(select(ind_sd,dominio,"sd"=d1),by="dominio")
  ind <- na.omit(ind)
  
  #-------------------------------------------------------------------------------
  #------------------ CREANDO VARIABLES FALTANTES ----------------------------
  #----------------------------------------------------------------------------
  
  ind$mer <-  (ind$d1_se/ind$d1)*1.96
  ind$rho <- (ind$d1_deff-1)/(prom_hogares_upm$n-1)
  ind$conf <- 0.95
  ind$tnr <- 0.2
  ind$b <- 1
  ind$upm_pobl <- upm_dom$n
  ind$upm_efect <- ind$n
  ind$prom_hogares_upm <- prom_hogares_upm$n
  
  #-----------------------------------------------------------------------------
  #------- SELECCION DE VARIABLES PARA LA COSNTRUCCION DEL TAM -----------------
  #-----------------------------------------------------------------------------
  
  ind <- ind %>% select(dominio,d1,sd,d1_se,mer,d1_deff,N,upm_pobl,upm_efect,
                        prom_hogares_upm,b,tnr,conf,rho)
  
  ind$nombre_dom <- nombres_dom <- c("Azuay","Bolívar","Cañar","Carchi","Cotopaxi",
                                     "Chimborazo","El Oro","Esmeraldas","Guayas",
                                     "Imbabura","Loja","Los Rios","Manabí","Morona Santiago",
                                     "Napo","Pastaza","Pichincha","Tungurahua",
                                     "Zamora Chinchipe","Galápagos","Sucumbíos",
                                     "Orellana","Santo Domingo de los Tsachilas",
                                     "Santa Elena","Quito","Guayaquil","Cuenca",
                                     "Machala","Ambato","Esmeraldas Ciudad",
                                     "Santo Domingo","Manta","Loja Ciudad")
  
  #-----------------------------------------------------------------------------
  #---------------------- EXPORTANDO LIBRO -------------------------------------
  #-----------------------------------------------------------------------------
   writeData(wb,sheet = lista_variables[i],ind)
}

saveWorkbook(wb, "ESTIMACIONES_12_VARIABLES.xlsx")
