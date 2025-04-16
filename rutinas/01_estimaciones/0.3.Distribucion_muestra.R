rm(list = ls())

#-------------------------------------------------------------------------------
# Función para realizar la distribución por dominio ----------------------------
#-------------------------------------------------------------------------------

distribucion_prov_ciu <- function(base,muestra,prov_var)
{
  
  # buscando el tamaño de la muestra para la ciudad autorepresentada
  
  n_ciu <- 0
  aux <- viviendas_por_estrato %>%  mutate(id_aux = substr(estrato,1,2)) %>% 
    filter(id_dom==prov_var)
  c_aux <- unique(aux$id_aux)
  entrada_ciu <- c_aux[c_aux!=prov_var]
  if( length(entrada_ciu) != 0 )
  {
    aux_2 <- muestra %>% filter(id_dom==entrada_ciu) %>% select(PSUinSample)
    n_ciu <- aux_2$PSUinSample
    n_ciu <- if_else(is.na(n_ciu),0,n_ciu)
  }
  
  # buscando el tamaño de la muestra para la provincia
  
  aux_3 <- muestra %>% filter(id_dom==prov_var) %>% select(PSUinSample)
  n_prov <- aux_3$PSUinSample
  n_prov <- if_else(is.na(n_prov),0,n_prov)
  
  # Elaboracion algoritmo distribucion 
  
  paso_1 <- base %>% filter(id_dom==prov_var) %>% 
    mutate(proporcion_prov = num_viviendas/sum(num_viviendas),
           id_aux = substr(estrato,1,2))
  
  paso_2 <- paso_1 %>% filter(id_dom == prov_var & 
                                id_dom != id_aux) %>% 
    mutate(proporcion_ciu = num_viviendas/sum(num_viviendas)) %>% 
    select(estrato,proporcion_ciu) %>% 
    full_join(paso_1,by="estrato") %>% 
    mutate(proporcion_ciu = if_else(is.na(proporcion_ciu),0,
                                    proporcion_ciu))
  # todas las proporciones a nivel de la povincia
  paso_3 <- paso_2 %>% mutate(asignacion_prov = ceiling(n_prov*proporcion_prov),
                              asignacion_prov = if_else(asignacion_prov==1,2,asignacion_prov),
                              asignacion_ciu = ceiling(n_ciu*proporcion_ciu),
                              asignacion_ciu = if_else(asignacion_ciu==1,2,asignacion_ciu),
                              asignacion_final = if_else(asignacion_prov>asignacion_ciu,
                                                         asignacion_prov,asignacion_ciu) )%>% 
    group_by(id_aux) %>% summarise(Tam_final=sum(asignacion_final)) %>% 
    rename("Id_Dom"=id_aux)
  paso_3
}

#-------------------------------------------------------------------------------
# Lectura insumos
#-------------------------------------------------------------------------------

viviendas_por_estrato <- read_excel("PRODUCTOS/DISTRIBUCION/viviendas por estrato.xlsx")
#viviendas_por_estrato <- read_excel("INSUMOS/viviendas por estrato.xlsx")
#muestra <- read_excel("INSUMOS/muestra.xlsx")
muestra <- read_excel("muestra.xlsx")

# -------------------------------------------------------------------------
# CREAMOS EL ID PARA EMPATAR LOS DOMINIOS CON LA BASE DE VIVIENDAS --------
# -------------------------------------------------------------------------

muestra$id_dom <- str_pad(as.character(c(1:33)),width = 2,side = "left", pad = "0")
viviendas_por_estrato <- viviendas_por_estrato %>% rename("id_dom"=provincia)
base <- viviendas_por_estrato

# -------------------------------------------------------------------------
# CORREMOS LA FUNCION PARA LOS DOMINIOS (PROVINCIAS)
# -------------------------------------------------------------------------

provincias_id <- as.character(str_pad(as.character(c(1:24)),width = 2,side = "left", pad = "0"))

r <- NULL
r <- distribucion_prov_ciu(viviendas_por_estrato,muestra,provincias_id[1])

for(i in c(2:24)){
  a <- distribucion_prov_ciu(viviendas_por_estrato,muestra,provincias_id[i])
  r = rbind(r,a) %>% arrange(Id_Dom)
  
}

# -------------------------------------------------------------------------
# Nombres de los dom y ciudades auto
# -------------------------------------------------------------------------

r$nombre_dom <-  c("Azuay",
                "Bolívar",
                "Cañar",
                "Carchi",
                "Cotopaxi",
                "Chimborazo",
                "El Oro",
                "Esmeraldas",
                "Guayas",
                "Imbabura",
                "Loja",
                "Los Rios",
                "Manabí",
                "Morona Santiago",
                "Napo",
                "Pastaza",
                "Pichincha",
                "Tungurahua",
                "Zamora Chinchipe",
                "Galápagos",
                "Sucumbíos",
                "Orellana",
                "Santo Domingo de los Tsachilas",
                "Santa Elena",
                "Quito",
                "Guayaquil",
                "Cuenca",
                "Machala",
                "Ambato",
                "Esmeraldas Ciudad",
                "Loja Ciudad",
                "Manta",
                "Santo Domingo"
                )
r <- r %>% select(Id_Dom, nombre_dom, Tam_final)
# Galápagos predefinida operativamente en 104 UPM´s
r$Tam_final[20] <- 104
r <- r %>% adorn_totals()
export(r,"muestra_final.xlsx")
