
# m = 12
# tnr = 0.20
# delta = 0.060

rm(list = ls())

source("rutinas/02_tamanio/01_tamanio.R")

#domtotal_12 <- tam_eut_2025(m = 12, tnr = 0.20, delta = 0.05)
#domtotal_10 <- tam_eut_2025(m = 10, tnr = 0.20, delta = 0.05)
domtotal_09 <- tam_eut_2025(m =  9, tnr = 0.20, delta = 0.055)

export(domtotal_09 %>% select(dom, dominio, n_upm_muestra, n_viv_muestra), "resultado_DOMTOTAL_todo_0055PORC_002.xlsx")

#-------------------------------------------------------------------------------
# EXPORTANDO
#-------------------------------------------------------------------------------

ruta <- "productos/01_tamanio/03_escenarios_sin_galápagos_envio_02"

#export(tamanio_muestra, paste0(ruta, "tamanio_muestra.xlsx"))

wb <- createWorkbook("Tamaños_EUT")
addWorksheet(wb, "m = 12")
addWorksheet(wb, "m = 10")
addWorksheet(wb, "m = 8")

writeData(wb, sheet = "m = 12", domtotal_12)
writeData(wb, sheet = "m = 10", domtotal_10)
writeData(wb, sheet = "m = 8", domtotal_08)

saveWorkbook(wb, paste0(ruta,"/resultado_DOMTOTAL_6PORC.xlsx"), overwrite = T)
