
domtotal_12 <- tam_eut_2025(m = 12, tnr = 0.20, delta = 0.060)
domtotal_10 <- tam_eut_2025(m = 10, tnr = 0.20, delta = 0.060)
domtotal_08 <- tam_eut_2025(m =  8, tnr = 0.20, delta = 0.060)

#-------------------------------------------------------------------------------
# EXPORTANDO
#-------------------------------------------------------------------------------

ruta <- "productos/01_tamanio/01_escenarios"

#export(tamanio_muestra, paste0(ruta, "tamanio_muestra.xlsx"))

wb <- createWorkbook("Tamaños_EUT")
addWorksheet(wb, "m = 12")
addWorksheet(wb, "m = 10")
addWorksheet(wb, "m = 8")

writeData(wb, sheet = "m = 12", domtotal_12)
writeData(wb, sheet = "m = 10", domtotal_10)
writeData(wb, sheet = "m = 8", domtotal_08)

saveWorkbook(wb, paste0(ruta,"/resultado_TTT.xlsx"), overwrite = T)





