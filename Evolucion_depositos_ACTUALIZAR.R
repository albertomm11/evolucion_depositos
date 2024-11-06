
message("Evolucion depositos - Descarga datos a excel")
message("==============================")
message("")

message("Descargando datos...")
source("Datos depositos BdE.R")
source("Datos BCE_Beta Stocks_v2.R")
source("Datos BCE_Liquidez bancos.R")
source("Datos BCE_Beta Nuevos_v2.R")
source("Datos BCE_Tipos.R")

library(openxlsx)

Evolucion_depositos_xlsx <- createWorkbook()

addWorksheet(Evolucion_depositos_xlsx, "Tipos_BCE")
addWorksheet(Evolucion_depositos_xlsx, "Beta_stocks_BCE")
addWorksheet(Evolucion_depositos_xlsx, "Beta_stocks_wide_BCE")
addWorksheet(Evolucion_depositos_xlsx, "Peso_depositos_BCE")
addWorksheet(Evolucion_depositos_xlsx, "Peso_plazo_BCE")
addWorksheet(Evolucion_depositos_xlsx, "Volumen_depositos_BdE")
addWorksheet(Evolucion_depositos_xlsx, "DepositosPIB_BdE")

writeData(Evolucion_depositos_xlsx, sheet = "Tipos_BCE", x=tipos_combinado_df)
writeData(Evolucion_depositos_xlsx, sheet = "Beta_stocks_BCE", x=beta_stocks_depositos_wide_df)
writeData(Evolucion_depositos_xlsx, sheet = "Beta_stocks_wide_BCE", x=beta_stocks_depositos_df)
writeData(Evolucion_depositos_xlsx, sheet = "Peso_depositos_BCE", x=deposits_over_assets_df)
writeData(Evolucion_depositos_xlsx, sheet = "Peso_plazo_BCE", x=term_weights_wide_df)
writeData(Evolucion_depositos_xlsx, sheet = "Volumen_depositos_BdE", x=volumen_depositos_bde_df)
writeData(Evolucion_depositos_xlsx, sheet = "DepositosPIB_BdE", x=depositos_sobre_PIBnominal_df)

saveWorkbook(wb=Evolucion_depositos_xlsx, 
             file="Evolucion_depositos.xlsx", 
             overwrite = TRUE)
