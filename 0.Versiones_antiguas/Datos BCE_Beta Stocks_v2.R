#Cargar librerías necesarias
library(tidyverse)
library(lubridate)
library(writexl)
library(ecb)

# Definir parámetros
paises_comparacion <- c("U2","FR","DE","IT","ES")
#1.Obtener datos volumenes y tipos depósitos para última fecha HOGARES  -----
# Función para obtener y filtrar datos para un código específico
get_deposit_data <- function(codigo) {
     ecb::get_data(codigo) %>%
          mutate(fecha = as.Date(paste0(obstime, "-01"))) %>%
          arrange(desc(fecha)) %>%
          slice(1)
}

# Inicializar una lista para almacenar los resultados
 resultados_hh <- list()
 
# Obtener datos para cada país y cada tipo de depósito
for (pais in paises_comparacion) {
     HH_Vista_data <- get_deposit_data(paste0("BSI.M.", pais, ".N.A.L21.A.1.U2.2250.Z01.E"))
     HH_Plazo_data <- get_deposit_data(paste0("BSI.M.", pais, ".N.A.L22.A.1.U2.2250.Z01.E"))
     HH_Preaviso_data <- get_deposit_data(paste0("BSI.M.", pais, ".N.A.L23.A.1.U2.2250.Z01.E"))
     
     Tipo_HH_Vista_data <- get_deposit_data(paste0("MIR.M.", pais, ".B.L21.A.R.A.2250.EUR.N"))
     Tipo_HH_Plazo_data <- get_deposit_data(paste0("MIR.M.", pais, ".B.L22.A.R.A.2250.EUR.O")) #Outstanding, no New
     Tipo_HH_Preaviso_data <- get_deposit_data(paste0("MIR.M.", pais, ".B.L23.A.R.A.2250.EUR.N"))
     
     # Almacenar resultados en la lista
     resultados_hh[[pais]] <- data.frame(  #resultados_hh[[pais]] está creando una entrada en la lista, indexando con el nombre del pais
          pais = pais,                     #data.frame crea un nuevo df con las columnas y datos especificados, nombre_columna=valor
          fecha = HH_Vista_data$fecha,
          HH_Vista = HH_Vista_data$obsvalue,
          HH_Plazo = HH_Plazo_data$obsvalue,
          HH_Preaviso = HH_Preaviso_data$obsvalue,
          Tipo_HH_Vista = Tipo_HH_Vista_data$obsvalue,
          Tipo_HH_Plazo = Tipo_HH_Plazo_data$obsvalue,
          Tipo_HH_Preaviso = Tipo_HH_Preaviso_data$obsvalue
     )
}

# Combinar todos los data frames en un solo data frame
beta_stock_hh_df <- do.call(rbind, resultados_hh) #do.call toma la función rbind y la lista resultados. Es como si estuvieras llamando a rbind con todos los data frames de la lista como argumentos

#2.Obtener datos volumenes y tipos depósitos para última fecha NFCs -----

resultados_nfc <- list()

for (pais in paises_comparacion) {
     NFC_Vista_data <- get_deposit_data(paste0("BSI.M.", pais, ".N.A.L21.A.1.U2.2240.Z01.E"))
     NFC_Plazo_data <- get_deposit_data(paste0("BSI.M.", pais, ".N.A.L22.A.1.U2.2240.Z01.E"))
     NFC_Preaviso_data <- get_deposit_data(paste0("BSI.M.", pais, ".N.A.L23.A.1.U2.2240.Z01.E"))
     
     Tipo_NFC_Vista_data <- get_deposit_data(paste0("MIR.M.", pais, ".B.L21.A.R.A.2240.EUR.N"))
     Tipo_NFC_Plazo_data <- get_deposit_data(paste0("MIR.M.", pais, ".B.L22.A.R.A.2240.EUR.O")) #Outstanding, no New

     # Almacenar resultados en la lista
     resultados_nfc[[pais]] <- data.frame(
          pais = pais,
          fecha = NFC_Vista_data$fecha,
          NFC_Vista = NFC_Vista_data$obsvalue,
          NFC_Plazo = NFC_Plazo_data$obsvalue,
          NFC_Preaviso = NFC_Preaviso_data$obsvalue,
          Tipo_NFC_Vista = Tipo_NFC_Vista_data$obsvalue,
          Tipo_NFC_Plazo = Tipo_NFC_Plazo_data$obsvalue
     )
}

beta_stock_nfc_df <- do.call(rbind, resultados_nfc)

#3. #Calcular tipos medios ponderados----
# Replace missing values with zeros using replace_na 
beta_stock_hh_df <- beta_stock_hh_df %>% 
     mutate(across(everything(), ~replace_na(., 0)))

beta_stock_nfc_df <- beta_stock_nfc_df %>% 
     mutate(across(everything(), ~replace_na(., 0)))

#Calculo tipos medios ponderados
beta_stock_hh_df <- beta_stock_hh_df %>% 
     mutate(tipo_medio_ponderado_hh = (HH_Vista * Tipo_HH_Vista + HH_Plazo * Tipo_HH_Plazo + HH_Preaviso * Tipo_HH_Preaviso) /
                 (HH_Vista + HH_Plazo + HH_Preaviso))

beta_stock_nfc_df <- beta_stock_nfc_df %>%
     mutate(tipo_medio_ponderado_nfc = (NFC_Vista * Tipo_NFC_Vista + NFC_Plazo * Tipo_NFC_Plazo) /
                 (NFC_Vista + NFC_Plazo))

beta_stock_join_df <- full_join(beta_stock_hh_df, beta_stock_nfc_df, by = c("pais", "fecha"))


#Exportar a XLSX----

write_xlsx(list(beta_stock=beta_stock_join_df), #primero nombre de la hoja luego = df
          path = "beta_stocks_depositos_BCE.xlsx")