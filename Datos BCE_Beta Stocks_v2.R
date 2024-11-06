# Cargar librerías necesarias
library(tidyverse)
library(lubridate)
library(writexl)
library(ecb)

# Definir parámetros
datos_path <- "datos/"
excels_path <- "excels_output/"
paises_comparacion <- c("U2","DE","FR","NL","IT","ES")
fecha_depositos  <- "2022-08-01"
fecha_referencia_deltas <- "2022-08-01"
fecha_max <- ecb::get_data("MIR.M.ES.B.L21.A.R.A.2250.EUR.N") %>% mutate(fecha = as.Date(paste0(obstime, "-01")))%>% tail(1) %>% pull(fecha)  

#1.Obtener datos volumenes y tipos depósitos para última fecha HOGARES  -----
# Función para obtener y filtrar datos para un código específico
get_deposit_data <- function(codigo) {
     ecb::get_data(codigo) %>%
          mutate(fecha = as.Date(paste0(obstime, "-01"))) %>%
          filter(fecha >= fecha_depositos & fecha <= fecha_max)  
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
# beta_stock_hh_df <- bind_rows(lapply(resultados_hh, as.data.frame))  #esto es más robusto que do.call y rbind, #bind_rows can handle different data structures and column names, making it more versatile

beta_stock_hh_df <- bind_rows(resultados_hh)


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

beta_stock_nfc_df <- bind_rows(resultados_nfc)
     
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

#4.Calculo betas----
#Obtener tipo medio BCE---

DFR <- ecb::get_data("FM.D.U2.EUR.4F.KR.DFR.LEV") %>% 
     mutate(fecha = as.Date(obstime)) %>%  #formatea obstime como fecha
     filter(fecha >= fecha_depositos) %>%  #filtra desde fecha corte para reducir carga posterior
     mutate(mes =  lubridate::floor_date(as.Date(fecha), "month")) %>% #crea nueva columna mes y lubridate redondea a la baja la fecha (alinea el mes)
     group_by(mes) %>% #agrupa por mes y filtra por máxima fecha del mes (último día)
     summarise(tipo_medio_BCE = mean(obsvalue)) %>%
     mutate(nombres = "Tipo de interés BCE",
            fecha = mes) %>% #crea columna nombres
     ungroup() %>%
     select(fecha,tipo_medio_BCE)

datos_stocks_depositos_df <- full_join(beta_stock_hh_df, beta_stock_nfc_df, by=c("fecha","pais")) %>% full_join(DFR, by="fecha") %>% na.omit() #na.omit porque DFR va con 2 meses de adelanto

#Calcular betas y poner en formato wide----
beta_stocks_depositos_df <- datos_stocks_depositos_df %>%
     group_by(pais) %>%
     mutate(delta_bce = `tipo_medio_BCE` - `tipo_medio_BCE`[fecha == fecha_referencia_deltas],
            delta_hh = `tipo_medio_ponderado_hh` - `tipo_medio_ponderado_hh`[fecha == fecha_referencia_deltas],
            delta_nfc = `tipo_medio_ponderado_nfc` - `tipo_medio_ponderado_nfc`[fecha == fecha_referencia_deltas],
            Beta_hh = delta_hh / delta_bce,
            Beta_nfc = delta_nfc / delta_bce) %>%
     ungroup()

beta_stocks_depositos_wide_df <- beta_stocks_depositos_df %>%
     pivot_wider(id_cols=fecha,
                 names_from = pais,
                 values_from = c(Beta_hh, Beta_nfc)) %>% 
     filter(fecha>fecha_referencia_deltas) %>% 
     arrange(desc(fecha))

write_xlsx(list(beta_stocks_wide=beta_stocks_depositos_wide_df,
                beta_stocks_depositos=beta_stocks_depositos_df),
           path = paste0(excels_path,"beta_stocks_depositos_BCE_overtime.xlsx"))

#guardar df para otros análisis
saveRDS(beta_stocks_depositos_df,paste0(datos_path, "datos_beta_stock_depositos.Rds")) 


