# Cargar librerías
library(tidyverse)
library(lubridate)
library(writexl)
library(ecb)
library(purrr)

# Inputs
paises_comparacion <- c("ES","DE","FR","IT","U2")
fecha_corte_series <- as.Date("2022-01-01")
fecha_referencia_deltas <- as.Date("2022-08-01")
datos_path <- "datos/"
excels_path <- "excels_output/"
#1. Rate Deposits from Households y NFC (haciendo un loop para lista países) ----
# Función para obtener y procesar datos de tipos de depósitos

get_deposit_data <- function(pais, tipo) {
     series_code <- if (tipo == "hh") "2250" else "2240"
     ecb::get_data(paste0("MIR.M.", pais, ".B.L22.A.R.A.", series_code, ".EUR.N")) %>%
          mutate(fecha = as.Date(paste0(obstime, "-01")),
                 nombres = paste0("Rates_nuevosdepos_", tipo, "_", pais),
                 valores = obsvalue) %>%
          select(fecha, nombres, valores) %>%
          filter(fecha >= fecha_corte_series)
}
#Obtener datos de DFR
DFR <- ecb::get_data("FM.D.U2.EUR.4F.KR.DFR.LEV") %>%
     mutate(fecha = as.Date(obstime)) %>%
     filter(fecha >= fecha_corte_series) %>%
     mutate(mes = floor_date(fecha, "month")) %>%
     group_by(mes) %>%
     summarise(valores = mean(obsvalue), .groups = 'drop') %>%
     mutate(nombres = "Tipo de interés BCE", fecha = mes) %>%
     select(fecha, nombres, valores)

# Obtener y combinar datos de depósitos de hogares
rates_newdepos_hh <- map_dfr(paises_comparacion, ~get_deposit_data(.x, "hh")) 
# How map_dfr Works 
# Iteration: It iterates over each element in the input list or vector.
# Function Application: For each element, it applies the specified function.
# Combining Results: It combines all the resulting data frames into one by row-binding them.

# Obtener y combinar datos de depósitos de empresas
rates_newdepos_nfc <- map_dfr(paises_comparacion, ~get_deposit_data(.x, "nfc"))
# Combinar todas las series en un único data frame
rates_newdepos_combined_df <- bind_rows(rates_newdepos_hh, rates_newdepos_nfc, DFR) %>%
     pivot_wider(names_from = nombres, values_from = valores) %>%
     arrange(desc(fecha)) %>%
     drop_na()

#2. Transmisión a tipo de interés de los depósitos a hogares -----
betas_hh_list <- list()

transmision_depositos_hh_df <- DFR

for(pais_comparacion in paises_comparacion) {
     var_nombre <- paste0("Tipos nuevos depositos plazo ", pais_comparacion)
     var_nombre_delta <- paste0("delta_interes_", pais_comparacion)
     var_nombre_beta <- paste0("beta_", pais_comparacion)
     
     transmision_depositos_hh_pais_df <- bind_rows(DFR, 
                                                   # descargamos serie tipo de interés depositos país en cuestión
                                                   ecb::get_data(paste0("MIR.M.", pais_comparacion, ".B.L22.A.R.A.2250.EUR.N")) %>% 
                                                        mutate(fecha= as.Date(paste0(obstime, "-01"))) %>% 
                                                        mutate(nombres = paste0("Tipos nuevos depositos plazo ", pais_comparacion)) %>%
                                                        rename(valores = obsvalue) %>%
                                                        select(fecha, nombres, valores)) %>%
          arrange(fecha) %>%
          filter(!is.na(valores))
     
     pais_a_fecha_referencia <- transmision_depositos_hh_pais_df %>% 
          filter(grepl(paste0("Tipos nuevos depositos plazo ", pais_comparacion), nombres), fecha == fecha_referencia_deltas) %>% pull(valores)
     
     transmision_depositos_hh_pais_df <- transmision_depositos_hh_pais_df %>%
          pivot_wider(id_cols = c("fecha"),
                      names_from="nombres",
                      values_from = "valores") %>% 
          filter(fecha >= fecha_corte_series) %>% 
          mutate(delta_bce = `Tipo de interés BCE` - `Tipo de interés BCE`[fecha == fecha_referencia_deltas],
                 !!var_nombre_delta := !!as.name(paste0("Tipos nuevos depositos plazo ", pais_comparacion)) - pais_a_fecha_referencia, #!!var_nombre_delta unquotes the expression stored in the var_nombre_delta variable, resulting in a column name. !!as.name(paste0("Tipos nuevos depositos plazo ", pais_comparacion)) creates the column name by pasting "Tipos nuevos depositos plazo " and the pais_comparacion value.
                 !!paste0("beta_", pais_comparacion,"_hh") := !!as.name(paste0("delta_interes_", pais_comparacion)) / delta_bce) %>%  #usamos := por el contexto (utilzamos paquete data.table en vez de dyplr donde nos valdría con =). Además, R will handle the column names without the need for backticks in this context.
    select(fecha, starts_with("Beta")) %>% 
    filter(fecha > fecha_referencia_deltas) %>% #para quitar el NaN (delta BCE es cero)
    drop_na()      

     betas_hh_list[[pais_comparacion]] <- transmision_depositos_hh_pais_df
}

#3. Transmisión a tipo de interés de los depósitos de empresas ----
betas_nfc_list <- list()

transmision_depositos_nfc_df <- DFR

for(pais_comparacion in paises_comparacion) {
     var_nombre <- paste0("Tipos nuevos depositos plazo ", pais_comparacion)
     var_nombre_delta <- paste0("delta_interes_", pais_comparacion)
     var_nombre_beta <- paste0("beta_", pais_comparacion)
     
     transmision_depositos_nfc_pais_df <- bind_rows(DFR, 
                                                   # descargamos serie tipo de interés depositos país en cuestión
                                                   ecb::get_data(paste0("MIR.M.", pais_comparacion, ".B.L22.A.R.A.2240.EUR.N")) %>% 
                                                        mutate(fecha= as.Date(paste0(obstime, "-01"))) %>% 
                                                        mutate(nombres = paste0("Tipos nuevos depositos plazo ", pais_comparacion)) %>%
                                                        rename(valores = obsvalue) %>%
                                                        select(fecha, nombres, valores)) %>%
          arrange(fecha) %>%
          filter(!is.na(valores))
     
     pais_a_fecha_referencia <- transmision_depositos_nfc_pais_df %>% 
          filter(grepl(paste0("Tipos nuevos depositos plazo ", pais_comparacion), nombres), fecha == fecha_referencia_deltas) %>% pull(valores)
     
     transmision_depositos_nfc_pais_df <- transmision_depositos_nfc_pais_df %>%
          pivot_wider(id_cols = c("fecha"),
                      names_from="nombres",
                      values_from = "valores") %>% 
          filter(fecha >= fecha_corte_series) %>% 
          mutate(delta_bce = `Tipo de interés BCE` - `Tipo de interés BCE`[fecha == fecha_referencia_deltas],
                 !!var_nombre_delta := !!as.name(paste0("Tipos nuevos depositos plazo ", pais_comparacion)) - pais_a_fecha_referencia, #!!var_nombre_delta unquotes the expression stored in the var_nombre_delta variable, resulting in a column name. !!as.name(paste0("Tipos nuevos depositos plazo ", pais_comparacion)) creates the column name by pasting "Tipos nuevos depositos plazo " and the pais_comparacion value.
                 !!paste0("beta ", pais_comparacion,"_nfc") := !!as.name(paste0("delta_interes_", pais_comparacion)) / delta_bce) %>%  #usamos := por el contexto (utilzamos paquete data.table en vez de dyplr donde nos valdría con =). Además, R will handle the column names without the need for backticks in this context.
          select(fecha, starts_with("Beta")) %>%  
          filter(fecha > fecha_referencia_deltas) %>% #para quitar el NaN (delta BCE es cero)
          drop_na() 
     
     betas_nfc_list[[pais_comparacion]] <- transmision_depositos_nfc_pais_df
}

#4. Combinarlo todo en un único data frame por fecha----
betas_hh_df <- reduce(betas_hh_list, full_join, by="fecha") #The reduce function from the purrr package is used to apply a function recursively to a list or vector, reducing it to a single value
betas_nfc_df <- reduce(betas_nfc_list, full_join, by="fecha")

betas_combinado_df <- list(rates_newdepos_combined_df,betas_hh_df,betas_nfc_df) %>% 
                      reduce(full_join, by="fecha") 


#Exportar a Excel (diferentes hojas, mismo excel)---------  

write_xlsx(list(Betas_nuevos=betas_combinado_df), #primero nombre de la hoja luego = df
           path = paste0(excels_path, "datos_nota_depos_BCE.xlsx"))


