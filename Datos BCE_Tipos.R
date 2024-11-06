# Cargar librerías
library(tidyverse)
library(lubridate)
library(writexl)
library(ecb)
library(purrr)

# Inputs
paises_comparacion <- c("ES","U2")
fecha_corte_series <- as.Date("2022-01-01")
fecha_referencia_deltas <- as.Date("2022-08-01")
datos_path <- "datos/"
excels_path <- "excels_output/"

#1. Tipos nuevos depositos Hogares y SNF (haciendo un loop para lista países) ----
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

#2.Tipos de stock de depósitos

#Cargar data frame de cálculo de beta stocks donde ya están los datos de tipos (así no los descargamos otra vez)
datos_betas_df <- readRDS(paste0(datos_path, "datos_beta_stock_depositos.Rds"))

#Nos quedamos con columnas de tipos de stock (vista y plazo)
cols_interes <- c("pais","fecha","Tipo_HH_Vista","Tipo_HH_Plazo","Tipo_NFC_Vista",
                  "Tipo_NFC_Plazo")

tipos_stock_df <- datos_betas_df %>% 
     select(all_of(cols_interes)) %>% 
     filter(pais=="ES") %>% 
     arrange(desc(fecha)) %>% 
     select(-pais)

#3. Combinarlo todo en un único data frame por fecha----

tipos_combinado_df <- full_join(rates_newdepos_combined_df, tipos_stock_df, by="fecha") 


#Exportar a Excel (diferentes hojas, mismo excel)---------  

write_xlsx(list(tipos_bce=tipos_combinado_df), #primero nombre de la hoja luego = df
           path = paste0(excels_path,"tipos_BCE.xlsx"))


