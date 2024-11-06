#Cargar librerías necesarias
library(tidyverse)
library(lubridate)
library(writexl)
library(ecb)
library(scales)
library(ggrepel)
library(plotly)
library(ggplot2)
library(tesorotools)

datos_path <- "datos/"

#Cargar data frames ----
datos_betas_df <- readRDS(paste0(datos_path, "datos_beta_stock_depositos.Rds"))

#1.Tipos de interés depósitos----

cols_interes <- c("pais","fecha","HH_Vista","HH_Plazo","HH_Preaviso",
                  "NFC_Vista","NFC_Plazo","NFC_Preaviso")

term_weights_df <- datos_betas_df %>% 
     select(all_of(cols_interes)) %>% 
     mutate(peso_plazo_hh=(HH_Plazo+HH_Preaviso)/(HH_Vista+HH_Plazo+HH_Preaviso),
            peso_plazo_nfc=(NFC_Plazo+NFC_Preaviso)/(NFC_Vista+NFC_Plazo+NFC_Preaviso))

term_weights_wide_df <- term_weights_df %>%
     pivot_wider(id_cols=fecha, #the identifier column: each row will correspond to a unique 'fecha'
                 names_from = pais, #column names will come from unique values in 'pais'
                 values_from = c(peso_plazo_hh, peso_plazo_nfc))


write_xlsx(list(pesos_plazo=term_weights_wide_df),
           path = "pesos_plazo.xlsx")


#2. Estacionalidad volumen depos

#Cargar data frames ----
datos_depos_bde_df <- readRDS(paste0(datos_path, "datos_depositos_bde.Rds"))

var_intermensual_depos_df <- datos_depos_bde_df %>% 
     select(fecha,
            Total_Depos_SNF,
            # Vista_SNF,
            # Plazo_SNF,
            Total_Depos_Hogares) %>%
            # Vista_Hogares,
            # Plazo_Hogares)
     arrange(fecha) %>%
     filter(fecha>="2002-12-01") %>% #lag() gives you the value from one row earlier, entonces hay que ordenar de más antiguo a más nuevo
     mutate(
          mes = month(fecha, label = TRUE),  # Extract month for grouping
          MoM_Change_SNF = (Total_Depos_SNF - lag(Total_Depos_SNF)) / lag(Total_Depos_SNF) * 100, #By default, lag() shifts the data by 1 row
          MoM_Change_Hogares = (Total_Depos_Hogares - lag(Total_Depos_Hogares)) / lag(Total_Depos_Hogares) * 100
     ) %>%
      filter(mes=="jul")

# Group by month and calculate summary statistics
monthly_summary_df <- var_intermensual_depos_df %>%
     group_by(mes) %>%
     summarise(
          Avg_MoM_Change_SNF = mean(MoM_Change_SNF, na.rm = TRUE),
          Avg_MoM_Change_Hogares = mean(MoM_Change_Hogares, na.rm = TRUE),
          Count = n()
     ) %>% 
     filter(mes=="ago")

write_xlsx(list(media_var_mom=monthly_summary_df),
           path = "variacion_mensual_depositos.xlsx")








