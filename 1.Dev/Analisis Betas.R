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




ggplot(depos_hh_rates_df, aes(x = fecha, y = tipo_medio_ponderado_hh, color = pais)) +
     geom_line() +
     tema_gabinete +
     scale_color_manual(values = colores_new[c(1,2,3,4,5,6,7,8)])+
     labs(title = "Average Weighted Interest Rate (Household Deposits)",
          x = "Date", y = "Interest Rate") +
     facet_wrap(~ pais, nrow = 3) +  # Adjust nrow for better visualization
     theme_bw() +
     theme(legend.position = "bottom")



depos_hh_rates_wide_df <- depos_hh_rates_df %>%
     pivot_wider(id_cols=fecha,
                 names_from = pais,
                 values_from = c(tipo_medio_ponderado_hh,tipo_medio_ponderado_nfc))
