#Cargar librerias----
library(tidyverse)
library(lubridate)
library(writexl)
library(ecb)

datos_path <- "datos/"
excels_path <- "excels_output/"
paises_comparacion <- c("U2","ES","FR","DE","IT", "NL")
fecha_depositos  <- "2005-01-01"


#1. Peso depositos sobre activos bancarios----
# customer_depos_total_liabilities <- map_dfr(paises_comparacion, ~{
#                     ecb::get_data(paste0("CBD2.A.", .x, ".W0.11._Z._Z.A.F.I3013._Z._Z._Z._Z._Z._Z.PC")) }) %>% 
#                     mutate(fecha = obstime) %>%  
#                     rename(pais = ref_area) %>% 
#                     select(fecha, pais, obsvalue) %>% 
#                     pivot_wider(id_cols = c("fecha"),
#                                 names_from = "pais",
#                                 values_from = "obsvalue")

##1.1 Obtener datos volumenes y tipos depósitos para fecha determinada HOGARES----
# Get data for each type of deposit and bind rows
customer_deposits_over_assets_df <- map_dfr(paises_comparacion, ~{
  ecb::get_data(paste0("BSI.M.", .x, ".N.A.L21.A.1.U2.2250.Z01.E")) %>% 
    mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha >= fecha_depositos) %>%  
    rename(pais = ref_area) %>% 
    rename(HH_Vista = obsvalue) %>% 
    select(fecha, pais, HH_Vista)
}) %>% 
  bind_cols(
    map_dfr(paises_comparacion, ~{
      ecb::get_data(paste0("BSI.M.", .x, ".N.A.L22.A.1.U2.2250.Z01.E")) %>% 
        mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha >= fecha_depositos) %>%  
        rename(pais = ref_area) %>% 
        rename(HH_Plazo = obsvalue) %>% 
        select(HH_Plazo)
    }) 
  ) %>% 
  
  bind_cols(
    map_dfr(paises_comparacion, ~{
      ecb::get_data(paste0("BSI.M.", .x, ".N.A.L23.A.1.U2.2250.Z01.E")) %>% 
        mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha >= fecha_depositos) %>%  
        rename(pais = ref_area) %>% 
        rename(HH_Preaviso = obsvalue) %>% 
        select(HH_Preaviso)
    })
  )  %>% 
  # # mutate("Plazo+Preaviso" = Plazo + Preaviso) %>% 
  # # select(-c(Plazo, Preaviso))
  # bind_cols(
  #   map_dfr(paises_comparacion, ~{
  #     ecb::get_data(paste0("MIR.M.", .x, ".B.L21.A.R.A.2250.EUR.N")) %>% 
  #       mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha == fecha_depositos) %>%  
  #       rename(pais = ref_area) %>% 
  #       rename(Tipo_HH_Vista = obsvalue) %>% 
  #       select(Tipo_HH_Vista)
  #   })
  # ) %>% 
  # bind_cols(
  #   map_dfr(paises_comparacion, ~{
  #     ecb::get_data(paste0("MIR.M.", .x, ".B.L22.A.R.A.2250.EUR.N")) %>% 
  #       mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha == fecha_depositos) %>%  
  #       rename(pais = ref_area) %>% 
  #       rename(Tipo_HH_Plazo = obsvalue) %>% 
  #       select(Tipo_HH_Plazo)
  #   })
  # ) %>% 
  # bind_cols(
  #   map_dfr(paises_comparacion, ~{
  #     ecb::get_data(paste0("MIR.M.", .x, ".B.L23.A.R.A.2250.EUR.N")) %>% 
  #       mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha == fecha_depositos) %>%  
  #       rename(pais = ref_area) %>% 
  #       rename(Tipo_HH_Preaviso = obsvalue) %>% 
  #       select(Tipo_HH_Preaviso)
  #   })
  # )

# mutate("Plazo+Preaviso" = Plazo + Preaviso) %>% 
# select(-c(Plazo, Preaviso))

##1.2 Obtener datos volumenes y tipos depósitos para fecha determinada EMPRESAS----

bind_cols(
  map_dfr(paises_comparacion, ~{
  ecb::get_data(paste0("BSI.M.", .x, ".N.A.L21.A.1.U2.2240.Z01.E")) %>% 
    mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha >= fecha_depositos) %>%  
    rename(pais = ref_area) %>% 
    rename(NFC_Vista = obsvalue) %>% 
    select(NFC_Vista)
})) %>% 
  bind_cols(
    map_dfr(paises_comparacion, ~{
      ecb::get_data(paste0("BSI.M.", .x, ".N.A.L22.A.1.U2.2240.Z01.E")) %>% 
        mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha >= fecha_depositos) %>%  
        rename(pais = ref_area) %>% 
        rename(NFC_Plazo = obsvalue) %>% 
        select(NFC_Plazo)
    }) 
  ) %>% 
  
  bind_cols(
    map_dfr(paises_comparacion, ~{
      ecb::get_data(paste0("BSI.M.", .x, ".N.A.L23.A.1.U2.2240.Z01.E")) %>% 
        mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha >= fecha_depositos) %>%  
        rename(pais = ref_area) %>% 
        rename(NFC_Preaviso = obsvalue) %>% 
        select(NFC_Preaviso)
    })
  ) %>% 
  
  bind_cols(
  total_mfi_assets_df <- map_dfr(paises_comparacion, ~{
    ecb::get_data(paste0("BSI.M.", .x, ".N.A.T00.A.1.Z5.0000.Z01.E")) %>% 
      mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha >= fecha_depositos) %>%  
      rename(pais = ref_area) %>% 
      rename(total_mfi_assets = obsvalue) %>% 
      select(total_mfi_assets)
  }))
  # mutate("Plazo+Preaviso" = Plazo + Preaviso) %>% 
  # select(-c(Plazo, Preaviso))
  # bind_cols(
  #   map_dfr(paises_comparacion, ~{
  #     ecb::get_data(paste0("MIR.M.", .x, ".B.L21.A.R.A.2240.EUR.N")) %>% 
  #       mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha == fecha_depositos) %>%  
  #       rename(pais = ref_area) %>% 
  #       rename(Tipo_NFC_Vista = obsvalue) %>% 
  #       select(Tipo_NFC_Vista)
  #   })
  # ) %>% 
  # bind_cols(
  #   map_dfr(paises_comparacion, ~{
  #     ecb::get_data(paste0("MIR.M.", .x, ".B.L22.A.R.A.2240.EUR.N")) %>% 
  #       mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha == fecha_depositos) %>%  
  #       rename(pais = ref_area) %>% 
  #       rename(Tipo_NFC_Plazo = obsvalue) %>% 
  #       select(Tipo_NFC_Plazo)
  #   })
  # ) 


##1.3 Calcular tipos medios ponderados----
# # Replace missing values with zeros using replace_na 
# HH_deposit_balances_df <- HH_deposit_balances_df %>% 
#   mutate(across(everything(), ~replace_na(., 0)))
# 
# NFC_deposit_balances_df <- NFC_deposit_balances_df %>% 
#   mutate(across(everything(), ~replace_na(., 0)))

#Calculo depositos sobre activos totales (MFI)
customer_deposits_over_assets <- customer_deposits_over_assets_df %>% 
  mutate(depos_clientes_totales = (HH_Vista + HH_Plazo + HH_Preaviso + NFC_Vista + NFC_Plazo + NFC_Preaviso)) %>% 
  mutate(customer_deposits_over_assets = depos_clientes_totales/total_mfi_assets) %>%
  select(fecha, pais, customer_deposits_over_assets) %>% 
  pivot_wider(id_cols=fecha,
              names_from=pais,
              values_from=customer_deposits_over_assets)

household_deposits_over_assets <- customer_deposits_over_assets_df %>% 
mutate(household_deposits_over_assets= (HH_Vista + HH_Plazo + HH_Preaviso)/total_mfi_assets) %>% 
  select(fecha, pais, household_deposits_over_assets) %>% 
  pivot_wider(id_cols=fecha,
              names_from=pais,
              values_from=household_deposits_over_assets)

deposits_over_assets_df <- full_join(customer_deposits_over_assets, household_deposits_over_assets, by="fecha") %>% 
     arrange(desc(fecha))

#2. Peso depositos a plazo y preaviso sobre total depositos ----

#Cargar data frames stocks-
datos_betas_df <- readRDS(paste0(datos_path, "datos_beta_stock_depositos.Rds"))

#Nos quedamos con columnas de volumenes de depositos
cols_interes <- c("pais","fecha","HH_Vista","HH_Plazo","HH_Preaviso",
                  "NFC_Vista","NFC_Plazo","NFC_Preaviso")

term_weights_df <- datos_betas_df %>% 
     select(all_of(cols_interes)) %>% 
     mutate(peso_plazo_hh=(HH_Plazo+HH_Preaviso)/(HH_Vista+HH_Plazo+HH_Preaviso),
            peso_plazo_nfc=(NFC_Plazo+NFC_Preaviso)/(NFC_Vista+NFC_Plazo+NFC_Preaviso))

#A formato wide
term_weights_wide_df <- term_weights_df %>%
     pivot_wider(id_cols=fecha, #the identifier column: each row will correspond to a unique 'fecha'
                 names_from = pais, #column names will come from unique values in 'pais'
                 values_from = c(peso_plazo_hh, peso_plazo_nfc)) %>% 
     arrange(desc(fecha))


#Exportar a XLSX----

write_xlsx(list(deposits_over_assets=deposits_over_assets_df),#primero nombre de la hoja luego = df
            path = paste0(excels_path,"customer_deposits_over_assets.xlsx"))

write_xlsx(list(pesos_plazo=term_weights_wide_df),
           path = paste0(excels_path,"pesos_plazo.xlsx"))

