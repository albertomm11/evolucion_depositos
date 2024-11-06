library(tidyverse)
library(lubridate)
library(writexl)
library(ecb)

paises_comparacion <- c("U2","FR","DE","IT","ES")
fecha_depositos  <- "2023-08-01"

#Obtener datos volumenes y tipos depósitos para fecha determinada HOGARES----
# Get data for each type of deposit and bind rows
HH_deposit_balances_df <- map_dfr(paises_comparacion, ~{
     ecb::get_data(paste0("BSI.M.", .x, ".N.A.L21.A.1.U2.2250.Z01.E")) %>% 
          mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha == fecha_depositos) %>%  
          rename(pais = ref_area) %>% 
          rename(HH_Vista = obsvalue) %>% 
          select(fecha, pais, HH_Vista)
         }) %>% 
    bind_cols(
          map_dfr(paises_comparacion, ~{
            ecb::get_data(paste0("BSI.M.", .x, ".N.A.L22.A.1.U2.2250.Z01.E")) %>% 
              mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha == fecha_depositos) %>%  
              rename(pais = ref_area) %>% 
              rename(HH_Plazo = obsvalue) %>% 
              select(HH_Plazo)
          }) 
          ) %>% 
      
     bind_cols(
          map_dfr(paises_comparacion, ~{
               ecb::get_data(paste0("BSI.M.", .x, ".N.A.L23.A.1.U2.2250.Z01.E")) %>% 
              mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha == fecha_depositos) %>%  
              rename(pais = ref_area) %>% 
              rename(HH_Preaviso = obsvalue) %>% 
              select(HH_Preaviso)
          })
     ) %>% 
               # mutate("Plazo+Preaviso" = Plazo + Preaviso) %>% 
               # select(-c(Plazo, Preaviso))
    bind_cols(
         map_dfr(paises_comparacion, ~{
         ecb::get_data(paste0("MIR.M.", .x, ".B.L21.A.R.A.2250.EUR.N")) %>% 
         mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha == fecha_depositos) %>%  
         rename(pais = ref_area) %>% 
         rename(Tipo_HH_Vista = obsvalue) %>% 
         select(Tipo_HH_Vista)
          })
    ) %>% 
    bind_cols(
         map_dfr(paises_comparacion, ~{
         ecb::get_data(paste0("MIR.M.", .x, ".B.L22.A.R.A.2250.EUR.N")) %>% 
         mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha == fecha_depositos) %>%  
         rename(pais = ref_area) %>% 
         rename(Tipo_HH_Plazo = obsvalue) %>% 
         select(Tipo_HH_Plazo)
         })
      ) %>% 
    bind_cols(
         map_dfr(paises_comparacion, ~{
         ecb::get_data(paste0("MIR.M.", .x, ".B.L23.A.R.A.2250.EUR.N")) %>% 
         mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha == fecha_depositos) %>%  
         rename(pais = ref_area) %>% 
         rename(Tipo_HH_Preaviso = obsvalue) %>% 
         select(Tipo_HH_Preaviso)
    })
  )

# mutate("Plazo+Preaviso" = Plazo + Preaviso) %>% 
# select(-c(Plazo, Preaviso))

#Obtener datos volumenes y tipos depósitos para fecha determinada EMPRESAS----

NFC_deposit_balances_df <- map_dfr(paises_comparacion, ~{
  ecb::get_data(paste0("BSI.M.", .x, ".N.A.L21.A.1.U2.2240.Z01.E")) %>% 
    mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha == fecha_depositos) %>%  
    rename(pais = ref_area) %>% 
    rename(NFC_Vista = obsvalue) %>% 
    select(fecha, pais, NFC_Vista)
}) %>% 
  bind_cols(
    map_dfr(paises_comparacion, ~{
      ecb::get_data(paste0("BSI.M.", .x, ".N.A.L22.A.1.U2.2240.Z01.E")) %>% 
        mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha == fecha_depositos) %>%  
        rename(pais = ref_area) %>% 
        rename(NFC_Plazo = obsvalue) %>% 
        select(NFC_Plazo)
    }) 
  ) %>% 
  
  bind_cols(
    map_dfr(paises_comparacion, ~{
      ecb::get_data(paste0("BSI.M.", .x, ".N.A.L23.A.1.U2.2240.Z01.E")) %>% 
        mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha == fecha_depositos) %>%  
        rename(pais = ref_area) %>% 
        rename(NFC_Preaviso = obsvalue) %>% 
        select(NFC_Preaviso)
    })
  ) %>% 
  # mutate("Plazo+Preaviso" = Plazo + Preaviso) %>% 
  # select(-c(Plazo, Preaviso))
  bind_cols(
    map_dfr(paises_comparacion, ~{
      ecb::get_data(paste0("MIR.M.", .x, ".B.L21.A.R.A.2240.EUR.N")) %>% 
        mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha == fecha_depositos) %>%  
        rename(pais = ref_area) %>% 
        rename(Tipo_NFC_Vista = obsvalue) %>% 
        select(Tipo_NFC_Vista)
    })
  ) %>% 
  bind_cols(
    map_dfr(paises_comparacion, ~{
      ecb::get_data(paste0("MIR.M.", .x, ".B.L22.A.R.A.2240.EUR.N")) %>% 
        mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha == fecha_depositos) %>%  
        rename(pais = ref_area) %>% 
        rename(Tipo_NFC_Plazo = obsvalue) %>% 
        select(Tipo_NFC_Plazo)
    })
  ) 

#Calcular tipos medios ponderados----
# Replace missing values with zeros using replace_na 
HH_deposit_balances_df <- HH_deposit_balances_df %>% 
  mutate(across(everything(), ~replace_na(., 0)))

NFC_deposit_balances_df <- NFC_deposit_balances_df %>% 
  mutate(across(everything(), ~replace_na(., 0)))

#Calculo tipos medios ponderados
HH_deposit_balances_df <- HH_deposit_balances_df %>% 
  mutate(tipo_medio_ponderado = (HH_Vista * Tipo_HH_Vista + HH_Plazo * Tipo_HH_Plazo + HH_Preaviso * Tipo_HH_Preaviso) /
           (HH_Vista + HH_Plazo + HH_Preaviso))

NFC_deposit_balances_df <- NFC_deposit_balances_df %>%
  mutate(tipo_medio_ponderado = (NFC_Vista * Tipo_NFC_Vista + NFC_Plazo * Tipo_NFC_Plazo) /
           (NFC_Vista + NFC_Plazo))


#Exportar a XLSX----

write_xlsx(list(HH_deposit_balances=HH_deposit_balances_df,  #primero nombre de la hoja luego = df
                NFC_deposit_balances=NFC_deposit_balances_df), 
           path = "beta_stocks_depositos_BCE.xlsx")


