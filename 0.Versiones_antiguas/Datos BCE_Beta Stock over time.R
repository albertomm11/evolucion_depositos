#Cargar librerias----
library(tidyverse)
library(lubridate)
library(writexl)
library(ecb)

paises_comparacion <- c("U2","ES","DE","IT","FR","NL")
fecha_depositos  <- "2022-08-01"
fecha_referencia_deltas <- "2022-08-01"

#Obtener datos volumenes y tipos depósitos desde fecha_depositos----
# Get data for each type of deposit and bind rows
deposit_balances_df <- map_dfr(paises_comparacion, ~{
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
     ) %>% 
            
    bind_cols(
         map_dfr(paises_comparacion, ~{
         ecb::get_data(paste0("MIR.M.", .x, ".B.L21.A.R.A.2250.EUR.N")) %>% 
         mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha >= fecha_depositos) %>%  
         rename(pais = ref_area) %>% 
         rename(Tipo_HH_Vista = obsvalue) %>% 
         select(Tipo_HH_Vista)
          })
    ) %>% 
    bind_cols(
         map_dfr(paises_comparacion, ~{
         ecb::get_data(paste0("MIR.M.", .x, ".B.L22.A.R.A.2250.EUR.N")) %>% 
         mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha >= fecha_depositos) %>%  
         rename(pais = ref_area) %>% 
         rename(Tipo_HH_Plazo = obsvalue) %>% 
         select(Tipo_HH_Plazo)
         })
      ) %>% 
    bind_cols(
         map_dfr(paises_comparacion, ~{
         ecb::get_data(paste0("MIR.M.", .x, ".B.L23.A.R.A.2250.EUR.N")) %>% 
         mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha >= fecha_depositos) %>%  
         rename(pais = ref_area) %>% 
         rename(Tipo_HH_Preaviso = obsvalue) %>% 
         select(Tipo_HH_Preaviso)
    })
  )  %>% 

#Obtener datos volumenes y tipos depósitos para EMPRESAS---

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
    map_dfr(paises_comparacion, ~{
      ecb::get_data(paste0("MIR.M.", .x, ".B.L21.A.R.A.2240.EUR.N")) %>% 
        mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha >= fecha_depositos) %>%  
        rename(pais = ref_area) %>% 
        rename(Tipo_NFC_Vista = obsvalue) %>% 
        select(Tipo_NFC_Vista)
    })
  ) %>% 
  bind_cols(
    map_dfr(paises_comparacion, ~{
      ecb::get_data(paste0("MIR.M.", .x, ".B.L22.A.R.A.2240.EUR.N")) %>% 
        mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha >= fecha_depositos) %>%  
        rename(pais = ref_area) %>% 
        rename(Tipo_NFC_Plazo = obsvalue) %>% 
        select(Tipo_NFC_Plazo)
    })
  ) 

#Calcular tipos medios ponderados----
# Replace missing values with zeros using replace_na 
deposit_balances_df <- deposit_balances_df %>% 
  mutate(across(everything(), ~replace_na(., 0)))

deposit_balances_df <- deposit_balances_df %>% 
  mutate(across(everything(), ~replace_na(., 0)))

#Calculo tipos medios ponderados
deposit_balances_df <- deposit_balances_df %>% 
  mutate(tipo_medio_ponderado_HH= (HH_Vista * Tipo_HH_Vista + HH_Plazo * Tipo_HH_Plazo + HH_Preaviso * Tipo_HH_Preaviso) /
           (HH_Vista + HH_Plazo + HH_Preaviso)) %>% 
  mutate(tipo_medio_ponderado_NFC= (NFC_Vista * Tipo_NFC_Vista + NFC_Plazo * Tipo_NFC_Plazo) /
           (NFC_Vista + NFC_Plazo))

#Obtener tipo medio BCE----

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

beta_stocks_depositos_df <- full_join(deposit_balances_df, DFR, by="fecha") %>% na.omit()

#Calcular betas y poner en formato wide----
beta_stocks_depositos_df <- beta_stocks_depositos_df %>%
  group_by(pais) %>%
  mutate(delta_bce = `tipo_medio_BCE` - `tipo_medio_BCE`[fecha == fecha_referencia_deltas],
         delta_HH = `tipo_medio_ponderado_HH` - `tipo_medio_ponderado_HH`[fecha == fecha_referencia_deltas],
         delta_NFC = `tipo_medio_ponderado_NFC` - `tipo_medio_ponderado_NFC`[fecha == fecha_referencia_deltas],
         Beta_HH = delta_HH / delta_bce,
         Beta_NFC = delta_NFC / delta_bce) %>%
  ungroup()

beta_stocks_depositos_wide <- beta_stocks_depositos_df %>%
  pivot_wider(id_cols=fecha,
              names_from = pais,
              values_from = c(Beta_HH, Beta_NFC))

#Exportar a XLSX----

write_xlsx(list(beta_stocks_depositos=beta_stocks_depositos_df,
                beta_stocks_depositos_wide=beta_stocks_depositos_wide), 
                 
           path = "beta_stocks_depositos_BCE_overtime.xlsx")


