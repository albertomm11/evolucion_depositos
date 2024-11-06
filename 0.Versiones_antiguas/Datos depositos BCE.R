#Cargar librerias----
library(tidyverse)
library(lubridate)
library(writexl)
library(ecb)

paises_comparacion <- c("ES","DE","FR","IT","U2") #inputs
fecha_corte_series  <- "2021-01-01"
fecha_referencia_deltas <- "2022-08-01"

# Bank interest rates (MRI-ECB)- deposits with an agreed maturity (new business) - Monthly ----

## Deposit facility rate---------

DFR <- ecb::get_data("FM.D.U2.EUR.4F.KR.DFR.LEV") %>% 
  mutate(fecha = as.Date(obstime)) %>%  #formatea obstime como fecha
  filter(fecha >= fecha_corte_series) %>%  #filtra desde fecha corte para reducir carga posterior
  mutate(mes =  lubridate::floor_date(as.Date(fecha), "month")) %>% #crea nueva columna mes y lubridate redondea a la baja la fecha (alinea el mes)
  group_by(mes) %>% #agrupa por mes y filtra por máxima fecha del mes (último día)
  summarise(valores = mean(obsvalue)) %>%
  mutate(nombres = "Tipo de interés BCE",
         fecha = mes) %>% #crea columna nombres
  ungroup() %>%
  select(fecha, nombres, valores)

## Deposits from Households ---- haciendo un loop para lista países ---------

Rates_newdepos_HH <- list()

for(pais_comparacion in paises_comparacion) {
  Rates_newdepos_HH_pais <- (ecb::get_data(paste0("MIR.M.", pais_comparacion, ".B.L22.A.R.A.2250.EUR.N")) %>% 
                               mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% 
                               mutate(nombres = paste0("Rates_nuevosdepos_hogares_", pais_comparacion)) %>% #crea columna nombres con nombre de la serie (luego sera encabezado columna con pivot wider)
                               rename(valores = obsvalue) %>%
                               select(fecha, nombres, valores)) %>% 
    filter(fecha >= fecha_corte_series)
  #Add the data frame to the list
  
  Rates_newdepos_HH[[pais_comparacion]] <- Rates_newdepos_HH_pais
  
}  
# Combine all data frames into a single data frame 

Rates_Newtermdepo_HHs <- bind_rows(Rates_newdepos_HH) %>% #añade una debajo de otra en filas
  bind_rows(DFR) %>% 
  pivot_wider(names_from = nombres,             #Para poner en formato excel (wide). Encabezados de nombres (nombre de la serie) y valores de values)
              values_from = valores) %>% 
  arrange(desc(fecha)) %>% 
  drop_na() #solo se queda con filas completas, si alguna columna no tiene valor, se carga la fila entera                                 

## Deposits from NFCs ---- haciendo un loop para lista países ---------

Rates_newdepos_NFC <- list()     #mismo mecanismo: iniciar lista para nuevo grupo de series, hacer loop añadendo a la lista, combinar df de la lista

for(pais_comparacion in paises_comparacion) {
  Rates_newdepos_NFC_pais <- (ecb::get_data(paste0("MIR.M.", pais_comparacion, ".B.L22.A.R.A.2240.EUR.N")) %>% 
                                mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% 
                                mutate(nombres = paste0("Rates_nuevosdepos_empresas_", pais_comparacion)) %>%
                                rename(valores = obsvalue) %>%
                                select(fecha, nombres, valores)) %>% 
    filter(fecha >= fecha_corte_series)
  #Add the data frame to the list
  
  Rates_newdepos_NFC[[pais_comparacion]] <- Rates_newdepos_NFC_pais
  
}  
# Combine all data frames into a single data frame 

Rates_Newtermdepo_NFCs <- bind_rows(Rates_newdepos_NFC) %>% 
  bind_rows(DFR) %>% 
  pivot_wider(names_from = nombres,           #Para poner en formato excel (wide)
              values_from = valores) %>% 
  arrange(desc(fecha)) %>% 
  drop_na()

# Exportar a Excel (diferentes hojas, mismo excel)---------  

write_xlsx(list(Rates_Newtermdepo_HHs=Rates_Newtermdepo_HHs,  #primero nombre de la hoja luego = df
                Rates_Newtermdepo_NFCs=Rates_Newtermdepo_NFCs), 
           path = "datos_nota_depos_BCE.xlsx")
