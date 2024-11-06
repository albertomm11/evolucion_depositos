# Cargar librerias ----------
library(tidyverse)
library(lubridate)
library(bdeseries)
library(writexl)

# Traer series depositos residentes total, vista, plazo ---------
volumen_depositos_bde <- bdeseries::get_series(c("DF_MESNAL20A1U62240Z01E", 
                                           "DF_MESNAL21A1U62240Z01E",
                                           "DF_MESNAL22A1U62240Z01E",
                                           "DF_MESNAL20A1U62251Z01E",
                                           "DF_MESNAL21A1U62251Z01E",
                                           "DF_MESNAL22A1U62251Z01E"
                                           )) %>% 
                  select(fecha, valores, codigo) %>%
  
                  pivot_wider(names_from = codigo, 
                              values_from = valores) %>%  
  
                  filter(fecha >= "2002-01-01") %>%
                  arrange(desc(fecha)) %>%
                  rename(
                    Total_Depos_SNF = DF_MESNAL20A1U62240Z01E,  # Renombra columnas
                    Vista_SNF = DF_MESNAL21A1U62240Z01E,
                    Plazo_SNF = DF_MESNAL22A1U62240Z01E,
                    Total_Depos_Hogares = DF_MESNAL20A1U62251Z01E,
                    Vista_Hogares = DF_MESNAL21A1U62251Z01E,
                    Plazo_Hogares = DF_MESNAL22A1U62251Z01E
                       )

write_xlsx(list(volumen_depositos_bde = volumen_depositos_bde), 
                path = "datos_nota_depos_BdE.xlsx")


#Depositos / PIB Nominal  ---------
depositos_PIBnominal <- bdeseries::get_series(c("DF_MESNAL20A1U62240Z01E",
                                                "DF_MESNAL20A1U62251Z01E")) %>% 
                        select(fecha, valores, codigo) %>% 
                        pivot_wider(names_from = codigo, 
                                    values_from = valores) %>%  
  
                        filter(fecha >= "2002-01-01") %>%
                        arrange(desc(fecha)) %>%
                        rename(
                         Total_Depos_SNF = DF_MESNAL20A1U62240Z01E,  # Renombra columnas
                         Total_Depos_Hogares = DF_MESNAL20A1U62251Z01E,
                                ) 
PIBnominal <- bdeseries::get_series("DTNSEC2010_PIBPM") %>% 
              mutate(valores = valores/1000) %>% 
              select(fecha, valores) %>% 
              rename(PIB_Nominal_4T = valores)

depositos_PIBnominal <- full_join(depositos_PIBnominal, PIBnominal, by="fecha") 

depositos_PIBnominal <- na.omit(depositos_PIBnominal)

depositos_sobre_PIBnominal <- depositos_PIBnominal %>% 
              mutate("Depósitos SNF/PIB"=Total_Depos_SNF/PIB_Nominal_4T,
                     "Depósitos Hogares/PIB"=Total_Depos_Hogares/PIB_Nominal_4T,
                     "Depósitos Hogares+SNF/PIB"=(Total_Depos_SNF+Total_Depos_Hogares)/(PIB_Nominal_4T))

write_xlsx(list("DepositosPIB" = depositos_sobre_PIBnominal), 
           path = "datos_nota_PIB_BdE.xlsx")
                                                  
