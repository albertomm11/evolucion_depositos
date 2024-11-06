# Cargar librerias ----------
library(tidyverse)
library(lubridate)
library(bdeseries)
library(readxl)
library(writexl)

datos_path <- "datos/"
excels_path <- "excels_output/"

#1 - Cargar series depositos --------- 
#Cuadro 8.22 Boletin BdE
volumen_depositos_bde_df <- bdeseries::get_series(c("DF_MESNAL20A1U62240Z01E", 
                                           "DF_MESNAL21A1U62240Z01E",
                                           "DF_MESNAL22A1U62240Z01E",
                                           "DF_MESNAL20A1U62251Z01E",
                                           "DF_MESNAL21A1U62251Z01E",
                                           "DF_MESNAL22A1U62251Z01E"
                                           )) %>% 
                  select(fecha, valores, codigo) %>%
  
                  pivot_wider(id_cols="fecha",
                              names_from = codigo, 
                              values_from = valores) %>%  
  
                  filter(fecha >= "2002-01-01") %>%
                  #arrange(desc(fecha)) %>%
                  rename(
                    Total_Depos_SNF = DF_MESNAL20A1U62240Z01E,  # Renombra columnas
                    Vista_SNF = DF_MESNAL21A1U62240Z01E,
                    Plazo_SNF = DF_MESNAL22A1U62240Z01E,
                    Total_Depos_Hogares = DF_MESNAL20A1U62251Z01E,
                    Vista_Hogares = DF_MESNAL21A1U62251Z01E,
                    Plazo_Hogares = DF_MESNAL22A1U62251Z01E)
                       

##1.1 Calculo totales y variaciones interanuales ----

volumen_depositos_bde_df <- volumen_depositos_bde_df %>% 
          mutate("total_depositos"=Total_Depos_SNF+Total_Depos_Hogares,
                 "total_vista"= Vista_SNF+Vista_Hogares,
                 "total_plazo"= Plazo_SNF+Plazo_Hogares,
                 "perc_plazo_snf"= Plazo_SNF/Total_Depos_SNF,
                 "perc_plazo_hogares"=Plazo_Hogares/Total_Depos_Hogares) %>% 
          mutate(var_yoy_total_depos_snf = Total_Depos_SNF - lag(Total_Depos_SNF, 12),  
                 var_yoy_vista_snf = (Vista_SNF)-lag(Vista_SNF, 12),
                 var_yoy_plazo_snf = (Plazo_SNF)-lag(Plazo_SNF,12),
                 var_yoy_total_depos_hogares=(Total_Depos_Hogares)-lag(Total_Depos_Hogares,12),
                 var_yoy_vista_hogares=(Vista_Hogares)-lag(Vista_Hogares,12),
                 var_yoy_plazo_hogares=(Plazo_Hogares)-lag(Plazo_Hogares,12)) 
volumen_depositos_bde_df <- volumen_depositos_bde_df  %>%    
          mutate(varperc_yoy_snf= ((Total_Depos_SNF / lag(Total_Depos_SNF, 12) -1)),
                 varperc_yoy_hogares=((Total_Depos_Hogares / lag(Total_Depos_Hogares,12)-1)),
                 varperc_yoy_total_depos=((total_depositos / lag(total_depositos,12)-1)))

#Cálculo columnas índice 100
fecha_referencia <- as.Date("2022-09-30") 

reference_values <- volumen_depositos_bde_df %>%
     filter(fecha == fecha_referencia) %>%
     select(Total_Depos_Hogares, Total_Depos_SNF, total_depositos)

reference_value_hogares <- reference_values$Total_Depos_Hogares # Extract individual reference values
reference_value_snf <- reference_values$Total_Depos_SNF
reference_value_total <- reference_values$total_depositos

volumen_depositos_bde_df <- volumen_depositos_bde_df %>%
     mutate(indexed_Total_Depos_Hogares = (Total_Depos_Hogares / reference_value_hogares) * 100,
            indexed_Total_Depos_SNF = (Total_Depos_SNF / reference_value_snf) * 100,
            indexed_total_depositos = (total_depositos / reference_value_total) * 100)

#reordenar por fecha y pasar a Excel
volumen_depositos_bde_df <- volumen_depositos_bde_df %>% 
     arrange(desc(fecha))

#guardar df para otros análisis
saveRDS(volumen_depositos_bde_df,paste0(datos_path, "datos_depositos_bde.Rds")) 

write_xlsx(list(volumen_depositos_bde = volumen_depositos_bde_df), 
                path = paste0(excels_path,"datos_nota_depos_BdE.xlsx"))


#2. Depositos / PIB Nominal  ---------
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

depositos_sobre_PIBnominal_df <- depositos_PIBnominal %>% 
              mutate("Depósitos SNF/PIB"=Total_Depos_SNF/PIB_Nominal_4T,
                     "Depósitos Hogares/PIB"=Total_Depos_Hogares/PIB_Nominal_4T,
                     "Depósitos Hogares+SNF/PIB"=(Total_Depos_SNF+Total_Depos_Hogares)/(PIB_Nominal_4T))

write_xlsx(list("DepositosPIB" = depositos_sobre_PIBnominal_df), 
           path = paste0(excels_path, "datos_nota_PIB_BdE.xlsx"))
                                                  
