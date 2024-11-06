# Cargar librerias ----------
library(tidyverse)
library(lubridate)
library(bdeseries)
library(writexl)

# Traer series hogares (VNA,VNP)
VNAVNP_CF_Hogares <- bdeseries::get_series(c("DMZ10F0000H.Q",
                                           "DMZ10F1000H.Q",
                                           "DMZ10F4000H.Q",
                                           "DMZ10F5000H.Q",
                                           "DMZ10F2000H.Q",
                                           "DMZ10F3000H.Q",
                                           "DMZ10F000H0.Q",
                                           "DMZ10F800H0.Q",
                                           "DMZ10F300H0.Q"
                                          )) %>% 
                  select(fecha, valores, codigo) %>% 
                  mutate(fecha = format(fecha,"%Y-%m")) %>%  #se queda con fecha y mes
                  mutate(valores = valores/1000) %>% # divide "valores" column by 1000
                  pivot_wider(names_from = codigo, 
                              values_from = valores) %>%   
  
                  filter(fecha >= "2021-01-01") %>% 
                  arrange(desc(fecha)) %>% 
                  rename( #renombra columnas
                    VNA_Hogares_Todos_activos = DMZ10F0000H.Q,
                    VNA_Hogares_Efectivo_depositos = DMZ10F1000H.Q,
                    VNA_Hogares_Valores_deuda = DMZ10F4000H.Q,
                    VNA_Hogares_Capital_FI = DMZ10F5000H.Q,
                    VNA_Hogares_Seguros_PP = DMZ10F2000H.Q,
                    VNA_Hogares_Otros_activos = DMZ10F3000H.Q,
                    VNP_Hogares_Todos_pasivos = DMZ10F000H0.Q,
                    VNP_Hogares_Prestamos = DMZ10F800H0.Q,
                    VNP_Hogares_Otros_pasivos = DMZ10F300H0.Q,
                       )

write_xlsx(list(VNAVNP_CF_Hogares=VNAVNP_CF_Hogares),  #primero nombre de la hoja luego = df
           path = "datos_CF.xlsx")

######
# Cargar librerias ----------
library(tidyverse)
library(lubridate)
library(bdeseries)
library(writexl)

# Traer series hogares (VNA,VNP)
opsfinancieras_CF_hogares <- bdeseries::get_series(c("DMZ10F000H0.Q",
                                             "DMZ10F1000H.Q",
                                             "DMZ10F4000H.Q",
                                             "DMZ10F51Z0H.Q",
                                             "DMZ10F5200H.Q",
                                             "DMZ10F2000H.Q",
                                             "DMZ10F000H0.Q",
                                             "DMZ10F800H0.Q",
                                             "DMZ10F840H0.Q",
                                             "DMZ10F310H0.Q"
                                            )) %>% 
  select(fecha, valores, codigo) %>% 
  filter(fecha >= "2018-01-01") %>% 
  mutate(valores = valores/1000) %>% # divide "valores" column by 1000
  pivot_wider(id_cols= fecha,
              names_from = codigo, 
              values_from = valores) %>%   
  
  filter(fecha >= "2018-01-01") %>% 
  arrange(desc(fecha)) %>% 
  rename( #renombra columnas
    VNA_Hogares_Todos_activos = DMZ10F0000H.Q,
    VNA_Hogares_Efectivo_depositos = DMZ10F1000H.Q,
    VNA_Hogares_Valores_deuda = DMZ10F4000H.Q,
    VNA_Hogares_Acciones = DMZ10F51Z0H.Q,
    VNA_Hogares_FI = DMZ10F5200H.Q,
    VNA_Hogares_Seguros_PP = DMZ10F2000H.Q,
    VNP_Hogares_Todos_pasivos = DMZ10F000H0.Q,
    VNP_Hogares_Prestamos = DMZ10F800H0.Q,
    VNP_Hogares_Creditos_comerciales = DMZ10F840H0.Q,
    VNP_Hogares_Otros_pasivos = DMZ10F310H0.Q,
    )

write_xlsx(list(opsfinancieras_CF_hogares=opsfinancieras_CF_hogares),  #lista con dataframes, primero nombre de la hoja luego = df
           path = "datos_CF.xlsx")                                     #excel en el que los va a pegar


