install.packages("writexlsx")

library(ecb)
library(tidyverse)
library(eurostat)
library(tesorotools)
library(xlsx)
library(openxlsx)
library(writexl)

datos_path <- "datos/"
gráficos_path <- "gráficos/"

# ANÁLISIS DE LAS CUENTAS FINANCIERAS DE LOS HOGARES ----

# VOLUMEN DE FINANCIACIÓN ----

### Financiación ----

financiación_df <- bdeseries::get_series(c("DCF_M.N.ES.W0.S1M.S1.N.L.LE.FD.T._Z.XDC._T.M.V.N._T", 
                                           "DCF_M.N.ES.W0.S1M.S1.N.L.F.FD.T._Z.XDC._T.M.V.N._T",
                                           "DCF_M.N.ES.W0.S1M.S1.N.L.LE.F4B.T._Z.XDC._T.M.V.N._T", 
                                           "DCF_M.N.ES.W0.S1M.S1.N.L.LE.F4A.T._Z.XDC._T.M.V.N._T",
                                           "DCF_M.N.ES.W0.S1M.S1.N.L.LE.F4C.T._Z.XDC._T.M.V.N._T", 
                                           "DN_1TI2TIE96", "DN_1TI2TIE98", "DN_1TI2TIE97", 
                                           "DN_1TI2TIE47", "DN_1TI2TIE51", "DN_1TI2TIE80")) %>% 
  pivot_wider(id_cols = "fecha", names_from = "nombres", values_from = "valores") %>% 
  rename(`Stock Préstamos Hogares ISFLSH` = ' Series públicas de cuentas financieras e indicadores de liquidez y financiación basados en las normas NA_SEC. Ajuste: Sin ajuste de estacionalidad. Área de referencia: España. Área de contrapartida: Todo el mundo (W1 + W2). Sector de referencia: Hogares e Instituciones Sin Fines de Lucro al Servicio de los Hogares. Sector de contrapartida: Total de la economía. Consolidación: Datos no consolidados. Apunte contable: Pasivos. Saldos, transacciones y otros flujos: Balance financiero (saldo a final de periodo). Instrumento: Total financiación (F3 + F4). Vencimiento: Todos los vencimientos originales (tanto corto como largo plazo). Categoría de gasto: No aplicable. Unidad de medida: Moneda doméstica (incluye la conversión a la moneda actual aplicando un tipo de cambio fijo). Moneda de denominación: Todas las monedas. Valoración: Valor de mercado. Precios: Precios corrientes. Transformación: Datos no transformados. Desglose personalizado: Total',
         `Flujos netos de préstamos a hogares` = ' Series públicas de cuentas financieras e indicadores de liquidez y financiación basados en las normas NA_SEC. Ajuste: Sin ajuste de estacionalidad. Área de referencia: España. Área de contrapartida: Todo el mundo (W1 + W2). Sector de referencia: Hogares e Instituciones Sin Fines de Lucro al Servicio de los Hogares. Sector de contrapartida: Total de la economía. Consolidación: Datos no consolidados. Apunte contable: Pasivos. Saldos, transacciones y otros flujos: Transacciones de activos financieros y pasivos. Instrumento: Total financiación (F3 + F4). Vencimiento: Todos los vencimientos originales (tanto corto como largo plazo). Categoría de gasto: No aplicable. Unidad de medida: Moneda doméstica (incluye la conversión a la moneda actual aplicando un tipo de cambio fijo). Moneda de denominación: Todas las monedas. Valoración: Valor de mercado. Precios: Precios corrientes. Transformación: Datos no transformados. Desglose personalizado: Total',
         `Stock crédito hipotecario` = ' Series públicas de cuentas financieras e indicadores de liquidez y financiación basados en las normas NA_SEC. Ajuste: Sin ajuste de estacionalidad. Área de referencia: España. Área de contrapartida: Todo el mundo (W1 + W2). Sector de referencia: Hogares e Instituciones Sin Fines de Lucro al Servicio de los Hogares. Sector de contrapartida: Total de la economía. Consolidación: Datos no consolidados. Apunte contable: Pasivos. Saldos, transacciones y otros flujos: Balance financiero (saldo a final de periodo). Instrumento: Préstamos para vivienda. Vencimiento: Todos los vencimientos originales (tanto corto como largo plazo). Categoría de gasto: No aplicable. Unidad de medida: Moneda doméstica (incluye la conversión a la moneda actual aplicando un tipo de cambio fijo). Moneda de denominación: Todas las monedas. Valoración: Valor de mercado. Precios: Precios corrientes. Transformación: Datos no transformados. Desglose personalizado: Total',
         `Stock de crédito al consumo` = ' Series públicas de cuentas financieras e indicadores de liquidez y financiación basados en las normas NA_SEC. Ajuste: Sin ajuste de estacionalidad. Área de referencia: España. Área de contrapartida: Todo el mundo (W1 + W2). Sector de referencia: Hogares e Instituciones Sin Fines de Lucro al Servicio de los Hogares. Sector de contrapartida: Total de la economía. Consolidación: Datos no consolidados. Apunte contable: Pasivos. Saldos, transacciones y otros flujos: Balance financiero (saldo a final de periodo). Instrumento: Préstamos para consumo. Vencimiento: Todos los vencimientos originales (tanto corto como largo plazo). Categoría de gasto: No aplicable. Unidad de medida: Moneda doméstica (incluye la conversión a la moneda actual aplicando un tipo de cambio fijo). Moneda de denominación: Todas las monedas. Valoración: Valor de mercado. Precios: Precios corrientes. Transformación: Datos no transformados. Desglose personalizado: Total',
         `Stock de crédito para otros fines` = ' Series públicas de cuentas financieras e indicadores de liquidez y financiación basados en las normas NA_SEC. Ajuste: Sin ajuste de estacionalidad. Área de referencia: España. Área de contrapartida: Todo el mundo (W1 + W2). Sector de referencia: Hogares e Instituciones Sin Fines de Lucro al Servicio de los Hogares. Sector de contrapartida: Total de la economía. Consolidación: Datos no consolidados. Apunte contable: Pasivos. Saldos, transacciones y otros flujos: Balance financiero (saldo a final de periodo). Instrumento: Préstamos para otros fines. Vencimiento: Todos los vencimientos originales (tanto corto como largo plazo). Categoría de gasto: No aplicable. Unidad de medida: Moneda doméstica (incluye la conversión a la moneda actual aplicando un tipo de cambio fijo). Moneda de denominación: Todas las monedas. Valoración: Valor de mercado. Precios: Precios corrientes. Transformación: Datos no transformados. Desglose personalizado: Total',
         `Flujos brutos de crédito hipotecario con renegociaciones` = 'Tipo de interés. Nuevas operaciones. EC y EFC. Importes totales estimados. Hogares e ISFLSH. Crédito a la vivienda', 
         `Flujos brutos de crédito al consumo` = 'Tipo de interés. Nuevas operaciones. EC y EFC. Importes totales estimados. Hogares e ISFLSH. Crédito al consumo', 
         `Flujos brutos de crédito a otros fines` = 'Tipo de interés. Nuevas operaciones. EC y EFC. Importes totales estimados. Hogares e ISFLSH. Crédito otros fines', 
         `Flujos brutos tarjetas créd. pago aplazado y tarjetas revolving` = 'Tipo de interés. Nuevas operaciones. EC y EFC. Importes totales estimados. Hogares e ISFLSH. Tarjetas créd. pago aplazado y tarjetas revolving', 
         `Flujos brutos de crédito a la vivienda exlcuidas renegociaciones` = 'Tipo de interés. Nuevas operaciones. EC y EFC. Importes totales estimados. Hogares e ISFLSH. Crédito vivienda. Excluidas renegociaciones', 
         `Renegociaciones crédito hipotecario` = 'Tipo de interés. Nuevas operaciones. EC y EFC. Importes totales estimados. Hogares e ISFLSH. Crédito a la vivienda. Renegociados') %>% 
  mutate(across(c(starts_with("Stock"), "Flujos netos de préstamos a hogares"), ~ ceiling(. / 1000))) %>% 
  mutate(`Suma crédito hipotecario y al consumo` = rowSums(select(., c(`Stock crédito hipotecario`, `Stock de crédito al consumo`)), na.rm = TRUE)) %>% 
  mutate(`Suma flujo hipotecario y consumo` = rowSums(select(., c(`Flujos brutos de crédito hipotecario con renegociaciones`, `Flujos brutos de crédito al consumo`)), na.rm = TRUE)) %>% 
  mutate(`Tasa variación anual stock total (con otros fines)` = round((( `Stock Préstamos Hogares ISFLSH` - lag(`Stock Préstamos Hogares ISFLSH`, 12) ) / lag(`Stock Préstamos Hogares ISFLSH`, 12)) * 100, 2),
         `Tasa variación mensual stock total (con otros fines)` = round((`Stock Préstamos Hogares ISFLSH` - lag(`Stock Préstamos Hogares ISFLSH`)) / lag(`Stock Préstamos Hogares ISFLSH`) * 100, 2)) %>% 
  mutate(`Tasa variación anual stock total` = round(((`Suma crédito hipotecario y al consumo` - lag(`Suma crédito hipotecario y al consumo`, 12) ) / lag(`Suma crédito hipotecario y al consumo`, 12)) * 100, 2),
         `Tasa variación mensual stock total` = round((`Suma crédito hipotecario y al consumo` - lag(`Suma crédito hipotecario y al consumo`)) / lag(`Suma crédito hipotecario y al consumo`) * 100, 2)) %>% 
  mutate(`Tasa variacion anual stock crédito hipotecario` = round((( `Stock crédito hipotecario` - lag(`Stock crédito hipotecario`, 12) ) / lag(`Stock crédito hipotecario`, 12)) * 100, 2),
         `Tasa variacion mensual stock crédito hipotecario` = round((`Stock crédito hipotecario` - lag(`Stock crédito hipotecario`)) / lag(`Stock crédito hipotecario`) * 100, 2)) %>% 
  mutate(`Tasa variacion anual stock crédito al consumo` = round((( `Stock de crédito al consumo` - lag(`Stock de crédito al consumo`, 12) ) / lag(`Stock de crédito al consumo`, 12)) * 100, 2),
         `Tasa variacion mensual stock crédito al consumo` = round((`Stock de crédito al consumo` - lag(`Stock de crédito al consumo`)) / lag(`Stock de crédito al consumo`) * 100, 2)) %>% 
  mutate(`Tasa variacion anual stock crédito para otros fines` = round((( `Stock de crédito para otros fines` - lag(`Stock de crédito para otros fines`, 12) ) / lag(`Stock de crédito para otros fines`, 12)) * 100, 2),
         `Tasa variacion mensual stock crédito para otros fines` = round((`Stock de crédito para otros fines` - lag(`Stock de crédito para otros fines`)) / lag(`Stock de crédito para otros fines`) * 100, 2)) %>% 
  mutate(`Tasa variacion anual flujo crédito hipotecario con renegociaciones` = round((( `Flujos brutos de crédito hipotecario con renegociaciones` - lag(`Flujos brutos de crédito hipotecario con renegociaciones`, 12) ) / lag(`Flujos brutos de crédito hipotecario con renegociaciones`, 12)) * 100, 2),
         `Tasa variacion mensual flujo crédito hipotecario con renegociaciones` = round((`Flujos brutos de crédito hipotecario con renegociaciones` - lag(`Flujos brutos de crédito hipotecario con renegociaciones`)) / lag(`Flujos brutos de crédito hipotecario con renegociaciones`) * 100, 2)) %>% 
  mutate(`Tasa variación anual flujo crédito al consumo` = round((( `Flujos brutos de crédito al consumo` - lag(`Flujos brutos de crédito al consumo`, 12) ) / lag(`Flujos brutos de crédito al consumo`, 12)) * 100, 2),
         `Tasa variación mensual flujo crédito al consumo` = round((`Flujos brutos de crédito al consumo` - lag(`Flujos brutos de crédito al consumo`)) / lag(`Flujos brutos de crédito al consumo`) * 100, 2)) %>% 
  mutate(`Tasa variación anual flujo crédito otros fines` = round((( `Flujos brutos de crédito a otros fines` - lag(`Flujos brutos de crédito a otros fines`, 12) ) / lag(`Flujos brutos de crédito a otros fines`, 12)) * 100, 2),
         `Tasa variación mensual flujo crédito otros fines` = round((`Flujos brutos de crédito a otros fines` - lag(`Flujos brutos de crédito a otros fines`)) / lag(`Flujos brutos de crédito a otros fines`) * 100, 2)) %>% 
  mutate(`Tasa variación anual suma crédito hipotecario y al consumo` = round((( `Suma crédito hipotecario y al consumo` - lag(`Suma crédito hipotecario y al consumo`, 12) ) / lag(`Suma crédito hipotecario y al consumo`, 12)) * 100, 2),
         `Tasa variación mensual suma crédito hipotecario y al consumo` = round((`Suma crédito hipotecario y al consumo` - lag(`Suma crédito hipotecario y al consumo`)) / lag(`Suma crédito hipotecario y al consumo`) * 100, 2)) %>% 
  mutate(`Tasa variacion anual renegociaciones crédito hipotecario` = round((( `Renegociaciones crédito hipotecario` - lag(`Renegociaciones crédito hipotecario`, 12) ) / lag(`Renegociaciones crédito hipotecario`, 12)) * 100, 2),
         `Tasa variacion mensual renegociaciones crédito hipotecario` = round((`Renegociaciones crédito hipotecario` - lag(`Renegociaciones crédito hipotecario`)) / lag(`Renegociaciones crédito hipotecario`) * 100, 2)) %>% 
  mutate('Tasa variación anual suma flujo hipotecario y consumo' = round(((`Suma flujo hipotecario y consumo` - lag(`Suma flujo hipotecario y consumo`, 12)) / lag(`Suma flujo hipotecario y consumo`, 12)) * 100, 2),
         'Tasa variación mensual suma flujo hipotecario y consumo' = round((`Suma flujo hipotecario y consumo` - lag(`Suma flujo hipotecario y consumo`)) / lag(`Suma flujo hipotecario y consumo`) * 100, 2))

feather::write_feather(financiación_df, paste0(datos_path, "financiación_df.feather"))

### Amortizaciones de crédito hipotecario

amortizaciones_crédito_hipotecario_df <- bdeseries::get_series (c("DCF_M.N.ES.W0.S1M.S1.N.L.LE.F4B.T._Z.XDC._T.M.V.N._T", "DN_1TI2TIE98",
                                                                  "DN_1TI2TIE97")) %>% 
  pivot_wider(id_cols = "fecha", names_from = "nombres", values_from = "valores") %>% 
  rename(`Stock crédito hipotecario` = ' Series públicas de cuentas financieras e indicadores de liquidez y financiación basados en las normas NA_SEC. Ajuste: Sin ajuste de estacionalidad. Área de referencia: España. Área de contrapartida: Todo el mundo (W1 + W2). Sector de referencia: Hogares e Instituciones Sin Fines de Lucro al Servicio de los Hogares. Sector de contrapartida: Total de la economía. Consolidación: Datos no consolidados. Apunte contable: Pasivos. Saldos, transacciones y otros flujos: Balance financiero (saldo a final de periodo). Instrumento: Préstamos para vivienda. Vencimiento: Todos los vencimientos originales (tanto corto como largo plazo). Categoría de gasto: No aplicable. Unidad de medida: Moneda doméstica (incluye la conversión a la moneda actual aplicando un tipo de cambio fijo). Moneda de denominación: Todas las monedas. Valoración: Valor de mercado. Precios: Precios corrientes. Transformación: Datos no transformados. Desglose personalizado: Total',
         `Flujos brutos de crédito a la vivienda exlcuidas renegociaciones` = 'Tipo de interés. Nuevas operaciones. EC y EFC. Importes totales estimados. Hogares e ISFLSH. Crédito vivienda. Excluidas renegociaciones', 
         `Renegociaciones crédito hipotecario` = 'Tipo de interés. Nuevas operaciones. EC y EFC. Importes totales estimados. Hogares e ISFLSH. Crédito a la vivienda. Renegociados') %>% 
  mutate(across(c(`Stock crédito hipotecario`), ~ ceiling(. / 1000))) %>% 
  mutate(amortizaciones = lag(`Stock crédito hipotecario`) - `Stock crédito hipotecario` + `Flujos brutos de crédito a la vivienda exlcuidas renegociaciones`) %>% 
  mutate(`Tasa variacion anual amortizaciones crédito hipotecario` = round((( `amortizaciones` - lag(`amortizaciones`, 12) ) / lag(`amortizaciones`, 12)) * 100, 2),
         `Tasa variacion mensual amortizaicones crédito hipotecario` = round((`amortizaciones` - lag(`amortizaciones`)) / lag(`amortizaciones`) * 100, 2))
  
feather::write_feather(amortizaciones_crédito_hipotecario_df, paste0(datos_path, "amortizaciones_crédito_hipotecario_df.feather"))

### Hipotecas ----

hipotecas_df <- bdeseries::get_series(c("DN_1TI2TIE42", "DN_1TI2TIE43", "DN_1TI2TIE44", "DN_1TI2TIE45", "DN_1TI2TIE46")) %>% 
  pivot_wider(id_cols="fecha", names_from="nombres", values_from="valores") %>% 
  rename('Flujos brutos crédito' = 'Tipo de interés. Nuevas operaciones. EC y EFC. Importes totales estimados. Hogares e ISFLSH. Crédito vivienda', 
         'Crédito 1 año'='Tipo de interés. Nuevas operaciones. EC y EFC. Importes totales estimados. Hogares e ISFLSH. Crédito vivienda hasta 1 año',
         'Crédito 1-5 años' = 'Tipo de interés. Nuevas operaciones. EC y EFC. Importes totales estimados. Hogares e ISFLSH. Crédito vivienda. Más 1 año y hasta 5 años',
         'Crédito 5-10 años'='Tipo de interés. Nuevas operaciones. EC y EFC. Importes totales estimados. Hogares e ISFLSH. Crédito vivienda. Más 5 años y hasta 10 años',
         'Crédito más de 10 años' = 'Tipo de interés. Nuevas operaciones. EC y EFC. Importes totales estimados. Hogares e ISFLSH. Crédito vivienda. Más de 10 años') %>% 
  mutate('Crédito tipo mixto' = rowSums(select(., c('Crédito 1-5 años', 'Crédito 5-10 años')), na.rm = TRUE)) %>% 
  mutate('Crédito tipo variable %' = round((`Crédito 1 año` / `Flujos brutos crédito`)*100, 2)) %>% 
  mutate('Crédito tipo mixto %' = round((`Crédito tipo mixto` / `Flujos brutos crédito`)*100, 2)) %>% 
  mutate('Crédito tipo fijo %' = round((`Crédito más de 10 años` / `Flujos brutos crédito`)*100, 2))

feather::write_feather(hipotecas_df, paste0(datos_path, "hipotecas_df.feather"))

# TIPOS DE INTERÉS ----

### Tipos de interés hogares ----

tipos_interés_hogares_df <- bdeseries::get_series((c("DN_1TI2T0002", "DN_1TI2T0136", "DN_1TI2T0137", "DN_1TI2T0007", "DN_1TI2T0011", "DN_1TI2T0082", "DN_1TI2T0087", "DN_1TI2T0091", 
                                                     "DN_1TI1T0050","DN_1TI1T0051", "DN_1TI2T0116", "D_1NBAC972", "D_DNBAF172	"))) %>% #Euribor 12 meses no conseguí encontrarlo
  pivot_wider(id_cols="fecha", names_from="nombres", values_from="valores") %>% 
  rename('% TEDR medio ponderado. Hogares e ISFLSH' = 'Tipo de interés. Nuevas operaciones. EC y EFC. TEDR. Hogares e ISFLSH. Crédito a la vivienda. Tipo medio ponderado', 'Tipos de interés flujos de crédito al consumo' = 'Tipo de interés. Nuevas operaciones. EC y EFC. TEDR. Hogares e ISFLSH. Crédito al consumo. Tipo medio ponderado',
         'Tipos de interés flujos de crédito a otros fines' = 'Tipo de interés. Nuevas operaciones. EC y EFC. TEDR. Hogares e ISFLSH. Crédito para otros fines. Tipo medio ponderado', 'Tipos de interés nuevas operaciones de crédito a la vivienda TAE' = 'Tipo de interés. Nuevas operaciones. EC y EFC. TAE. Hogares e ISFLSH. Crédito a la vivienda',
         'Tipos de interés nuevas operaciones de crédito al consumo TAE' = 'Tipo de interés. Nuevas operaciones. EC y EFC. TAE. Hogares e ISFLSH. Crédito al consumo', 'Tipos de interés nuevas operaciones de crédito a otros fines TAE' = 'Tipo de interés. Nuevas operaciones. EC y EFC. TAE. Hogares e ISFLSH. Crédito para otros fines',
         'Tipo de interés stock crédito a vivienda' = 'Tipo de interés. Saldos vivos. EC y EFC. TEDR. Hogares e ISFLSH. Crédito a la vivienda. Tipo medio ponderado', 'Tipo de interés stocks crédito al consumo y otros fines' = 'Tipo de interés. Saldos vivos. EC y EFC. TEDR. Hogares e ISFLSH. Crédito al consumo y otros fines. Tipo medio ponderado',
         'TEDR renegociados vivienda' = 'Tipo de interés. Nuevas operaciones. EC y EFC. TEDR. Hogares e ISFLSH. Crédito a la vivienda. Renegociados', 'TEDR vivienda excluidas renegociaciones' = 'Tipo de interés. Nuevas operaciones. EC y EFC. TEDR. Hogares e ISFLSH. Crédito a la vivienda. Excluidas renegociaciones',
         'Euribor a 1 mes' = 'Tipo de interés. Nuevas operaciones. EC y EFC. TEDR. Hogares e ISFLSH. Tarjetas de crédito de pago aplazado')

feather::write_feather(tipos_interés_hogares_df, paste0(datos_path, "tipos_interés_hogares_df.feather"))

### Tipos de interés hipotecas ----

tipos_interés_hipotecas_df <- bdeseries::get_series(c("DN_1TI2T0003", "DN_1TI2T0006", "DN_1TI2T0004", "DN_1TI2T0005")) %>% 
  pivot_wider(id_cols="fecha", names_from="nombres", values_from="valores") %>% 
  rename('Tipo variable' = 'Tipo de interés. Nuevas operaciones. EC y EFC. TEDR. Hogares e ISFLSH. Crédito a la vivienda hasta 1 año', 
         'Tipo mixto 1-5 años' = 'Tipo de interés. Nuevas operaciones. EC y EFC. TEDR. Hogares e ISFLSH. Crédito a la vivienda a más de 1 año y hasta 5 años',
         'Tipo mixto 5-10 años' = 'Tipo de interés. Nuevas operaciones. EC y EFC. TEDR. Hogares e ISFLSH. Crédito a la vivienda a más de 5 años y hasta 10 años',
         'Tipo fijo' = 'Tipo de interés. Nuevas operaciones. EC y EFC. TEDR. Hogares e ISFLSH. Crédito a la vivienda más de 10 años') %>% 
  mutate('Tipo mixto' = ((`Tipo mixto 1-5 años` + `Tipo mixto 5-10 años`)/2)) %>% 
  mutate_all(~round(., 2)) #el punto significa aplicar la función round () a cada columna del data frame. El punto representa cada columna del dataframe.

feather::write_feather(tipos_interés_hipotecas_df, paste0(datos_path, "tipos_interés_hipotecas_df.feather"))

### Evolución del Euribor-12 meses y su curva forward en diferentes momentos temporales ----

Expectativas_euribor12M <- tesoroseries::search_series("EURIBOR12_", field="nombre") %>% 
  .$nombre %>% 
  tesoroseries::get_series() %>% mutate(fecha = lubridate::floor_date(fecha,"month")) #También tienes la función ceiling

Euribor12M_y_expectativas_df <- bdeseries::get_series(c("D_DNBAF172")) %>%
  bind_rows(Expectativas_euribor12M) %>%
  pivot_wider(id_cols="fecha", names_from="nombres", values_from="valores") %>%
  mutate_all(~ round(., 2)) %>% 
  arrange(fecha)

Euribor12M_y_expectativas_df <- Euribor12M_y_expectativas_df %>%
  tidyr::fill(-fecha, .direction = "down") #interpolar hacia abajo

fecha_limite <- as.Date("2024-06-01")

Euribor12M_y_expectativas_df <- Euribor12M_y_expectativas_df %>%
  mutate(`Tipo de interés. UEM. Mercado monetario. Euríbor. A 12 meses` = 
           if_else(fecha >= fecha_limite, NA_real_, `Tipo de interés. UEM. Mercado monetario. Euríbor. A 12 meses`))

Euribor12M_y_expectativas_df <- Euribor12M_y_expectativas_df %>%
  mutate(`Curva forward del Euribor-12M. Estimación a 20240101` = if_else(fecha == as.Date("2024-01-01"),
                                                                     `Tipo de interés. UEM. Mercado monetario. Euríbor. A 12 meses`, 
                                                                     `Curva forward del Euribor-12M. Estimación a 20240101`),
        `Curva forward del Euribor-12M. Estimación a 20240508` = if_else(fecha == as.Date("2024-05-01"), 
                                                                     `Tipo de interés. UEM. Mercado monetario. Euríbor. A 12 meses`, 
                                                                     `Curva forward del Euribor-12M. Estimación a 20240508`)) %>% 
  rename('Euribor 12M'=`Tipo de interés. UEM. Mercado monetario. Euríbor. A 12 meses`, 'Euribor 12M esperado 01/01/2024' =  `Curva forward del Euribor-12M. Estimación a 20240101`,
         'Euribor 12M esperado 01/05/2024' = `Curva forward del Euribor-12M. Estimación a 20240508`)

### Aumento del Euribor respecto al año anterior ----



# ENCUESTA DE PRÉSTAMOS BANCARIOS ----

# Descargamos las series del BCE
## Cambios en la demanda, términos y condiciones y estándares de crédito para adquisición de vivienda ----

vivienda_df <- ecb::get_data("BLS.Q.ES.ALL.Z.H.H.B3.ST.S.FNET") |>
  mutate(nombres = "estándares_crédito_vivienda") |> 
  # mutate es una función de dplyr que modifica o crea nuevas columnas en un data frame
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.Z.H.H.B3.ZZ.D.FNET") |>
              mutate(nombres = "demanda_vivienda")) |> 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.O.H.H.B3.TC.S.FNET") |>
              mutate(nombres = "términos_y_condiciones_vivienda")) |>
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.Z.H.H.F3.ST.S.FNET") %>% 
              mutate(nombres = 'Expectativas estándares de crédito')) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.Z.H.H.F3.ZZ.D.FNET") %>% 
              mutate(nombres = 'Expectativas demanda vivienda')) %>% 
## select(obstime, obsvalue) 
  mutate(fecha = case_when (stringr::str_sub(obstime, 6,7) == "Q1" ~ as.Date(paste0(as.numeric(stringr::str_sub(obstime, 1,4))-1, "-12-01")),
                            ## Paste0 se usa para concatenar elementos en una sola cadena de carácteres, no agrega ningún espacio entre los elementos concatenados, a diferencia de la función paste()
                            stringr::str_sub(obstime, 6,7) == "Q2" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-03-01")),
                            stringr::str_sub(obstime, 6,7) == "Q3" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-06-01")),
                            stringr::str_sub(obstime, 6,7) == "Q4" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-09-01")),
                            TRUE ~ NA)) |>
  pivot_wider(id_cols = c("fecha"), names_from="nombres", values_from="obsvalue") 

feather::write_feather(vivienda_df, paste0(datos_path, "vivienda_df.feather"))

## Determinantes de los cambios de la demanda de crédito para adquisición de vivienda ----

demanda_vivienda_df <- ecb::get_data("BLS.Q.ES.ALL.HMP.H.H.B3.ZZ.D.FNET") %>% 
  mutate (nombres = "Perspectivas del mercado de la vivienda") %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.CCF.H.H.B3.ZZ.D.FNET") |>
              mutate(nombres = "Confianza consumidores")) |> 
  bind_rows (ecb::get_data("BLS.Q.ES.ALL.GLI.H.H.B3.ZZ.D.FNET") |>
               mutate(nombres = "Nivel general de tipos de interés")) %>% 
  bind_rows (ecb::get_data("BLS.Q.ES.ALL.IFS.H.H.B3.ZZ.D.FNET") |>
               mutate(nombres = "Internal finance out of savings")) %>% 
  bind_rows (ecb::get_data("BLS.Q.ES.ALL.LOB.H.H.B3.ZZ.D.FNET") |>
               mutate(nombres = "Loans from other banks")) %>% 
  bind_rows (ecb::get_data("BLS.Q.ES.ALL.OSF.H.H.B3.ZZ.D.FNET") |>
               mutate(nombres = "Other sources of finance")) %>% 
  mutate(fecha = case_when (stringr::str_sub(obstime, 6,7) == "Q1" ~ as.Date(paste0(as.numeric(stringr::str_sub(obstime, 1,4))-1, "-12-01")),
                            stringr::str_sub(obstime, 6,7) == "Q2" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-03-01")),
                            stringr::str_sub(obstime, 6,7) == "Q3" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-06-01")),
                            stringr::str_sub(obstime, 6,7) == "Q4" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-09-01")),
                            TRUE ~ NA)) |>
  pivot_wider(id_cols = c("fecha"), names_from="nombres", values_from="obsvalue") %>% 
  mutate (Otras_fuentes_financiación = rowMeans(select(., starts_with(c("Internal finance out of savings", "Loans from other banks", "Other sources of finance"))), 
                                                na.rm = TRUE)) %>% 
  rename('Otras fuentes de financiación' = Otras_fuentes_financiación) # Las comillas invertidas se usan para permitir nombres de columnas con espacios
  # También se podría calcular la media con mean y rowise.

feather::write_feather(demanda_vivienda_df, paste0(datos_path, "demanda_vivienda_df.feather"))

## Determinantes evolución estándares de crédito aplicados préstamos para adquisición de vivienda ----

estándares_vivienda_df <- ecb::get_data("BLS.Q.ES.ALL.CP.H.H.B3.ST.S.FNET") %>% 
  mutate(nombres = "Capital position") %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.MF.H.H.B3.ST.S.FNET") %>% 
              mutate(nombres = "Acces to market financing")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.LP.H.H.B3.ST.S.FNET") %>% 
              mutate(nombres = "Liquidity position")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.BC.H.H.B3.ST.S.FNET") %>% 
              mutate(nombres = "Bank competition")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.NBC.H.H.B3.ST.S.FNET") %>% 
              mutate(nombres = "Non-bank competition")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.GEA.H.H.B3.ST.S.FNET") %>% 
              mutate(nombres = "Impact of general economic activity")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.CWB.H.H.B3.ST.S.FNET") %>% 
              mutate(nombres = "Creditworthiness of borrower")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.HMP.H.H.B3.ST.S.FNET") %>% 
              mutate(nombres = "Housing market prospects")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.RTO.H.H.B3.ST.S.FNET") %>% 
              mutate(nombres = "Tolerancia al riesgo de los bancos")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.OF1.H.H.B3.ST.S.FNET") %>% 
              mutate(nombres = "Otros factores")) %>% 
  mutate(fecha = case_when(stringr::str_sub(obstime, 6,7) == "Q1" ~ as.Date(paste0(as.numeric(stringr::str_sub(obstime, 1,4))-1, "-12-01")),
                           stringr::str_sub(obstime, 6,7) == "Q2" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-03-01")),
                           stringr::str_sub(obstime, 6,7) == "Q3" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-06-01")),
                           stringr::str_sub(obstime, 6,7) == "Q4" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-09-01")),
                           TRUE ~ NA)) %>% 
  pivot_wider(id_cols = c("fecha"), names_from = "nombres", values_from = "obsvalue")%>% 
  mutate (Coste_fondos = rowMeans(select(., starts_with(c("Capital position", "Acces to market financing", "Liquidity position"))),
                                na.rm = TRUE)) %>% 
  rename('Coste de los fondos y restricciones de balance' = Coste_fondos) %>% 
  mutate (Competencia = rowMeans(select(., starts_with(c("Bank competition", "Non-bank competition"))),
                                 na.rm = TRUE)) %>% 
  mutate (Percepción_riesgo = rowMeans(select(., starts_with(c("Impact of general economic activity", "Creditworthiness of borrower", "Housing market prospects"))),
                                       na.rm = TRUE)) %>% 
  rename('Percepción del riesgo' = Percepción_riesgo)

feather::write_feather(estándares_vivienda_df, paste0(datos_path, "estándares_vivienda_df.feather"))

## Determinantes evolución términos y condiciones préstamos para adquisición de vivienda ----

términos_y_condiciones_vivienda_df <- ecb::get_data("BLS.Q.ES.ALL.MAL.H.H.B3.TC.S.FNET") %>% 
  mutate(nombres = "Margen prestamos promedio") %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.MRL.H.H.B3.TC.S.FNET") %>% 
              mutate(nombres = "Margen préstamos más arriesgados")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.OF1.H.H.B3.TC.S.FNET") %>% 
              mutate(nombres = "Otros factores")) %>% 
  mutate(fecha = case_when(stringr::str_sub(obstime, 6,7) == "Q1" ~ as.Date(paste0(as.numeric(stringr::str_sub(obstime, 1,4))-1, "-12-01")),
                           stringr::str_sub(obstime, 6,7) == "Q2" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-03-01")),
                           stringr::str_sub(obstime, 6,7) == "Q3" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-06-01")),
                           stringr::str_sub(obstime, 6,7) == "Q4" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-09-01")),
                           TRUE ~ NA)) %>% 
  pivot_wider(id_cols = c("fecha"), names_from = "nombres", values_from = "obsvalue")

feather::write_feather(términos_y_condiciones_vivienda_df, paste0(datos_path, "términos_y_condiciones_vivienda_df.feather"))

## Cambios en la demanda, términos y condiciones y estándares de crédito al consumo ----

consumo_df <- ecb::get_data("BLS.Q.ES.ALL.Z.H.C.B3.ST.S.FNET") |>
  mutate(nombres = "Estándares de crédito") |> 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.Z.H.C.B3.ZZ.D.FNET") |>
              mutate(nombres = "Demanda")) |> 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.O.H.C.B3.TC.S.FNET") |>
              mutate(nombres = "Términos y condiciones")) |>
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.Z.H.C.F3.ST.S.FNET") %>% 
              mutate(nombres = 'Expectativas estándares de crédito')) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.Z.H.C.F3.ZZ.D.FNET") %>% 
              mutate(nombres = 'Expectativas demanda consumo')) %>%
  mutate(fecha = case_when(stringr::str_sub(obstime, 6,7) == "Q1" ~ as.Date(paste0(as.numeric(stringr::str_sub(obstime, 1,4))-1, "-12-01")),
                            stringr::str_sub(obstime, 6,7) == "Q2" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-03-01")),
                            stringr::str_sub(obstime, 6,7) == "Q3" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-06-01")),
                            stringr::str_sub(obstime, 6,7) == "Q4" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-09-01")),
                            TRUE ~ NA)) |>
  pivot_wider(id_cols = c("fecha"), names_from="nombres", values_from="obsvalue") 

feather::write_feather(consumo_df, paste0(datos_path, "consumo_df.feather"))

## Determinantes de la evolución de la demanda de crédito al consumo ----

demanda_consumo_df <- ecb::get_data("BLS.Q.ES.ALL.SDC.H.C.B3.ZZ.D.FNET") %>% 
  mutate (nombres = "Gasto en consumo de bienes duraderos") %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.CCF.H.C.B3.ZZ.D.FNET") |>
              mutate(nombres = "Confianza consumidores")) |> 
  bind_rows (ecb::get_data("BLS.Q.ES.ALL.GLI.H.C.B3.ZZ.D.FNET") |>
               mutate(nombres = "Nivel general de tipos de interés")) %>% 
  bind_rows (ecb::get_data("BLS.Q.ES.ALL.CRE.H.C.B3.ZZ.D.FNET") |>
               mutate(nombres = "Real Estate")) %>% 
  bind_rows (ecb::get_data("BLS.Q.ES.ALL.IFS.H.C.B3.ZZ.D.FNET") |>
               mutate(nombres = "Financiación interna con cargo al ahorro")) |> 
  bind_rows (ecb::get_data("BLS.Q.ES.ALL.LOB.H.C.B3.ZZ.D.FNET") |>
               mutate(nombres = "Préstamos de otros bancos")) %>% 
  bind_rows (ecb::get_data("BLS.Q.ES.ALL.OSF.H.C.B3.ZZ.D.FNET") |>
               mutate(nombres = "Otras fuentes de financiación")) %>% 
  bind_rows (ecb::get_data("BLS.Q.ES.ALL.OF1.H.C.B3.ZZ.D.FNET") |>
               mutate(nombres = "Otros factores")) %>% 
  mutate(fecha = case_when(stringr::str_sub(obstime, 6,7) == "Q1" ~ as.Date(paste0(as.numeric(stringr::str_sub(obstime, 1,4))-1, "-12-01")),
                           stringr::str_sub(obstime, 6,7) == "Q2" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-03-01")),
                           stringr::str_sub(obstime, 6,7) == "Q3" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-06-01")),
                           stringr::str_sub(obstime, 6,7) == "Q4" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-09-01")),
                           TRUE ~ NA)) %>% 
  pivot_wider(id_cols = c("fecha"), names_from = "nombres", values_from = "obsvalue")%>% 
  mutate (Financiación_alternativa = rowMeans(select(., starts_with(c("Financiación interna con cargo al ahorro", "Préstamos de otros bancos", "Otras fuentes de financiación"))),
                                  na.rm = TRUE)) %>% 
  rename('Uso financiación alternativa' = Financiación_alternativa)

feather::write_feather(demanda_consumo_df, paste0(datos_path, "demanda_consumo_df.feather"))

## Determinantes variación estándares de crédito aplicados a préstamos al consumo ----

estándares_consumo_df <- ecb::get_data("BLS.Q.ES.ALL.CP.H.C.B3.ST.S.FNET") %>% 
              mutate(nombres = "Posición de capital") %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.MF.H.C.B3.ST.S.FNET") %>% 
              mutate(nombres = "Acceso de la financiación de mercado")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.LP.H.C.B3.ST.S.FNET") %>% 
              mutate(nombres = "Posición de liquidez")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.RTO.H.C.B3.ST.S.FNET") %>% 
              mutate(nombres = "Tolerancia al riesgo de los bancos")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.GEA.H.C.B3.ST.S.FNET") %>% 
              mutate(nombres = "Actividad económica general")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.CWC.H.C.B3.ST.S.FNET") %>% 
              mutate(nombres = "Solvencia del prestatario")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.RCD.E.Z.B3.ST.S.FNET") %>% 
              mutate(nombres = "Colateral")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.BC.H.C.B3.ST.S.FNET") %>% 
            mutate(nombres = "Competencia bancaria")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.NBC.H.C.B3.ST.S.FNET") %>% 
              mutate(nombres = "Competencia no bancaria")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.OF1.H.C.B3.ST.S.FNET") %>% 
              mutate(nombres = "Otros factores"))  %>% 
              mutate(fecha = case_when(stringr::str_sub(obstime, 6,7) == "Q1" ~ as.Date(paste0(as.numeric(stringr::str_sub(obstime, 1,4))-1, "-12-01")),
                                       stringr::str_sub(obstime, 6,7) == "Q2" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-03-01")),
                                       stringr::str_sub(obstime, 6,7) == "Q3" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-06-01")),
                                       stringr::str_sub(obstime, 6,7) == "Q4" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-09-01")),
                                       TRUE ~ NA)) %>% 
  pivot_wider(id_cols = c("fecha"), names_from = "nombres", values_from = "obsvalue")%>% 
  mutate (Coste_fondos = rowMeans(select(., starts_with(c("Posición de capital", "Acceso a la financiación de mercado", "Posición de liquidez"))),
                                                          na.rm = TRUE)) %>% 
  rename('Coste de fondos y restricciones de balance' = Coste_fondos) %>% 
    mutate (Percepción_riesgo = rowMeans(select(., starts_with(c("Actividad económica general", "Solvencia del prestatario", "Colateral"))),
                                    na.rm = TRUE)) %>% 
    rename('Percepción del riesgo' = Percepción_riesgo) %>% 
    mutate (Competencia = rowMeans(select(., starts_with(c("Competencia bancaria", "Competencia no bancaria"))),
                                         na.rm = TRUE)) 

feather::write_feather(estándares_consumo_df, paste0(datos_path, "estándares_consumo_df.feather"))

## Determinantes cambios términos y condiciones crédito al consumo ----

términos_y_condiciones_consumo_df <- ecb::get_data("BLS.Q.ES.ALL.MAL.H.C.B3.TC.S.FNET") %>% 
  mutate(nombres = "Margen préstamos promedio") %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.MRL.H.C.B3.TC.S.FNET") %>% 
              mutate(nombres = "Margen préstamos más arriesgados")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.CRQ.H.C.B3.TC.S.FNET") %>% 
              mutate(nombres = "Requisitos de colateral")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.SZL.H.C.B3.TC.S.FNET") %>% 
              mutate(nombres = "Tamaño de los préstamos")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.NIC.H.C.B3.TC.S.FNET") %>% 
              mutate(nombres = "Gastos no relacionados con los tipos de interés"))  %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.MTY.H.C.B3.TC.S.FNET") %>% 
              mutate(nombres = "Vencimiento")) %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.OF1.H.C.B3.TC.S.FNET") %>% 
              mutate(nombres = "Otros factores")) %>% 
  mutate(fecha = case_when(stringr::str_sub(obstime, 6,7) == "Q1" ~ as.Date(paste0(as.numeric(stringr::str_sub(obstime, 1,4))-1, "-12-01")),
                           stringr::str_sub(obstime, 6,7) == "Q2" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-03-01")),
                           stringr::str_sub(obstime, 6,7) == "Q3" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-06-01")),
                           stringr::str_sub(obstime, 6,7) == "Q4" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-09-01")),
                           TRUE ~ NA)) %>% 
  pivot_wider(id_cols = c("fecha"), names_from = "nombres", values_from = "obsvalue") %>% 
  mutate (Otros = rowMeans(select(., starts_with(c("Tamaño de los préstamos", "Gastos no relacionados con los tipos de interés", "Vencimiento"))),
                                  na.rm = TRUE)) %>% 
  rename('Otros términos y condiciones' = Otros)

feather::write_feather(términos_y_condiciones_consumo_df, paste0(datos_path, "términos_y_condiciones_consumo_df.feather"))

## Solicitudes de préstamos rechazadas ----

rechazadas_df <- ecb::get_data("BLS.Q.ES.ALL.Z.H.H.B3.RA.D.FNET") %>% 
  mutate(nombres = "Vivienda") %>% 
  bind_rows(ecb::get_data("BLS.Q.ES.ALL.Z.H.C.B3.RA.D.FNET") %>% 
              mutate(nombres = "Consumo")) %>% 
  mutate(fecha = case_when(stringr::str_sub(obstime, 6,7) == "Q1" ~ as.Date(paste0(as.numeric(stringr::str_sub(obstime, 1,4))-1, "-12-01")),
                           stringr::str_sub(obstime, 6,7) == "Q2" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-03-01")),
                           stringr::str_sub(obstime, 6,7) == "Q3" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-06-01")),
                           stringr::str_sub(obstime, 6,7) == "Q4" ~ as.Date(paste0(stringr::str_sub(obstime, 1,4), "-09-01")),
                           TRUE ~ NA)) %>% 
  pivot_wider(id_cols = c("fecha"), names_from = "nombres", values_from = "obsvalue")

feather::write_feather(rechazadas_df, paste0(datos_path, "rechazadas_df.feather"))

# DUDOSIDAD - en millones de Euros (el BDE da los datos de dudosidad en miles) ----

dudosidad_completo_df <- bdeseries::get_series(c("D_MEE62000", "D_MEE62800","D_MEE62100", "D_MEE62110", "D_MEE62120", "D_MEE62600","D_MEE62300", "D_MEE62200", "D_MEE62201",
                                        "D_MEADU201", "D_MEADU202", "D_MEADU210", "D_MEADU211", "D_MEADU213", "D_MEADU212", "D_MEADU226", "D_MEADU221", "D_MEADU225")) %>%  
  pivot_wider(id_cols = c("fecha"), names_from = "nombres", values_from = "valores") %>% 
  rename (Total = 'EC y EFC. Créditos a OSR por finalidades. Otra financiación a hogares por funciones de gasto. Total', Consumo = 'EC y EFC. Créditos a OSR por finalidades. Otra financiación a hogares por funciones de gasto. Consumo. Total',
          Vivienda = 'EC y EFC. Créditos a OSR por finalidades. Otra financiación a hogares por funciones de gasto. Adquisición vivienda. Total', 'Rehabilitación vivienda' = 'EC y EFC. Créditos a OSR por finalidades. Otra financiación a hogares por funciones de gasto. Rehabilitación vivienda',
          Resto = 'EC y EFC. Créditos a OSR por finalidades. Otra financiación a hogares por funciones de gasto. Resto', 'Dudosos total' = 'EC y EFC. Créditos dudosos a OSR por finalidades. Otras financiaciones a hogares por funciones de gasto. Total',
          'Dudosos vivienda' = 'EC y EFC. Créditos dudosos a OSR por finalidades. Otras financ. a hogares por funciones de gasto. Adquisición vivienda. Total', 'Dudosos rehabilitación vivienda' = 'EC y EFC. Créditos dudosos a OSR por finalidades. Otras financ. a hogares por funciones de gasto. Rehabilitación vivienda',
          'Dudosos consumo' = 'EC y EFC. Créditos dudosos a OSR por finalidades. Otras financiaciones a hogares por funciones de gasto. Consumo. Total', 'Dudosos resto' = 'EC y EFC. Créditos dudosos a OSR por finalidades. Otras financiaciones a hogares por funciones de gasto. Resto') %>%
  mutate(across(where(is.numeric), ~ . / 1000))

# Primero, filtramos el dataframe para mantener solo las fechas desde junio de 2007
# Luego, aplicamos el formato deseado a la columna de fechas y quitamos los puntos

dudosidad_df <- dudosidad_completo_df %>%
  filter(fecha >= as.Date("2007-06-01")) %>%
  select(fecha, Total, Consumo, Vivienda, 'Rehabilitación vivienda', 'Resto','Dudosos vivienda', 'Dudosos consumo', 'Dudosos rehabilitación vivienda', 'Dudosos resto', 'Dudosos total') %>%
  mutate('Ratio dudosidad total' = as.numeric(`Dudosos total`) / as.numeric(Total)*100) %>%
  mutate('Ratio dudosidad vivienda' = as.numeric(`Dudosos vivienda`) / as.numeric(Vivienda)*100) %>%
  mutate('Ratio dudosidad consumo' = as.numeric(`Dudosos consumo`) / as.numeric(Consumo)*100) |> 
  pivot_longer(cols = -c("fecha"), names_to="nombres", values_to="valores") 
  # mutate(label = scales::number_format(scale=1, accuracy=0.01, suffix="%", decimal.mark=",", big.mark="")(valores)) |> 
  # mutate(columna_excel = scales::number_format(scale=0.01, accuracy=0.0001, suffix="", decimal.mark=".", big.mark="")(valores) |> as.double()) 

# dudosidad_df |> select(fecha, nombres, columna_excel) |> 
#   filter(nombres %in% c('Ratio dudosidad total','Ratio dudosidad vivienda','Ratio dudosidad consumo')) |>
#   pivot_wider(id_cols = c(fecha), names_from="nombres", values_from="columna_excel") |> 
#   writexl::write_xlsx("prueba.xlsx")

# dudosidad_df <- dudosidad_completo_df %>%
#   filter(fecha >= as.Date("2007-06-01")) %>%
#   select(fecha, Total, Consumo, Vivienda, 'Rehabilitación vivienda', 'Resto','Dudosos vivienda', 'Dudosos consumo', 'Dudosos rehabilitación vivienda', 'Dudosos resto', 'Dudosos total') %>% 
#   mutate(
#     'Ratio dudosidad total' = paste0(round(as.numeric(`Dudosos total`) / as.numeric(Total)*100, 2), "%"),
#     'Ratio dudosidad vivienda' = paste0(round(as.numeric(`Dudosos vivienda`) / as.numeric(Vivienda)*100, 2), "%"),
#     'Ratio dudosidad consumo' = paste0(round(as.numeric(`Dudosos consumo`) / as.numeric(Consumo)*100, 2), "%")
#   ) %>% 
#   na.omit()

base_100_dudosidad_df <- dudosidad_df %>%
  pivot_wider(id_cols = c("fecha"), names_from="nombres", values_from="valores" ) |>
  filter(fecha >= as.Date("2019-03-01")) %>%
  select(#-fecha,
         -'Rehabilitación vivienda', 
         -'Resto', 
         -'Dudosos rehabilitación vivienda', 
         -'Dudosos resto', 
         -'Ratio dudosidad total', 
         -'Ratio dudosidad vivienda',
         -'Ratio dudosidad consumo') %>%
  mutate(across(where(is.numeric), ~ . / first(.)) * 100) %>%
  rename('Total base 100' = Total, 
         'Vivienda base 100' = Vivienda,
         'Consumo base 100' = Consumo, 
         'Dudosos vivienda base 100' = 'Dudosos vivienda', 
         'Dudosos consumo base 100' = 'Dudosos consumo', 
         'Dudosos total base 100' = 'Dudosos total')

dudosidad_hogares_df <- merge(dudosidad_df |> pivot_wider(id_cols = c("fecha"), names_from="nombres", values_from="valores"), 
                              base_100_dudosidad_df, all.x = TRUE) 
  

# Guardar datos ----
dfs_a_guardar <- ls()[ls() |> stringr::str_detect("_df")]

for(df_a_guardar in dfs_a_guardar) {
  message("Guardando dataframe: ", df_a_guardar)
  feather::write_feather(get(df_a_guardar),
                         paste0(datos_path,
                                "dataframes/",
                                df_a_guardar,
                                ".feather"
                                ))
}



                      
                                                             