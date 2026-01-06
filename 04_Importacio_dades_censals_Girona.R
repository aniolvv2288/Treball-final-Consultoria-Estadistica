################################################################################
# ----- IMPORTACIÓ DADES DE LA PROVÍNCIA DE GIRONA -----
################################################################################

## ----- DADES DEMOGRÀFIQUES -----

poblacio_girona <- read_delim("poblacio_girona.csv", 
                                 delim = ";", escape_double = FALSE, 
                                 trim_ws = TRUE)

pob_gir1 <- poblacio_girona %>%
  mutate(
    Total_clean = Total) %>%
  
  filter(!is.na(Secciones), Secciones != "NA") %>%
  
  filter(Periodo %in% c(2015, 2016, 2019, 2023)) %>%
  
  group_by(Municipios, Distritos, Secciones, Periodo, `Indicadores demográficos`) %>%
  
  summarise(Total_clean = mean(Total_clean, na.rm = TRUE), .groups = "drop") %>%
  
  pivot_wider(
    names_from = `Indicadores demográficos`,
    values_from = Total_clean
  ) %>%
  
  mutate(
    Codi_municipi   = str_extract(Municipios, "^[0-9]+"),
    Municipi   = str_trim(str_remove(Municipios, "^[0-9]+")),
    Districte  = str_extract(Distritos, "^[0-9]+"),
    Seccio     = str_extract(Secciones, "^[0-9]+")
  ) %>%
  
  dplyr::select(-Municipios, -Distritos, -Secciones) %>%
  
  relocate(
    Municipi, Codi_municipi, Districte, Seccio, .before = 1) %>% 
  
  mutate(
    `Edad media de la población` = ifelse(`Edad media de la población` > 100,
                                          `Edad media de la población` / 10,
                                          `Edad media de la población`),
    Población = ifelse(Población %% 1 != 0,
                       Población * 1000,
                       Población)
  ) %>%
  
  dplyr::select(Municipi, Codi_municipi, Districte, Seccio, Periodo, 
                `Edad media de la población`, Población)


poblacio_girona2 <- read_delim("poblacio_girona.csv", 
                              delim = ";", escape_double = FALSE, 
                              trim_ws = TRUE, locale = locale(decimal_mark = ","))

pob_gir2 <- poblacio_girona2 %>%
  mutate(
    Total_clean = Total) %>%
  
  filter(!is.na(Secciones), Secciones != "NA") %>%
  
  filter(Periodo %in% c(2015, 2016, 2019, 2023)) %>%
  
  group_by(Municipios, Distritos, Secciones, Periodo, `Indicadores demográficos`) %>%
  
  summarise(Total_clean = mean(Total_clean, na.rm = TRUE), .groups = "drop") %>%
  
  pivot_wider(
    names_from = `Indicadores demográficos`,
    values_from = Total_clean
  ) %>%
  
  mutate(
    Codi_municipi   = str_extract(Municipios, "^[0-9]+"),
    Municipi   = str_trim(str_remove(Municipios, "^[0-9]+")),
    Districte  = str_extract(Distritos, "^[0-9]+"),
    Seccio     = str_extract(Secciones, "^[0-9]+")
  ) %>%
  
  dplyr::select(-Municipios, -Distritos, -Secciones) %>%
  
  relocate(
    Municipi, Codi_municipi, Districte, Seccio, .before = 1) %>%
  
  dplyr::select(-`Edad media de la población`, -`Población`)


pob_gir <- pob_gir1 %>%
  left_join(pob_gir2, by = c("Municipi", "Codi_municipi", "Districte", "Seccio", "Periodo"))

View(pob_gir)


## ----- DADES DE GINI I DISTRIBUCIÓ RENDA -----

gini_girona <- read_delim("gini_girona.csv", 
                             delim = ";", escape_double = FALSE, 
                             trim_ws = TRUE)

gini_gir <- gini_girona %>%
  mutate(
    Total_clean = Total %>%
      str_replace_all("\\.", "") %>%   
      str_replace(",", ".") %>%        
      na_if(".") %>%                  
      as.numeric()
  ) %>%
  
  filter(!is.na(Secciones), Secciones != "NA") %>%
  
  filter(Periodo %in% c(2015, 2016, 2019, 2023)) %>%
  
  group_by(Municipios, Distritos, Secciones, Periodo, `Índice de Gini y Distribución de la renta P80/P20`) %>%
  summarise(Total_clean = mean(Total_clean, na.rm = TRUE), .groups = "drop") %>%
  
  pivot_wider(
    names_from = `Índice de Gini y Distribución de la renta P80/P20`,
    values_from = Total_clean
  ) %>%
  
  mutate(
    Codi_municipi   = str_extract(Municipios, "^[0-9]+"),
    Municipi   = str_trim(str_remove(Municipios, "^[0-9]+")),
    Districte  = str_extract(Distritos, "^[0-9]+"),
    Seccio     = str_extract(Secciones, "^[0-9]+")
  ) %>%
  
  dplyr::select(-Municipios, -Distritos, -Secciones) %>%
  
  relocate(Municipi, Codi_municipi, Districte, Seccio, .before = 1)


## ----- FUENTE DE INGRESSOS -----

ingressos_girona <- read_delim("ingressos_girona.csv", 
                                  delim = ";", escape_double = FALSE, 
                                  trim_ws = TRUE)

ing_gir <- ingressos_girona %>%
  mutate(
    Total_clean = Total %>%
      str_replace_all("\\.", "") %>%   
      str_replace(",", ".") %>%        
      na_if(".") %>%                  
      as.numeric()
  ) %>%
  
  filter(!is.na(Secciones), Secciones != "NA") %>%
  
  filter(Periodo %in% c(2015, 2016, 2019, 2023)) %>%
  
  group_by(Municipios, Distritos, Secciones, Periodo, `Distribución por fuente de ingresos`) %>%
  summarise(Total_clean = mean(Total_clean, na.rm = TRUE), .groups = "drop") %>%
  
  pivot_wider(
    names_from = `Distribución por fuente de ingresos`,
    values_from = Total_clean
  ) %>%
  
  mutate(
    Codi_municipi   = str_extract(Municipios, "^[0-9]+"),
    Municipi   = str_trim(str_remove(Municipios, "^[0-9]+")),
    Districte  = str_extract(Distritos, "^[0-9]+"),
    Seccio     = str_extract(Secciones, "^[0-9]+")
  ) %>%
  
  dplyr::select(-Municipios, -Distritos, -Secciones) %>%
  
  relocate(Municipi, Codi_municipi, Districte, Seccio, .before = 1)


## ----- DADES DE RENDA MITJANA I MEDIANA -----

renta_girona <- read_delim("renta_girona.csv", 
                              delim = ";", escape_double = FALSE, 
                              trim_ws = TRUE)

renta_gir <- renta_girona %>%
  mutate(
    Total_clean = Total %>%
      str_replace_all("\\.", "") %>%   
      str_replace(",", ".") %>%        
      na_if(".") %>%                  
      as.numeric()
  ) %>%
  
  filter(!is.na(Secciones), Secciones != "NA") %>%
  
  filter(Periodo %in% c(2015, 2016, 2019, 2023)) %>%
  
  group_by(Municipios, Distritos, Secciones, Periodo, `Indicadores de renta media y mediana`) %>%
  summarise(Total_clean = mean(Total_clean, na.rm = TRUE), .groups = "drop") %>%
  
  pivot_wider(
    names_from = `Indicadores de renta media y mediana`,
    values_from = Total_clean
  ) %>%
  
  mutate(
    Codi_municipi   = str_extract(Municipios, "^[0-9]+"),
    Municipi   = str_trim(str_remove(Municipios, "^[0-9]+")),
    Districte  = str_extract(Distritos, "^[0-9]+"),
    Seccio     = str_extract(Secciones, "^[0-9]+")
  ) %>%
  
  dplyr::select(-Municipios, -Distritos, -Secciones) %>%
  
  relocate(Municipi, Codi_municipi, Districte, Seccio, .before = 1)


## ----- UNIFICACIÓ DADES GIRONA -----

dades_girona <- pob_gir %>%
  left_join(gini_gir,  by = c("Municipi", "Codi_municipi", "Districte", "Seccio", "Periodo")) %>%
  left_join(ing_gir,   by = c("Municipi", "Codi_municipi", "Districte", "Seccio", "Periodo")) %>%
  left_join(renta_gir, by = c("Municipi", "Codi_municipi", "Districte", "Seccio", "Periodo"))

# saveRDS(dades_girona, "dades_girona.rds")

