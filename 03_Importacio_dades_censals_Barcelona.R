################################################################################
# ----- IMPORTACIÓ DADES DE LA PROVÍNCIA DE BARCELONA -----
################################################################################

## ----- DADES DEMOGRÀFIQUES -----

poblacio_barcelona <- read_delim("poblacio_barcelona.csv", 
                                 delim = ";", escape_double = FALSE, 
                                 trim_ws = TRUE)

pob_bar <- poblacio_barcelona %>%
  mutate(
    Total_clean = Total %>%
      str_replace_all("\\.", "") %>%   
      str_replace(",", ".") %>%        
      na_if(".") %>%                  
      as.numeric()
  ) %>%
  
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
    Municipi, Codi_municipi, Districte, Seccio, .before = 1)


## ----- DADES DE GINI I DISTRIBUCIÓ RENDA -----

gini_barcelona <- read_delim("gini_barcelona.csv", 
                                 delim = ";", escape_double = FALSE, 
                                 trim_ws = TRUE)

gini_bar <- gini_barcelona %>%
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

ingressos_barcelona <- read_delim("ingressos_barcelona.csv", 
                             delim = ";", escape_double = FALSE, 
                             trim_ws = TRUE)

ing_bar <- ingressos_barcelona %>%
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

renta_barcelona <- read_delim("renta_barcelona.csv", 
                                  delim = ";", escape_double = FALSE, 
                                  trim_ws = TRUE)

renta_bar <- renta_barcelona %>%
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


## ----- UNIFICACIÓ DADES BARCELONA -----

dades_barcelona <- pob_bar %>%
  left_join(gini_bar,  by = c("Municipi", "Codi_municipi", "Districte", "Seccio", "Periodo")) %>%
  left_join(ing_bar,   by = c("Municipi", "Codi_municipi", "Districte", "Seccio", "Periodo")) %>%
  left_join(renta_bar, by = c("Municipi", "Codi_municipi", "Districte", "Seccio", "Periodo"))

# saveRDS(dades_barcelona, "dades_barcelona.rds")
