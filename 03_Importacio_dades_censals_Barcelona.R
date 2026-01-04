################################################################################
# ----- IMPORTACIÓ DADES POBLACIONALS DE LA PROVÍNCIA DE BARCELONA -----
################################################################################

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

