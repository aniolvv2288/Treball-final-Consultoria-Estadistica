############################################################
## 01_transformacio_variables.R
## Transformació i preparació de la base de dades electoral
############################################################

## 1. Lectura de dades
dades <- readRDS("dfcomplet.rds")

## 2. Eliminació de variables no informatives
dades <- subset(
  dades,
  select = -c(proces, volta, ccaa, municipi)
)

## 3. Creació de la variable eleccio (any_mes abreujat)
dades$eleccio <- paste0(
  dades$any, "_",
  ifelse(dades$mes == "04", "abr",
         ifelse(dades$mes == "06", "jun",
                ifelse(dades$mes == "07", "jul",
                       ifelse(dades$mes == "11", "nov",
                              ifelse(dades$mes == "12", "des", NA)))))
)

## Ordenació temporal del factor eleccio
nivells_eleccio <- unique(dades$eleccio[order(dades$any, dades$mes)])
dades$eleccio <- factor(dades$eleccio, levels = nivells_eleccio, ordered = TRUE)

## 4. Correcció del tipus de variables
dades <- dplyr::mutate(
  dades,
  # Numèriques (temporals)
  any = as.numeric(any),
  mes = as.numeric(mes),
  
  # Factors
  candidatura  = factor(candidatura),
  provincia    = factor(provincia),
  districte    = factor(districte),
  seccio       = factor(seccio),
  tipus        = factor(tipus),
  sigles       = factor(sigles),
  nom_partit   = factor(nom_partit),
  Municipi     = factor(Municipi),
  Codi_municipi= factor(Codi_municipi),
  Districte    = factor(Districte)
)

## 5. Versió factoritzada de variables socio-demogràfiques

## 5.1 Edat mitjana (criteri social)
dades$fac_Edad_media <- cut(
  dades[["Edad media de la población"]],
  breaks = c(-Inf, 40, 45, Inf),
  labels = c(
    "Secció jove",
    "Secció d'edat mitjana",
    "Secció envellida"
  ),
  include.lowest = TRUE
)

## 5.2 Població (quartils)
cuts_pob <- quantile(
  dades$Población,
  probs = c(0, 0.25, 0.5, 0.75, 1),
  na.rm = TRUE
)

dades$fac_Población <- cut(
  dades$Población,
  breaks = cuts_pob,
  include.lowest = TRUE,
  labels = c(
    "Població baixa",
    "Població mitjana-baixa",
    "Població mitjana-alta",
    "Població alta"
  )
)

## 5.3 Mida mitjana de la llar (tercils)
cuts_hogar <- quantile(
  dades[["Tamaño medio del hogar"]],
  probs = c(0, 1/3, 2/3, 1),
  na.rm = TRUE
)

dades$fac_Tamaño_medio_del_hogar <- cut(
  dades[["Tamaño medio del hogar"]],
  breaks = cuts_hogar,
  include.lowest = TRUE,
  labels = c(
    "Llars petites",
    "Llars mitjanes",
    "Llars grans"
  )
)

## 5.4 Renda bruta mitjana per persona (quintils)
cuts_renda_persona <- quantile(
  dades[["Renta bruta media por persona"]],
  probs = seq(0, 1, 0.2),
  na.rm = TRUE
)

dades$fac_renda_bruta_persona <- cut(
  dades[["Renta bruta media por persona"]],
  breaks = cuts_renda_persona,
  include.lowest = TRUE,
  labels = c(
    "Renda molt baixa",
    "Renda baixa",
    "Renda mitjana",
    "Renda alta",
    "Renda molt alta"
  )
)

## 5.5 Renda bruta mitjana per llar (quintils)
cuts_renda_llar <- quantile(
  dades[["Renta bruta media por hogar"]],
  probs = seq(0, 1, 0.2),
  na.rm = TRUE
)

dades$fac_renda_bruta_llar <- cut(
  dades[["Renta bruta media por hogar"]],
  breaks = cuts_renda_llar,
  include.lowest = TRUE,
  labels = c(
    "Renda molt baixa",
    "Renda baixa",
    "Renda mitjana",
    "Renda alta",
    "Renda molt alta"
  )
)

## 5.6 Renda mitjana per unitat de consum (quintils)
cuts_renda_uc <- quantile(
  dades[["Media de la renta por unidad de consumo"]],
  probs = seq(0, 1, 0.2),
  na.rm = TRUE
)

dades$fac_renda_media_uc <- cut(
  dades[["Media de la renta por unidad de consumo"]],
  breaks = cuts_renda_uc,
  include.lowest = TRUE,
  labels = c(
    "Renda molt baixa",
    "Renda baixa",
    "Renda mitjana",
    "Renda alta",
    "Renda molt alta"
  )
)

## 5.7 Mediana de la renda per unitat de consum (quintils)
cuts_mediana_uc <- quantile(
  dades[["Mediana de la renta por unidad de consumo"]],
  probs = seq(0, 1, 0.2),
  na.rm = TRUE
)

dades$fac_renda_mediana_uc <- cut(
  dades[["Mediana de la renta por unidad de consumo"]],
  breaks = cuts_mediana_uc,
  include.lowest = TRUE,
  labels = c(
    "Renda molt baixa",
    "Renda baixa",
    "Renda mitjana",
    "Renda alta",
    "Renda molt alta"
  )
)

## 6. Guardar base de dades transformada
saveRDS(dades, "df_transformada.rds")
