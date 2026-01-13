############################################
## TRANSFORMACIÓ DE VARIABLES
############################################


# Llegim dades
dades <- readRDS("dfcomplet.rds")


############################################
## ELIMINACIÓ DE VARIABLES NO INFORMATIVES
############################################

dades <- subset(
  dades,
  select = -c(proces, volta, ccaa, municipi, candidatura, Districte, tipus)
)


############################################
## DICCIONARI DE PARTITS (UNIFICACIÓ SIGLES)
############################################

diccionari_partits <- tribble(
  ~sigla_original,            ~sigla_std,
  "C's",                      "CS",
  "Cs",                       "CS",
  "ERC",                      "ERC",
  "ERC-CATSI",                "ERC",
  "ERC-CATSÍ",                "ERC",
  "ERC-SOBIRAN",              "ERC",
  "ERC-SOBIRANISTES",         "ERC",
  "JxCAT-JUNTS",              "CONV./JxCAT",
  "JxCAT - JUNTS",            "CONV./JxCAT",
  "CDC",                      "CONV./JxCAT",
  "CNV",                      "CONV./JxCAT",
  "DL",                       "CONV./JxCAT",
  "RECORTES CE",              "RECORTES CERO",
  "RECORTES CERO",            "RECORTES CERO",
  "RECORTES CERO-GRUPO VERDE","RECORTES CERO",
  "RECORTES CERO-GV",         "RECORTES CERO",
  "unio.cat",                 "UNIO",
  "Unio.Cat",                 "UNIO",
  "ECP",                      "ECP/SUMAR",
  "ECP-GUANYEM",              "ECP/SUMAR",
  "EN COMÚ",                  "ECP/SUMAR",
  "SUMAR - ECP",              "ECP/SUMAR",
  "MAS PAÍS",                 "ECP/SUMAR",
  "PSC",                      "PSC/PSOE",
  "PSC-PSOE",                 "PSC/PSOE",
  "CUP-PR",                   "CUP/PR"
)

dades <- dades %>%
  left_join(diccionari_partits, by = c("sigles" = "sigla_original")) %>%
  mutate(sigles_final = coalesce(sigla_std, sigles)) %>%
  select(-sigles, -sigla_std) %>%
  rename(sigles = sigles_final)


############################################
## VARIABLE ELECCIÓ (ANY + MES)
############################################

dades$eleccio <- paste0(
  dades$any, "_",
  ifelse(dades$mes == "04", "abr",
         ifelse(dades$mes == "06", "jun",
                ifelse(dades$mes == "07", "jul",
                       ifelse(dades$mes == "11", "nov",
                              ifelse(dades$mes == "12", "des", NA)))))
)

nivells_eleccio <- unique(dades$eleccio[order(dades$any, dades$mes)])
dades$eleccio <- factor(dades$eleccio, levels = nivells_eleccio, ordered = TRUE)


############################################
## SELECCIÓ DELS 8 PARTITS MÉS RELLEVANTS
############################################

vots_per_partit <- dades %>%
  group_by(sigles) %>%
  summarise(vots_totals = sum(vots, na.rm = TRUE), .groups = "drop")

top8_partits <- vots_per_partit %>%
  arrange(desc(vots_totals)) %>%
  slice_head(n = 8)

dades <- dades %>%
  filter(sigles %in% top8_partits$sigles)


############################################
## VARIABLES IDEOLÒGIQUES
############################################

# Eix esquerra-dreta
diccionari_ideologia <- tribble(
  ~sigles,         ~ideologia,
  "ERC",           "Esquerra",
  "ECP/SUMAR",     "Esquerra",
  "CUP/PR",        "Esquerra",
  "PSC/PSOE",      "Esquerra",
  "CS",            "Dreta",
  "CONV./JxCAT",   "Dreta",
  "PP",            "Dreta",
  "VOX",           "Dreta"
)

dades <- dades %>%
  left_join(diccionari_ideologia, by = "sigles")

dades$ideologia <- factor(dades$ideologia, levels = c("Esquerra", "Dreta"))

# Independentisme
diccionari_independentisme <- tribble(
  ~sigles,         ~independentista,
  "ERC",           "Sí",
  "CONV./JxCAT",   "Sí",
  "CUP/PR",        "Sí",
  "PSC/PSOE",      "No",
  "PP",            "No",
  "CS",            "No",
  "VOX",           "No",
  "ECP/SUMAR",     "No"
)

dades <- dades %>%
  left_join(diccionari_independentisme, by = "sigles")

dades$independentista <- as.factor(dades$independentista)


############################################
## CORRECCIÓ DE TIPUS DE VARIABLE
############################################

dades <- dades %>%
  mutate(
    any = as.numeric(any),
    mes = as.numeric(mes),
    provincia     = factor(provincia),
    districte     = factor(districte),
    seccio        = factor(seccio),
    sigles        = factor(sigles),
    nom_partit    = factor(nom_partit),
    Municipi      = factor(Municipi),
    Codi_municipi = factor(Codi_municipi)
  )


############################################
## FACTORITZACIÓ DE VARIABLES NUMÈRIQUES
############################################

# Edat mitjana
dades$fac_Edad_media <- cut(
  dades$`Edad media de la población`,
  breaks = c(-Inf, 40, 45, Inf),
  labels = c("Secció jove", "Secció d'edat mitjana", "Secció envellida"),
  include.lowest = TRUE
)

# Població (quartils)
cuts_pob <- quantile(dades$Población, probs = c(0, .25, .5, .75, 1), na.rm = TRUE)
dades$fac_Población <- cut(
  dades$Población,
  breaks = cuts_pob,
  include.lowest = TRUE,
  labels = c("Població baixa", "Població mitjana-baixa",
             "Població mitjana-alta", "Població alta")
)

# Mida mitjana de la llar (tercils)
cuts_hogar <- quantile(dades$`Tamaño medio del hogar`,
                       probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

dades$fac_Tamaño_medio_del_hogar <- cut(
  dades$`Tamaño medio del hogar`,
  breaks = cuts_hogar,
  include.lowest = TRUE,
  labels = c("Llars petites", "Llars mitjanes", "Llars grans")
)

# Rendes (quintils)
cuts_renda_persona <- quantile(dades$`Renta bruta media por persona`,
                               probs = seq(0, 1, 0.2), na.rm = TRUE)

dades$fac_renda_bruta_persona <- cut(
  dades$`Renta bruta media por persona`,
  breaks = cuts_renda_persona,
  include.lowest = TRUE,
  labels = c("Renda molt baixa", "Renda baixa",
             "Renda mitjana", "Renda alta", "Renda molt alta")
)

cuts_renda_llar <- quantile(dades$`Renta bruta media por hogar`,
                            probs = seq(0, 1, 0.2), na.rm = TRUE)

dades$fac_renda_bruta_llar <- cut(
  dades$`Renta bruta media por hogar`,
  breaks = cuts_renda_llar,
  include.lowest = TRUE,
  labels = c("Renda molt baixa", "Renda baixa",
             "Renda mitjana", "Renda alta", "Renda molt alta")
)

cuts_renda_uc <- quantile(dades$`Media de la renta por unidad de consumo`,
                          probs = seq(0, 1, 0.2), na.rm = TRUE)

dades$fac_renda_media_uc <- cut(
  dades$`Media de la renta por unidad de consumo`,
  breaks = cuts_renda_uc,
  include.lowest = TRUE,
  labels = c("Renda molt baixa", "Renda baixa",
             "Renda mitjana", "Renda alta", "Renda molt alta")
)

cuts_mediana_uc <- quantile(dades$`Mediana de la renta por unidad de consumo`,
                            probs = seq(0, 1, 0.2), na.rm = TRUE)

dades$fac_renda_mediana_uc <- cut(
  dades$`Mediana de la renta por unidad de consumo`,
  breaks = cuts_mediana_uc,
  include.lowest = TRUE,
  labels = c("Renda molt baixa", "Renda baixa",
             "Renda mitjana", "Renda alta", "Renda molt alta")
)

# Desigualtat (P80/P20) – tercils
cuts_p80p20 <- quantile(dades$`Distribución de la renta P80/P20`,
                        probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

dades$fac_P80_P20 <- cut(
  dades$`Distribución de la renta P80/P20`,
  breaks = cuts_p80p20,
  include.lowest = TRUE,
  labels = c("Baixa desigualtat", "Desigualtat mitjana", "Alta desigualtat")
)

# Immigració (població no espanyola – tercils)
dades$Porcentaje_poblacion_no_española <- 
  100 - dades$`Porcentaje de población española`

cuts_immigracio <- quantile(dades$Porcentaje_poblacion_no_española,
                            probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

dades$fac_poblacio_no_espanyola <- cut(
  dades$Porcentaje_poblacion_no_española,
  breaks = cuts_immigracio,
  include.lowest = TRUE,
  labels = c("Baixa immigració", "Mitjana immigració", "Alta immigració")
)
