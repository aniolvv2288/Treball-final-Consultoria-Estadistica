############################################################
## Transformació de variables
############################################################

dades <- readRDS("dfcomplet.rds")


############################################################
## Eliminació de variables poc informatives
############################################################

dades <- subset(
  dades,
  select = -c(proces, volta, ccaa, municipi, tipus, candidatura, Districte)
)


############################################################
## Diccionari de partits (unificació de sigles)
############################################################

diccionari_partits <- tribble(
  ~sigla_original,             ~sigla_std,
  "C's",                       "CS",
  "Cs",                        "CS",
  "ERC",                       "ERC",
  "ERC-CATSI",                 "ERC",
  "ERC-CATSÍ",                 "ERC",
  "ERC-SOBIRAN",               "ERC",
  "ERC-SOBIRANISTES",          "ERC",
  "JxCAT-JUNTS",               "CONV.-JxCAT",
  "JxCAT - JUNTS",             "CONV.-JxCAT",
  "CDC",                       "CONV.-JxCAT",
  "CNV",                       "CONV.-JxCAT",
  "DL",                        "CONV.-JxCAT",
  "RECORTES CE",               "RECORTES CERO",
  "RECORTES CERO",             "RECORTES CERO",
  "RECORTES CERO-GRUPO VERDE", "RECORTES CERO",
  "RECORTES CERO-GV",          "RECORTES CERO",
  "unio.cat",                  "UNIO",
  "Unio.Cat",                  "UNIO",
  "ECP",                       "ECP-SUMAR",
  "ECP-GUANYEM",               "ECP-SUMAR",
  "EN COMÚ",                   "ECP-SUMAR",
  "SUMAR - ECP",               "ECP-SUMAR",
  "MAS PAÍS",                  "EPC-SUMAR",
  "PSC",                       "PSC-PSOE",
  "PSC-PSOE",                  "PSC-PSOE"
)

dades <- dades %>%
  left_join(diccionari_partits, by = c("sigles" = "sigla_original")) %>%
  mutate(sigles_final = coalesce(sigla_std, sigles)) %>%
  select(-sigles, -sigla_std) %>%
  rename(sigles = sigles_final)


############################################################
## Creació de la variable elecció (any + mes)
############################################################

dades$eleccio <- paste0(
  dades$any, "_",
  ifelse(dades$mes == "04", "abr",
         ifelse(dades$mes == "06", "jun",
                ifelse(dades$mes == "07", "jul",
                       ifelse(dades$mes == "11", "nov",
                              ifelse(dades$mes == "12", "des", NA)))))
)

nivells_eleccio <- unique(dades$eleccio[order(dades$any, dades$mes)])

dades$eleccio <- factor(
  dades$eleccio,
  levels = nivells_eleccio,
  ordered = TRUE
)


############################################################
## Identificació dels 10 partits més rellevants
############################################################

vots_per_partit <- dades %>%
  group_by(sigles) %>%
  summarise(vots_totals = sum(vots, na.rm = TRUE), .groups = "drop")

top10_partits <- vots_per_partit %>%
  arrange(desc(vots_totals)) %>%
  slice_head(n = 10)

percentatge_top10 <- sum(top10_partits$vots_totals) /
  sum(vots_per_partit$vots_totals) * 100

resum_top10 <- tibble(
  grup = c("Top 10 partits", "Resta de partits"),
  vots = c(
    sum(top10_partits$vots_totals),
    sum(vots_per_partit$vots_totals) - sum(top10_partits$vots_totals)
  )
) %>%
  mutate(percentatge = vots / sum(vots) * 100)

ggplot(resum_top10, aes(x = grup, y = vots, fill = grup)) +
  geom_col() +
  geom_text(aes(label = paste0(round(percentatge, 1), "%")), vjust = -0.5) +
  labs(
    title = "Pes electoral del Top 10 de partits",
    x = "",
    y = "Nombre total de vots"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


############################################################
## Filtratge: només top 10 partits
############################################################

dades <- dades %>%
  filter(sigles %in% top10_partits$sigles)


############################################################
## Variables d’ideologia política
############################################################

diccionari_ideologia <- tribble(
  ~sigles,         ~ideologia,
  "ERC",           "Esquerra",
  "ECP-SUMAR",     "Esquerra",
  "CUP-PR",        "Esquerra",
  "PACMA",         "Esquerra",
  "FRONT REPUB",   "Esquerra",
  "PSC-PSOE",      "Esquerra",
  "CS",            "Centre",
  "CONV.-JxCAT",   "Centre",
  "PP",            "Dreta",
  "VOX",           "Dreta"
)

dades <- dades %>%
  left_join(diccionari_ideologia, by = "sigles")

dades$ideologia <- factor(
  dades$ideologia,
  levels = c("Esquerra", "Centre", "Dreta"),
  ordered = TRUE
)

diccionari_independentisme <- tribble(
  ~sigles,         ~independentista,
  "ERC",           "Sí",
  "CONV./JxCAT",   "Sí",
  "CUP-PR",        "Sí",
  "FRONT REPUB",   "Sí",
  "PSC-PSOE",      "No",
  "PP",            "No",
  "CS",            "No",
  "VOX",           "No",
  "ECP/SUMAR",     "No",
  "PACMA",         "No"
)

dades <- dades %>%
  left_join(diccionari_independentisme, by = "sigles")


############################################################
## Correcció del tipus de variables
############################################################

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
    Codi_municipi = factor(Codi_municipi),
  )


############################################################
## Factorització de variables socioeconòmiques
############################################################

## Edat mitjana
dades$fac_Edad_media <- cut(
  dades[["Edad media de la población"]],
  breaks = c(-Inf, 40, 45, Inf),
  labels = c("Secció jove", "Secció d'edat mitjana", "Secció envellida"),
  include.lowest = TRUE
)

## Població
cuts_pob <- quantile(dades$Población, probs = c(0, .25, .5, .75, 1), na.rm = TRUE)

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

## Mida mitjana de la llar
cuts_hogar <- quantile(
  dades[["Tamaño medio del hogar"]],
  probs = c(0, 1/3, 2/3, 1),
  na.rm = TRUE
)

dades$fac_Tamaño_medio_del_hogar <- cut(
  dades[["Tamaño medio del hogar"]],
  breaks = cuts_hogar,
  include.lowest = TRUE,
  labels = c("Llars petites", "Llars mitjanes", "Llars grans")
)

## Renda per persona
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

## Renda per llar
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

## Renda per unitat de consum (mitjana)
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

## Renda per unitat de consum (mediana)
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

# saveRDS(dades, "df_final.rds")