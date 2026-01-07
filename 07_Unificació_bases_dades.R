################################################################################
# ----- CREACIÓ DE LA BASE DE DADES UNINT LES PROVÍNCIES -----
################################################################################

dades_barcelona <- readRDS("dades_barcelona.rds")
dades_girona <- readRDS("dades_girona.rds")
dades_tarragona <- readRDS("dades_tarragona.rds")
dades_lleida <- readRDS("dades_lleida.rds")
resultatsvots <- readRDS("resultats_totals.rds")

dades_catalunya <- bind_rows(dades_barcelona, dades_girona, dades_lleida, dades_tarragona)

resultatsvots <- resultatsvots %>%
  mutate(
    seccio = as.character(seccio),
    any  = as.character(any)
  )

dades_catalunya <- dades_catalunya %>%
  mutate(
    Seccio  = as.character(Seccio),
    Periodo = as.character(Periodo)
  )

dfcomplet <- resultatsvots %>%
  left_join(
    dades_catalunya,
    by = c("seccio" = "Seccio", "any" = "Periodo")
  )


#passar NaN a NA

sapply(dfcomplet, function(x) {
  na_count  <- sum(is.na(x) & !is.nan(x))
  nan_count <- sum(is.nan(x))
  c(n_NA = na_count, n_NaN = nan_count)
}) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "columna") %>%
  filter(n_NA + n_NaN > 0) %>%
  print()

dfcomplet[] <- lapply(dfcomplet, function(x) {
  x[is.nan(x)] <- NA
  x
})

#saveRDS(dfcomplet, "dfcomplet.rds")
