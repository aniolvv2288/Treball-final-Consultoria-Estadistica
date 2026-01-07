################################################################################
# ----- IMPORTACIÓ I EMMAGATZEMATGE DE LES DADES CENSALS HISTÒRIQUES -----
################################################################################

fitxers <- c("10020003.DAT", "10020403.DAT", "10020803.DAT", "10021111.DAT",
  "10021512.DAT", "10021606.DAT", "10021904.DAT", "10021911.DAT",
  "10022307.DAT", "10029603.DAT")

processa <- function(path) {
  lines <- readLines(path)
  df <- data.frame(
    proces = substr(lines, 1, 2),
    any = substr(lines, 3, 6),
    mes = substr(lines, 7, 8),
    volta = substr(lines, 7, 8),
    ccaa = substr(lines, 10, 11),
    provincia = substr(lines, 12, 13),
    municipi = substr(lines, 12, 16),
    districte = substr(lines, 12, 18),
    seccio = substr(lines, 12, 21),
    tipus = substr(lines, 23, 23),
    candidatura = substr(lines, 24, 29),
    vots = as.integer(substr(lines, 30, 36)),
    stringsAsFactors = FALSE
  )
  
  df <- df[df$provincia %in% c("08", "17", "25","43"), ]
  df <- df[df$any %in% c("2023", "2019", "2016", "2015"), ]
  
}

for (f in fitxers) {
  df <- processa(f)
  nom_rds <- sub("\\.DAT$", ".rds", f)
  #saveRDS(df, nom_rds)
}

llista_df <- lapply(fitxers, processa)
df_total <- do.call(rbind, llista_df)

canditatures <- sub("^10", "03", fitxers)

processacanditatures <- function(path) {
  lines <- readLines(path, encoding = "latin1")
  lines <- iconv(lines, from = "latin1", to = "UTF-8")
  df <- data.frame(
    candidatura = substr(lines, 9, 14),
    sigles = trimws(substr(lines, 15, 64)),
    nom_partit = trimws(substr(lines, 65, 214)),
    stringsAsFactors = FALSE
  )
  return(df)
}

llista_partits <- lapply(canditatures, processacanditatures)
df_partits <- do.call(rbind, llista_partits)

df_total <- merge(df_total, df_partits, by = "candidatura", all.x = TRUE)

#saveRDS(df_total, "resultats_totals.rds")

