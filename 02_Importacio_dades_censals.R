# Obtenció i emmagatzematge de les dades censals històriques

library(data.table)
library(dplyr)

fitxers <- c("10020003.DAT", "10020403.DAT", "10020803.DAT", "10021111.DAT",
  "10021512.DAT", "10021606.DAT", "10021904.DAT", "10021911.DAT",
  "10022307.DAT", "10029603.DAT")

processa <- function(path) {
  lines <- readLines(path)
  
  df <- data.frame(
    proces      = substr(lines, 1, 2),
    any         = substr(lines, 3, 6),
    mes         = substr(lines, 7, 8),
    ccaa        = substr(lines, 9, 10),
    provincia   = substr(lines, 11, 12),
    municipi    = substr(lines, 13, 15),
    districte   = substr(lines, 16, 18),
    seccio      = substr(lines, 19, 21),
    tipus       = substr(lines, 23, 23),
    candidatura = substr(lines, 24, 29),
    vots        = as.integer(substr(lines, 30, 36)),
    stringsAsFactors = FALSE
  )
  
  df <- df[df$provincia %in% c("08", "17", "25", "43"), ]
  
  df$codi <- paste0(
    sprintf("%02s", df$provincia),
    sprintf("%03s", df$municipi),
    sprintf("%02s", df$districte),
    sprintf("%03s", df$seccio)
  )
  
  df
}

for (f in fitxers) {
  df <- processa(f)
  nom_rds <- sub("\\.DAT$", ".rds", f)
  saveRDS(df, nom_rds)
}

llista_df <- lapply(fitxers, processa)
df_total <- do.call(rbind, llista_df)

saveRDS(df_total, "resultats_totals.rds")

View(df_total)


