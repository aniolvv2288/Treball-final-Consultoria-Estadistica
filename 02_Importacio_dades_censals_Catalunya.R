################################################################################
# FITXERS
################################################################################

fitxers <- c(
  "10020003.DAT", "10020403.DAT", "10020803.DAT", "10021111.DAT",
  "10021512.DAT", "10021606.DAT", "10021904.DAT", "10021911.DAT",
  "10022307.DAT", "10029603.DAT"
)

candidatures <- sub("^10", "03", fitxers)

################################################################################
# FUNCIONS
################################################################################

processa_resultats <- function(path) {
  lines <- readLines(path)
  
  df <- data.frame(
    proces      = substr(lines, 1, 2),
    any         = substr(lines, 3, 6),
    mes         = substr(lines, 7, 8),
    volta       = substr(lines, 9, 9),
    ccaa        = substr(lines, 10, 11),
    provincia   = substr(lines, 12, 13),
    municipi    = substr(lines, 12, 16),
    districte   = substr(lines, 12, 18),
    seccio      = substr(lines, 12, 21),
    tipus       = substr(lines, 23, 23),
    candidatura = substr(lines, 24, 29),
    vots        = as.integer(substr(lines, 30, 36)),
    stringsAsFactors = FALSE
  )
  
  df <- df[df$provincia %in% c("08", "17", "25", "43"), ]
  df <- df[df$any %in% c("2023", "2019", "2016", "2015"), ]
  
  return(df)
}

processa_candidatures <- function(path) {
  lines <- readLines(path, encoding = "latin1")
  lines <- iconv(lines, from = "latin1", to = "UTF-8")
  
  df <- data.frame(
    candidatura = substr(lines, 9, 14),
    sigles      = trimws(substr(lines, 15, 64)),
    nom_partit  = trimws(substr(lines, 65, 214)),
    stringsAsFactors = FALSE
  )
  
  return(unique(df))  # 🔑 important
}

################################################################################
# PROCESSAMENT PARELL A PARELL
################################################################################

llista_df <- vector("list", length(fitxers))

for (i in seq_along(fitxers)) {
  
  df_resultats <- processa_resultats(fitxers[i])
  df_partits   <- processa_candidatures(candidatures[i])
  
  df_mergejat <- merge(
    df_resultats,
    df_partits,
    by = "candidatura",
    all.x = TRUE
  )
  
  llista_df[[i]] <- df_mergejat
}

################################################################################
# UNIR-HO TOT
################################################################################

df_total <- do.call(rbind, llista_df)

saveRDS(df_total, "resultats_totals.rds")


