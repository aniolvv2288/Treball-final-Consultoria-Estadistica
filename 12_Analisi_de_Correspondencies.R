
################################################################################
# ----- INCORPORACIÓ DADES -----
################################################################################

dades <- readRDS("dades_finals.rds") %>% 
  na.omit()

vars_socio <- c(
  "fac_Edad_media",
  "fac_Población",
  "fac_Tamaño_medio_del_hogar",
  "fac_renda_media_uc",
  "fac_indice_gini",
  "fac_poblacio_no_espanyola"
)

noms_curts <- c(
  "fac_Edad_media_Secció d'edat mitjana" = "Edat_Mitjana",
  "fac_Edad_media_Secció envellida" = "Edat_Alta",
  "fac_Edad_media_Secció jove" = "Edat_Baixa",
  "fac_Población_Població alta" = "Pob_Alta",
  "fac_Población_Població baixa" = "Pob_Baixa",
  "fac_Población_Població mitjana-alta" = "Pob_MitjanaAlta",
  "fac_Población_Població mitjana-baixa" = "Pob_MitjanaBaixa",
  "fac_Tamaño_medio_del_hogar_Llars grans" = "Llar_Gran",
  "fac_Tamaño_medio_del_hogar_Llars mitjanes" = "Llar_Mitjana",
  "fac_Tamaño_medio_del_hogar_Llars petites" = "Llar_Petita",
  "fac_indice_gini_Alta desigualtat" = "Gini_Alt",
  "fac_indice_gini_Baixa desigualtat" = "Gini_Baix",
  "fac_indice_gini_Desigualtat mitjana" = "Gini_Mitjà",
  "fac_poblacio_no_espanyola_Alta immigració" = "Immig_Alta",
  "fac_poblacio_no_espanyola_Baixa immigració" = "Immig_Baixa",
  "fac_poblacio_no_espanyola_Mitjana immigració" = "Immig_Mitjana",
  "fac_renda_media_uc_Renda alta" = "Renda_Alta",
  "fac_renda_media_uc_Renda baixa" = "Renda_Baixa",
  "fac_renda_media_uc_Renda mitjana" = "Renda_Mitjana",
  "fac_renda_media_uc_Renda molt alta" = "Renda_MoltAlta",
  "fac_renda_media_uc_Renda molt baixa" = "Renda_MoltBaixa"
)


llista_eleccions <- c("2015_des", "2016_jun", "2019_abr", "2019_nov", "2023_jul")
noms_eleccions   <- c("Desembre 2015", "Juny 2016", "Abril 2019", 
                      "Novembre 2019", "Juliol 2023")

################################################################################
# ----- CA I RESULTATS -----
################################################################################

for (i in seq_along(llista_eleccions)) {
  
  eleccio_id  <- llista_eleccions[i]
  eleccio_nom <- noms_eleccions[i]
  
  message("------------------------------------------------------------")
  message("Processant elecció: ", eleccio_nom)
  message("------------------------------------------------------------")
  
  # ------------------------------
  # Filtrar dades
  # ------------------------------
  dades_e <- dades %>% 
    filter(eleccio == eleccio_id) %>%
    select(eleccio, seccio, sigles, vots, all_of(vars_socio))
  
  # ------------------------------
  # Construir matriu CA
  # ------------------------------
  matriu_ca <- dades_e %>%
    pivot_longer(cols = all_of(vars_socio),
                 names_to = "variable",
                 values_to = "categoria") %>%
    unite("var_cat", variable, categoria, sep = "_") %>%
    group_by(sigles, var_cat) %>%
    summarise(vots_total = sum(vots), .groups = "drop") %>%
    pivot_wider(names_from = var_cat,
                values_from = vots_total,
                values_fill = 0)
  
  colnames(matriu_ca) <- recode(colnames(matriu_ca), !!!noms_curts)
  
  matriu_ca <- matriu_ca %>% 
    column_to_rownames("sigles")
  
  matriu_ca <- matriu_ca[rowSums(matriu_ca) > 0, ]
  
  # ------------------------------
  # TEST χ² + BALLOONPLOT
  # ------------------------------
  dt <- as.table(as.matrix(matriu_ca))
  
  balloonplot(dt,
              main = paste("Balloonplot -", eleccio_nom),
              xlab = "",
              ylab = "",
              label = FALSE,
              show.margins = FALSE)
  
  chisq <- chisq.test(matriu_ca)
  print(chisq)
  
  # ------------------------------
  # CA
  # ------------------------------
  
  res.ca <- CA(matriu_ca, graph = FALSE)
  
  print(
    fviz_ca_biplot(
      res.ca,
      repel = TRUE,
      label = "all",
      col.row = "black",
      col.col = "blue",
      title = paste("CA - Partits i factors socioeconòmics (", eleccio_nom, ")", 
                    sep = "")
    )
  )
  
  print(
    fviz_screeplot(
      res.ca,
      addlabels = TRUE,
      ylim = c(0, 85),
      title = paste("Screeplot -", eleccio_nom)
    )
  )
  
  print(
    fviz_ca_row(
      res.ca,
      col.row = "cos2",
      repel = TRUE,
      title = paste("Qualitat representació partits -", eleccio_nom)
    ) +
      scale_color_gradientn(
        colours = c("#00AFBB", "#E7B800", "#FC4E07"), 
        limits = c(0, 1),
        values = c(0, 0.5, 1)
      )
  )
  
  print(
    fviz_ca_col(
      res.ca,
      col.col = "cos2",
      repel = TRUE,
      title = paste("Qualitat representació variables -", eleccio_nom)
    ) +
      scale_color_gradientn(
        colours = c("#00AFBB", "#E7B800", "#FC4E07"), 
        limits = c(0, 1),
        values = c(0, 0.5, 1)
      )
  )
}

