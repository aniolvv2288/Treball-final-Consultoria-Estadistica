
################################################################################
# ----- INCORPORACIÓ DADES I MAPES.shp -----
################################################################################

dades <- readRDS("dades_finals.rds")

seccions_2015 <- st_read(here("Fitxers_Mapes", "bseccenv10sh1f1_20150101_2.shp"))
seccions_2015 <- seccions_2015 %>%
  mutate(
    MUNDISSEC = paste0(
      substr(MUNDISSEC, 1, 5),  
      substr(MUNDISSEC, 7, 11)   
    )
  )

seccions_2016 <- st_read(here("Fitxers_Mapes", "bseccenv10sh1f1_20160101_2.shp"))
seccions_2016 <- seccions_2016 %>%
  mutate(
    MUNDISSEC = paste0(
      substr(MUNDISSEC, 1, 5),  
      substr(MUNDISSEC, 7, 11)   
    )
  )

seccions_2019 <- st_read(here("Fitxers_Mapes", "bseccenv10sh1f1_20190101_2.shp"))
seccions_2019 <- seccions_2019 %>%
  mutate(
    MUNDISSEC = paste0(
      substr(MUNDISSEC, 1, 5),  
      substr(MUNDISSEC, 7, 11)   
    )
  )

seccions_2023 <- st_read(here("Fitxers_Mapes", "bseccenv10sh1f1_20230101_0.shp"))
seccions_2023 <- seccions_2023 %>%
  mutate(
    MUNDISSEC = paste0(
      substr(MUNDISSEC, 1, 5),  
      substr(MUNDISSEC, 7, 11)   
    )
  )


################################################################################
# ----- UNIFICACIÓ MAPES I DADES DE CADA SECCIÓ CENSAL PER ELECCIÓ -----
################################################################################

# ------------------------------
# Eleccions desembre 2015
# ------------------------------
dades_2015 <- dades %>% filter(eleccio %in% "2015_des") %>% 
  dplyr::select(seccio, Municipi, sigles, vots) %>%
  pivot_wider(
    names_from = sigles,
    values_from = vots,
    values_fill = 0
  )

mapa_2015 <- seccions_2015 %>%
  left_join(dades_2015, by = c("MUNDISSEC" = "seccio"))

# ------------------------------
# Eleccions juny 2016
# ------------------------------
dades_2016 <- dades %>% filter(eleccio %in% "2016_jun") %>% 
  dplyr::select(seccio, Municipi, sigles, vots) %>%
  pivot_wider(
    names_from = sigles,
    values_from = vots,
    values_fill = 0
  )

mapa_2016 <- seccions_2016 %>%
  left_join(dades_2016, by = c("MUNDISSEC" = "seccio"))

# ------------------------------
# Eleccions abril 2019
# ------------------------------
dades_2019 <- dades %>% 
  filter(eleccio == "2019_abr") %>% 
  select(seccio, Municipi, sigles, vots) %>%
  group_by(seccio, Municipi, sigles) %>%
  summarise(vots = sum(vots), .groups = "drop") %>%
  pivot_wider(
    names_from  = sigles,
    values_from = vots,
    values_fill = 0
  )

mapa_2019 <- seccions_2019 %>%
  left_join(dades_2019, by = c("MUNDISSEC" = "seccio"))

# ------------------------------
# Eleccions novembre 2019
# ------------------------------
dades_2019_2 <- dades %>% filter(eleccio %in% "2019_nov") %>% 
  dplyr::select(seccio, Municipi, sigles, vots) %>%
  pivot_wider(
    names_from = sigles,
    values_from = vots,
    values_fill = 0
  )

mapa_2019_2 <- seccions_2015 %>%
  left_join(dades_2019_2, by = c("MUNDISSEC" = "seccio"))

# ------------------------------
# Eleccions juliol 2023
# ------------------------------
dades_2023 <- dades %>% filter(eleccio %in% "2023_jul") %>% 
  dplyr::select(seccio, Municipi, sigles, vots) %>%
  pivot_wider(
    names_from = sigles,
    values_from = vots,
    values_fill = 0
  )

mapa_2023 <- seccions_2023 %>%
  left_join(dades_2023, by = c("MUNDISSEC" = "seccio"))


################################################################################
# ----- MAPA INTERACTIU -----
################################################################################

# ------------------------------
# Eleccions desembre 2015
# ------------------------------
partits <- setdiff(names(dades_2015), "seccio")

popup_text <- apply(
  mapa_2015[, partits] |> st_drop_geometry(),
  1,
  function(row) {
    paste0("<b>", partits, ":</b> ", row, collapse = "<br>")
  }
)

mapa_2015 <- mapa_2015 %>%
  mutate(
    info_popup = paste0(
      "<b>Municipi:</b> ", Municipi, "<br>",
      "<b>Secció censal:</b> ", MUNDISSEC, "<br><br>",
      popup_text
    )
  )

mapa_2015 <- st_transform(mapa_2015, 4326)

leaflet(mapa_2015) %>%
  addTiles() %>%
  addPolygons(
    fillColor = "steelblue",
    fillOpacity = 0.6,
    color = "white",
    weight = 0.3,
    label = ~lapply(info_popup, HTML),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "black",
      fillOpacity = 0.8,
      bringToFront = TRUE
    )
  )

# ------------------------------
# Eleccions juny 2016
# ------------------------------
partits <- setdiff(names(dades_2016), "seccio")

popup_text <- apply(
  mapa_2016[, partits] |> st_drop_geometry(),
  1,
  function(row) {
    paste0("<b>", partits, ":</b> ", row, collapse = "<br>")
  }
)

mapa_2016 <- mapa_2016 %>%
  mutate(
    info_popup = paste0(
      "<b>Municipi:</b> ", Municipi, "<br>",
      "<b>Secció censal:</b> ", MUNDISSEC, "<br><br>",
      popup_text
    )
  )

mapa_2016 <- st_transform(mapa_2016, 4326)

leaflet(mapa_2016) %>%
  addTiles() %>%
  addPolygons(
    fillColor = "steelblue",
    fillOpacity = 0.6,
    color = "white",
    weight = 0.3,
    label = ~lapply(info_popup, HTML),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "black",
      fillOpacity = 0.8,
      bringToFront = TRUE
    )
  )

# ------------------------------
# Eleccions abril 2019
# ------------------------------
partits <- setdiff(names(dades_2019), "seccio")

popup_text <- apply(
  mapa_2019[, partits] |> st_drop_geometry(),
  1,
  function(row) {
    paste0("<b>", partits, ":</b> ", row, collapse = "<br>")
  }
)

mapa_2019 <- mapa_2019 %>%
  mutate(
    info_popup = paste0(
      "<b>Municipi:</b> ", Municipi, "<br>",
      "<b>Secció censal:</b> ", MUNDISSEC, "<br><br>",
      popup_text
    )
  )

mapa_2019 <- st_transform(mapa_2019, 4326)

leaflet(mapa_2019) %>%
  addTiles() %>%
  addPolygons(
    fillColor = "steelblue",
    fillOpacity = 0.6,
    color = "white",
    weight = 0.3,
    label = ~lapply(info_popup, HTML),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "black",
      fillOpacity = 0.8,
      bringToFront = TRUE
    )
  )

# ------------------------------
# Eleccions novembre 2019
# ------------------------------
partits <- setdiff(names(dades_2019_2), "seccio")

popup_text <- apply(
  mapa_2019_2[, partits] |> st_drop_geometry(),
  1,
  function(row) {
    paste0("<b>", partits, ":</b> ", row, collapse = "<br>")
  }
)

mapa_2019_2 <- mapa_2019_2 %>%
  mutate(
    info_popup = paste0(
      "<b>Municipi:</b> ", Municipi, "<br>",
      "<b>Secció censal:</b> ", MUNDISSEC, "<br><br>",
      popup_text
    )
  )

mapa_2019_2 <- st_transform(mapa_2019_2, 4326)

leaflet(mapa_2019_2) %>%
  addTiles() %>%
  addPolygons(
    fillColor = "steelblue",
    fillOpacity = 0.6,
    color = "white",
    weight = 0.3,
    label = ~lapply(info_popup, HTML),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "black",
      fillOpacity = 0.8,
      bringToFront = TRUE
    )
  )

# ------------------------------
# Eleccions juliol 2023
# ------------------------------
partits <- setdiff(names(dades_2023), "seccio")

popup_text <- apply(
  mapa_2023[, partits] |> st_drop_geometry(),
  1,
  function(row) {
    paste0("<b>", partits, ":</b> ", row, collapse = "<br>")
  }
)

mapa_2023 <- mapa_2023 %>%
  mutate(
    info_popup = paste0(
      "<b>Municipi:</b> ", Municipi, "<br>",
      "<b>Secció censal:</b> ", MUNDISSEC, "<br><br>",
      popup_text
    )
  )

mapa_2023 <- st_transform(mapa_2023, 4326)

leaflet(mapa_2023) %>%
  addTiles() %>%
  addPolygons(
    fillColor = "steelblue",
    fillOpacity = 0.6,
    color = "white",
    weight = 0.3,
    label = ~lapply(info_popup, HTML),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "black",
      fillOpacity = 0.8,
      bringToFront = TRUE
    )
  )
