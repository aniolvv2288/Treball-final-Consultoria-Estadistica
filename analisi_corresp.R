library(FactoMineR)
library(factoextra)

### 2023
dades2023 <-na.omit(dades)
dades2023 <-subset(dades2023,eleccio=='2023_jul')
varsactives<- c(
  "fac_indice_gini",
  "fac_renda_media_uc",
  "fac_poblacio_no_espanyola",
  "fac_Tamaño_medio_del_hogar" 
)
quali.sup  <- c("sigles")
quanti.sup <- c(
  "Porcentaje de población de 65 y más años",
  "Porcentaje de población menor de 18 años"
)


res.mca <- MCA(dades2023[,c(varsactives, quali.sup, quanti.sup)], quanti.sup = quanti.sup ,quali.sup = quali.sup , graph = FALSE)

fviz_mca_var(
  res.mca,
  repel = TRUE,
)

fviz_mca_var(
  res.mca,
  repel = TRUE,
  invisible = c("var", "quanti.sup"),
)


#Variables de les dimensions:

sort(res.mca$var$contrib[,1], decreasing = TRUE)[1:7]
#Dim 1: Inmigració i desigualtat
sort(res.mca$var$contrib[,2], decreasing = TRUE)[1:7]
#Dim 2: LLars i Renta

sort(res.mca$var$cos2[,1], decreasing = TRUE)[1:7]
sort(res.mca$var$cos2[,2], decreasing = TRUE)[1:7]
#Podem veure que l'explicabilitat de les modalitats del camps mencionats anteriorment per cada variable presenta un 
#cos2 superior a 0.3 i fins i tot, superior a 0.5 el que reforça la explicació anterior.

fviz_mca_var(
  res.mca,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

#Variables cuantitavies suplementaries

res.mca$quanti.sup$coord

#L'única cosa que podem extreure de les variables quantitatives es que les seccions amb més menors, tenen tendéncia a votar a l'independentisme catalá.

### 2019 NOVEMBRE

dades2019_2 <-na.omit(dades)
dades2019_2 <-subset(dades2019_2,eleccio=='2019_nov')
varsactives<- c(
  "fac_indice_gini",
  "fac_renda_media_uc",
  "fac_poblacio_no_espanyola",
  "fac_Tamaño_medio_del_hogar" 
)
quali.sup  <- c("sigles")
quanti.sup <- c(
  "Porcentaje de población de 65 y más años",
  "Porcentaje de población menor de 18 años"
)


res.mca <- MCA(dades2019_2[,c(varsactives, quali.sup, quanti.sup)], quanti.sup = quanti.sup ,quali.sup = quali.sup , graph = FALSE)

fviz_mca_var(
  res.mca,
  repel = TRUE,
)

fviz_mca_var(
  res.mca,
  repel = TRUE,
  invisible = c("var", "quanti.sup"),
)


#Variables de les dimensions:

sort(res.mca$var$contrib[,1], decreasing = TRUE)[1:7]
#Dim 1: Inmigració i desigualtat
sort(res.mca$var$contrib[,2], decreasing = TRUE)[1:7]
#Dim 2: LLars i Renta

sort(res.mca$var$cos2[,1], decreasing = TRUE)[1:7]
sort(res.mca$var$cos2[,2], decreasing = TRUE)[1:7]
#Podem veure que l'explicabilitat de les modalitats del camps mencionats anteriorment per cada variable presenta un 
#cos2 superior a 0.3 i fins i tot, superior a 0.5 el que reforça la explicació anterior.

fviz_mca_var(
  res.mca,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

#Variables cuantitavies suplementaries

res.mca$quanti.sup$coord

#L'única cosa que podem extreure de les variables quantitatives es que les seccions amb més menors, tenen tendéncia a votar a l'independentisme catalá.

### 2019 ABRIL

dades2019_1 <-na.omit(dades)
dades2019_1 <-subset(dades2019_1,eleccio=='2019_abr')
varsactives<- c(
  "fac_indice_gini",
  "fac_renda_media_uc",
  "fac_poblacio_no_espanyola",
  "fac_Tamaño_medio_del_hogar" 
)
quali.sup  <- c("sigles")
quanti.sup <- c(
  "Porcentaje de población de 65 y más años",
  "Porcentaje de población menor de 18 años"
)


res.mca <- MCA(dades2019_1[,c(varsactives, quali.sup, quanti.sup)], quanti.sup = quanti.sup ,quali.sup = quali.sup , graph = FALSE)

fviz_mca_var(
  res.mca,
  repel = TRUE,
)

fviz_mca_var(
  res.mca,
  repel = TRUE,
  invisible = c("var", "quanti.sup"),
)


#Variables de les dimensions:

sort(res.mca$var$contrib[,1], decreasing = TRUE)[1:7]
#Dim 1: Inmigració i desigualtat
sort(res.mca$var$contrib[,2], decreasing = TRUE)[1:7]
#Dim 2: LLars i Renta

sort(res.mca$var$cos2[,1], decreasing = TRUE)[1:7]
sort(res.mca$var$cos2[,2], decreasing = TRUE)[1:7]
#Podem veure que l'explicabilitat de les modalitats del camps mencionats anteriorment per cada variable presenta un 
#cos2 superior a 0.3 i fins i tot, superior a 0.5 el que reforça la explicació anterior.

fviz_mca_var(
  res.mca,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

#Variables cuantitavies suplementaries

res.mca$quanti.sup$coord

#L'única cosa que podem extreure de les variables quantitatives es que les seccions amb més menors, tenen tendéncia a votar a l'independentisme catalá.

### 2016 

dades2016 <-na.omit(dades)
dades2016 <-subset(dades2016,eleccio=='2016_jun')
varsactives<- c(
  "fac_indice_gini",
  "fac_renda_media_uc",
  "fac_poblacio_no_espanyola",
  "fac_Tamaño_medio_del_hogar" 
)
quali.sup  <- c("sigles")
quanti.sup <- c(
  "Porcentaje de población de 65 y más años",
  "Porcentaje de población menor de 18 años"
)


res.mca <- MCA(dades2016[,c(varsactives, quali.sup, quanti.sup)], quanti.sup = quanti.sup ,quali.sup = quali.sup , graph = FALSE)

fviz_mca_var(
  res.mca,
  repel = TRUE,
)

fviz_mca_var(
  res.mca,
  repel = TRUE,
  invisible = c("var", "quanti.sup"),
)


#Variables de les dimensions:

sort(res.mca$var$contrib[,1], decreasing = TRUE)[1:7]
#Dim 1: Inmigració i desigualtat
sort(res.mca$var$contrib[,2], decreasing = TRUE)[1:7]
#Dim 2: LLars i Renta

sort(res.mca$var$cos2[,1], decreasing = TRUE)[1:7]
sort(res.mca$var$cos2[,2], decreasing = TRUE)[1:7]
#Podem veure que l'explicabilitat de les modalitats del camps mencionats anteriorment per cada variable presenta un 
#cos2 superior a 0.3 i fins i tot, superior a 0.5 el que reforça la explicació anterior.

fviz_mca_var(
  res.mca,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

#Variables cuantitavies suplementaries

res.mca$quanti.sup$coord

#L'única cosa que podem extreure de les variables quantitatives es que les seccions amb més menors, tenen tendéncia a votar a l'independentisme catalá.




### 2015

dades2015 <-na.omit(dades)
dades2015 <-subset(dades2015,eleccio=='2015_des')
varsactives<- c(
  "fac_indice_gini",
  "fac_renda_media_uc",
  "fac_poblacio_no_espanyola",
  "fac_Tamaño_medio_del_hogar" 
)
quali.sup  <- c("sigles")
quanti.sup <- c(
  "Porcentaje de población de 65 y más años",
  "Porcentaje de población menor de 18 años"
)


res.mca <- MCA(dades2015[,c(varsactives, quali.sup, quanti.sup)], quanti.sup = quanti.sup ,quali.sup = quali.sup , graph = FALSE)

fviz_mca_var(
  res.mca,
  repel = TRUE,
)

fviz_mca_var(
  res.mca,
  repel = TRUE,
  invisible = c("var", "quanti.sup"),
)


#Variables de les dimensions:

sort(res.mca$var$contrib[,1], decreasing = TRUE)[1:7]
#Dim 1: Inmigració i desigualtat
sort(res.mca$var$contrib[,2], decreasing = TRUE)[1:7]
#Dim 2: LLars i Renta

sort(res.mca$var$cos2[,1], decreasing = TRUE)[1:7]
sort(res.mca$var$cos2[,2], decreasing = TRUE)[1:7]
#Podem veure que l'explicabilitat de les modalitats del camps mencionats anteriorment per cada variable presenta un 
#cos2 superior a 0.3 i fins i tot, superior a 0.5 el que reforça la explicació anterior.

fviz_mca_var(
  res.mca,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

#Variables cuantitavies suplementaries

res.mca$quanti.sup$coord

#L'única cosa que podem extreure de les variables quantitatives es que les seccions amb més menors, tenen tendéncia a votar a l'independentisme catalá.
