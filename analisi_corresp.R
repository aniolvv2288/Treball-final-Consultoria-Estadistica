library(FactoMineR)
library(factoextra)

dades2015 <-subset(dades, eleccio=='2015_des')
res.mca <- MCA(dades2015, quanti.sup = c("Porcentaje de población española",
                                         'Porcentaje de población de 65 y más años',
                                         'Porcentaje de población menor de 18 años',
                                         'Índice de Gini') ,quali.sup = c('sigles',
                                                                          'provincia'), graph = FALSE)

fviz_mca_var(res.mca, repel = TRUE, 
             title = "MCA ")
