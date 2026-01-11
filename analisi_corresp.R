library(FactoMineR)
library(factoextra)

dades2015 <-subset(dades, eleccio=='2015_des')
res.mca <- MCA(dades2015, quanti.sup = c(                
                                         "Población",
                                         "Porcentaje de hogares unipersonales",
                                         "Porcentaje de población de 65 y más años",
                                         "Porcentaje de población española",
                                         "Porcentaje de población menor de 18 años",
                                         "Tamaño medio del hogar",
                                         "Índice de Gini",
                                         "Fuente de ingreso: otros ingresos",
                                         "Fuente de ingreso: pensiones",
                                         "Fuente de ingreso: prestaciones por desempleo",
                                         "Fuente de ingreso: salario",
                                         "Media de la renta por unidad de consumo") ,quali.sup = c('sigles'), graph = FALSE)

fviz_mca_var(res.mca, repel = TRUE, 
             title = "MCA ")