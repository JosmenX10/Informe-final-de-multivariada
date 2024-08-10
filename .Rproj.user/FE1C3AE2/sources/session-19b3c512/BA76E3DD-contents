#Carga de datos y analisis exploratorios 

setwd(choose.dir())

library(readxl)

Datos_amb <- read_excel("vltava.xls", sheet = "Vltava-env data")


c <- cor(Datos_amb[,], method = c("pearson"))

c1 <- cor(Datos_amb, method = c("spearman"))

library(corrplot)

x11()
corrplot(c, method = "ellipse", addCoef.col = "black",number.cex = 0.5)

x11()
corrplot(c1, method = "ellipse", addCoef.col = "black",number.cex = 0.5)

