#Elegimos directorio
setwd(choose.dir())

#llamamos las librerias necesarias.
library(dplyr)
library(ade4)
library(adespatial)
library(vegan)
library(gclus)
library(cluster)
library(pvclust)
library(RColorBrewer)
library(labdsv)
library(rioja)
library(indicspecies)
library(mvpart)
library(MVPARTwrap)
library(dendextend)
library(vegclust)
library(colorspace)
library(agricolae)
library(picante)
#Recurso de funciones adicionales 
source("drawmap.R")
source("drawmap3.R")
source("hcoplot.R")
source("test.a.R")
source("coldiss.R")
source("bartlett.perm.R")
source("boxplerk.R")
source("boxplert.R")

datos_bio <- read_xlsx("C:\\Users\\Asus\\Desktop\\Analisis multivariado\\Informe-final-de-multivariada\\Base de datos y contextos\\vltava.xlsx",
                   sheet = "Vltava-species")
 
datos_bio <- t(as.matrix(datos_bio))

colnames(datos_bio) <- datos_bio[1,]

datos_bio = datos_bio[-1,]


