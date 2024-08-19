#Elegimos directorio
setwd(choose.dir())

#llamamos las librerias necesarias.
library(readxl)
library(dplyr)
library(ade4)
library(adespatial)
library(vegan)
library(gclus)
library(cluster)
library(pvclust)
library(RColorBrewer)

source("hcoplot.R")

#Cargamos la base de datos de vegetación.

datos <- read_xlsx("C:\\Users\\Asus\\Desktop\\Analisis multivariado\\Informe-final-de-multivariada\\Base de datos y contextos\\vltava.xlsx",
                   sheet = "Vltava-species")

#Se transpone la base de datos dejando las parcelas como filas
datos_bio <- datos[,-1]

datos_bio <- t(datos_bio)

colnames(datos_bio) <- datos$...1
#------------------------------

datos_trans <- log1p(datos_bio)


dist_bio <- vegdist(datos_trans,method = "bray")
dist_bio_sqrt <- sqrt(dist_bio)
Cluster1 <- hclust(dist_bio_sqrt,method = "ward.D2")
plot(Cluster1, hang = -1)

clus_ward_cut <- cutree (Cluster1, k = 4) # "cut the tree" - to which groups individual samples belong?

plot (clus_ward, cex = .5)  # argument cex reduced the size of the dendrogram leaf labels to make them readable
clus_in_dendro <- unique (clus_ward_cut[clus_ward$order]) # make sure to know which box is which cluster!
rect.hclust (clus_ward, k = 4, border = clus_in_dendro)
legend ('topleft', legend = paste ('Cluster', 1:5), col = 1:5, pch = 22, bty = 'n')




