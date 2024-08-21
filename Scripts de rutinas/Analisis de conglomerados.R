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

datos_env <- read.delim("https://raw.githubusercontent.com/JosmenX10/Informe-final-de-multivariada/main/Base%20de%20datos%20y%20contextos/vltava_env.csv,sep"= ";")

datos_bio <- read.delim("https://raw.githubusercontent.com/JosmenX10/Informe-final-de-multivariada/main/Base%20de%20datos%20y%20contextos/datos_bio.csv", sep =",")

datos_bio <- datos_bio[,-1]

#------------------------------

datos_trans <- log1p(datos_bio)


dist_bio <- vegdist(datos_trans,method = "bray")
dist_bio_sqrt <- sqrt(dist_bio)
Cluster1 <- hclust(dist_bio_sqrt,method = "ward.D2")
plot(Cluster1, hang = -1)

clus_ward_cut <- cutree (Cluster1, k = 5) # "cut the tree" - to which groups individual samples belong?

plot (Cluster1, cex = .5)  # argument cex reduced the size of the dendrogram leaf labels to make them readable
clus_in_dendro <- unique(clus_ward_cut[Cluster1$order]) # make sure to know which box is which cluster!
rect.hclust (Cluster1, k = 5, border = clus_in_dendro)
legend ('topleft', legend = paste ('Cluster', 1:5), col = 1:5, pch = 22, bty = 'n')




