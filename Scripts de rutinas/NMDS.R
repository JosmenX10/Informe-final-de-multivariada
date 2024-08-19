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

datos_env <- read.delim("https://raw.githubusercontent.com/JosmenX10/Informe-final-de-multivariada/main/Base%20de%20datos%20y%20contextos/vltava_env.csv,sep"= ";")

datos_bio <- read.delim("https://raw.githubusercontent.com/JosmenX10/Informe-final-de-multivariada/main/Base%20de%20datos%20y%20contextos/datos_bio.csv", sep =",")

datos_bio <- datos_bio[,-1]

dist_comunidad <- vegdist(datos_bio, method = "bray")


nmds <- metaMDS(dist_comunidad, k = 2, trymax = 100)


envfit_result <- envfit(nmds, datos_env[,c("Luz","Temperatura","Continentalidad","Humedad",
                                           "Reactividad","Nutrientes")], permutations = 999)

#NMDs, con el factor de tipo de vegetación

colores <- c("#a6d96a","#ca0020", "#c2a5cf",
             "#92c5de")

x11()
plot(nmds, type = "n")
ordispider(nmds,factor(datos_env$Grupos), col = colores)
points(nmds, display = "sites", col = colores[as.numeric(datos_env$Grupos)], pch = 16)
plot(envfit_result, p.max = 0.05, col = "black")  # Solo muestra vectores significativos
legend("topleft", title="Tipo de vegetación",
       c("1","2","3","4"), fill=colores, horiz=FALSE, cex=.9) # adicionar leyenda







