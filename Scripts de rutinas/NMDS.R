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

#Cargamos la base de datos de vegetación.

datos <- read_xlsx("C:\\Users\\Asus\\Desktop\\Analisis multivariado\\Informe-final-de-multivariada\\Base de datos y contextos\\vltava.xlsx",
                   sheet = "Vltava-species")

#Se transpone la base de datos dejando las parcelas como filas
datos_bio <- datos[,-1]
 
datos_bio <- t(datos_bio)

colnames(datos_bio) <- datos$...1



#Cargamos la base de datos ambientales.

datos_env <- read_xlsx("C:\\Users\\Asus\\Desktop\\Analisis multivariado\\Informe-final-de-multivariada\\Base de datos y contextos\\vltava.xlsx",
                       sheet = "Vltava-env data")

# Preparamos la base de datos ambientales 
colnames(datos_env) = c("Parcela","Transecto","Elevación","Pendiente","Orientación SE",
                    "Orientación S-SE", "CCSE","CCS-SE","Superficie PD","Superficie ISO",
                    "Lítico","Esqueletico","Cambisoles","Fluvisoles","P.del suelo","pH",
                    "Cobertura veg.","Luz","Temperatura","Continentalidad","Humedad",
                    "Reactividad","Nutrientes","Riqueza sp","Grupos")


datos1 <- datos_env[,c("Parcela","Transecto","Elevación","Pendiente","Orientación SE",
                   "Orientación S-SE", "CCSE","CCS-SE","Superficie PD","Superficie ISO","P.del suelo","pH")]

datos2 <- datos_env[,c("Lítico","Esqueletico","Cambisoles","Fluvisoles", "Cobertura veg.","Luz","Temperatura","Continentalidad","Humedad",
                   "Reactividad","Nutrientes","Riqueza sp","Grupos")]

datos_env <- cbind(datos1,datos2)

datos_trans <- sqrt(datos_bio)

dist_comunidad <- vegdist(datos_bio, method = "bray")


nmds <- metaMDS(dist_comunidad, k = 2, trymax = 100)


envfit_result <- envfit(nmds, datos_env[,c("Luz","Temperatura","Continentalidad","Humedad",
                                           "Reactividad","Nutrientes")], permutations = 999)

#NMDs, con el factor de tipo de vegetación

colores <- c("#a6d96a","#ca0020", "#c2a5cf",
             "#92c5de")

plot(nmds, type = "n")
ordispider(nmds,factor(datos_env$Grupos), col = colores)
points(nmds, display = "sites", col = colores[as.numeric(datos_env$Grupos)], pch = 16)
plot(envfit_result, p.max = 0.05, col = "black")  # Solo muestra vectores significativos
legend("topleft", title="Tipo de vegetación",
       c("1","2","3","4"), fill=colores, horiz=FALSE, cex=.9) # adicionar leyenda







