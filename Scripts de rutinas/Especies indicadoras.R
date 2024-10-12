
#Llamamos la base de datos


datos_env = read.delim("https://raw.githubusercontent.com/JosmenX10/Informe-final-de-multivariada/main/Base%20de%20datos%20y%20contextos/vltava_env.csv",sep = ",")

datos_sp = read.delim("https://raw.githubusercontent.com/JosmenX10/Informe-final-de-multivariada/main/Base%20de%20datos%20y%20contextos/datos_bio.csv",sep = ",")

datos_sp <- datos_sp[,-1]

datos_sp 

#caragamos libreria
library(vegan)
library(indicspecies)
library(tidyverse)

datos_sp_pa <- decostand(datos_sp, method = 'pa') #Transformamos a datos de presencia ausencia

phi <- multipatt(datos_sp_pa, cluster = datos_env$Grupos, fun = 'r.g') #aplicamos la funcion para calcula la fidelidad de las especies

re <- phi$sign[phi$sign$p.value<=0.01,] #se filtran las especies que son significativas.


no_diag_sp <- rowSums (select(re, s.1, s.2, s.3, s.4))

re1<-re %>% arrange (no_diag_sp, desc (s.1), desc (s.2), desc (s.3), desc (s.4), desc (stat))

summary(phi,indvalcomp = TRUE, alpha = 0.01)





