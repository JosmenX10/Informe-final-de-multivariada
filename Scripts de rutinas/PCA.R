library(ggrepel)
library(ggforce)

#PCA 
amb1 <- datos_env[,c(2,3,4,7:10,11:16,17:22)]
pca <- prcomp(decostand(datos_bio,method = "hellinger"))

#pca <- prcomp(datos_bio1)


summary(pca)

coord.sit2 <- as.data.frame(pca$x[,1:2])     # Coordenadas de los sitios
coord.sit2$sitio <- rownames(coord.sit2)      # Crear una columna con nombres de los sitios
coord.sit2$grp <-  factor(datos_env$Grupos)               # Adicionar columna de grupos por Epoca
head(coord.sit2)                             # vista resumida de las coordenadas de sitios



amb2 = envfit(pca, amb1) 
amb2
coord.amb2 = as.data.frame(scores(amb2, "vectors"))
coord.amb2$amb <- rownames(coord.amb2)         # Insertar columna con nombres de las ambientales
head(coord.amb2) 


coord.tax2 <- as.data.frame(pca$rotation[,1:2])    # Dos primeros ejes
coord.tax2$especies <- rownames(coord.tax2)         # Insertar columna con nombres de las especies
head(coord.tax2) 

ggplot() +
  # Sitios
  geom_point(data = coord.sit2,aes(PC1,PC2,colour=grp),size=4)+
  scale_shape_manual(values = c(21:25)) +

  # Taxones  *valores de cero para caracteres de las flechas (arrow)
  geom_segment(data = coord.tax2,aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(angle=0,length = unit(0,"cm"),
                             type = "closed"),linetype=0, size=0,colour = "red")+
  geom_text_repel(data = coord.tax2,aes(PC1,PC2,label=especies),colour = "red", max.overlaps =  getOption("ggrepel.max.overlaps", default = 100)) +
  # Ambiental  
  geom_segment(data = coord.amb2,aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(angle=22.5,length = unit(0.25,"cm"),
                             type = "closed"),linetype=1, size=0.6,colour = "blue") +
  geom_text_repel(data = coord.amb2,aes(PC1,PC2,label=row.names(coord.amb2)),colour = "#00abff") +
  # Factor
  geom_mark_ellipse(data=coord.sit2,aes(x=PC1,y=PC2,group=grp,
                                       colour=grp),alpha= 1, size = 1)  +
  
  geom_hline(yintercept=0,linetype=3,size=1) + 
  geom_vline(xintercept=0,linetype=3,size=1) +
  labs(color = "Tipos de vegetación", shape = "Tipos de vegetación", fill = "Tipos de vegetación", 
       x = paste0("PC1 (",round(summary(pca)$importance[2,1],3),"%)"), y = paste0("PC2 (",round(summary(pca)$importance[2,2],3),"%)")) + 
  theme_classic() + scale_colour_manual(values = c("#fc8d59","#af8dc3","#d8b365","#1a9850"))




#datos_env[,c(2,3,4,7:10,11,16:22)]
library(ggrepel)
library(ggforce)

library(readxl)

#-------------------------------------------------------------------------------------#


#PCA
#ambiental <- datos_env[,c(2,3,5,7:11,17:21)]
ambiental <- datos_env[,c(2,3,5,7:11,17:22)]

factores <- read_excel("Base de datos y contextos/vltava.xlsx", 
                                 sheet = "Condiciones de suelo")



pca2 <- prcomp(scale(ambiental[,c(-15)],scale = T))

#pca <- prcomp(datos_bio1)


summary(pca2)

coord.amb2 <- as.data.frame(pca2$rotation[,1:2])    # Dos primeros ejes
coord.amb2$amb <- rownames(coord.amb2)         # Insertar columna con nombres de las especies
head(coord.amb2) 

#-----------------------------------------------------------------------------------------#
ambiental$factores <- factores[,4]
#Fluvisoles
Factor <- na.omit(cbind(ambiental,ambiental$factores))$FLUVISOL

coord.sit2 <- as.data.frame(pca2$x[,1:2])     # Coordenadas de los sitios
coord.sit2$sitio <- rownames(coord.sit2)      # Crear una columna con nombres de los sitios
coord.sit2$grp <-  factor(Factor)               # Adicionar columna de grupos por Epoca
head(coord.sit2)                             # vista resumida de las coordenadas de sitios



g.pc1 <- ggplot() +
  # Sitios
  geom_point(data = coord.sit2,aes(PC1,PC2,colour=grp),size=4)+
  scale_shape_manual(values = c(21:25)) +
  
  # Taxones  *valores de cero para caracteres de las flechas (arrow)
  geom_segment(data = coord.amb2,aes(x = 0, y = 0, xend = PC1*9, yend = PC2*9), 
               arrow = arrow(angle=22.5,length = unit(0.25,"cm"),
                             type = "closed"),linetype=1, size=0.6,colour = "red") +
  geom_text_repel(data = coord.amb2,aes(PC1*9,PC2*9,label=amb),colour = "red", max.overlaps =  getOption("ggrepel.max.overlaps", default = 12)) +
  
  geom_mark_ellipse(data=coord.sit2,aes(x=PC1,y=PC2,group=grp,
                                        colour=grp),alpha= 1, size = 1)  +
  
  geom_hline(yintercept=0,linetype=3,size=1) + 
  geom_vline(xintercept=0,linetype=3,size=1) +
  labs(color = "Cond. Suelo", shape = "Cond. Suelo", fill = "Cond. Suelo", 
       x = paste0("PC1 (",round(summary(pca2)$importance[2,1]*100,digits = 1),"%)"), y = paste0("PC2 (",round((summary(pca2)$importance[2,2]*100),digits = 1),"%)") ) + 
  theme_classic() + scale_colour_manual(values = c("#91cf60","grey"))

g.pc1
#-----------------------------------------------------------------------------------------#
#Cambisoles
ambiental$factores <- factores[,3]
Factor <- cbind(ambiental,ambiental$factores)$CAMBISOL

coord.sit2 <- as.data.frame(pca2$x[,1:2])     # Coordenadas de los sitios
coord.sit2$sitio <- rownames(coord.sit2)      # Crear una columna con nombres de los sitios
coord.sit2$grp <-  factor(Factor)               # Adicionar columna de grupos por Epoca
head(coord.sit2)                             # vista resumida de las coordenadas de sitios



g.pc2 <- ggplot() +
  # Sitios
  geom_point(data = coord.sit2,aes(PC1,PC2,colour=grp),size=4)+
  scale_shape_manual(values = c(21:25)) +
  
  # Taxones  *valores de cero para caracteres de las flechas (arrow)
  geom_segment(data = coord.amb2,aes(x = 0, y = 0, xend = PC1*9, yend = PC2*9), 
               arrow = arrow(angle=22.5,length = unit(0.25,"cm"),
                             type = "closed"),linetype=1, size=0.6,colour = "red") +
  geom_text_repel(data = coord.amb2,aes(PC1*9,PC2*9,label=amb),colour = "red", max.overlaps =  getOption("ggrepel.max.overlaps", default = 12)) +
  
  geom_mark_ellipse(data=coord.sit2,aes(x=PC1,y=PC2,group=grp,
                                        colour=grp),alpha= 1, size = 1)  +
  
  geom_hline(yintercept=0,linetype=3,size=1) + 
  geom_vline(xintercept=0,linetype=3,size=1) +
  labs(color = "Cond. Suelo", shape = "Cond. Suelo", fill = "Cond. Suelo", 
       x = paste0("PC1 (",round(summary(pca2)$importance[2,1]*100,digits = 1),"%)"), y = paste0("PC2 (",round((summary(pca2)$importance[2,2]*100),digits = 1),"%)") ) + 
  theme_classic() + scale_colour_manual(values = c("#99d594","grey"))

g.pc2

#------------------------------------------------------------------------------------------#
#Liticos
ambiental$factores <- factores[,1]
Factor <- cbind(ambiental,ambiental$factores)$LITHIC

coord.sit2 <- as.data.frame(pca2$x[,1:2])     # Coordenadas de los sitios
coord.sit2$sitio <- rownames(coord.sit2)      # Crear una columna con nombres de los sitios
coord.sit2$grp <-  factor(Factor)               # Adicionar columna de grupos por Epoca
head(coord.sit2)                             # vista resumida de las coordenadas de sitios



g.pc3 <- ggplot() +
  # Sitios
  geom_point(data = coord.sit2,aes(PC1,PC2,colour=grp),size=4)+
  scale_shape_manual(values = c(21:25)) +
  
  # Taxones  *valores de cero para caracteres de las flechas (arrow)
  geom_segment(data = coord.amb2,aes(x = 0, y = 0, xend = PC1*9, yend = PC2*9), 
               arrow = arrow(angle=22.5,length = unit(0.25,"cm"),
                             type = "closed"),linetype=1, size=0.6,colour = "red") +
  geom_text_repel(data = coord.amb2,aes(PC1*9,PC2*9,label=amb),colour = "red", max.overlaps =  getOption("ggrepel.max.overlaps", default = 12)) +
  
  geom_mark_ellipse(data=coord.sit2,aes(x=PC1,y=PC2,group=grp,
                                        colour=grp),alpha= 1, size = 1)  +
  
  geom_hline(yintercept=0,linetype=3,size=1) + 
  geom_vline(xintercept=0,linetype=3,size=1) +
  labs(color = "Cond. Suelo", shape = "Cond. Suelo", fill = "Cond. Suelo", 
       x = paste0("PC1 (",round(summary(pca2)$importance[2,1]*100,digits = 1),"%)"), y = paste0("PC2 (",round((summary(pca2)$importance[2,2]*100),digits = 1),"%)") ) + 
  theme_classic() + scale_colour_manual(values = c("#d7191c","grey"))

g.pc3

#-------------------------------------------------------------------------------------

#Liticos
ambiental$factores <- factores[,2]
Factor <- cbind(ambiental,ambiental$factores)$SKELETIC

coord.sit2 <- as.data.frame(pca2$x[,1:2])     # Coordenadas de los sitios
coord.sit2$sitio <- rownames(coord.sit2)      # Crear una columna con nombres de los sitios
coord.sit2$grp <-  factor(Factor)               # Adicionar columna de grupos por Epoca
head(coord.sit2)                             # vista resumida de las coordenadas de sitios



g.pc4 <- ggplot() +
  # Sitios
  geom_point(data = coord.sit2,aes(PC1,PC2,colour=grp),size=4)+
  scale_shape_manual(values = c(21:25)) +
  
  # Taxones  *valores de cero para caracteres de las flechas (arrow)
  geom_segment(data = coord.amb2,aes(x = 0, y = 0, xend = PC1*9, yend = PC2*9), 
               arrow = arrow(angle=22.5,length = unit(0.25,"cm"),
                             type = "closed"),linetype=1, size=0.6,colour = "red") +
  geom_text_repel(data = coord.amb2,aes(PC1*9,PC2*9,label=amb),colour = "red", max.overlaps =  getOption("ggrepel.max.overlaps", default = 12)) +
  
  geom_mark_ellipse(data=coord.sit2,aes(x=PC1,y=PC2,group=grp,
                                        colour=grp),alpha= 1, size = 1)  +
  
  geom_hline(yintercept=0,linetype=3,size=1) + 
  geom_vline(xintercept=0,linetype=3,size=1) +
  labs(color = "Cond. Suelo", shape = "Cond. Suelo", fill = "Cond. Suelo", 
       x = paste0("PC1 (",round(summary(pca2)$importance[2,1]*100,digits = 1),"%)"), y = paste0("PC2 (",round((summary(pca2)$importance[2,2]*100),digits = 1),"%)") ) + 
  theme_classic() + scale_colour_manual(values = c("#fdae61","grey"))

g.pc4


(g.pc1 + g.pc2)/(g.pc3 + g.pc4)






fviz_contrib(pca2,choice="var",axes=1)

