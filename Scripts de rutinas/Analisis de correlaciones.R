
#Elegimos directorio
setwd(choose.dir())

#Llamamos a las librerias necesarias
library(psych)
library(readxl)
library(ellipse)
library(corrplot)
library(Hmisc)

 #Llamamos a la base de datos.

datos_env <- read.delim("https://raw.githubusercontent.com/JosmenX10/Informe-final-de-multivariada/main/Base%20de%20datos%20y%20contextos/vltava_env.csv", sep = ",")

#Seleccionamos entre grupos de variables
topo <- datos_env[,4:17]#Seleccionamos solamente las variables topograficas y de suelo.

ellin <- datos_env[,19:24]#seleccionamos solo las variables indices-ellinger.
#-----------------------------------------------------------------------

#Generamos las matrices de correlación para analisis 
topocor <- cor(topo, method = "spearman") #correlación entre topograficas.

pval_topo <- corr.test(topo, adjust="none",method = "spearman")$p #p-value de correlaciones entre topograficas.


ellin_topocor <- cor(y = topo, x = ellin, method = "spearman") #correlación entre topograficas.

pval_ellin<- corr.test(y= topo, x= ellin, adjust="none",method = "spearman")$p #p-value de correlaciones entre topograficas.


#graficos de corrplot

# Topografico vs Topografico

x11()

corrplot.mixed(topocor, lower = 'number', upper = 'ellipse', order = "original",
               lower.col = "black", tl.pos = "lt",tl.col = "black",sig.level = c(0.001,0.01,0.05),
               p.mat = pval_topo, insig = c("label_sig"), pch= "*",pch.cex = 1, pch.col ="black"
               ,tl.cex = 0.9,number.cex = 0.45)

corrplot(topocor, add = TRUE, type = 'lower', method = 'number', order = 'original',
         col = 'black', diag = FALSE, tl.pos = 'n', cl.pos = 'n', number.cex = 0.45,
         p.mat = pval_topo, insig = c("pch"), pch= "_",pch.cex = 1.5, pch.col ="red")

#Topografico vs ellinberg

par(mfrow=c(2,1))
corrplot(ellin_topocor,method = "ellipse",
          tl.pos = "l",tl.col = "black",sig.level = c(0.001,0.01,0.05),
               p.mat = pval_ellin, insig = c("label_sig"), pch= "*",pch.cex = 1.5, pch.col ="black"
               ,tl.cex = 0.9, number.cex = 0.9,addgrid.col = "#f5f5f5")

corrplot(ellin_topocor,  method = "number",
         col = 'black', tl.pos = "l",tl.col = "black", cl.pos = 'n', number.cex = 0.9,
         p.mat = pval_ellin, insig = c("pch"), pch= "_",pch.cex = 1.5, pch.col ="red",
         tl.cex = 0.9)



#segundo grafico

corrplot(topocor,method = "ellipse",type ="lower",diag = FALSE,
         tl.pos = "ld",order ="original",tl.col = "black",addCoef.col= NULL, cl.pos = T, sig.level = c(0.001,0.01,0.05),
         p.mat = pval_topo,insig = c("label_sig"), pch= "*",pch.cex = 0.5, pch.col ="black",
         tl.cex = 1.0, number.cex = 0.9, cl.cex = 0.8,tl.srt = 50,addgrid.col = "#f5f5f5")
         
       
corrplot(ellin_topocor,method = "ellipse",
         tl.pos = "dt",tl.col = "black",sig.level = c(0.001,0.01,0.05),
         p.mat = pval_ellin, insig = c("label_sig"), pch= "*",pch.cex = 1.0, pch.col ="black"
         ,tl.cex = 1.0, number.cex = 0.9,addgrid.col = "#f5f5f5")




         
        





