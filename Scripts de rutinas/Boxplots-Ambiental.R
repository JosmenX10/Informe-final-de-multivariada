library(ggplot2)

datos_env = read.delim("https://raw.githubusercontent.com/JosmenX10/Informe-final-de-multivariada/main/Base%20de%20datos%20y%20contextos/vltava_env.csv",sep= ",")

datos_amb 
#se generan los colores
colores <- c("#a6d96a","#ca0020", "#c2a5cf","#92c5de") 

require(ggplot2)
ggboxplot <- function(datos,vX,vY,ejex,ejey) { 
ggplot(data = datos) + geom_boxplot(aes(x= factor(vX), y=vY,),fill=colores, notch = T)  + theme_classic() +
xlab(ejex) + ylab(ejey)}


x11()

attach(datos_env)

ggboxplot(datos=datos_env,vX = Grupos, 
          vY = Elevación,ejex = "Grupos",ejey="Elevación(m.s.n.m)")

geomboxplot(datos=datos_env,vX = Grupos, 
          Y = Pendiente,ejeX = "Grupos",ejey="Pendiente")

geomboxplot(datos=datos_env,vX = Grupos, Y = `Cobertura veg.`,
          ejeX = "Grupos",ejey="Cobertura vegetal")

geomboxplot(datos=datos_env,vX = Grupos, 
          Y = `Riqueza sp` ,ejeX = "Grupos", ejey="Riqueza de especies")

geomboxplot(datos=datos_env,vX = Grupos, 
          Y = datos_env$pH ,ejeX = "Grupos", ejey="pH")

geomboxplot(datos=datos_env,vX = Grupos, 
            Y = datos_env$`P.del suelo`,ejeX = "Grupos", ejey="Profundidad del suelo")

geomboxplot(datos=datos_env,vX = Grupos, 
            Y = datos_env$`Orientación S-SE` ,ejeX = "Grupos", ejey="Orientación S-SE")

geomboxplot(datos=datos_env,vX = Grupos, 
            Y = datos_env$CCSE ,ejeX = "Grupos", ejey="Indice de carga calorifica SE")+scale_y_continuous(limits = c(0,1.5))

geomboxplot(datos=datos_env,vX = Grupos, 
          Y = datos_env$`CCS-SE` ,ejeX = "Grupos", ejey="Indice de carga calorifica S-SE")+scale_y_continuous(limits = c(0,1.5))



