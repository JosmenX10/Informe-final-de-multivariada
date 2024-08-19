library(ggplot2)

ggboxplot <- function(datos,X,Y,ejex,ejey) { 
ggplot(data = datos,aes(x= factor(X),y=Y)) + 
    geom_boxplot(notch = T,fill = colores) + theme_classic() +
xlab(ejex) + ylab(ejey)}

x11()

attach(datos_env)

ggboxplot(datos=datos_env,X = Grupos, 
          Y = Elevación,ejex = "Grupos",ejey="Elevación(m.s.n.m)")

ggboxplot(datos=datos_env,X = Grupos, 
          Y = Pendiente,ejex = "Grupos",ejey="Pendiente")

ggboxplot(datos=datos_env,X = Grupos, Y = `Cobertura veg.`,
          ejex = "Grupos",ejey="Cobertura vegetal")

ggboxplot(datos=datos_env,X = Grupos, 
          Y = `Riqueza sp` ,ejex = "Grupos", ejey="Riqueza de especies")

ggboxplot(datos=datos_env,X = Grupos, 
          Y = datos_env$pH ,ejex = "Grupos", ejey="pH")

ggboxplot(datos=datos_env,X = Grupos, 
            Y = datos_env$`P.del suelo`,ejex = "Grupos", ejey="Orientación SE")

ggboxplot(datos=datos_env,X = Grupos, 
            Y = datos_env$`Orientación S-SE` ,ejex = "Grupos", ejey="Orientación S-SE")

ggboxplot(datos=datos_env,X = Grupos, 
            Y = datos_env$CCSE ,ejex = "Grupos", ejey="Orientación S-SE")+scale_y_continuous(limits = c(0,1.5))

ggboxplot(datos=datos_env,X = Grupos, 
          Y = datos_env$`CCS-SE` ,ejex = "Grupos", ejey="Orientación S-SE")+scale_y_continuous(limits = c(0,1.5))

