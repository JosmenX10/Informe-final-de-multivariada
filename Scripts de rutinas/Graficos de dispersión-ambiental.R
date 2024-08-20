#llamamos la base de datos
datos_env = read.delim("https://raw.githubusercontent.com/JosmenX10/Informe-final-de-multivariada/main/Base%20de%20datos%20y%20contextos/vltava_env.csv,sep"= ";")

ggpoint <- function(datos,X,Y,grupo,ejex,ejey,grupolab) {ggplot(data = datos,aes(x = X,y = Y)) +   
  geom_point(aes(color = factor(grupo)),size = 3) +  
  geom_smooth(method= "glm", se = F) +
  theme_classic()+xlab(ejex) + ylab(ejey) + labs(color = grupolab)}

attach(datos_env)
ggpoint(datos_env,X = `CCS-SE`, Y = Humedad,grupo = Grupos,ejex="CCS-SE",ejey ="EIV - Humedad", 
        grupolab = "Tipo de vegetación")


