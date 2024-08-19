#Cargamos la base de datos de vegetación.
library(readxl)

datos <- read_xlsx("C:\\Users\\Asus\\Desktop\\Analisis multivariado\\Informe-final-de-multivariada\\Base de datos y contextos\\vltava.xlsx",
                   sheet = "Vltava-species")

#Se transpone la base de datos dejando las parcelas como filas
datos_bio <- datos[,-1]

datos_bio <- t(datos_bio)

colnames(datos_bio) <- datos$...1

write.csv(datos_bio,file = "datos_bio.csv")

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

write.csv(datos_env,"vltava_env.csv")