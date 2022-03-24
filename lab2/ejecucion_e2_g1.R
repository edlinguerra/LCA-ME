#Comandos para resolver la 2da parte de la actividad 2

#Con este comando importo los datos usando paquete básico de R
datos2 <- read.csv("C:/Users/Edlin/OneDrive/Documents/UNAM/ENES/Lic_Ciencias_Ambientales/Modelacion_Estadistica/LCA-ME/lab2/datos2.csv", stringsAsFactors=TRUE)

#Con estos comandos importo los datos usando el paquete readr
library(readr)
datos2_v2 <- read_csv("lab2/datos2.csv", col_types = cols(DBO = col_number()))
