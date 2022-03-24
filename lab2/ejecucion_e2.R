# comandos para resolver la 2da parte de la actividad 2

datos2 <- read.csv("C:/Users/Edlin/OneDrive/Documents/UNAM/ENES/Lic_Ciencias_Ambientales/Modelacion_Estadistica/LCA-ME/lab2/datos2.csv", stringsAsFactors=TRUE)


hist(datos2$DBO)
abline(v=mean(datos2$DBO[1:25]), col = "green")
abline(v=mean(datos2$DBO[26:50]), col = "red")

boxplot(formula = DBO ~ Momento, dat = datos2)

summary(datos2)

mean()
sd()

#Con esta función podemos estimar parámetro para cada momento
#Promedio
tabla1 <- aggregate(formula = DBO ~ Momento, data = datos2, FUN = mean)
colnames(tabla1) <- c("Momento", "Promedio")

#Desvación estandar
tabla2 <- aggregate(formula = DBO ~ Momento, data = datos2, FUN = sd)
library(moments)
# Estimar kurtosis
tabla3 <- aggregate(formula = DBO ~ Momento, data = datos2, FUN = kurtosis)
# Estimar simetría
tabla4 <- aggregate(formula = DBO ~ Momento, data = datos2, FUN = skewness)

#combinar estmadores
tabla1$desv <- tabla2$DBO
tabla1$kurtosis <- tabla3$DBO
tabla1$simetria <- tabla4$DBO
tabla1

chancla <- function(datos){
  s <- sd(datos)
  n <- length(datos)
  ee <- s/sqrt(n)
 return(ee)
}

#Estimar el error estandar a cada momento
tabla5 <- aggregate(formula = DBO ~ Momento, data = datos2, FUN = chancla)

tabla1$Error_estandar <- tabla5$DBO

# Ha: DBO despues > DBO antes
# H0: DBO despues < o =  DBO antes

# t = DBO despues - DBO antes / Error estandar
# Si Ho es falsa que signo debe tener t?  debe ser positivo

# t = DBO antes - DBO despues / Error estandar
# Si Ho es falsa que signo debe tener t?  debe ser negativo

qt(p = 0.05, df = 48, lower.tail = FALSE)

t.test(DBO ~ Momento, data = datos2, 
       alternative = "less",
       var.equal = TRUE)

# Existen evidencias para descartar la H0, por lo que
# la DBO aumentó después del funcionamiento de la planta.
# La probabilidad de errar con esta conclusión es menor 5%

#Prueba reconociendo que las varianzas difieren

t.test(DBO ~ Momento, data = datos2, 
       alternative = "less",
       var.equal = FALSE)




















