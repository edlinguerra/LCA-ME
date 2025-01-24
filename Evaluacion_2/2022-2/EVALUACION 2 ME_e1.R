############# GENERACIÓN DE DATOS POR EQUIPO #####################
# Correr todo el código para generar los datos.
# Ingrese el número de su equipo en los paréntesis de la función set.seed
# ejemplo set.seed(3) asumiendo que está en el equipo 3
set.seed(1) #incluya el núemro de su equipo
#Datos parte 1
aldrin <- c(seq(0.005,0.05, 0.005))
dieldrin <- c(seq(0.005,0.04, 0.005))
DDT <- c(seq(0.05,1.5,0.05))

1

dat1 <- data.frame(
  "Aldrin" = abs(rnorm(10, sample(aldrin, 1), 0.005)),
  "Dieldrin" = abs(rnorm(10, sample(dieldrin, 1), 0.005)),
  "DDT" = abs(rnorm(10, sample(DDT, 1), 0.05))
)
write.csv(x = dat1, file = "datos1.csv")
boxplot(dat1)
summary(dat1)
dat1
library(tidyr)
#transformamos la tabla 
tabla <- pivot_longer(dat1, col= c(1:3), names_to = "compuestos", values_to = "valores")
tabla
#SE REALIZO UNA GRAFICA DE CAJAS
boxplot(valores~compuestos, data = tabla)

#Pruebas estadísticas ANOVA
modlineal <- lm(valores~compuestos, data = tabla)
modlineal
#transformar el mod lineal a un ANOVA
modanova <- aov(modlineal)
modanova
#resumen anova
summary(modanova)
#prueba tukey
TukeyHSD(modanova)

#PARTE DOS

set.seed(1) #incluya el núemro de su equipo
#Datos parte 2
efectos <- seq(0,1,0.1)
a <- abs(rnorm(10, sample(seq(0.5,0.8,0.1), 1), 0.05))
p10<-data.frame("profundidad" = rep("10m", 10),
                
                "DDT" = abs(rnorm(10, mean = sample(efectos,1) + a, sd = 0.3)))

p20<-data.frame("profundidad" = rep("20m", 10),
                
                "DDT" = abs(rnorm(10, mean = sample(efectos,1) + a/2, sd = 0.3)))

p30<-data.frame("profundidad" = rep("30m", 10),
                
                "DDT" = abs(rnorm(10, mean = sample(a, 1), sd = 0.3)))

datos2 <- rbind(p10, p20, p30)
write.csv(dat2, file = "datos2.csv")


datos2
#visualizar datos con grafico de cajas
boxplot(DDT~profundidad, data = datos2)
aggregate(DDT~profundidad, data = datos2, FUN = max)
#Pruebas estadísticas ANOVA
modlineal2 <- lm(DDT~profundidad, data = datos2 )
modlineal2
#transformar el mod lineal a un ANOVA
modanova2 <- aov(modlineal2)
modanova2
#resumen anova
summary(modanova2)
#prueba tukey
TukeyHSD(modanova2)

#Gráfico de promedios y desv est
#obtener datos para los promedios 
promedios <- aggregate(DDT~profundidad, data = datos2, FUN = mean)
promedios
#desviacion estandar
DDTSD <- aggregate(DDT~profundidad, data = datos2, FUN = sd)
DDTSD
#grafico

# Combinar ambos resultados en una tabla
tabla1<-data.frame("profundidad" = promedios$profundidad, "DDTpromedio" = promedios$DDT, "DDTSD" = DDTSD$DDT)
tabla1

library(ggplot2)
fig1.4 <- ggplot(data = tabla1, aes(y =DDTpromedio, x = profundidad )) +
  geom_errorbar(
    data = tabla1,
    aes(
      x = profundidad,
      ymin = DDTpromedio - DDTSD,
      ymax = DDTpromedio + DDTSD
    ),
    width = 0.2
  ) +
  geom_point(aes(col = profundidad), size = 3) +
  theme_bw() 
fig1.4

