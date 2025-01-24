############# GENERACIÓN DE DATOS POR EQUIPO #####################
# Correr todo el código para generar los datos.
# Ingrese el número de su equipo en los paréntesis de la función set.seed
# ejemplo set.seed(3) asumiendo que está en el equipo 3
set.seed(11) #incluya el núemro de su equipo
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