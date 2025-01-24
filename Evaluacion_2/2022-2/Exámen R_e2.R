############# GENERACIÓN DE DATOS POR EQUIPO #####################
# Correr todo el código para generar los datos.
# Ingrese el número de su equipo en los paréntesis de la función set.seed
# ejemplo set.seed(3) asumiendo que está en el equipo 3
set.seed(2) #incluya el núemro de su equipo
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

#hipotesis de investigacion 
#Las aguas subterraneas del estado de Yucatán estan contaminadas por
#actividades agricolas causando que la población consuma agua con
#altos niveles de POC, teniendo casos registrados en leche materna
#Hipotesis nula
#las concentraciones de POC en aguas subteraneas estan dentro de 
#los limites maximos permisibles estipulados por la NORMA OFICIAL MEXICANA NOM-127-SSA1-1994
#siendo iguales o menores.
#hipotesis alternativa
#las concentraciones de POC en aguas subteraneas estan fuera de 
#los limites maximos permisibles estipulados por la la NORMA OFICIAL MEXICANA NOM-127-SSA1-1994
#siendo mayores.

#promedios
PromAL<- mean(dat1$Aldrin)
PromDI<-mean(dat1$Dieldrin)
PromDDT<- mean(dat1$DDT)
desvAL<- sd(dat1$Aldrin)
desvDI<- sd(dat1$Dieldrin)
desvDDT<- sd(dat1$DDT)
#histograma
hist(dat1$Aldrin)

abline(v= mean(dat1$Aldrin), col= "green")
hist(dat1$Dieldrin)

abline(v= mean(dat1$Dieldrin), col= "green")
hist(dat1$DDT)

abline(v= mean(dat1$DDT), col= "green")

#ee
eeAL<- desvAL/sqrt(10)
eeDI<- desvDI/sqrt(10)
eeDDT<- desvDDT/sqrt(10)

# someter a prueba t.test
refALDI<- 0.03
refDDT<- 1

boxplot_ALDI<- boxplot(aldrin, dieldrin, data=dat1)
boxplot_ALDI
points(c(refALDI,refALDI),pch= 8, col= "black",lwd=1)


boxplot_DDT<-boxplot(DDT, data=dat1)
  names("DDT en aguas subterraneas de Yucatán")
boxplot_DDT
points(c(refDDT),pch= 8, col= "black",lwd=1)



staccked_dat1<-stack(dat1)
head(staccked_dat1)

boxplot_stack<-boxplot(staccked_dat1$values~staccked_dat1$ind,col= rainbow(ncol(dat1)))
boxplot_stack

t.taldrin<-t.test(x = dat1$Aldrin, mu = refALDI, alternative = "less")
t.taldrin
t.tdieldrin<-t.test(x = dat1$Dieldrin, mu = refALDI, alternative = "less")
t.tdieldrin
t.tDDT<-t.test(x = dat1$Aldrin, mu = refDDT, alternative = "less")
t.tDDT


#Parte 2
set.seed(2) #incluya el núemro de su equipo
#Datos parte 2
efectos <- seq(0,1,0.1)
a <- abs(rnorm(10, sample(seq(0.5,0.8,0.1), 1), 0.05))
p10<-data.frame("profundidad" = rep("10", 10),
                
                "DDT" = abs(rnorm(10, mean = sample(efectos,1) + a, sd = 0.3)))

p20<-data.frame("profundidad" = rep("20", 10),
                
                "DDT" = abs(rnorm(10, mean = sample(efectos,1) + a/2, sd = 0.3)))

p30<-data.frame("profundidad" = rep("30", 10),
                
                "DDT" = abs(rnorm(10, mean = sample(a, 1), sd = 0.3)))

datos2 <- rbind(p10, p20, p30)
write.csv(datos2, file = "datos2.csv")

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

