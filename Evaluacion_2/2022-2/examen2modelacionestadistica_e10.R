############# GENERACIÓN DE DATOS POR EQUIPO #####################
# Correr todo el código para generar los datos.
# Ingrese el número de su equipo en los paréntesis de la función set.seed
# ejemplo set.seed(3) asumiendo que está en el equipo 3
set.seed(10) #incluya el núemro de su equipo
#Datos parte 1
aldrin <- c(seq(0.005,0.05, 0.005))
dieldrin <- c(seq(0.005,0.04, 0.005))
DDT <- c(seq(0.05,1.5,0.05))



dat1 <- data.frame(
  "Aldrin" = abs(rnorm(10, sample(aldrin, 1), 0.005)),
  "Dieldrin" = abs(rnorm(10, sample(dieldrin, 1), 0.005)),
  "DDT" = abs(rnorm(10, sample(DDT, 1), 0.05)))
write.csv(x = dat1, file = "datos1.csv")

summary(dat1)
boxplot(dat1)
library(dplyr)

library(tidyr)
 
dat1<-pivot_longer(dat1, col=c(1:3),names_to="POC", values_to="muestras")
boxplot(dat1$muestras~dat1$POC) 


#refutar hipotesis con anova
modelo1<-lm(dat1$muestras~dat1$POC)
summary(modelo1)
anova<-aov(modelo1)
summary(anova)
#observar variables
TUK<-TukeyHSD(x=anova)
TUK


#Parte 2

############# GENERACIÓN DE DATOS POR EQUIPO #####################
# Correr todo el código para generar los datos.
# Ingrese el número de su equipo en los paréntesis de la función set.seed
# ejemplo set.seed(3) asumiendo que está en el equipo 3
set.seed(10) #incluya el núemro de su equipo
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
write.csv(datos2, file = "datos2.csv")
ss

datos2

#Evaluar DDT en los diferentes puntos y profundidades
boxplot(datos2$DDT~datos2$profundidad)
aggregate(DDT~profundidad,data=datos2,mean)

aggregate(DDT~profundidad,data=datos2,max)


#Crear modelo lineal
modelo2<-lm(datos2$DDT~datos2$profundidad)
summary(modelo2)
plot(modelo2)
#Prueba anova
anova<-aov(modelo2)
summary(anova2)
TUK2<-TukeyHSD(x=anova)
TUK2


#Sacar desviación estandar y graficar
promedio <- aggregate(DDT~profundidad, data =datos2, mean)
sd <- aggregate(DDT~profundidad, data =datos2,sd)

tabladatos <- data.frame("profu"=promedio$profundidad,
                         "promDDT"=promedio$DDT, 
                         "desvDDT"=sd$DDT )
tabladatos
#Grafica
library(ggplot2)
grafica <- ggplot(data=tabladatos, aes(x=profu, y =promDDT))+
  geom_errorbar(data=tabladatos, aes(x=profu, ymin=promDDT-desvDDT, 
                                     ymax=promDDT+desvDDT), width=0.2)+
  geom_point(aes(col=profu), size=2)+
  theme_classic()
grafica

                     



