#Caso 1: Calidad del agua primer manto acuífero--------------------------------

#10 muestras (n) de agua a una profundidad de 10 metros.
#Cada muestra de una perforación diferente en puntos aleatorios en área
#aprox 10 ha
#A partir de estas se estimaron 3 compuestos organoclorados: 
#Aldrín, Dieldrín y DDT.

############# GENERACIÓN DE DATOS POR EQUIPO #####################
# Correr todo el código para generar los datos.
# Ingrese el número de su equipo en los paréntesis de la función set.seed
# ejemplo set.seed(3) asumiendo que está en el equipo 3
set.seed(4) #incluya el núemro de su equipo

#Datos parte 1
aldrin <- c(seq(0.005,0.05, 0.005))
dieldrin <- c(seq(0.005,0.04, 0.005))
DDT <- c(seq(0.05,1.5,0.05))
dat1 <- data.frame(
  "Aldrin" = abs(rnorm(10, sample(aldrin, 1), 0.005)),
  "Dieldrin" = abs(rnorm(10, sample(dieldrin, 1), 0.005)),
  "DDT" = abs(rnorm(10, sample(DDT, 1), 0.05))
)
write.csv(x = dat1, file = "datos1.csv")

#Límite permisible Aldrín y Dieldrín 0.03 mg/l (separados o combinados)
#DDT 1.00 mg/l (total de isómeros)

#p-value probabilidad que nos equivoquemos con la alternativa

#PRIMERO PRUEBA T STUDENT PARA ALDRIN-------------------------------------

#Media Aldrin
mediaAD<-mean(dat1$Aldrin)
mediaAD

#Desviación estandar Aldrin
desvAD<-sd(dat1$Aldrin)
desvAD

#Distribución con promedio señalado
hist(dat1$Aldrin,main= "Histograma de Aldrín", xlab= "Concentración (mg/L)", ylab="Frecuencia")
+ abline(v=mean(dat1$Aldrin))
#a primera vista supera el límite

#Sacamos el error estandar
ee<-desvAD/sqrt(10)
ee

#Establecer valor de referencia (límite)
ref<- 0.03
ref

#Aplicamos prueba t-student
t.test(x= dat1$Aldrin,mu= ref , alternative="greater",conf.level = 0.95, lower.tail=FALSE)


#PRUEBA T STUDENT PARA DIELDRIN-------------------------------------------

#Media Dieldrin
mediaDI<-mean(dat1$Dieldrin)
mediaDI

#Desviación estandar Dieldrin
desvDI<-sd(dat1$Dieldrin)
desvDI

#Distribución con promedio señalado
hist(dat1$Dieldrin,main= "Histograma de Dieldrin", xlab= "Concentración (mg/L)", ylab="Frecuencia")
+ abline(v=mean(dat1$Dieldrin))
#a primera vista supera el límite

#sacamos el error estandar
ee2<-desvDI/sqrt(10)
ee2

#establecer valor de referencia (limite)
ref2<- 0.03
ref2

#Aplicamos t-student
t.test(x= dat1$Dieldrin,mu= ref2 , alternative="greater",conf.level = 0.95, lower.tail=FALSE)


#PRUEBA T STUDENT PARA DDT------------------------------------------------

#Media DDt
mediaDT<-mean(dat1$DDT)
mediaDT

#Desviación DDT
desvDT<-sd(dat1$DDT)
desvDT

#Distribución con promedio señalado
hist(dat1$DDT,main= "Histograma de Dieldrin", xlab= "Concentración (mg/L)", ylab="Frecuencia")
+ abline(v=mean(dat1$DDT))
#a primera vista supera el límite

#sacamos el error estandar
ee3<-desvDT/sqrt(10)
ee3

#establecer valor de referencia (limite)
ref3<- 1.00
ref3

#Aplicamos t-student
t.test(x= dat1$DDT,mu= ref3 , alternative="greater",conf.level = 0.95, lower.tail=FALSE)



#Caso 2: Calidad del agua según profundidad-----------------------------

#Modelo: la contaminación por DDT debe ser > a poca profundidad
#debido a la adsorción por parte de las partículas orgánicas más 
#abundantes en los primeros metros del suelo.
#Estudio: 10 muestras (n) en perforaciones con
#10, 20 y 30 m de profundidad

#¿10 muestras a 10 m, otras 10 en 20 m y otras 10 en 30?
#¿Total de 30 muestras?

############# GENERACIÓN DE DATOS POR EQUIPO #####################
# Correr todo el código para generar los datos.
# Ingrese el número de su equipo en los paréntesis de la función set.seed
# ejemplo set.seed(3) asumiendo que está en el equipo 3
set.seed(4) #incluya el núemro de su equipo
#Datos parte 2
efectos <- seq(0,1,0.1)
a <- abs(rnorm(10, sample(seq(0.5,0.8,0.1), 1), 0.05))
p10<-data.frame("profundidad" = rep("10m", 10),
                "DDT" = abs(rnorm(10, mean = sample(efectos,1) + a, sd = 0.3)))
p20<-data.frame("profundidad" = rep("20m", 10),
                "DDT" = abs(rnorm(10, mean = sample(efectos,1) + a/2, sd = 0.3)))
p30<-data.frame("profundidad" = rep("30m", 10),
                "DDT" = abs(rnorm(10, mean = sample(a, 1), sd = 0.3)))
dat2 <- rbind(p10, p20, p30)
write.csv(dat2, file = "datos2.csv")
#
#PRUEBA ANOVA---------------------------------------------------------

##Se realiza un boxplot, se sacan los promedios y las desviaciones para 
#la exploración previa de los datos
boxplot (formula = DDT ~ profundidad, data = dat2)


#Promedio de las 3 profundidades en una tabla
promedio <- aggregate(formula= DDT ~ profundidad, data = dat2, FUN = mean)
promedio

#Desviación estándar de las 3 profundidades en una tabla
desv.est <- aggregate(formula= DDT ~ profundidad, data = dat2, FUN = sd)
desv.est

#Modelo lineal---------------------------------------------------------

#Especificar qué localidades son una variable explicativa (factor)

dat2$profundidad <- as.factor(dat2$profundidad)

#Preguntamos si localidades son reconocidas como factor en R

is.factor(dat2$profundidad)

# Combinar ambos resultados en una tabla
tabla1 <- data.frame(
  "profundidad" = promedio$profundidad,
  "DDT" = c(NA, NA, NA),
  "Desv.Est" = c(NA, NA, NA)
)
tabla1[,2]<-promedio[,2]
tabla1[,3]<-desv.est[,2]
tabla1

#Ahora sí Modelo lineal
#Lo anterior se hace porque lm trabaja con la variable respuesta en función
#de la variable explicativa, la cual debe ser un factor

lm(DDT ~ profundidad, data = dat2)

#Se guarda el modelo bajo un objeto mod1

mod1 <- lm(DDT ~ profundidad, data = dat2)

#Aplicar función anova al objeto anterior (al modelo lineal)

anova(mod1)

#Obtenemos el valor crítico de F bajo la HO
#Buscamos valores de los g.l para numerador y el denominador de la tabla anterior
#considerando valor de (alfa) p= 0.05

qf(p = 0.05 , df1 = 2, df2 =  27, lower.tail=F)

#Aplicamos summary al modelo lineal ajustado

summary(mod1)

#Comparaciones múltiples de promedios: Aplicación de Prueba t

pairwise.t.test(x = dat2$DDT,
                g = dat2$profundidad,
                p.adjust.method = "bonferroni",
                alternative = "two.sided")

#Aplicamos TUKEY

res <- aov(mod1)

DHS <- TukeyHSD(x = res)

DHS

plot(DHS)

#REALIZAR GRÁFICOS--------------

library(ggplot2)

#Figura b?sica
fig1 <- ggplot(dat2, aes(y = DDT, x = profundidad)) +
  geom_point()
fig1

#Figura b?sica con los promedios
fig1.1 <- fig1 +
  geom_point(dat2 = tabla1,
             
             aes(x = profundidad, y = DDT, col = profundidad),
             size = 3)

fig1.1

#Figura b?sica con promedios y barras de desviaci?n est?ndar
fig1.2 <- fig1.1 +
  geom_point(data = tabla1,
             
             aes(x = profundidad, y = DDT, col = profundidad),
             size = 3) +
  geom_errorbar(
    data = tabla1,
    aes(
      x = profundidad,
      ymin = DDT - Desv.Est,
      ymax = DDT + Desv.Est
    ),
    width = 0.2
  )
fig1.2

#figura b?sica con promedios, barras de desviaci?n est?ndar y cambios en la est?tica de
#la figura
fig1.3 <- fig1.2 +
  theme_bw() +
  ylab(expression(paste("DDT ", "(mg ", "/l)"))) +
  xlab("Profundidad")
fig1.3

#figura s?lo con promedios, barras de desviaci?n est?ndar y cambios en la est?tica de
#la figura
fig1.4 <- ggplot(data = tabla1, aes(y = DDT, x = profundidad)) +
  geom_errorbar(
    data = tabla1,
    aes(
      x = profundidad,
      ymin = DDT - Desv.Est,
      ymax = DDT + Desv.Est
    ),
    width = 0.2
  ) +
  geom_point(aes(col = profundidad), size = 3) +
  theme_bw() +
  ylab(expression(paste("DDT ", "(mg ", "/l)"))) +
  xlab("Profundidad")
fig1.4
