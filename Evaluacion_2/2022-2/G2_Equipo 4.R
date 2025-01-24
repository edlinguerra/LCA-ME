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
#promedio 
prom.Aldrin <- mean(dat1$Aldrin)
prom.Dieldrin <- mean(dat1$Dieldrin)
prom.DDT <- mean(dat1$DDT)
# desviacion estandar 
desvAldrin<- sd(dat1$Aldrin)
desv.Dieldrin <- sd(dat1$Dieldrin)
desv.DDT <- sd(dat1$DDT)
# histograma Aldrin
hist(dat1$Aldrin)
abline(v=promAldrin)
abline(v=median(dat1$Aldrin),col="red")
#histograma Dieldrin
hist(dat1$Dieldrin)
abline(v=prom.Dieldrin)
abline(v=median(dat1$Dieldrin),col="red")
#histograma DDT
hist(dat1$DDT)
abline(v=prom.DDT)
abline(v=median(dat1$DDT),col="red")

# error estandar
ee <-desvAldrin/sqrt(dat1)
ee
ee <-desv.Dieldrin/sqrt(dat1)
ee
ee <-desv.DDT/sqrt(dat1)
ee
#la prediccion del modelo es: EN EL AGUA EXISTE UN
#aumento de POC que superan los límites permisibles de la 
#Norma Oficial Mexicana NOM-127-SSA1-1994
#lo opuesto a la prediccion es que un aumento de POC está presente en el agua 
#pero no llega a los límites permisibles por la 
## HO: ALDRIN&DIELDRIN< o =0.03mg/l #####DDT<o=1mg/l
#Ha: ALDRIN&DIELDRIN>0.03mg/l #####DDT>1mg/l
#t limite para rechazar 0
qt(0.05,10,lower.tail = FALSE)
t.test(x=dat1$Aldrin, alternative = "greater",mu=0.03)
qt(0.05,10,lower.tail = FALSE)
t.test(x=dat1$Dieldrin, alternative = "greater",mu=0.03)
qt(0.05,10,lower.tail = FALSE)
t.test(x=dat1$DDT, alternative = "greater",mu=1)
anova(dat1)


#############################################
##############PARTE 2########################
#############################################

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

datos2 <- rbind(p10, p20, p30)
write.csv
###grafico 
boxplot(datos2$DDT~datos2$profundidad)
aggregate(DDT~profundidad,data = datos2,mean)
# modelo lineal
modelo.lin2<-lm(datos2$DDT~datos2$profundidad)
summary(modelo.lin2)
plot(modelo2)
anova.DDT<-aov(modelo.lin2)
summary(anova.DDT)
promedio <- aggregate(DDT~profundidad, data =datos2, mean)
sd <- aggregate(DDT~profundidad, data =datos2,sd)
sd

tabladatos <- data.frame("profundidad"=promedio$profundidad,
                         "promedioDDT"=promedio$DDT, 
                         "desviacionDDT"=sd$DDT )
tabladatos

library(ggplot2)
grafica <- ggplot(data=tabladatos, aes(x=profundidad, y =promedioDDT))+
  geom_errorbar(data=tabladatos, aes(x=profundidad, ymin=promedioDDT-desviacionDDT, 
                                     ymax=promedioDDT+desviacionDDT), width=0.2)+
  geom_point(aes(col=profundidad), size=5)+
  theme_bw()
grafica
mean(datos2$DDT)
tabla1=aggregate(formula=DDT~profundidad, dat= datos2,FUN=mean)
colnames(tabla1) <- c("moneto","promedio")
tabla1
