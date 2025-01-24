############# GENERACIÓN DE DATOS POR EQUIPO #####################
# Correr todo el código para generar los datos.
# Ingrese el número de su equipo en los paréntesis de la función set.seed
# ejemplo set.seed(6) asumiendo que está en el equipo 3
set.seed(6) #incluya el núemro de su equipo
aldrin <- c(seq(0.005,0.05, 0.005))
dieldrin <- c(seq(0.005,0.04, 0.005))
DDT <- c(seq(0.05,1.5,0.05))
dat1 <- data.frame(
  "Aldrin" = abs(rnorm(10, sample(aldrin, 1), 0.005)),
  "Dieldrin" = abs(rnorm(10, sample(dieldrin, 1), 0.005)),
  "DDT" = abs(rnorm(10, sample(DDT, 1), 0.05))
)
write.csv(x = dat1, file = "datos1.csv")
hist(dat1$DDT)

hist(dat1$Aldrin)

hist(dat1$Dieldrin)

#Realizamos una prueba de T.Test para el caso 1

t.test(x=dat1$DDT, mu=1, alternative = "greater")
boxplot(dat1)
summary(dat1)

############# GENERACIÓN DE DATOS POR EQUIPO #####################
# Correr todo el código para generar los datos.
# Ingrese el número de su equipo en los paréntesis de la función set.seed
# ejemplo set.seed(3) asumiendo que está en el equipo 3
set.seed(6) #incluya el núemro de su equipo
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

datos2
modelolineal<-lm(DDT~profundidad,data = datos2)
summary(modelolineal)

anova<-aov(modelolineal)
summary(anova)
boxplot(DDT~profundidad,data = datos2)       
aggregate(DDT~profundidad,data = datos2,max)   
prom<-aggregate(DDT~profundidad,data = datos2,mean)
des<-aggregate(DDT~profundidad,data = datos2,sd)
tbl<-data.frame("prof"=prom$profundidad,"crom"= prom$DDT, "sd"=des$DDT)
  tbl             
library(ggplot2)

fig1.4 <- ggplot(data = tbl, aes(y = crom, x = prof)) +
  geom_errorbar(
    data = tbl,
    aes(
      x = prof,
      ymin = crom - sd,
      ymax = crom + sd
    ),
    width = 0.2
  ) +
  geom_point(aes(col = prof), size = 3) +
  theme_bw()
fig1.4 

