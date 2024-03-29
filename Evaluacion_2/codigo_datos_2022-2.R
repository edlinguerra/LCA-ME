############# GENERACIÓN DE DATOS POR EQUIPO #####################
# correr todo el código para generar los datos de su examen
# ingrese el número de su equipo en los paréntesis de la función set.seed
# ejemplo set.seed(3) asumiendo que está en el equipo 3

set.seed() 

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
datos2

boxplot(DDT~profundidad, data = datos2)

write.csv(dat2, file = "datos2.csv")