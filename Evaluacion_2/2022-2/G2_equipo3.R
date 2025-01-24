

set.seed(3)
aldrin <- c(seq(0.005,0.05, 0.005))
dieldrin <- c(seq(0.005,0.04, 0.005))
DDT <- c(seq(0.05,1.5,0.05))


dat1 <- data.frame(
  "Aldrin" = abs(rnorm(10, sample(aldrin, 1), 0.005)),
  "Dieldrin" = abs(rnorm(10, sample(dieldrin, 1), 0.005)),
  "DDT" = abs(rnorm(10, sample(DDT, 1), 0.05))
)
write.csv(x = dat1, file = "datos1.csv")

dat1
boxplot(dat1)
summary(dat1)
library(tidyr)
dat1 <- pivot_longer(dat1,col=c(1:3),names_to = "muestras",values_to = "resultados")        
dat1


modelolineal <- lm(resultados~muestras,data=dat1)
anova1 <- aov(modelolineal)  
summary(anova1)
TukeyHSD(anova1)

##PARTE 2

set.seed(3)

efectos <- seq(0,1,0.1)
a <- abs(rnorm(10, sample(seq(0.5,0.8,0.1), 1), 0.05))
p10<-data.frame("Prof" = rep("10m", 10),
                
                "DDT" = abs(rnorm(10, mean = sample(efectos,1) + a, sd = 0.3)))

p20<-data.frame("Prof" = rep("20m", 10),
                
                "DDT" = abs(rnorm(10, mean = sample(efectos,1) + a/2, sd = 0.3)))

p30<-data.frame("Prof" = rep("30m", 10),
                
                "DDT" = abs(rnorm(10, mean = sample(a, 1), sd = 0.3)))

datos2 <- rbind(p10, p20, p30)
write.csv(datos2, file = "datos2.csv")



datos2
boxplot(DDT~Prof,datos2)
maximos <- aggregate(DDT~Prof,data=datos2, max)
maximos

modelolineal2 <- lm(DDT~Prof,data=datos2)
anova2 <- aov(modelolineal2)
summary(anova2)
TukeyHSD(anova2)

promedio <- aggregate(DDT~Prof, data=datos2, mean)
sd <- aggregate(DDT~Prof, data= datos2, sd)


promedio;sd


basedata <- data.frame("Prof"=promedio$Prof, 
                       "DDTmean"=promedio$DDT,
                       "DDTsd"=sd$DDT)
basedata




library(ggplot2)

fig1.4 <- ggplot(data = basedata, aes(y = DDTmean, x = Prof)) +
  geom_errorbar(
    data = basedata,
    aes(
      x = Prof,
      ymin = DDTmean - DDTsd,
      ymax = DDTmean + DDTsd
    ),
    width = 0.2
  ) +
  geom_point(aes(col = Prof), size = 3) +
  theme_bw()
fig1.4
