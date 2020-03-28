
datos1<-data.frame("aldrin" = rnorm(10, mean = 0.059, sd = 0.02),
                   "dieldrin" = rnorm(10, mean = 0.069, sd = 0.02),
                   "DDT" = rnorm(10, mean = 1.6, sd = 0.3))

datos1

write.csv(datos1, file = "datos1.csv")

p10.ll<-data.frame("profundidad" = rep("10m", 10), "epoca" = rep("lluvia", 10), "DDT" = rnorm(10, mean = 1.6, sd = 0.3))
p10.sec<-data.frame("profundidad" = rep("10m", 10), "epoca" = rep("secas", 10), "DDT" = rnorm(10, mean = 1.1, sd = 0.4))
p30.ll<-data.frame("profundidad" = rep("30m", 10), "epoca" = rep("lluvia", 10), "DDT" = rnorm(10, mean = 0.8, sd = 0.3))
p30.sec<-data.frame("profundidad" = rep("30m", 10), "epoca" = rep("secas", 10), "DDT" = rnorm(10, mean = 0.3, sd = 0.3))

datos2 <- rbind(p10.ll, p10.sec, p30.ll, p30.sec)
datos2
write.csv(datos2, file = "datos2.csv")

boxplot(DDT ~ profundidad*epoca, data = datos2)

library(ggplot2)
library(dplyr)

ggplot(datos2, aes(x=profundidad, y=DDT, fill=epoca))+
  geom_boxplot()

mod1 <- lm(DDT ~ profundidad*epoca, data = datos2)

anova(mod1)

ps <- position_dodge(0.3)

datos2 %>%
  group_by(profundidad, epoca) %>%
   summarise(pro = mean(DDT), des = sd (DDT)) %>%
    ggplot(aes(x = profundidad, y = pro,colour = epoca, group = epoca)) +
    geom_errorbar(aes(ymin = pro - des, ymax = pro + des), 
                  width=.2, size=0.25, colour="black", position=ps)+
    geom_point(position = ps, size = 3)+
    xlab("Profundidad")+
    ylab("DDT (mg/l)")+
    theme_bw()


  