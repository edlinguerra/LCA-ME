library(GAD)

datos<-dat
BA <- as.fixed(datos$BA)
CI <- as.fixed(datos$CI)
DBO <-datos$DBO 
mod1<-lm(DBO~BA*CI)

estimates(mod1)

gad(mod1)

2*2*(25-1)

boxplot(DBO~BA*CI)


# Modelo anidado, en el que zonas se anida en localidades (CI)

BA<-as.fixed(datos$BA)
CI<-as.fixed(datos$CI)
Zona<-as.random(datos$Zona) #esta es la novedad del diseño, reconocemos la fuente de variación zona
DBO <- datos$DBO

mod2<-lm(DBO ~ BA + CI + BA*CI + Zona%in%(BA*CI)) # con %in% anidamos un factor aleatorio en otro

estimates(mod2) # con estimates identificamos los cuadrados medios esperados y el correcto denominador para el F

gad(mod2)

Analysis of Variance Table

Response: DBO
              Df  Sum Sq Mean Sq F value    Pr(>F)    
  BA          1  170.32  170.32 11.8657 0.0033309 ** 
  CI          1  427.88  427.88 29.8097 5.242e-05 ***
  BA:CI       1  289.64  289.64 20.1787 0.0003695 ***
  Zona       16  229.66   14.35  1.1481 0.3278256    
  Residual   80 1000.19   12.50 
  
  
library(ggplot2)
  
fig <- ggplot(datos, aes(x=CI, y=DBO, fill=BA))+
       geom_boxplot()

fig




