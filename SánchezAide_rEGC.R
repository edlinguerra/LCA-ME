#Modelación estadística
#Sánchez Miranda Aide Itzayana
#Evaluación 2

#CASO 1 (2/3 puntos)
#Considerando que el interés es sobre posible contaminación de POC, indique y resuelva:

#Hipótesis de investigación 
#R= Si el primer manto acuífero cumple con las características organolépticas establecidas
#por la NORMA OFICIAL MEXICANA NOM-127-SSA1-1994 entonces, se establecerá como alternativa
#para el abastecimiento de agua potable de un nuevo fraccionamiento 
 
#Hipótesis Nula e hipótesis alternativa
  #HO=Se espera que los niveles de concentración de organoclorados serán nulos o menores a 
#los niveles establecidos de acuerdo con la NOM
  #Ha=Los niveles de concentración de organoclorados serán mayores a los niveles establecidos
#de acuerdo con la NOM 

#Someta a aprueba la hipótesis con la prueba estadística que mejor considere. Justifique la elección de la prueba estadística. NO SE JUSTIFICÓ EL USO DE LA PRUEBA 
View(`datos1.(1)`)
##Para probar la hipótesis se usará la prueba t porque medira los ni
datos<-`datos1.(1)`

t.test(datos$aldrin,alternative = c("greater"),mu=0.03)
t.test(datos$dieldrin,alternative = c("greater"),mu=0.03)
t.test(datos$DDT,alternative = c("greater"),mu=0.03). NO, el Mu es 0.1

#Describa los resultados, interprete. Use las herramientas gráficas y descriptivas que
#mejor considere.
boxplot(datos,(dat$DDT~profundidad*epoca))

#R= los niveles de elementos organoclorados establecidos (dependiendo el rango) que se
#permiten por la NOM-127-SSA1-1994,generan afectaciones hacia la salud y dentro de los
#ecosistemas y su funcionamiento, así como las especies dependientes del agua del acuífero.

#¿Cuál es la recomendación respecto al uso del primer manto acuífero como fuente de agua potable?
#R= Debido a los altos niveles de concentración de organoclorados en el el primer manto acuífero
#se recomienda no hacer uso de esta agua para uso o consumo domestico ni de cualquier
#tipo. 

#CASO 2. Indique y resuelva (3/7 puntos)
install.packages("GAD")
library(GAD)
library(ggplot2)
install.packages("matrxStars")
library(matrixStats)

#Hipótesis de investigación
#R=Si las concentraciones de DDT varian dependiendo las condiciones climáticas y de suelo,
#entonces su presencia será diferente en cada estacionalidad y a diferentes profundidades. ESTA PREDICCIÓN NO INDICA DIRECCIONALIDAD, NO ES UNA BUENA HIPÓTESIS.

#Hipótesis y modelo lineal completo. Indique la naturaleza de los factores y número de niveles de cada uno. NO, LAS HIPÓTESIS ESTADÍSTICAS SE DEFINEN POR FUENTE DE VARIACIÓN, DE FORMA INDEPENDIENTE, Y LUEGO CON LA INTERACCIÓN.
#H1=Las concentraciones de DDT serán mayores en temporadas de lluvias y a poca profundidad (10 mts.)
#Las concentraciones de DDT serán mayores en temporadas de lluvia
#Las concentraciones de DDT serán mayores a poca profundidad (10 mts.)

#H2:Las concentraciones de DDT serán menores en temporadas de sequía y a gran profundidad (30 mts.)
#Las Las concentraciones de DDT serán menores en temporadas de sequía
#Las Las concentraciones de DDT serán menores a gran profundidad (30 mts.)


#Someta a aprueba las hipótesis con la prueba estadística que mejor considere. Justifique la elección de la prueba estadística, asi como que se cumplan las espectativas de distribución de los residuales y homogeneidad de varianzas. Demuestre el resultado proporcionando la tabla estadística que generó la función que eligió.
#R=Usamos la función anova para poner a prueba más de 2 factores
dat<-datos2
P<-as.fixed(dat$profundidad)
E<-as.fixed(dat$epoca)
DDT<-dat$DDT
Com.H<-lm(DDT~P*E)
anova(Com.H)
Tabla1<-data.frame("profundidad"=promedio$profundidad,"DDT"=promedio$epoca,"epoca"= c (1.3910232,0.7782492,1.1590860,0.2977898),"D.E"= c(0.3260213,0.2252244,0.4076108,0.2703780))
Tabla1
FALTÓ VALIDACIÓN DE CONDICIONES (DISTRIBUCIÓN DE RESIDUALES Y HOMOGENEIDAD DE VARIANZAS)
FALTÓ LA INTERPRETACIÓN DEL RESULTADO

#Replique el siguiente gráfico usando el paquete ggplot2. Consiste en proyectar los promedios y desviacion estándar según profundidad y época. Le recomiendo ver el libro en línea de Chang (2018), especialmente el capítulo 7.7.

promedio<-aggregate(DDT~profundidad*epoca,data=datos2,FUN=mean)
promedio 

desv.estp<-aggregate(DDT~profundidad*epoca,data=datos2,FUN=sd)
desv.estp

#En epoca de lluvia y baja profundidad (10 mts)
TLB<-data.frame("profundidad"=promedio$profundidad,"DDT"=promedio$epoca,"epoca"= c (1.3910232,0.7782492,1.1590860,0.2977898),"D.E"= c(0.3260213,0.2252244,0.4076108,0.2703780))
TLB
TLB[,2]<-promedio[,3]
TLB[,2]
TLB[,3]<-desv.estp[,2]
TLB[,3]

graficoR<-ggplot(data=LB, aes(x=profundidad, y=DDT, fill=epoca))+
          geom_errobar(data=LB, aes(x=profundidad,ymin=DDT-DE,ymax=DDT+DE), position=position_dodge(0.5),width-0.2)+
          geom_point(aes(col=epoca),position=position_dodge(0.5),size=2)+
          theme_bw()+
ylab(expression(paste("DDT","(mg","/l")))+
xlab("profundidad")
graficoR

#¿Cuál es la recomendación considerando los resultados?
#R= Debido a las altas concentraciones de DDT encontradas se recomienda la disminución 
#parcial o total de plaguicidas, así como politicas que le den seguimiento a las
#afectaciones socioambientales de estos plaguicidas para determinar cuales
#pueden ser utilizados a diferencia de aquellos que generen afectaciones a los ecosistemas
#como es el caso de por infiltración de estos contaminantes en el acuífero. 

PARA HACER ESTA RECOMENDACIÓN, NO ERA NECESARIO UN ANÁLISIS ESTADÍSTICO. DEL ANÁLISIS ESTADÍSTICO HECHO PUEDES HACER RECOMENDACIONES MÁS ESPECÍFICAS. 