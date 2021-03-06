#Modelaci�n estad�stica
#S�nchez Miranda Aide Itzayana
#Evaluaci�n 2

#CASO 1 (2/3 puntos)
#Considerando que el inter�s es sobre posible contaminaci�n de POC, indique y resuelva:

#Hip�tesis de investigaci�n 
#R= Si el primer manto acu�fero cumple con las caracter�sticas organol�pticas establecidas
#por la NORMA OFICIAL MEXICANA NOM-127-SSA1-1994 entonces, se establecer� como alternativa
#para el abastecimiento de agua potable de un nuevo fraccionamiento 
 
#Hip�tesis Nula e hip�tesis alternativa
  #HO=Se espera que los niveles de concentraci�n de organoclorados ser�n nulos o menores a 
#los niveles establecidos de acuerdo con la NOM
  #Ha=Los niveles de concentraci�n de organoclorados ser�n mayores a los niveles establecidos
#de acuerdo con la NOM 

#Someta a aprueba la hip�tesis con la prueba estad�stica que mejor considere. Justifique la elecci�n de la prueba estad�stica. NO SE JUSTIFIC� EL USO DE LA PRUEBA 
View(`datos1.(1)`)
##Para probar la hip�tesis se usar� la prueba t porque medira los ni
datos<-`datos1.(1)`

t.test(datos$aldrin,alternative = c("greater"),mu=0.03)
t.test(datos$dieldrin,alternative = c("greater"),mu=0.03)
t.test(datos$DDT,alternative = c("greater"),mu=0.03). NO, el Mu es 0.1

#Describa los resultados, interprete. Use las herramientas gr�ficas y descriptivas que
#mejor considere.
boxplot(datos,(dat$DDT~profundidad*epoca))

#R= los niveles de elementos organoclorados establecidos (dependiendo el rango) que se
#permiten por la NOM-127-SSA1-1994,generan afectaciones hacia la salud y dentro de los
#ecosistemas y su funcionamiento, as� como las especies dependientes del agua del acu�fero.

#�Cu�l es la recomendaci�n respecto al uso del primer manto acu�fero como fuente de agua potable?
#R= Debido a los altos niveles de concentraci�n de organoclorados en el el primer manto acu�fero
#se recomienda no hacer uso de esta agua para uso o consumo domestico ni de cualquier
#tipo. 

#CASO 2. Indique y resuelva (3/7 puntos)
install.packages("GAD")
library(GAD)
library(ggplot2)
install.packages("matrxStars")
library(matrixStats)

#Hip�tesis de investigaci�n
#R=Si las concentraciones de DDT varian dependiendo las condiciones clim�ticas y de suelo,
#entonces su presencia ser� diferente en cada estacionalidad y a diferentes profundidades. ESTA PREDICCI�N NO INDICA DIRECCIONALIDAD, NO ES UNA BUENA HIP�TESIS.

#Hip�tesis y modelo lineal completo. Indique la naturaleza de los factores y n�mero de niveles de cada uno. NO, LAS HIP�TESIS ESTAD�STICAS SE DEFINEN POR FUENTE DE VARIACI�N, DE FORMA INDEPENDIENTE, Y LUEGO CON LA INTERACCI�N.
#H1=Las concentraciones de DDT ser�n mayores en temporadas de lluvias y a poca profundidad (10 mts.)
#Las concentraciones de DDT ser�n mayores en temporadas de lluvia
#Las concentraciones de DDT ser�n mayores a poca profundidad (10 mts.)

#H2:Las concentraciones de DDT ser�n menores en temporadas de sequ�a y a gran profundidad (30 mts.)
#Las Las concentraciones de DDT ser�n menores en temporadas de sequ�a
#Las Las concentraciones de DDT ser�n menores a gran profundidad (30 mts.)


#Someta a aprueba las hip�tesis con la prueba estad�stica que mejor considere. Justifique la elecci�n de la prueba estad�stica, asi como que se cumplan las espectativas de distribuci�n de los residuales y homogeneidad de varianzas. Demuestre el resultado proporcionando la tabla estad�stica que gener� la funci�n que eligi�.
#R=Usamos la funci�n anova para poner a prueba m�s de 2 factores
dat<-datos2
P<-as.fixed(dat$profundidad)
E<-as.fixed(dat$epoca)
DDT<-dat$DDT
Com.H<-lm(DDT~P*E)
anova(Com.H)
Tabla1<-data.frame("profundidad"=promedio$profundidad,"DDT"=promedio$epoca,"epoca"= c (1.3910232,0.7782492,1.1590860,0.2977898),"D.E"= c(0.3260213,0.2252244,0.4076108,0.2703780))
Tabla1
FALT� VALIDACI�N DE CONDICIONES (DISTRIBUCI�N DE RESIDUALES Y HOMOGENEIDAD DE VARIANZAS)
FALT� LA INTERPRETACI�N DEL RESULTADO

#Replique el siguiente gr�fico usando el paquete ggplot2. Consiste en proyectar los promedios y desviacion est�ndar seg�n profundidad y �poca. Le recomiendo ver el libro en l�nea de Chang (2018), especialmente el cap�tulo 7.7.

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

#�Cu�l es la recomendaci�n considerando los resultados?
#R= Debido a las altas concentraciones de DDT encontradas se recomienda la disminuci�n 
#parcial o total de plaguicidas, as� como politicas que le den seguimiento a las
#afectaciones socioambientales de estos plaguicidas para determinar cuales
#pueden ser utilizados a diferencia de aquellos que generen afectaciones a los ecosistemas
#como es el caso de por infiltraci�n de estos contaminantes en el acu�fero. 

PARA HACER ESTA RECOMENDACI�N, NO ERA NECESARIO UN AN�LISIS ESTAD�STICO. DEL AN�LISIS ESTAD�STICO HECHO PUEDES HACER RECOMENDACIONES M�S ESPEC�FICAS. 