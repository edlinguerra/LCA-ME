############# GENERACIÓN DE DATOS POR EQUIPO ##################### 
# Correr todo el código para generar los datos. 
# Ingrese el número de su equipo en los paréntesis de la función set.seed # ejemplo set.seed(3) asumiendo que está en el equipo 3 
set.seed(11) #incluya el núemro de su equipo 
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

#promedios
PromAL<- mean(dat1$Aldrin)
PromDI<-mean(dat1$Dieldrin)
PromDDT<- mean(dat1$DDT)
desvAL<- sd(dat1$Aldrin)
desvDI<- sd(dat1$Dieldrin)
desvDDT<- sd(dat1$DDT)
#histograma
hist(dat1$Aldrin)

abline(v= mean(dat1$Aldrin), col= "green")
hist(dat1$Dieldrin)

abline(v= mean(dat1$Dieldrin), col= "green")
hist(dat1$DDT)

abline(v= mean(dat1$DDT), col= "green")

#ee
eeAL<- desvAL/sqrt(10)
eeDI<- desvDI/sqrt(10)
eeDDT<- desvDDT/sqrt(10)


# someter a prueba t.test
refALDI<- 0.03
refDDT<- 1
boxplot_ALDI<- boxplot(aldrin, dieldrin, data=dat1)
boxplot_ALDI

boxplot_DDT<-boxplot(DDT, data=dat1)
boxplot_DDT

staccked_dat1<-stack(dat1)
head(staccked_dat1)

boxplot_stack<-boxplot(staccked_dat1$values~staccked_dat1$ind,col= rainbow(ncol(dat1)))
boxplot_stack
points(c(refALDI,refDDT),psh=8,col="black",lwd=1)

t.taldrin<-t.test(x = dat1$Aldrin, mu = refALDI, alternative = "less")
t.taldrin
t.tdieldrin<-t.test(x = dat1$Dieldrin, mu = refALDI, alternative = "less")
t.tdieldrin
t.tDDT<-t.test(x = dat1$DDT, mu = refDDT, alternative = "less")
t.tDDT