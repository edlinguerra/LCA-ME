
#mi archivo de comandos

#este comando es para calcular el promedio de los datos
promDBO <- mean(datos1$DBO)

#Comando para desviación estandar
desvDBO <- sd(datos1$DBO)

#comando para el histograma con linea vertical de promedio
hist(datos1$DBO)
abline(v = promDBO)

#El error estándar (ee) de la media se calcula así: desviación / raíz del n
ee <- desvDBO / sqrt(50)

# La hipótesis alternativa es que la DBO en el río sea mayor 
# a la referencia (30 mg O/l/d según normativa) 

#La hipótesis nula sería que la DBO del río sea menor o igual a la referencia

ref <- 30 # esta es la referencia 

#aplicar prueba t con funcion t.test

t.test(x = datos1$DBO, mu = ref, alternative = "greater")

#con base en la evidencia, podemos descartar la H0, con menos del 5%
#de probabilidad de errar en descartarla. Esto implica que es muy probable 
#que la DBO sea mayor al valor de referencia.


#Valor de t para el IC al 95%

t.95 <- qt(p = 0.025, df = 49, lower.tail = FALSE)

IC95s <- promDBO + ee * t.95
IC95i <- promDBO - ee * t.95


#Valor de t para el IC al 99%

t.99 <- qt(p = 0.005, df = 49, lower.tail = FALSE)

IC99s <- promDBO + ee * t.99
IC99i <- promDBO - ee * t.99


# simular datos mas variables

datos1$sim <- rnorm(50, mean = promDBO, sd = 3 * desvDBO)
ee.sim <- sd(datos1$sim)/sqrt(nrow(datos1))

# Estimación de IC con datos más variados
IC95s.sim <- promDBO + ee.sim * t.95
IC95i.sim <- promDBO - ee.sim * t.95







