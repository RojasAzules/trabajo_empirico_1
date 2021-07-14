
#instalando paquetes
install.packages("urca")
install.packages("astsa")

library(urca) #para pruebas de raices unitarias 
library(astsa) # para estimar modelos arima o arma

#para quitar notación científica 
options (scipen=999)

#SERIE 1
attach(TE1)

#tengo 300 observaciones en cada una de las series

#genero variable n
n<- 1:300

#Visualizo mi serie
plot(n,S1,type="l") 
#Del gráfico se puede observsar que la serie es estacionaria, con varianza similar (homocedástica).
#Sin embargo, a partir de la observacion 200 se observa un poco de dispersión.
#No hay cambios de nivel.
#La media se ve alrededor de cero.

#Prueba de raíz unitaria dicky fuller
#Dado que no se conoce de qué trata la serie, se le pusieron 15 rezagos para no quedarnos cortos.
s1df1<-ur.df(S1,type="drift", lags=15, selectlags="AIC") 


#Funciones de autocorrelación y correlación parcial

acf2(S1)
#la ACF se ve con estructura.
#Podría ser un ARMA

#Estimación del modelo 





#SERIE 3
attach(TE1)

#Visualizo mi serie
plot(n,S3,type="l") 
