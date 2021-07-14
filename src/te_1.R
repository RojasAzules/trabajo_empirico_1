#####
# Librerías
library(tidyverse)
library(ggplot2)
library(urca)
library(astsa)

#####
# Cargar datos
data <- read.csv('./data/TE1.csv')
data$n <- 1:dim(data)[1]

#####
# Gráfica de la serie
data %>%
  ggplot(aes(x = n, y = s1)) + 
  geom_line()

#####
# Autocorrelaciones
acf2(data$s1)

# Dickey Fuller - AIC
df_s1 <- ur.df(data$s1, type = "drift", lags = 15, selectlags = "AIC")
# Revisar coeficientes
df_s1@testreg
# Dickey Fuller - SBIC
df_s1_ <- ur.df(data$s1, type = "drift", lags = 15, selectlags = "BIC")
# Revisar coeficientes
df_s1_@testreg

# ARIMA
s1_m1 <- sarima(data$s1, p = 1, d = 0, q = 1)
s1_m1[["ttable"]]

s1_m2 <- sarima(data$s1, p = 0, d = 0, q = 2)
s1_m2[["ttable"]]

s1_m3 <- sarima(data$s1, p = 0, d = 0, q = 3)
s1_m3[["ttable"]] # Si sobreidentifico un MA(2) el segundo tèrmino deja de ser significativo

s1_m4 <- sarima(data$s1, p = 3, d = 0, q = 3)
s1_m4[["ttable"]]

s1_m5 <- sarima(data$s1, p = 3, d = 0, q = 2)
s1_m5[["ttable"]] # Aquí el (3) del AR deja de ser significativo! 

s1_m6 <- sarima(data$s1, p = 2, d = 0, q = 2)
s1_m6[["ttable"]]

s1_m7 <- sarima(data$s1, p = 2, d = 0, q = 1)
s1_m7[["ttable"]]

s1_m8 <- sarima(data$s1, p = 3, d = 0, q = 1)
s1_m8[["ttable"]] # Si sobreidentifico se me desacomoda el segundo tèrmino del ar

# Por principio de parsimonía mejor un ARMA(2,1)
help(sarima.for)
help(sarima.sim)

#####
# Gráfica de la serie
data %>%
  ggplot(aes(x = n, y = s3)) + 
  geom_line()
# Autocorrelaciones
acf2(data$s3)
# Dickey Fuller
df_s3 <- ur.df(data$s3, type = "drift", lags = 15, selectlags = "BIC")
# Revisar coeficientes
df_s3@testreg["coefficients"]
# AIC
df_s3_ <- ur.df(data$s3, type = "drift", lags = 15, selectlags = "AIC")
# Revisar coeficientes
df_s3_@testreg["coefficients"]

# ARIMA
s3_m1 <- sarima(data$s3, p = 1, d = 0, q = 1)
s3_m1[["ttable"]]

s3_m2 <- sarima(data$s3, p = 0, d = 0, q = 1)
s3_m2[["ttable"]]

s3_m3 <- sarima(data$s3, p = 0, d = 0, q = 2)
s3_m3[["ttable"]]

# Es un MA(1) !


#####
# Gráfica de la serie
data %>%
  ggplot(aes(x = n, y = s2)) + 
  geom_line()
# Autocorrelaciones
acf2(data$s2)
# Dickey Fuller
df_s2 <- ur.df(data$s2, type = "drift", lags = 15, selectlags = "BIC")
# Revisar coeficientes
df_s2@testreg["coefficients"]
# AIC
df_s2_ <- ur.df(data$s2, type = "drift", lags = 15, selectlags = "AIC")
# Revisar coeficientes
df_s2_@testreg["coefficients"]

# ARIMA
s2_m1 <- sarima(data$s2, p = 1, d = 0, q = 1)
s2_m1[["ttable"]]

s2_m2 <- sarima(data$s3, p = 2, d = 0, q = 2)
s2_m2[["ttable"]]

s2_m3 <- sarima(data$s3, p = 1, d = 0, q = 2)
s2_m3[["ttable"]]

s2_m4 <- sarima(data$s3, p = 2, d = 0, q = 1)
s2_m4[["ttable"]]

s2_m5 <- sarima(data$s3, p = 2, d = 0, q = 0)
s2_m5[["ttable"]]

s2_m6 <- sarima(data$s3, p = 1, d = 0, q = 0)
s2_m6[["ttable"]]

# Es un AR(2), si no es que incluso un AR((2))
