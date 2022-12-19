"Desarrollar un modelo que pueda predecir las ventas 
segun el gasto en publicidad (Radio, TV, periodico). 

Sales (Ventas): variable dependiente 
TV, Radio, Newspaper: variables independientes"

library(dplyr)
library(ggplot2)

adv <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-06/data/advertising.csv")

####################################################
############# EXPLORACION DE LOS DATOS #############
####################################################

str(adv)
sum(complete.cases(adv)) # no NA's
round(cor(adv),4) # matriz de correlacion

pairs(~ Sales + TV + Radio + Newspaper, 
      data = adv, gap = 0.4, cex.labels = 1.5)

#################################################### 
####### AJUSTE DE MODELO DE REGRESION LINEAL #######
###### MEDIANTE MINIMOS CUADRADOS ORDINARIOS  ###### 
####################################################
"Ventas = beta0 + beta1*TV + beta2*Radio + beta3*Newspaper"

attach(adv) # para llamar a la variables directamente

m1 <- lm(Sales ~ TV + Radio + Newspaper) #ajuste

summary(m1) # resumen de los detalles del modelo

# Adjusted R-squared:  0.9011 
# El modelo explica el 90.11% de la variacion, considerando el número de variables

#H0: beta_i = 0
#Ha: beta_i != 0 
# Newspaper no es significativa, no se puede rechazar que el coeficiente sea 0 

m2 <- update(m1, ~.-Newspaper)
summary(m2)

# Adjusted R-squared:  0.9016
# El modelo explica el 90.16% de la variacion

#################################################### 
#######   VERIFICAR DIST NORMAL DE ERRORES   #######
####################################################

residuos_estandarizados_m1 <- rstandard(m1)
residuos_estandarizados_m2 <- rstandard(m2)

# H0: distribucion normal
# Ha: distribucion diferente a la normal

shapiro.test(residuos_estandarizados_m1)
# p-value = 0.001339

shapiro.test(residuos_estandarizados_m2)
# p-value = 0.001365

"para ambos modelos, y para niveles de confianza de: 
90% (alpha=0.1) 
95% (alpha=0.05)
99% (alpha=0.01)
se rechaza la hipótesis nula, es decir, 
los residuos no tienen distribución normal"

par(mfrow = c(2,3))
plot(TV, residuos_estandarizados_m1)
plot(Radio, residuos_estandarizados_m1)
qqnorm(residuos_estandarizados_m1)
qqline(residuos_estandarizados_m1)

plot(TV, residuos_estandarizados_m2)
plot(Radio, residuos_estandarizados_m2)
qqnorm(residuos_estandarizados_m2)
qqline(residuos_estandarizados_m2)
dev.off()

#################################################### 
###########     PREDICCION DE VENTAS     ###########
####################################################

"Elegimos el modelo 2 (m2) como el mejor por el mayor r^2 ajustado"

data <-data.frame(
  TV = c(250,300,350),
  Radio = c(45,50,60)
)
predict(m2, newdata=data, interval = "confidence", level=0.95)


####################################################
#################################################### 
####################################################

m3 <- update(m2, ~.-Radio)
summary(m3)
# Adjusted R-squared:  0.8112
residuos_estandarizados_m3 <- rstandard(m3)
shapiro.test(residuos_estandarizados_m3)
# p-value = 0.526
# Errores con distribución normal para nivel de confianza de 90%, 95% o 99%.



