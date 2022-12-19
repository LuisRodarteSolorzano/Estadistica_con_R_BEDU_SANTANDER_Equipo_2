## Postwork. Predicciones de la temperatura global ##


# OBJETIVO
"Estimar modelos ARIMA y realizar predicciones"


# DESARROLLO
"Utilizando el siguiente vector numC)rico, realiza lo que se indica:"
  
url = "https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-07/Data/global.txt"
Global <- scan(url, sep="")
head(Global)
tail(Global)
class(Global)


###############################################################################

"
1. Crea una objeto de serie de tiempo con los datos de Global. La serie debe 
ser mensual comenzado en Enero de 1856
"
Global.ts <- ts(Global, start = c(1856, 1), frequency = 12)   # *datos mensuales


###############################################################################

"
2. Realiza una gr??fica de la serie de tiempo anterior de 2005
"
plot(Global.ts, xlab = "Tiempo", ylab = "Temperatura en B0C", 
     main = "Serie de Temperatura Global",
     sub = "Serie mensual: Enero de 1856 a Diciembre de 2005",
     col = "blue4")


###############################################################################

"
3. Ahora realiza una grafica de la serie de tiempo anterior, transformando a 
la primera diferencia:
"
plot(diff(Global.ts), xlab = "", ylab = "", col = "green")
title(main = "Serie de Temperatura Global",
      xlab = "Tiempo", ylab = "Dif log-Serie",
      sub = "Grafica de la serie transformada a la primera diferencia")


###############################################################################

"
4. ¿Consideras que la serie es estacionaria en niveles o en primera diferencia?
"
"R. La serie es estacionaria en primera diferencia"


###############################################################################

"
5. Con base en tu respuesta anterior, obten las funciones de autocorrelacion 
y autocorrelacion parcial?
"
acf(diff(Global.ts))
pacf(diff(Global.ts))


###############################################################################

"
6. De acuerdo con lo observado en las graficas anteriores, se sugiere un modelo 
ARIMA con AR(1), I(1) y MA desde 1 a 4 rezagos Estima los diferentes modelos 
ARIMA propuestos:
"
arima(Global.ts, order = c(1, 1, 1))
arima(Global.ts, order = c(1, 1, 2))
arima(Global.ts, order = c(1, 1, 3))
arima(Global.ts, order = c(1, 1, 4))


###############################################################################

"
7. Con base en el criterio de Akaike, estima el mejor modelo ARIMA y realiza 
una prediccion de 12 periodos (meses)
"

# Aquí se obtienen solo los valores AIC de cada modelo:
(ARIMA111 <- arima(Global.ts, order = c(1, 1, 1))$aic)
(ARIMA112 <- arima(Global.ts, order = c(1, 1, 2))$aic)
(ARIMA113 <- arima(Global.ts, order = c(1, 1, 3))$aic)
(ARIMA114 <- arima(Global.ts, order = c(1, 1, 4))$aic)


# Aqui se ordenan de menor a mayor en un data frame
tabla_resultados <- data.frame("Modelo"=c("ARIMA(1,1,4)", "ARIMA(1,1,3)", 
                                          "ARIMA(1,1,2)", "ARIMA(1,1,1)"),
  "Valor_AIC" = matrix(sort(c(ARIMA111, ARIMA112, 
                              ARIMA113, ARIMA114))))
View(tabla_resultados)



"Con base en el criterio de Akaike el mejor modelo ARIMA es el modelo 
ARIMA(1,1,4)."

# Prediccion a 12 periodos (meses)
fit <- arima(Global.ts, order = c(1, 1, 4))
fit
pr <- predict(fit, 12)$pred 
pr




