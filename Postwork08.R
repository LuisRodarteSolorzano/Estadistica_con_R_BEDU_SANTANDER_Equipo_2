################################################################
################            EQUIPO 2            ################
################################################################

## Postwork sesión 8. Análisis de la Inseguridad Alimentaria en México ##

"OBJETIVO

Realizar un análisis estadístico completo de un caso
Publicar en un repositorio de Github el análisis y el código empleado

DESARROLLO

Un centro de salud nutricional está interesado en analizar estadísticamente y 
probabilísticamente los patrones de gasto en alimentos saludables y no saludables 
en los hogares mexicanos con base en su nivel socioeconómico, en si el hogar 
tiene recursos financieros extrar al ingreso y en si presenta o no inseguridad 
alimentaria. Además, está interesado en un modelo que le permita identificar los 
determinantes socioeconómicos de la inseguridad alimentaria.

La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición (2012) 
levantada por el Instituto Nacional de Salud Pública en México. La mayoría de las 
personas afirman que los hogares con menor nivel socioeconómico tienden a gastar 
más en productos no saludables que las personas con mayores niveles socioeconómicos 
y que esto, entre otros determinantes, lleva a que un hogar presente cierta 
inseguridad alimentaria.

La base de datos contiene las siguientes variables:"
  
# - nse5f (Nivel socieconómico del hogar): 1 "Bajo", 2 "Medio bajo", 3 "Medio", 4 "Medio alto", 5 "Alto"
# - area (Zona geográfica): 0 "Zona urbana", 1 "Zona rural"
# - numpeho (Número de personas en el hogar)
# - refin (Recursos financieros distintos al ingreso laboral): 0 "no", 1 "sí"
# - edadjef (Edad del jefe/a de familia)
# - sexoje (Sexo del jefe/a de familia): 0 "Hombre", 1 "Mujer"
# - añosedu (Años de educación del jefe de familia)
# - ln_als (Logarítmo natural del gasto en alimentos saludables)
# - ln_alns (Logarítmo natural del gasto en alimentos no saludables)
# - IA (Inseguridad alimentaria en el hogar): 0 "No presenta IA", 1 "Presenta IA"


################################################################
################ 1. PLANTEAMIENTO DEL PROBLEMA  ################
################################################################

"La inseguridad alimentaria (IA), entendida como la disponibilidad limitada o 
incierta de alimentos nutricionalmente adecuados e inocuos, afecta el desarrollo 
personal en todos los ambitos (academico, laboral y social). 
La IA impacta de manera negativa en la calidad de vida, por lo que es de suma 
importancia identificar los factores que la determinan con el fin de poder combatirla.

En el presente proyecto analizaremos los un extracto de la Encuesta Nacional de 
Salud y Nutrición (2012). 

Primero haremos un análisis exploratorio con el fin de indentificar el tipo de 
variables, valores faltantes, distribuciones de las variables asi como su 
tendencia central y variablidad.

Posteriormente desarrollaremos modelos de regresion lineal y logistica que permitan 
identificar patrones en los gastos de als y alns, asi como patrones en la presencia 
o ausencia de IA. 

En todo el desarrollo usamos un nivel de confianza del 95%."

################################################################
#################   2. ANALISIS DESCRIPTIVO   ##################
################################################################
library(dplyr) 

df0 <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")

df <- na.omit(df0) # Eliminacion de NA's (de 40809 observaciones quedaron 20280)

str(df) # tipos de variables

#conversión de variables cualitativas (ordinales y nominales) indicadas como int a factor.
df$nse5f <- factor(df$nse5f, labels = c("Bajo", "Medio bajo", "Medio", "Medio alto", "Alto"))
df$area <- factor(df$area, labels = c("Zona urbana", "Zona rural"))
df$refin <- factor(df$refin, labels = c("no", "sí"))
df$sexojef <- factor(df$sexojef, labels = c("Hombre", "Mujer"))
df$Inseguridad_alimentaria <- factor(df$IA, labels = c("No presenta IA", "Presenta IA"))

summary(df[,-8]) # Resumen estadístico

#########################   GRAFICAS   #########################   

" si la variable es numérica se grafica un histograma, si la 
variable es cualitativa se hace un diagrama de barras"

for(i in seq_along(df)){
  if(is.numeric(df[,i])){
    hist(df[,i], xlab = names(df)[i], main = paste("Histograma para ", 
                                                      names(df)[i]))
  } else{
    plot(df[,i], xlab = names(df)[i], main = paste("Diagrama de barras para ", 
                                                   names(df)[i]))
  }
}

"
numpeho: distribución sesdaga a la derecha
edadjef: distribución sesdaga a la derecha
als: distribución sesdaga a la izquierda
alns: distribución sesdaga a la derecha
"

################################################################
#################  3. CÁLCULO DE PROBABILIDADES   ##############
################################################################

" Gráfica de la distribución del Ln del gasto en alimentos no saludables"

hist(df$ln_alns, 
     xlab = "Log natural del gasto en alimentos no saludables", ylab="Frecuencia", 
     main="  ")

mean <- mean(df$ln_alns)
"4.12"
sd <- sd(df$ln_alns)
"1.04"

x <- seq(-4, 4, 0.01)*sd + mean
y <- dnorm(x, mean = mean, sd = sd) 

plot(x, y, type = "l", xlab = "X", ylab = "f(x)",
     main = "Densidad de Probabilidad Normal", 
     sub = expression(paste(mu == 4.12, " y ", sigma == 1.04)))

"¿Cuál es la probabilidad de que un hogar tenga un logaritmo natural del gasto en alimentos no 
saludables mayor o igual a 6? "

valor_critico = 6

pnorm(q = valor_critico, mean = mean, sd = sd, lower.tail = FALSE)
"R. 0.036"

plot(x, y, type = "l", xlab = "", ylab = "")
polygon(c(valor_critico, x[x>=valor_critico], max(x)), c(0, y[x>=valor_critico], 0), col="blue")
title(main = "Densidad de Probabilidad Normal", 
      sub = expression(paste(mu == 4.12, " y ", sigma == 1.04)))

################################################################

"Grafica la distribución teórica de la variable aleatoria edadjef 
(Edad del jefe/a de familia) de los hogares con Inseguridad alimentaria"

mean <- mean(df[df$IA == 1, "edadjef"])
sd <- sd(df[df$IA == 1, "edadjef"])

x <- seq(-4, 4, 0.01)*sd + mean
y <- dnorm(x, mean = mean, sd = sd) 

hist(df[df$IA == 1, "edadjef"], 
     xlab = "edad jefe/a de familia", ylab="Frecuencia", 
     main="hogares con Inseguridad alimentaria")

plot(x, y, type = "l", xlab = "X", ylab = "f(x)",
     main = "Densidad de Probabilidad Normal", 
     sub = expression(paste(mu == 47.482, " y ", sigma == 15.066)))

integrate(dnorm, lower = x[1], upper = x[length(x)], mean=mean, sd = sd)

"¿Cuál es la probabilidad de que la edad del jefe/a de familia sea menor 
o igual a 30? "

pnorm(q = 30, mean = mean, sd = sd, lower.tail = TRUE)
"R. 0.1229423"

plot(x, y, type = "l", xlab = "", ylab = "")
polygon(c(min(x), x[x<=30], 30), c(0, y[x<=30], 0), col="blue")
title(main = "Densidad de Probabilidad Normal", 
      sub = expression(paste(mu == 47.482, " y ", sigma == 15.066)))

"¿Cuál es la probabilidad de que la edad del jefe/a de familia sea mayor 
o igual a 60? "

pnorm(q = 60, mean = mean, sd = sd, lower.tail = FALSE)
"R. 0.20302"

plot(x, y, type = "l", xlab = "", ylab = "")
polygon(c(60, x[x>=60], max(x)), c(0, y[x>=60], 0), col="blue")
title(main = "Densidad de Probabilidad Normal", 
      sub = expression(paste(mu == 47.48222, " y ", sigma == 15.06564)))

################################################################

" Gráfica de la distribución teórica de la variable aleatoria añosedu  
(Años de educación del jefe de familia) de los hogares con Inseguridad alimentaria"

hist(df[df$IA == 1, "añosedu"], 
     xlab = "años de educación", ylab="Frecuencia", 
     main="hogares con Inseguridad alimentaria")

mean <- mean(df[df$IA == 1, "añosedu"])
sd <- sd(df[df$IA == 1, "añosedu"])

x <- seq(-4, 4, 0.01)*sd + mean
y <- dnorm(x, mean = mean, sd = sd) 

plot(x, y, type = "l", xlab = "X", ylab = "f(x)",
     main = "Densidad de Probabilidad Normal", 
     sub = expression(paste(mu == 10.255, " y ", sigma == 4.540)))

integrate(dnorm, lower = x[1], upper = x[length(x)], mean=mean, sd = sd)

"¿Cuál es la probabilidad de que los años de educación del jefe de 
familia sea menor o igual a 6? "

pnorm(q = 6, mean = mean, sd = sd, lower.tail = TRUE)
"R. 0.1743161"

plot(x, y, type = "l", xlab = "", ylab = "")
polygon(c(min(x), x[x<=6], 6), c(0, y[x<=6], 0), col="blue")
title(main = "Densidad de Probabilidad Normal", 
      sub = expression(paste(mu == 10.255, " y ", sigma == 4.540)))

"¿Cuál es la probabilidad de que los años de educación del jefe de 
familia sea mayor o igual a 12? "

pnorm(q = 12, mean = mean, sd = sd, lower.tail = FALSE)
"R. 0.350393"

plot(x, y, type = "l", xlab = "", ylab = "")
polygon(c(12, x[x>=12], max(x)), c(0, y[x>=12], 0), col="blue")
title(main = "Densidad de Probabilidad Normal", 
      sub = expression(paste(mu == 10.255, " y ", sigma == 4.540)))

################################################################
################    4. HIPÓTESIS ESTADÍSTICAS    ###############
################################################################

"i) El promedio del número de personas en los hogares con IA es mayor que 
el promedio del número de personas en los hogares que no presentan IA

Planteamiento de hipótesis:
H0: prom_numpeho_con_IA - prom_numpeho_sin_IA <= 0
Ha: prom_numpeho_con_IA - prom_numpeho_sin_IA > 0

Primero realizamos una prueba de varianzas
H0: razón = 1
Ha: razón =! 1 "

var.test(df[df$IA == "1", "numpeho"], 
         df[df$IA == "0", "numpeho"], 
         ratio = 1, alternative = "two.sided", conf.level = 0.95)

"A un nivel de confianza del 95%, se rechaza H0, las varianzas NO son iguales"

t.test(df[df$IA == "1", "numpeho"],
       df[df$IA == "0", "numpeho"],
       alternative = "greater", mu = 0, var.equal = FALSE,
       conf.level = 0.95)

"A un nivel de confianza del 95%, se rechaza H0, el promedio del 
número de personas en los hogares con IA es mayor que el promedio 
del número de personas en los hogares que no presentan IA"

################################################################

"ii) El promedio de la edad del jefe/a de familia en los hogares con IA es mayor que 
el promedio de la edad del jefe/a de familia en los hogares que no presentan IA

Planteamiento de hipótesis:
H0: prom_edadjef_con_IA - prom_edadjef_sin_IA <= 0
Ha: prom_edadjef_con_IA - prom_edadjef_sin_IA > 0

Primero realizamos una prueba de varianzas
H0: razón = 1
Ha: razón =! 1"

var.test(df[df$IA == "1", "edadjef"], 
         df[df$IA == "0", "edadjef"], 
         ratio = 1, alternative = "two.sided", conf.level = 0.95)

"A un nivel de confianza del 95%, no se rechaza H0, las varianzas son iguales"

t.test(df[df$IA == "1", "edadjef"],
       df[df$IA == "0", "edadjef"],
       alternative = "greater", mu = 0, var.equal = TRUE,
       conf.level = 0.95)

"A nivel de confianza del 95%, se rechaza H0, y se concluye que el promedio de 
la edad del jefe/a de familia en los hogares con IA es mayor que 
el promedio de la edad del jefe/a de familia en los hogares que no presentan IA"

################################################################

"iii) El promedio de los años de educación del jefe/a de familia en los hogares con IA 
es menor que el promedio de los años de educación del jefe/a de familia en los hogares 
que no presentan IA

Planteamiento de hipótesis:
H0: prom_añosedu_con_IA - prom_añosedu_sin_IA >= 0
Ha: prom_añosedu_con_IA - prom_añosedu_sin_IA < 0

Primero realizamos una prueba de varianzas
Ho: razón = 1
Ha: razón =! 1"

var.test(df[df$IA == "1", "añosedu"], 
         df[df$IA == "0", "añosedu"], 
         ratio = 1, alternative = "two.sided", conf.level = 0.95)

"A un nivel de confianza del 95%, se rechaza H0, y se concluye que 
las varianzas NO son iguales"

t.test(df[df$IA == "1", "añosedu"],
       df[df$IA == "0", "añosedu"],
       alternative = "less", mu = 0, var.equal = FALSE,
       conf.level = 0.95)

"A nivel de confianza del 95%, se rechaza H0, y se concluye que el 
promedio de los años de educación del jefe/a de familia en los hogares 
con IA es menor que el promedio de los años de educación del jefe/a de 
familia en los hogares que no presentan IA"

################################################################

"iv) En promedio, no existe diferencia en el ln_alns 
(Logarítmo natural del gasto en alimentos no saludables) entre los 5 niveles 
socioeconómicos.

Planteamiento de hipótesis:
H0: las medias son iguales en todos los niveles socieconómicos
Ha: hay al menos alguna media distinta"

# Exploramos los datos de la muestra:
boxplot(ln_alns ~ nse5f, col = c("yellow", "blue","green", "purple", "red"),
        data = df,
        xlab = "Niveles socioeconómicos", 
        ylab = "LN del gasto en alimentos no saludables")


tapply(df$ln_alns, df$nse5f, mean) # Medias

anova <- aov(ln_alns ~ nse5f, data = df)
summary(anova)

################################################################

"v) En promedio, no existe diferencia en el ln_als 
(Logarítmo natural del gasto en alimentos saludables) entre los 5 niveles 
socioeconómicos.

Planteamiento de hipótesis:
H0: las medias son iguales en todos los niveles socieconómicos
Ha: hay al menos alguna media distinta"

# Exploramos los datos de la muestra:
boxplot(ln_als ~ nse5f, col = c("yellow", "blue","green", "purple", "red"),
        data = df,
        xlab = "Niveles socioeconómicos", 
        ylab = "LN del gasto en alimentos saludables")

tapply(df$ln_als, df$nse5f, mean)  # Medias

anova <- aov(ln_als ~ nse5f, data = df)
summary(anova)

################################################################
"Pruebas de independencia"

chisq.test(df$nse5f, df$IA, correct=FALSE)

chisq.test(df$area, df$IA, correct=FALSE)

chisq.test(df$refin, df$IA, correct=FALSE)

chisq.test(df$sexojef, df$IA, correct=FALSE)

"
La inseguridad alimentaria si depende del nivel socioeconómico, el área, 
los recursos financieros distintos al ingreso laboral y el sexo del jefe de 
familia
"

################################################################
########    5. MODELOS DE REGRESION LINEAL Y LOGISTICA    ######
################################################################

################################################################
##############       MODELOS DE REGRESION LINEAL     ###########
###### para identificar los patrones de gasto en als y alns ####
################################################################

"MODELO 1: ALS = BO + B1 NSE5F + B2 REFIN + B3 IA + E "

library(dplyr)
dfm1 <- select(df0, nse5f, refin, IA, ln_als)
sum(complete.cases(dfm1)) 
dfm1 = na.omit(dfm1) # 40022

m1 <- lm(ln_als ~ nse5f + refin + IA, data = dfm1)
summary(m1)

"El modelo m1 tiene todas sus variables significativas a un nivel de confianza de 95%. 
El modelo permite concluir que:
(1) pertenecer a un nivel socioeconómico más alto, en promedio incrementa 
en 18% el gasto en alimentos saludables,
(2) contar con recursos financieros externos incrementa en promedio 10% 
el gasto en alimentos saludables,
(3) tener inseguridad alimentaria reduce en promedio 2% el gasto en alimentos saludables"

"MODELO 2: ALNS = BO + B1 NSE5F + B2 REFIN + B3 IA + E "

dfm2 <- cbind(alns = df0$ln_alns, nse5f = df0$nse5f, refin = df0$refin, ia = df0$IA)
dfm2 <- na.omit(dfm2)

m2 <- lm(dfm2[,"alns"] ~ dfm2[,"nse5f"] + dfm2[,"refin"] + dfm2[,"ia"])
summary(m2)

"La variable de recursos financieros externos no es significativa 
para el modelo por lo que es eliminada"

"MODELO 3: ALNS = B0 + B1 NSE5F +  B2 IA + E"

m3 <- lm(dfm2[,"alns"] ~ dfm2[,"nse5f"] + dfm2[,"ia"])
summary(m3)

"El modelo m3 tiene todas sus variables significativas. 
El modelo permite concluir que:
(1) pertenecer a un nivel socioeconómico más alto implicaria 
un aumento en promedio de 20% del gasto en alimentos no saludables
(2) tener inseguridad alimentaria reduce en promedio 12% el gasto en alimentos no saludables"


################################################################
#############      MODELOS DE REGRESION LOGISTICA    ###########
######     para identificar los determinantes de la IA    ######
################################################################

library(MASS)

mod_logistic <- glm(IA ~ nse5f + area + numpeho + refin + sexojef + añosedu + ln_als + ln_alns, 
                    data=df, family = binomial)
summary(mod_logistic)


"A diferencia del modelo de regresión lineal, no es posible calcular el R^2 de una 
regresión logística, sin embargo, se puede calcular la bondad de ajuste con base en 
la log-verosimilitud del modelo nulo y el modelo actual."

pseudo_r2.1 <- (mod_logistic$null.deviance - mod_logistic$deviance)/mod_logistic$null.deviance
pseudo_r2.1
"0.09073219"

esttab <- coef(summary(mod_logistic)) # Valores de significancia
esttab

#########################   GRAFICAS   #########################
library(effects)
efectos <- allEffects(mod_logistic)
summary(efectos)


"Gráfica de relación entre inseguridad alimentaria con NSE"
plot(efectos$nse5f,
     axes = list(
       y = list(style = "stacked")),
     main = "Interacción con Inseguridad alimentaria",
     xlab = "Nivel socio-económico")


"Gráfica de relación entre inseguridad alimentaria con numpeho"
plot(efectos$numpeho,
     axes = list(
       y = list(style = "stacked")),
     main = "Interacción con Inseguridad alimentaria",
     xlab = "Número de personas en el hogar")


"Gráfica de relación entre inseguridad alimentaria con refin"
plot(efectos$refin,
     axes = list(
       y = list(style = "stacked")),
     main = "Interacción con Inseguridad alimentaria",
     xlab = "Recursos financieros distintos al ingreso laboral")


"Gráfica de relación entre inseguridad alimentaria con sexojef"
plot(efectos$sexojef,
     axes = list(
       y = list(style = "stacked")),
     main = "Interacción con Inseguridad alimentaria",
     xlab = "Sexo del jefe/a de familia")


"Gráfica de relación entre inseguridad alimentaria con añosedu"
plot(efectos$añosedu,
     axes = list(
       y = list(style = "stacked")),
     main = "Interacción con Inseguridad alimentaria",
     xlab = "Años de educación del jefe de familia")

############################### FIN ##################################



