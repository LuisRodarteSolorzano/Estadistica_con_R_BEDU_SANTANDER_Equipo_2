## Postwork Sesión 4 ##

# Objetivo
"Realizar un análisis probabilístico del total de cargos internacionales de una 
compañía de telecomunicaciones"

# Requisitos
"R, RStudio
Haber trabajado con el prework y el work"

# Desarrollo
"Utilizando la variable total_intl_charge de la base de datos telecom_service.csv 
de la sesión 3, realiza un análisis probabilístico. Para ello, debes determinar 
la función de distribución de probabilidad que más se acerque el comportamiento 
de los datos. Hint: Puedes apoyarte de medidas descriptivas o técnicas de 
visualización."


###############################################################################

telecom_s <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/telecom_service.csv")
View(telecom_s)
summary(telecom_s$total_intl_charge)

library(DescTools)
Mode(telecom_s$total_intl_charge); median(telecom_s$total_intl_charge); mean(telecom_s$total_intl_charge)
tc <- data.frame("Moda"= Mode(telecom_s$total_intl_charge),
                 "Mediana" = median(telecom_s$total_intl_charge),
                 "Media"= mean(telecom_s$total_intl_charge))
tc
View(tc)


hist(telecom_s$total_intl_charge)
boxplot(telecom_s$total_intl_charge, col = "green")


"Vamos a comenzar por calcular el número de clases y el ancho de la clase"
# Regla de la raiz = sqrt(n)
# Regla de Sturges = 1 + 3.3*log(n)

k = ceiling(1+3.3*log10(length(telecom_s$total_intl_charge)))
ac = (max(telecom_s$total_intl_charge)-min(telecom_s$total_intl_charge))/k


"Ahora vamos a crear una secuencia que vaya del valor mínimo al máximo con el ancho 
de clase. Esto nos va a permitir hacer cortes con las clases correspondientes:"

bins <- seq(min(telecom_s$total_intl_charge), max(telecom_s$total_intl_charge), by = ac)

total_intl_charge.clases <- cut(telecom_s$total_intl_charge, breaks = bins, include.lowest = TRUE, 
                         dig.lab = 8)


"Con esto, podemos crear nuestra tabla de distribución de frecuencias:"
dist.freq <- table(total_intl_charge.clases)
transform(dist.freq, 
          rel.freq=prop.table(Freq), 
          cum.freq=cumsum(prop.table(Freq)))


library(ggplot2)
ggplot(data = telecom_s,
       mapping = aes(x = total_intl_charge)) +
  geom_histogram(breaks = bins,
                 position = 'identity',
                 alpha = 0.8, color="pink", fill="blue") +
  labs(title = 'Histograma de total_intl_charge',
       x = 'total_intl_charge',
       y = 'conteos')


# Prueba Shapiro-Wilk
shapiro.test(telecom_s$total_day_charge)
"Ho: La variable distribuye como una normal
Ha: La variable no distribuye como una normal"


"La función de distribución de probabilidad que más se acerca al comportamiento 
de los datos es la función de distribución de probabilidad normal"
  

###############################################################################

"
Una vez que hayas seleccionado el modelo, realiza lo siguiente:
"

# 1. Grafica la distribución teórica de la variable aleatoria total_intl_charge

mean <- mean(telecom_s$total_intl_charge)
mean
sd <- sd(telecom_s$total_intl_charge)
sd

x <- seq(-4, 4, 0.01)*sd + mean
y <- dnorm(x, mean = mean, sd = sd) 

plot(x, y, type = "l", xlab = "X", ylab = "f(x)",
     main = "Densidad de Probabilidad Normal", 
     sub = expression(paste(mu == 2.764581, " y ", sigma == 0.7537726)))

integrate(dnorm, lower = x[1], upper = x[length(x)], mean=mean, sd = sd)


# 2. ¿Cuál es la probabilidad de que el total de cargos internacionales sea menor a 1.85 usd?

pnorm(q = 1.85, mean = mean, sd = sd)

plot(x, y, type = "l", xlab = "", ylab = "")
title(main = "Densidad de Probabilidad Normal", 
      sub = expression(paste(mu == 2.764581, " y ", sigma == 0.7537726)))

polygon(c(min(x), x[x<=1.85], 1.85), c(0, y[x<=1.85], 0), col="blue")

"R. 0.1125002"


# 3. ¿Cuál es la probabilidad de que el total de cargos internacionales sea mayor a 3 usd?

pnorm(q = 3, mean = mean, sd = sd, lower.tail = FALSE)

"R. 0.3773985"

plot(x, y, type = "l", xlab = "", ylab = "")
title(main = "Densidad de Probabilidad Normal", 
      sub = expression(paste(mu == 2.764581, " y ", sigma == 0.7537726)))

polygon(c(3, x[x>=3], max(x)), c(0, y[x>=3], 0), col="yellow")


  
# 4. ¿Cuál es la probabilidad de que el total de cargos internacionales esté entre 2.35 usd y 4.85 usd?
  
pnorm(q = 4.85, mean = mean, sd = sd) - pnorm(q = 2.35, mean = mean, sd = sd)

plot(x, y, type = "l", xlab="", ylab="")
title(main = "Densidad de Probabilidad Normal", 
      sub = expression(paste(mu == 2.764581, " y ", sigma == 0.7537726)))

polygon(c(2.35, x[x>=2.35 & x<=4.85], 4.85), c(0, y[x>=2.35 & x<=4.85], 0), 
        col="green")

"R. 0.7060114"



# 5. Con una probabilidad de 0.48, ¿cuál es el total de cargos internacionales más alto que podría esperar?

"Debemos encontrar el valor b, tal que P(X <= b) = 0.48:"
b <- qnorm(p = 0.48, mean = mean, sd = sd)
b

"Podemos combrar el resultaso anterior calculando P(X <= 2.726777):"
pnorm(q = 2.726777, mean = mean, sd = sd)
paste("Con una probabilidad de 0.48, el total de cargos internacionales más alto", 
      "que se podría esperar es de:", b)



# 6. ¿Cuáles son los valores del total de cargos internacionales que dejan exactamente al centro el 80% de probabilidad?"

"Debemos encontrar con las condiciones señaladas, los valores a y b, 
tales que P(a <= X <= b) = 0.80:"
a <- qnorm(p = 0.10, mean = mean, sd = sd)
a

b <- qnorm(p = 0.90, mean = mean, sd = sd)
b

"Podemos combrar el resultado anterior calculando P(1.798583 <= X <= 3.73058):"
pnorm(q = 3.73058, mean = mean, sd = sd) - pnorm(q = 1.798583, mean = mean, sd = sd)

plot(x, y, type = "l", xlab="", ylab="")
title(main = "Densidad de Probabilidad Normal", 
      sub = expression(paste(mu == 2.764581, " y ", sigma == 0.7537726)))

polygon(c(1.798583, x[x>=1.798583 & x<=3.73058], 3.73058), 
        c(0, y[x>=1.798583 & x<=3.73058], 0), col="dodgerblue2")




