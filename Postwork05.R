## Postwork Sesión 5 ##

"OBJETIVO
Realizar inferencia estadística para extraer información de la muestra que sea 
contrastable con la población

REQUISITOS
Haber desarrollado los postworks anteriores
Cubrir los temas del prework
Replicar los ejemplos de la sesión

DESARROLLO
El data frame iris contiene información recolectada por Anderson sobre 50 flores 
de 3 especies distintas (setosa, versicolor y virginca), incluyendo medidas en 
centímetros del largo y ancho del sépalo así como de los pétalos."

View(iris)

###############################################################################


"Estudios recientes sobre las mismas especies muestran que:"
  
"i) En promedio, el largo del sépalo de la especie setosa (Sepal.Length) es 
igual a 5.7 cm"

largo_esp_setosa <- subset(iris$Sepal.Length, iris$Species == "setosa")
largo_esp_setosa

"Planteamiento de hipótesis:"
"Ho: prom_sepal_length_setosa == 5.7"
"Ha: prom_sepal_length_setosa =! 5.7"

# Ho: mu = 5.7
# Ha: mu =! 5.7  # Dos colas

t.test(x = largo_esp_setosa, alternative = "two.sided", mu = 5.7,
       conf.level = 0.99)

"o también"
t.test(iris[iris$Species == 'setosa', "Sepal.Length"], 
       alternative = 'two.sided', mu=5.7,
       conf.level = 0.99)
       


# Nivel de significancia = 0.01 -> 0.01/2 = 0.005

"
A un nivel de confianza del 99%, se rechaza Ho, es decir, el promedio es distinto a 5.7 cm.
"

pt(4.872406, 5.139594)  # valor en cada cola



# Prueba de hipótesis para la media. Muestra grande

p_h_medias_mg <- function(media, mu_0, s, n) {
  num <- media - mu_0 
  den <- s/sqrt(n) 
  num/den
}

(media <- mean(iris[iris$Species == 'setosa', "Sepal.Length"]))
(desv_est <- sd(iris[iris$Species == 'setosa', "Sepal.Length"]))
(tm <- length(iris[iris$Species == 'setosa', "Sepal.Length"]))
e_p <- p_h_medias_mg(media, 5.7, desv_est, tm) 
e_p

alfa <- 0.01 
z1_alfa <- qnorm(1-alfa/2, lower.tail = FALSE) 
z1_alfa
z2_alfa <- qnorm(alfa/2, lower.tail = FALSE) 
z2_alfa

ifelse(e_p <= z1_alfa | e_p >= z2_alfa, "Se rechaza la hipótesis nula", 
       "No se rechaza la hipótesis nula")

###############################################################################

"ii) En promedio, el ancho del pétalo de la especie virginica (Petal.Width) es 
menor a 2.1 cm"

ancho_esp_virginica <- subset(iris$Petal.Width, iris$Species == "virginica")
ancho_esp_virginica

"Planteamiento de hipótesis:"
"Ho: prom_petal_width >= 2.1"
"Ha: prom_petal_width < 2.1"

# Ho: mu >= 2.1
# Ha: mu < 2.1  # Cola izquierda

t.test(x = ancho_esp_virginica, alternative = "less", mu = 2.1,
       conf.level = 0.99)

"o también"
t.test(iris[iris$Species == 'virginica', "Petal.Width"], 
       alternative = 'less', mu=2.1, conf.level = 0.99)


# Nivel de significancia = 0.01

"
A un nivel de confianza del 99%, se rechaza Ho, es decir, el promedio es 
menor a 2.1 cm.
"


###############################################################################

"iii) En promedio, el largo del pétalo de la especie virgínica es 1.1 cm más 
grande que el promedio del largo del pétalo de la especie versicolor."

# Primero realizamos una prueba de varianzas
# Ho: razón = 1
# Ha: razón =! 1

largop_esp_virginica <- subset(iris$Petal.Length, iris$Species == "virginica")
largop_esp_virginica

largop_esp_versicolor <- subset(iris$Petal.Length, iris$Species == "versicolor")
largop_esp_versicolor


var.test(largop_esp_virginica, largop_esp_versicolor, 
         ratio = 1, alternative = "two.sided", conf.level = 0.99)

"o también"
var.test(iris[iris$Species == "virginica", "Petal.Length"], 
         iris[iris$Species == "versicolor", "Petal.Length"], 
         ratio = 1, alternative = "two.sided", conf.level = 0.99)
         

"
A un nivel de confianza del 99%, no se rechaza Ho, es decir, no existe evidencia 
estadística suficiente para decir que las varianzas son diferentes.
"

"Planteamiento de hipótesis:"
"Ha: prom_largo_petalo_esp_virginica - prom_largo_petalo_esp_versicolor <= 1.1"
"Ha: prom_largo_petalo_esp_virginica - prom_largo_petalo_esp_versicolor > 1.1"

t.test(x = largop_esp_virginica, y = largop_esp_versicolor, 
       alternative = "greater", mu = 1.1, 
       var.equal = TRUE, conf.level = 0.99)

"o también"
t.test(iris[iris$Species == "virginica", "Petal.Length"],
       iris[iris$Species == "versicolor", "Petal.Length"],
       alternative = "greater", mu = 1.1, var.equal = TRUE)


"
A un nivel de confianza del 99%, se rechaza Ho, es decir, no existe suficiente 
evidencia para decir que el promedio del largo del pétalo de la especie virgínica 
es 1.1 cm más grande que el promedio del largo del pétalo de la especie versicolor
"


###############################################################################

"iv) En promedio, no existe diferencia en el ancho del sépalo entre las 3 especies."

"Planteamiento de hipótesis:
Ho: las medias son iguales en todas las especies
Ha: hay al menos alguna media distinta"


# Exploramos los datos de la muestra:
boxplot(iris$Sepal.Width ~ iris$Species, col = c("yellow", "blue","green"), 
        xlab = "Especies", ylab = "Ancho del sepalo")

boxplot(Sepal.Width ~ Species, data = iris)


# Medias
tapply(iris$Sepal.Width, iris$Species, mean)

# Esta es la forma de pedir un ANOVA en R:
anova_ancho_sepalo = aov(lm(iris$Sepal.Width ~ iris$Species))
anova_ancho_sepalo

"o también"
anova <- aov(Sepal.Width ~ Species,
             data = iris)
anova



# Pedimos un resumen de la tabla del ANOVA
summary(anova_ancho_sepalo)

"o también"
summary(anova)


# Elementos generados en el ANOVA:
names(anova_ancho_sepalo)

"
Bajo la Ho el estadístico de contraste F se distribuye como una F de grados 
de libertad (I-1), (n-I) donde I es el número de grupos que disponemos y n 
el tamaño total de la muestral. Así obtenemos el cuantil buscado:
"
  
qf(0.01, 3-1, 150-3, lower.tail = F)

"
A un nivel de confianza del 99% se rechaza Ho, y se concluye que hay evidencia
estadística suficiente para decir que hay al menos alguna media distinta
"

###############################################################################

"Utilizando pruebas de inferencia estadística, concluye si existe evidencia 
suficiente para concluir que los datos recolectados por Anderson están en línea 
con los nuevos estudios."



"Utiliza 99% de confianza para toda las pruebas, en cada caso realiza el 
planteamiento de hipótesis adecuado y concluye."