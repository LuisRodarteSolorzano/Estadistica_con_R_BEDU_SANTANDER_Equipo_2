# Postwork Sesión 3

#### Objetivo

#- Realizar un análisis descriptivo de las variables de un dataframe

#### Requisitos

#1. R, RStudio
#2. Haber realizado el prework y seguir el curso de los ejemplos de la sesión
#3. Curiosidad por investigar nuevos tópicos y funciones de R

#### Desarrollo

"Utilizando el dataframe `boxp.csv` realiza el siguiente análisis descriptivo. No olvides excluir los missing values y transformar las variables a su
tipo y escala correspondiente."

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/boxp.csv")
library(dplyr)
library(DescTools)
library(ggplot2)
library(moments)

str(df)
df <- na.omit(df)
View(df)

df$Categoria <- factor(df$Categoria)


###############################################################################

"
1) Calcula e interpreta las medidas de tendencia central de la variable `Mediciones`
"

# Medidas de tendencia central

mean(df$Mediciones)    # Media aritmética
mean(df$Mediciones, trim = 0.20) # Media truncada

median(df$Mediciones)  # Mediana

Mode(df$Mediciones)    # Moda


###############################################################################
"
2) Con base en tu resultado anterior, ¿qué se puede concluir respecto al sesgo de `Mediciones`?
"
    "R. Dado que Moda < Mediana < Media, se concluye que la distrución de la variable Mediciones
     está sesgada a la derecha."


###############################################################################
"
3) Calcula e interpreta la desviación estándar y los cuartiles de la distribución de `Mediciones`
"
    "R. Desviación estándar y cuartiles:"

     Desviacón_est <- sd(df$Mediciones)
     Desviacón_est

    "De acuerdo al valor de la desviación estándar (53.76972), los datos o valores se encuentran 
     muy dispersos."

     cuartiles <- quantile(df$Mediciones, probs = c(0.25, 0.5, 0.75))
     cuartiles
     
     "De acuerdo con los valores de los cuartiles, el 25% de las observaciones son menores o
     o iguales a 23.45, el 50% son menores o iguales a 49.3 y el 75% de las observaciones son
     menores o iguales a 82.85."


###############################################################################
"     
4) Con ggplot, realiza un histograma separando la distribución de `Mediciones` por `Categoría`
¿Consideras que sólo una categoría está generando el sesgo?
"

"Vamos a comenzar por calcular el número de clases y el ancho de la clase"
# Regla de la raiz = sqrt(n)
# Regla de Sturges = 1 + 3.3*log(n)

k = ceiling(1+3.3*log10(length(df$Mediciones)))
ac = (max(df$Mediciones)-min(df$Mediciones))/k


"Ahora vamos a crear una secuencia que vaya del valor mínimo al máximo con el ancho 
     de clase. Esto nos va a permitir hacer cortes con las clases correspondientes:"

bins <- seq(min(df$Mediciones), max(df$Mediciones), by = ac)

Mediciones.clases <- cut(df$Mediciones, breaks = bins, include.lowest = TRUE, 
                      dig.lab = 8)


"Con esto, podemos crear nuestra tabla de distribución de frecuencias:"
dist.freq <- table(Mediciones.clases)
transform(dist.freq, 
          rel.freq=prop.table(Freq), 
          cum.freq=cumsum(prop.table(Freq)))
     

     
     ggplot(data = df,
            mapping = aes(x = Mediciones,
                          fill = factor(Categoria))) +
       geom_histogram(breaks = bins,
                      position = 'identity',
                      alpha = 0.8) +
       labs(title = 'Histograma de Mediciones por Categoría',
            fill = 'Categoría',
            x = 'Mediciones',
            y = 'conteos')
     

     "R. No considero que sólo una categoría está generando el sesgo."


###############################################################################     
"          
5) Con ggplot, realiza un boxplot separando la distribución de `Mediciones` por `Categoría` 
y por `Grupo` dentro de cada categoría. ¿Consideras que hay diferencias entre categorías? 
¿Los grupos al interior de cada categoría podrían estar generando el sesgo?
"
     
library(ggplot2)
     
ggplot(df, aes(x = Categoria, y = Mediciones, fill = factor(Grupo))) + geom_boxplot() +
  ylab("Mediciones") + 
  ggtitle("Distribución de Mediciones por Categoría y por Grupo dentro de cada categoría") +
  scale_fill_discrete(name = "Grupo") +
  theme_bw()

      "R. Respecto a si hay o no diferencias entre categorías, pareciera que no existen 
          diferencias significativas entre categorías.  
          
          Mientras que respecto a si los grupos al interior de cada categoría podrían 
          estar generando el sesgo, se observa que los Grupos O, pareciera que son los 
          que están generando al menos una parte del sesgo, eso aunado a que todas 
          las categorías tienen datos atípicos."
