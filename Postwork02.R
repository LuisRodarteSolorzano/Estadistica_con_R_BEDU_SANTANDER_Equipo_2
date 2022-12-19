# Postwork Sesión 2.

#### Objetivo

"- Conocer algunas de las bases de datos disponibles en `R`
- Observar algunas características y manipular los DataFrames con `dplyr`
- Realizar visualizaciones con `ggplot`

#### Requisitos

1. Tener instalado R y RStudio
2. Haber realizado el prework y estudiado los ejemplos de la sesión."

#### Desarrollo

"
1) Inspecciona el DataSet iris disponible directamente en la librería de ggplot. 
Identifica las variables que contiene y su tipo, asegúrate de que no hayan datos faltantes y 
que los datos se encuentran listos para usarse.
"

library(rmarkdown)
library(ggplot2)
library(stats)
library(dplyr)

BD.iris <- iris

class(BD.iris)
str(BD.iris)
dim(BD.iris)


BD.iris <- na.omit(BD.iris)
View(BD.iris)


###############################################################################
"
2) Crea una gráfica de puntos que contenga `Sepal.Length` en el eje horizontal, 
`Sepal.Width` en el eje vertical, que identifique `Species` por color y que el tamaño 
de la figura está representado por `Petal.Width`. 
Asegúrate de que la geometría contenga `shape = 10` y `alpha = 0.5`."


library(ggplot2)
names(BD.iris)
head(BD.iris)

plot1 <- ggplot(BD.iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species, 
                             size = Petal.Width)) + 
         geom_point(shape=10, alpha=0.5)
plot1


###############################################################################
"
3) Crea una tabla llamada `iris_mean` que contenga el promedio de todas las variables 
agrupadas por `Species`.
"

# install.packages("tidyverse")
library(tidyverse)
iris_mean <- select(BD.iris, Sepal.Length, Sepal.Width, 
                    Petal.Length, Petal.Width, Species) %>%
  mutate(species=factor(Species)) %>%
  group_by(Species) %>%
  summarize(Sepal.Length = mean(Sepal.Length),
            Sepal.Width = mean(Sepal.Width),
            Petal.Length = mean(Petal.Length),
            Petal.Width = mean(Petal.Width))
View(iris_mean)


###############################################################################
"
4) Con esta tabla, agrega a tu gráfica anterior otra geometría de puntos para agregar 
los promedios en la visualización. Asegúrate que el primer argumento de la geometría 
sea el nombre de tu tabla y que los parámetros sean `shape = 23`, `size = 4`, 
`fill = 'black'` y `stroke = 2`. También agrega etiquetas, temas y los cambios 
necesarios para mejorar tu visualización.
"

plot1 + geom_point(data=iris_mean, shape = 23, size = 4, fill= "black", stroke = 2) +
  theme_classic()
