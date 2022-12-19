# Postwork Sesión 1.

#### Objetivo

"El Postwork tiene como objetivo que practiques los comandos básicos aprendidos 
durante la sesión, de tal modo que sirvan para reafirmar el conocimiento. Recuerda 
que la programación es como un deporte en el que se debe practicar, habrá caídas, 
pero lo importante es levantarse y seguir adelante. Éxito"

#### Requisitos
#- Concluir los retos
#- Haber estudiado los ejemplos durante la sesión

#### Desarrollo

"El siguiente postwork, te servirá para ir desarrollando habilidades como si se 
tratara de un proyecto que evidencie el progreso del aprendizaje durante el módulo, 
sesión a sesión se irá desarrollando.
A continuación aparecen una serie de objetivos que deberás cumplir, es un ejemplo 
real de aplicación y tiene que ver con datos referentes a equipos de la liga española 
de fútbol (recuerda que los datos provienen siempre de diversas naturalezas), en 
este caso se cuenta con muchos datos que se pueden aprovechar, explotarlos y generar 
análisis interesantes que se pueden aplicar a otras áreas. Siendo así damos paso 
a las instrucciones:" 

#1. Del siguiente enlace, descarga los datos de soccer de la temporada 2019/2020 de la primera división de la liga española: https://www.football-data.co.uk/spainm.php


#2. Importa los datos a R como un Dataframe. NOTA: No olvides cambiar tu dirección de trabajo a la ruta donde descargaste tu archivo

# Se importa y se lee la BD
sp1 <- read.csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv")
sp1

dim(sp1)
class(sp1)
is.vector(sp1)
is.data.frame(sp1)



#4. Del dataframe que resulta de importar los datos a `R`, extrae las columnas que contienen los números de goles anotados por los equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG); guárdalos en vectores separados

# números de goles anotados por los equipos que jugaron en casa
Goles_en_casa <- sp1$FTHG
Goles_en_casa

# goles anotados por los equipos que jugaron como visitante
Goles_como_visitante <- sp1$FTAG
Goles_como_visitante

# Gana local (H), empate (D), gana visitante (A)
Resultado_final <- sp1$FTR      
Resultado_final



#4. Consulta cómo funciona la función `table` en `R`. Para ello, puedes ingresar los comandos `help("table")` o `?table` para leer la documentación.



#5. Responde a las siguientes preguntas:

#  a) ¿Cuántos goles tuvo el partido con mayor empate?

#  b) ¿En cuántos partidos ambos equipos empataron 0 a 0?

#  c) ¿En cuántos partidos el equipo local (HG) tuvo la mayor goleada sin dejar que el equipo visitante (AG) metiera un solo gol?


### SOLUCIÓN ###

"Para poder responder las preguntas anteriores necesitamos elaborar una tabla cruzada:"

# Tablas de frecuencias de los totales de goles en casa (número de partidos)
Totales_de_goles_en_casa <- table(sp1$FTHG)
Totales_de_goles_en_casa

# Tablas de frecuencias de los totales de goles como visitante (número de partidos)
Totales_de_goles_como_visitante <- table(sp1$FTAG)
Totales_de_goles_como_visitante

# Tabla cruzada
Tabla_cruzada <- table(Goles_en_casa, Goles_como_visitante)
Tabla_cruzada



#  a) ¿Cuántos goles tuvo el partido con mayor empate?

# Aquí obtenemos el número máximo de goles de los partidos con empate
sp1$empates = ifelse(Goles_en_casa == Goles_como_visitante, 'empate', 'None')
Maximo <- max(Goles_en_casa[sp1$empates == 'empate'])
Maximo
Num_goles_Mayor_empate <- 2*Maximo
Num_goles_Mayor_empate

paste("El partido con mayor empate tuvo:", Num_goles_Mayor_empate, "goles", 
      Maximo, "por cada equipo")


# También podemos hacer lo siguiente:
# Aquí obtenemos número de partidos por empate: 0-0, 1-1, 2-2, 3-3, 4-4, 5-5
Totales_de_empates <- diag(Tabla_cruzada)
Totales_de_empates

# Aquí obtenemos el número de goles por cada equipo en el partido con mayor empate
(goles_mayor_empate <- Totales_de_empates[5])

print("El partido con mayor empate tuvo 8 goles, 4 de cada equipo")



# Desgloce
table(Goles_en_casa,Goles_como_visitante)
table(Resultado_final =="D", Goles_en_casa, Goles_como_visitante)



#  b) ¿En cuántos partidos ambos equipos empataron 0 a 0?

# La respuesta la podemos localizar en la tabla cruzada de la siguiente manera:
Tabla_cruzada[1,1]   # el marcador buscado se encuentra en la fila 1 y columna 1
paste("en", Tabla_cruzada[1,1], "partidos ambos equipos empataron 0 a 0")

# o bien
empate00 <- length(Goles_en_casa[sp1$empates == 'empate' & Goles_en_casa == 0])
empate00
paste("en", empate00, "partidos ambos equipos empataron 0 a 0")


"Por otro lado, para los incisos a) y b) también podemos construir el siguiente código que nos arroja los totales de los empates 0-0, 1-1, 2-2, 3-3,..."

visitante <- 0
empate <- 0
local <- 0
empate0 <- 0
empate1 <- 0
empate2 <- 0
empate3 <- 0
empate4 <- 0

for(i in 1:length(Resultado_final)) {
  if (Resultado_final[i] == "D") {
    empate <- empate+1
    if (Goles_en_casa[i] == 0) {
      empate0 <- empate0 + 1
    } else if (Goles_en_casa[i] == 1){
      empate1 <- empate1 + 1
    } else if (Goles_en_casa[i] == 2){
      empate2 <- empate2 + 1
    } else if (Goles_en_casa[i] == 3){
      empate3 <- empate3 + 1
    } else if (Goles_en_casa[i] == 4){
      empate4 <- empate4 + 1
    }
  } else if (Resultado_final[i] == "H") {
    local <- local+1
  } else {
    visitante <- visitante+1
  }
}


for(i in 1:length(Resultado_final)) {
  if (Resultado_final[i] == "D") {
    empate <- empate+1
    
  } else if (Resultado_final[i] == "H") {
    local <- local+1
  } else {
    visitante <- visitante+1
  }
}

empate
empate0
empate1
empate2
empate3
empate4




#  c) ¿En cuántos partidos el equipo local (HG) tuvo la mayor goleada sin dejar 
# que el equipo visitante (AG) metiera un solo gol?

"Aquí se obtiene de la tabla cruzada el número de partidos en los que el equipo local (HG) tuvo la mayor goleada sin dejar que el equipo visitante (AG) metiera un solo gol"
Tabla_cruzada[7,1]

paste("en", Tabla_cruzada[7,1], "partido el equipo local (HG) tuvo la mayor goleada sin dejar que el equipo visitante (AG) metiera un solo gol dejando el marcador 6-0")



#  __Notas para los datos de soccer:__ https://www.football-data.co.uk/notes.txt