# Postwork sesión 8. Análisis de la Inseguridad Alimentaria en México ##

Equipo 2:
- Karla Adriana Alamo Martínez
- Arturo Urbieta Reyes
- Demetrio Aguilar Esteva
- Gerardo Antonio Fuentes García
- Luis Manuel Rodarte Solórzano

## OBJETIVO

Realizar un análisis estadístico completo de un caso
Publicar en un repositorio de Github el análisis y el código empleado

## DESARROLLO

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
  
- nse5f (Nivel socieconómico del hogar): 1 "Bajo", 2 "Medio bajo", 3 "Medio", 4 "Medio alto", 5 "Alto"
- area (Zona geográfica): 0 "Zona urbana", 1 "Zona rural"
- numpeho (Número de personas en el hogar)
- refin (Recursos financieros distintos al ingreso laboral): 0 "no", 1 "sí"
- edadjef (Edad del jefe/a de familia)
- sexoje (Sexo del jefe/a de familia): 0 "Hombre", 1 "Mujer"
- añosedu (Años de educación del jefe de familia)
- ln_als (Logarítmo natural del gasto en alimentos saludables)
- ln_alns (Logarítmo natural del gasto en alimentos no saludables)
- IA (Inseguridad alimentaria en el hogar): 0 "No presenta IA", 1 "Presenta IA"



### 1. PLANTEAMIENTO DEL PROBLEMA


La inseguridad alimentaria (IA), entendida como la disponibilidad limitada o 
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

En todo el desarrollo usamos un nivel de confianza del 95%.


### 2. ANÁLISIS DESCRIPTIVO

Es importante mencionar que para poder realizar el análisis estadístico descriptivo de manera adecuada se realiza la conversión de las variables cualitativas (ordinales y nominales) indicadas como int a factor.

Explorando la distribución de cada variable aleatoria numérica de la BD se observa que:
- numpeho: distribución sesdaga a la derecha
- edadjef: distribución sesdaga a la derecha
- als: distribución sesdaga a la izquierda
- alns: distribución sesdaga a la derecha



### 3. CÁLCULO DE PROBABILIDADES

Se identificaron las variables aleatorias con distribución cercana a la normal y se calcularon algunas probabilidades para responder algunas preguntas de interés identificadas por el equipo.

Encontramos que tan solo el 3.6% de los hogares tienen un logaritmo natural del gasto en alimentos no saludables mayor o igual a 6.

Para hogares con inseguridad alimentaria:

- La probabilidad de que la edad del jefe/a de familia sea menor o igual a 30 es de 0.123, y que sea mayor o igual a 60 es de 0.203

- La probabilidad de que los años de educación del jefe/a de familia sea menor o igual a 6 es de 0.174, y que sea mayor o igual a 12 es de 0.350




### 4. HIPÓTESIS ESTADÍSTICAS

Se realizaron varias pruebas de hipótesis estadísticas obteniéndose los siguientes resultados:

A un nivel de confianza del 95%, se concluye que existe evidencia estadística suficiente de que:

- el promedio del número de personas en los hogares con IA es mayor que el promedio del número de personas en los hogares que no presentan IA.

- el promedio de la edad del jefe/a de familia en los hogares con IA es mayor que el promedio de la edad del jefe/a de familia en los hogares que no presentan IA.

- el promedio de los años de educación del jefe/a de familia en los hogares 
con IA es menor que el promedio de los años de educación del jefe/a de 
familia en los hogares que no presentan IA

- **al menos una media del Logarítmo natural del gasto en alimentos no saludables entre los 5 niveles socioeconómicos es diferente a las demás, es decir, que el nivel socieconómico si es un diferenciador del gasto en alimentos no saludables.**

- al menos una media del Logarítmo natural del gasto en alimentos saludables entre los 5 niveles socioeconómicos es diferente a las demás, es decir, que el nivel socieconómico si es un diferenciador del gasto en alimentos saludables.

- la inseguridad alimentaria si depende del nivel socioeconómico, el área, 
los recursos financieros distintos al ingreso laboral y el sexo del jefe de 
familia.


### 5. MODELOS DE REGRESIÓN LINEAL Y LOGÍSTICA

Se realizaron 3 modelos de regresión lineal:

El modelo m1 tiene todas sus variables significativas a un nivel de confianza de 95% y permite concluir que:
- pertenecer a un nivel socioeconómico más alto, en promedio incrementa 
en 18% el gasto en alimentos saludables,
- contar con recursos financieros externos incrementa en promedio 10% 
el gasto en alimentos saludables,
- tener inseguridad alimentaria reduce en promedio 2% el gasto en alimentos saludables.

Del modelo m2 se encontró que la variable de recursos financieros externos no es significativa para el modelo por lo que fue eliminada.

El modelo m3 tiene todas sus variables significativas y permite concluir que:
- pertenecer a un nivel socioeconómico más alto implicaria 
un aumento en promedio de 20% del gasto en alimentos no saludables
- tener inseguridad alimentaria reduce en promedio 12% el gasto en alimentos no saludables.


Por último se realizaron varios modelos de regresión logística eligiéndose el más adecuado tomando en consideración los valores p de los coeficientes, el criterio AKAIKE, así como el valor de la pseudo R cuadrada.

Una forma práctica para interpretar los efectos de los factores en la probabilidad de Inseguridad alimentaria es por medio de una representación gráfica, donde se observa que:

- **Si el nivel socioeconómico del hogar es bajo es más probable que dicho hogar presente inseguridad alimentaria.**

- Cuanto mayor es el número de personas en el hogar mayor es la probabilidad de que dicho hogar presente inseguridad alimentaria.

- Si el hogar tiene recursos financieros distintos al ingreso laboral es ligeramente más probable que dicho hogar presente inseguridad alimentaria.

- Cuanto menor es el número de años de educación del jefe de familia es mas probable que el hogar presente inseguridad alimentaria.

- Los factores más influyentes en la inseguridad alimentaria son el nivel socioeconómico del hogar, el número de personas en el hogar y los años de educación del jefe de familia.

### CONCLUSIÓN
**Se confirma que niveles socioeconómicos bajos tienden a gastar más en alimentos no saludables llevando al hogar a presentar inseguridad alimentaria.**






