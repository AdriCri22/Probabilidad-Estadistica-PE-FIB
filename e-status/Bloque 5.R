        # Papilla de zombi
# El mundo ha caído bajo el contagio de un virus que ha transformado a toda la 
# población en zombis. Tu último refugio es nada más que tu coche, con el que 
# deberás atropellar a cada uno de los muertos vivientes.

# Tú (digamos A) y tu amigo (B) os habéis pasado la noche pisando zombis, y ha 
# llegado la hora de determinar quién ha jugado mejor, basándose en el número 
# promedio de pisoteos por partida. El registro del juego para cada partida 
# (cada vez juega solo uno) nos informa sobre el número de zombis aplastados 
# hasta que la acumulación de carne pútrida detiene el coche y es 
# reglamentariamente devorado el cerebro del conductor (game over).

A = c(82, 78, 91, 69, 51, 36, 112, 89, 50, 58, 78, 70, 81)
B = c(90, 49, 93, 51, 66, 72, 48, 94, 82, 24, 55, 107, 113, 89, 54, 59, 115)
nA = length(A); nB = length(B)

# 1.  Vamos a comparar los resultados. Primero, di cuál es la diferencia 
#     promedio entre los zombies que has atropellado tú y los que ha atropellado 
#     B por partida.
#     Si tú has atropellado menos, el valor ha de ser negativo.
p1 = mean(A) - mean(B)

# 2. Calcula qué desviación estándar tiene los resultados del jugador A o B.
SA = sd(A)
SB = sd(B)
#p2 = SA # Si preguntan por la desviación de A
p2 = SB # Si preguntan por la desviación de B

# 3.  Considerando que las características de dispersión (a lo largo de 
#     infinitas partidas) fueran iguales en los dos jugadores, calcula una 
#     estimación de la VARIANCIA o  DESVIACIÓN que tenéis en común.
#     (Piden que calculemos S o S^2)
S2 = ((nA - 1) * (SA^2) + (nB - 1) * (SB^2)) / (nA + nB - 2)
S = sqrt(S2)
#p3 = S # Si preguntan por la desviación
p3 = S2 # si preguntan por la variancia

# 4.  Sabemos que, de noche a noche, la diferencia de promedios de A y B 
#     fluctúa, con una desviación tipo que llamamos error tipo o standard error. 
#     ¿Puedes estimar este valor con los datos disponibles?
p4 = se = sqrt(S2/nA + S2/nB)

# 5.  La información que conocemos de la muestra de esta noche da lugar a un 
#     estadístico, utilizado para poner a prueba la hipótesis H: ??A = ??B, que 
#     tomaremos bilateral porque a priori ninguno de los dos tiene ventaja. 
#     ¿Cuánto vale este estadístico?
p5 = t = (mean(A) - mean(B)) / se

# 6.  Halla el valor P asociado a la prueba de hipótesis que has realizado.
p6 = P = 2 * (1 - pt(abs(t), nA + nB - 2))

# 7.  La pregunta definitiva es qué diferencia hay entre los promedios de los dos 
#     jugadores. Estima dicha diferencia con un intervalo de confianza 95% (al 
#     menos dos cifras decimales correctas).
prob = 0.95; alpha = 1 - prob
p7 = c(p1 - qt(1 - alpha / 2, nA + nB - 2) * se, 
       p1 + qt(1 - alpha / 2, nA + nB - 2) * se)

# 8
p8 = P > 0.05 # TRUE -> No se puede descartar que A y B tengan el mismo nivel
              # FALSE -> 

p1; p2; p3; p4; p5; p6; p7; p8



        # Lentillas para el trabajo

# Un fabricante del entorno de la óptica ha desarrollado unas lentes de contacto 
# muy porosas que asegura que permiten trabajar con total normalidad con el 
# ordenador, sin que se reseque el ojo. Para demostrarlo, quiere compararlas con 
# otra marca que ya está posicionada en el mercado, y con la que pretende competir.

# Para ello, se diseña un estudio con 12 voluntarios que van a hacer una prueba 
# con ambos tipos de lentilla, adecuadas a su graduación, una en cada ojo (se 
# escoge al azar, y sin que lo sepa el voluntario, el ojo que llevará la nueva 
# lente). Tras dos horas de presencia frente a una pantalla se sacan las 
# lentillas y se mide el grado de humedad en la córnea en ambos ojos. Para evitar 
# sesgos, la persona que mide la humedad tampoco sabe qué tipo de lente hay en 
# cada ojo. Sin embargo, para su análisis, se indica en los datos cuál es el ojo 
# utilizado en cada voluntario para probar el nuevo tipo, ya que el analista no 
# puede ser influenciado por esta información.

# Datos proporcionados:
#     · Identificador del paciente
#     · Medida de la humedad
#     · Ojo medido (0: izquierdo; 1: derecho)
#     · ¿Lentilla de nuevo tipo? (0: no; 1: sí)

#       Id	  Hume	 Ojo	 Nvo
#col3	  1	    59.3	  0	    0
#col20	2	    46.1	  1	    0
#col14	3	    33.9	  1	    0
#col21	4	    39.2	  0	    0
#col11	5	    68.1	  1	    0
#col22	6	    41.9	  0	    0
#col10	7	    46.4	  0	    0
#col16	8	    27.6	  1	    0
#col17	9	    66.2	  1	    0
#col13	10	  21	    0	    0
#col23	11	  40.1	  1	    0
#col8	  12	  40.7	  1	    0


#col9	  1	    43.7	  1	    1
#col2	  2	    59.1	  0	    1
#col5	  3	    36.7	  0	    1
#col4	  4	    41.9	  1	    1
#col18	5	    67.8	  0	    1
#col12	6	    46.3	  1	    1
#col6	  7	    44.4	  1	    1
#col19	8	    38.5	  0	    1
#col7	  9	    63.8	  0	    1
#col24	10	  32.4	  1   	1
#col15	11	  44.4	  0	    1
#col1	  12	  47.2	  0	    1

Ref = c(59.3, 46.1, 33.9, 39.2, 68.1, 41.9, 46.4, 27.6, 66.2, 21, 40.1, 40.7)
New = c(43.7, 59.1, 36.7, 41.9, 67.8, 46.3, 44.4, 38.5, 63.8, 32.4, 44.4, 47.2)
nRef = length(Ref); nNew = length(New)

# 1.  Calcula la media de la humedad medida en las lentillas de tipo REF o NEW.
p1 = mean(Ref) # Si preguntan por la media de Ref
#p1 = mean(New) # Si preguntan por la media de New

# 2.  Halla las diferencias (New - Ref) en la humedad medida entre los dos tipos 
#     de lente, y calcula el promedio de estas diferencias.
Diferencia = New - Ref
p2 = media = mean(Diferencia)

# 3.  Calcula la desviación típica de las diferencias
p3 = sd(Diferencia)

# 4.  Calcula la desviación típica de la media de las diferencias (el error 
#     típico).
p4 = se = sqrt(p3^2 /  length(Diferencia))

# 5.  Calcula el valor del estadístico para contrastar la hipotesis nula:
      "no hay diferencia en humedad entre lentillas de tipo New y de tipo Ref"
p5 = media / se

# 6.  ¿Cuántos grados de libertad hay que utilizar para la distribución de 
#     referencia del estadístico calculado?
p6 = length(Diferencia) - 1

# 7.  Calcula el p-valor de la prueba de hipótesis anterior.
p7 = 2 * (1 - pt(abs(p5), p6))

# 8.  Con un riesgo del 5%, ¿aceptarías o rechazarías la hipótesis nula? 
#     ¿Y con un riesgo del 1%?
prob1 = p7 > 0.01 # TRUE -> Aceptar   # FALSE -> rechazar
prob5 = p7 > 0.05 # TRUE -> Aceptar   # FALSE -> rechazar
p8 = c(prob1, prob5)

# 9.  Calcula el intervalo de confianza del 85 por ciento para el efecto que 
#     consigue el nuevo tipo de lentilla (incremento de humedad)
prob = 0.85; alpha = 1 - prob
p9 = c(p2 - qt(1 - alpha / 2, p6) * se, 
       p2 + qt(1 - alpha / 2, p6) * se)

p1; p2; p3; p4; p5; p6; p7; p8; p9



          # Competencia gasolinera
# El gráfico lateral representa un mapa en el que se han colocado las mediciones 
# realizadas en los puntos indicados, acerca del precio observado en el litro de 
# gasolina, para dos marcas determinadas que llamaremos G (en negro) y H (en rojo).

# Queremos estudiar si el precio de la gasolina presenta la misma dispersión 
# para las dos marcas, o si por el contrario una de ellas tiene precios con 
# mayor concentración alrededor del promedio

G = c(1.506, 1.414, 1.474, 1.528, 1.488, 1.484, 1.502, 1.476, 1.536)
H = c(1.509, 1.453, 1.441)

# 1.  ¿Cuál es la desviación típica para la marca G?
p1 = sd(G)

# 2.  ¿Cuál es la desviación típica para la marca H?
p2 = sd(H)

# 3.  Introduzca el valor del estadístico de referencia en comparación de 
#     variancias (el cociente entre la variancia mayor y la variancia menor). 
p3 = f = if (p1 > p2) p1^2 / p2^2 else p2^2 / p1^2

# 4.  Bajo la hipótesis nula de que ambas variancias poblacionales son iguales,
#     el estadístico anterior sigue una ley F de Fisher. Introduzca los 
#     parámetros de la ley que corresponde a este caso (primero, grados de 
#     libertad del numerador; despues, grados de libertad del denominador)
p4 = if (p1 > p2) c(length(G) - 1, length(H) - 1) else c(length(H) - 1, length(G) - 1)

# 5.  ¿Cuál es el valor que utilizará, con un riesgo ??=5% bilateral, para 
#     decidir si el estadístico anterior permite rechazar la hipótesis nula?
alpha = 0.05
p5 = qf(1 - alpha / 2, p4[1], p4[2])

# 6.  Aunque no lo necesita, diga cuál sería el valor crítico que limita el 
#     estadístico por la parte inferior.
p6 = qf(alpha / 2, p4[1], p4[2])

# 7.  Para hallar el p-valor de la prueba necesitará el ordenador. Tenga en 
#     cuenta que debe calcular la probabilidad de superar el valor del 
#     estadístico que ha encontrado, y multiplicar por 2, ya que se trata de 
#     una prueba bilateral. Obtenga tres decimales correctos, al menos.
p7 = 2 * (1 - pf(abs(f), p4[1], p4[2]))

# 8.  Una de estas tres frases es la conclusión correcta. ¿Cuál es?
p8 = p7 > 0.05 # TRUE -> No hay evidencias para rechazar (la aceptamos)

# 9.  Vamos a ver si tenemos claro lo de la distribución F de Fisher-Snedecor. 
#     Pruebe a calcular aquel valor x tal que una variable que sigue la 
#     distribución F con grados de libertad {10, 3} tenga probabilidad 0.061 de 
#     ser MAYOR o MENOR que x.
grados = c(10, 3); prob = 0.061
p9 = qf(1 - prob, grados[1], grados[2]) # MAYOR
#p9 = qf(prob, grados[1], grados[2]) # MENOR

p1; p2; p3; p4; p5; p6; p7; p8; p9


          # Mitja jornada no és treballar 12h al dia
# La consultora "Driven by evidence" està conduint un estudi per comparar dues 
# metodologies de treball. Dos equips similars, que es coneixeran com B i W, són 
# escollits, i es estableix un rigorós protocol per tal d'obtenir una mesura de 
# la seva activitat (que es tradueix a hores per facilitar la seva interpretació).

# Un responsable de la consultora asigna una mostra de tasques a cada equip, de 
# manera que aquestes tasques són independents entre sí. Després del periode de 
# seguiment es disposa de la següent informació: nombre de tasques per a cada 
# equip, temps en mitjana i desviació tipus del temps, per a B i per a W. Es 
# suposa que la variable temps de les tasques segueix aproximadament el model 
# Normal.

nB = 9; mitjanaB = 49.88889; SB = 11.55903 # Equip B
nW = 9; mitjanaW = 50.22222; SW = 13.96225 # Equip W

# 1.  Troba una estimació conjunta per a la variància del temps dedicat a una 
#     tasca, tenint en compte que es suposa que aquest paràmetre val el mateix 
#     per als dos equips.
p1 = S2 = ((nB - 1) * SB^2 + (nW - 1) * SW^2) / (nB + nW - 2)

# 2.  Com pots comprovar a les dades i gràficamente, hi ha una diferència entre 
#     les mitjanes, però no sabem si aquesta diferència és purament deguda a 
#     l'atzar.
#     Pots estimar quant val l'error tipus de la diferència de mitjanes en 
#     aquest cas? Assumeix la premissa de l'apartat anterior.
S = sqrt(S2)
p2 = se = S * sqrt(1 / nB + 1 / nW)

# 3.  Considerant una hipòtesi H: ??B = ??W, contesta amb el valor de l'estadístic 
#     t de la prova corresponent.
p3 = (mitjanaB - mitjanaW) / se

# 4.  Quina és correcta?
p4 = "No es pot asegurar res"

# 5.  Admitim provisionalment que la desviació tipus del temps val 15. En aquest 
#     cas, què val l'estadístic de la prova pertinent?
desvT = 9.5
p5 = (mitjanaB - mitjanaW) / (desvT * sqrt(1 / nB + 1 / nW))

# 6.  Suposant que la prova d'hipòtesis contempla una alternativa bilateral, 
#     calcula el valor P del resultat de la pregunta anterior.
p6 = 2 * (1 - pnorm(abs(p5)))

# 7.  Anem a estudiar un escenari diferent. El responsable de l'estudi pot estar 
#     dubtant sobre la homogeneïtat dels dos equips. Potser un d'ells és més 
#     expert, i la seva variabilitat és menor que la de l'altre.
#     Si fixem un nivell d'error ?? del 10% per fer una trova de comparació de 
#     les variàncies, troba el punt que separa la regió d'acceptació de la de 
#     rebuig per a aquest cas.
alpha = 0.1
p7 = if (SB >= SW) qf(1 - alpha / 2, nB - 1, nW - 1) else qf(1 - alpha / 2, nW - 1, nB - 1)

# 8.  D'acord amb els resultats trobats:
p8 = p7 > 0.05 # TRUE -> Es versemblant que els dos equips presentin 
#                       homogeneïtat quant la dispersió dels seus temps
#                       (no podem rebujtar H0)

# 9.  Quant més triga l'equip B respecte W a resoldre una tasca, en mitjana?
#     Feu una estimació per interval de confiança al 90% de la diferència de 
#     mitjanes del temps que B i W (assumiu variància comuna i desconeguda)
prob = 0.98; alpha = 1 - prob;
p9 = c(mitjanaB - mitjanaW - qt(1 - alpha / 2, nB + nW - 2) * se, 
       mitjanaB - mitjanaW + qt(1 - alpha / 2, nB + nW - 2) * se)

p1; p2; p3; p4; p5; p6; p7; p8; p9



          # El exprimidor

# La gran controversia del momento es si son mejores los exprimidores eléctricos 
# que giran solo en un sentido, o los que giran alternativamente en un sentido o 
# en otro. Por fin, la Academia de las Ciencias ha decidido tomar cartas en el 
# asunto y ha planteado un ambicioso estudio, dado que no hay presupuesto para 
# otros proyectos.

# Se realizan dos experimentos. El primero consiste en repartir al azar unas 
# naranjas para ser exprimidas bien con un sistema, bien con otro (1 o 2 denotan 
# "solo un sentido" o "ambos sentidos", respectivamente). En este experimento, 
# se han pesado las naranjas al principio, y también la cantidad de zumo obtenido. 
# El segundo experimento consiste en coger otro grupo de naranjas, partirlas por 
# la mitad, y usar cada mitad con un sistema distinto (1 o 2) midiendo el zumo 
# obtenido. El gráfico muestra los resultados para ambos experimentos.

# En los datos, Z es el peso del zumo obtenido en el primer experimento, e Y el 
# peso resultante (para medias naranjas) en el segundo. La unidad empleada es el 
# gramo.

Z1 = c(146, 115, 128, 120, 97, 111, 89, 111, 95)
Z2 = c(90, 65, 78, 92, 96, 98, 87, 76, 71)
Y1 = c(55, 37, 79, 52, 68, 54, 50, 65, 44)
Y2 = c(39, 41, 54, 45, 51, 47, 44, 48, 35)

n1 = length(Z1); n2 = length(Z2)

# 1.  Consideremos los datos basados en muestras independientes. Admitiendo que 
#     los dos sistemas comparten una variabilidad común en la producción de 
#     zumo, dé una estimación de la DESVIACIÓN o VARIANCIA tipo común.
p1 = S2 = ((n1 - 1) * sd(Z1)^2 + (n2 - 1) * sd(Z2)^2) / (n1 + n2 - 2) # VARIANCIA
#p1 = S = sqrt(S2) # DESVIACIÓN

# 2.  Resuelva la prueba de hipótesis para comparar los promedios del peso 
#     obtenido de zumo, y diga cuál es el resultado obtenido para el ESTADÍSTICO 
#     o el P-VALOR de la prueba.
se = (sqrt(S2) * sqrt(1 / n1 + 1 / n2))
p2 = t = (mean(Z1) - mean(Z2)) / se # ESTADÍSTICO
p2 = 2 * (1 - pt(abs(p2), n1 + n2 - 2)) # P-VALOR

# 3.  Planteamos una prueba de variancias:
#         H0: ??12 = ??22
#         H1: ??12 ??? ??22
#     Calcule el valor del estadístico de esta prueba (procurando que caiga en 
#     la zona de la derecha), y el límite establecido con un riesgo 5% que nos 
#     permitiría rechazar la hipótesis de partida.
# Distribución de Fisher: estadístico F = SZ1^2 / SZ2^2, teniendo en cuenta que SZ2^2 < SZ1^2
# Piden también el cuantil
alpha = 0.05
p3 = if (sd(Z1)^2 > sd(Z2)^2) c(sd(Z1)^2 / sd(Z2)^2, qf(1 - alpha / 2, n1 - 1, n2 - 1)) else 
                              c(sd(Z2)^2 / sd(Z1)^2, qf(1 - alpha / 2, n1 - 1, n2 - 1))

# 4.  Consideramos ahora la segunda parte. En este caso, tendremos en cuenta que 
#     los valores esperados que se comparan corresponden al zumo de media 
#     naranja. Calcule cuánto vale ahora el ESTADÍSTICO o el P-VALOR de la prueba.
Diferencia = Y1 - Y2
p4 = mean(Diferencia) / (sd(Diferencia) / sqrt(length(Diferencia))) # ESTADÍSTICO
p4 = 2 * (1 - pt(abs(p4), length(Diferencia) - 1)) # P-VALOR

# 5.  Estime por intervalo de confianza del 99% la diferencia de zumo promedio 
#     que nos puede dar una naranja exprimida con uno u otro sistema, de acuerdo 
#     con el procedimiento basado en muestras INDEPENDIENTES o APAREADAS.
#     El signo de los valores que introduzca se deriva de la diferencia 
#     sistema 1 - sistema 2.
prob = 0.99
#p5 = t.test(Z1, Z2, var.equal=TRUE, conf.level=prob)$conf.int # INDEPENDIENTES
p5 = t.test(Diferencia*2, var.equal=TRUE, conf.level=prob)$conf.int # APAREADAS
p5 = c(p5[1], p5[2])


# 6.  A partir del análisis según muestras INDEPENDIENTES o APAREADAS, elige la 
#     respuesta más acertada: 
#       Según los valores de p5
#         Si los 2 valores son positivos -> El exprimidor que gira solo en un sentido es mejor
#         Si 1 de los valores son negativos -> No lo sabemeos, no hemos hallado nada concluyente
#         Si los 2 valores son negativos -> El exprimidor que gira en ambos sentidos es mejor
p1; p2; p3; p4; p5


          # Técnicas de optimización
# Hemos preparado dos formulaciones distintas para cierto problema de 
# optimización que vamos a resolver utilizando intensivamente el Neos Server. El 
# objetivo es determinar qué formulación presenta la mínima tasa de fracaso para 
# llegar a la solución del problema.

# La experiencia consiste en enviar al Neos instancias del problema que se 
# resuelven bien con una formulación (Simplex), bien con la otra (Punto 
# Interior). Las instancias que se envían son independientes (el mismo problema 
# no se resuelve por las dos vías).

# Los datos adjuntos contienen la información de las ejecuciones, referente a la 
# formulación empleada (A: Simplex, B: Punto Interior) y al estado final de la 
# resolución:
#     0: solución óptima hallada
#     1: solución subóptima
#     2: imposible hallar solución (infactible, no acotado o fallo numérico)

# 1 - Click en "Copiar datos para pegar en otro programa"
# 2 - Selecionar todo
# 3 - Ctrl + C o click derecho copiar
# 4 - Ejecutar el siguiente trozo de codigo, así ya tenemos todos los datos en R
datos = read.table("clipboard")
frecuencias = list("A0"=0, "A1"=0, "A2"=0, "B0"=0, "B1"=0, "B2"=0)
for (i in 1:length(datos$met)) {
  x = paste(datos$met[i], datos$stat[i], sep="");
  frecuencias[[x]] = frecuencias[[x]] + 1;
}

# 1.  Complete la tabla de frecuencias.
#     Introduzca los valores por columnas, separados por un blanco.
#   -----------------------------------------------------------------------------
#                   |       óptimo	    |     subóptimo	    |     imposible     |
#   -----------------------------------------------------------------------------
#   Simplex	 	 	    |  frecuencias$"A0" |  frecuencias$"A1" |  frecuencias$"A2" |
#   -----------------------------------------------------------------------------
#   Punto Interior	|  frecuencias$"B0" |  frecuencias$"B1" |  frecuencias$"B2" |
#   -----------------------------------------------------------------------------
p1 = c(frecuencias$"A0", frecuencias$"B0", frecuencias$"A1",
       frecuencias$"B1", frecuencias$"A2", frecuencias$"B2");

# 2.  ¿Cuál sería el número esperado de observaciones, si en cuanto al estado 
#     final los dos procedimientos respondieran sin diferencias en su distribución?
#     Responda para el caso concreto de acabar en estado imposible usando la 
#     formulación Punto Interior.
probA = (frecuencias$"A0" + frecuencias$"A1" + frecuencias$"A2")/length(datos$met)
probB = (frecuencias$"B0" + frecuencias$"B1" + frecuencias$"B2")/length(datos$met)
prob0 = sum(p1[1:2])/length(datos$met)
prob1 = sum(p1[3:4])/length(datos$met)
prob2 = sum(p1[5:6])/length(datos$met)
frecuenciasEsperadas = list("A0" = length(datos$met) * probA * prob0,
                            "A1" = length(datos$met) * probA * prob1,
                            "A2" = length(datos$met) * probA * prob2,
                            "B0" = length(datos$met) * probB * prob0,
                            "B1" = length(datos$met) * probB * prob1,
                            "B2" = length(datos$met) * probB * prob2)
p2 = frecuenciasEsperadas$"B2"

# 3.  La prueba de Pearson consiste en observar si los valores esperados en caso 
#     de independencia están muy alejados de los reales. Si fueran parecidos, 
#     querría decir que no hay motivos para sospechar que un método funcione 
#     globalmente de forma distinta al otro. Pero solo que haya una celda que 
#     demuestre ser muy diferente, eso hace que el estadístico de Pearson (X2) 
#     aumente significativamente. En este caso, ¿cuánto vale el estadístico de 
#     Pearson?
#     El resultado se considerará correcto si el error relativo es inferior a 1%, 
#     por tanto las respuestas pequeñas necesitan más decimales correctos que las 
#     grandes.
p3 = 0
for (i in 1:length(frecuencias)) {
  x = (as.numeric(frecuencias[i]) -
         as.numeric(frecuenciasEsperadas[i]))^2 /
    as.numeric(frecuenciasEsperadas[i]);
  # print(x);
  p3 = p3 + x;
}

# 4.  Escoja la opción más adecuada:
#     La significación estadística de la prueba de Pearson es:
#       1.  muy escasa (1 de cada 5, o más). No hay evidencias sobre una relación
#       2.  posible (entre 1 de cada 5, y 1 de cada 20). Hay una evidencia muy 
#           débil
#       3.  moderada (entre 1 de cada 20, y 1 de cada 200). La evidencia sobre 
#           la relación es sustancial
#       4.  fuerte (1 de cada 200, o menos). Es muy raro que el resultado observado 
#           se pueda deber únicamente al azar.
p4 = 2 # posible (entre 1 de cada 5, y 1 de cada 20). Hay una evidencia muy débil
  # graus de llibertat = (files-1)*(columnes-1) = (2-1)*(3-1) = 2
  # P-valor = P(X22 > 5.0325) = 0.08076 [R: 1-pchisq(p3,2)]
  # Punt crític = X22,0.95 = 5.991465 [R: qchisq(0.95,2)]
  # Rebutgem H0 si P-valor < 0.05 o X2 > Punt crític
  # En aquest cas, la resposta és 2 perquè 5.0325 < 5.991465
  # i hi ha poca diferència entre els dos valors
  # Execució 2: X2 = 0.07983128 -> 1. muy escasa
  # Execució 3: X2 = 6.86417 -> 3. moderada
  # Execució 4: X2 = 18.38441 -> 4. Fuerte

# 5.  Mirando las proporciones logradas en los estados finales, ¿son similares 
#     Simplex y Punto Interior? ¿Hay más proporción de instancias que acaban en 
#     estado imposible con Simplex que con el otro método? Para responder, 
#     introduzca ambas proporciones, separadas por un espacio.
                  # ?????????
p5 = c(frecuencias$"A2" / (frecuencias$"A0" + frecuencias$"A1" +
        frecuencias$"A2"), frecuencias$"B2"/ (frecuencias$"B0" + frecuencias$"B1" + frecuencias$"B2"))
                                    # ?????????
# estado subóptimo -> numeradores: frecuencias$"A1" y frecuencias$"B1"
# estado imposible -> numeradores: frecuencias$"A2" y frecuencias$"B2"

# 6.  Suponiendo la hipótesis H
#     H: la proporción de resultados de tipo imposible es la misma para ambos métodos
#     ¿cuál es la estimación de la probabilidad común que presenta este tipo de resultado?
                # ????????? y            # ?????????
p6 = (frecuencias$"A2" + frecuencias$"B2")/length(datos$met)
# estado subóptimo -> numerador: frecuencias$"A1" + frecuencias$"B1"
# estado imposible -> numerador: frecuencias$"A2" + frecuencias$"B2"

# 7.  Calcule el error tipo correspondiente a la diferencia de dos proporciones, 
#     bajo el supuesto de la hipótesis H.
p7 = sqrt(p6*(1-p6)/(frecuencias$"A0" + frecuencias$"A1" +
          frecuencias$"A2") + p6*(1-p6)/(frecuencias$"B0" + frecuencias$"B1"
          + frecuencias$"B2"))

# 8.  Para responder a la prueba que compara las proporciones de ambos métodos 
#     en el estado imposible, diga cuánto vale el estadístico asociado a la 
#     prueba (puede obviar el signo, ya que se tratará como bilateral), y el 
#     valor de P de dicha prueba.
z = abs((p5[1]-p5[2])/p7)
p = 2*pnorm(-z) # P(|Z| > |z|) = 2*pnorm(-z)
  # = 2*(1-pnorm(z)) = 1-pnorm(z)+pnorm(-z)
p8 = c(z,p)

p1; p2; p3; p4; p5; p6; p7; p8