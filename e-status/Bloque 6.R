################################################################################

         # Modelo lineal

# Se ha estudiado la relación existente entre un factor X y una variable Y, y 
# con los datos de una muestra obtenida con este propósito se ha hallado la 
# siguiente modelización:

" Call:
   lm(formula = y ~ x)

 Residuals:
   Min       1Q   Median       3Q      Max 
 -0.87117 -0.26236 -0.09812  0.29711  0.96845 

 Coefficients:
   Estimate Std. Error  t value Pr(>|t|)    
 (Intercept) -19.740199   0.069128 -285.558   <2e-16 ***
   x            -0.002676   ¿#&#!?{?   -1.395    0.169    
 ---
   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

 Residual standard error: 0.4267 on 54 degrees of freedom
 Multiple R-squared:  0.0348,	Adjusted R-squared:  0.01692 
 F-statistic: 1.947 on 1 and 54 DF,  p-value: 0.1687"

b0 = -19.740199; Sb0 = 0.069128;  tb0 = -285.558; pb0 = 2e-16 # (Intercept)
b1 = -0.002676;  Sb1 = 0;        tb1 = -1.395;   pb1 = 0.169 # x
df = n = 54; # Degrees of freedom and DF 
e = 0.4267 # Residual standard error
R = 0.0348 # Multiple R-squared

# En la salida hay un valor corrompido. ¿Puede recuperar el valor original?
# tb1 = b1 / Sb1; tb1 # Si falta el t value de x
# tb0 = b0 / Sb0; tb0 # Si falta el t value de (Intercept)
Sb1 = b1 / tb1; Sb1 # Si falta Std. Error de x
# Sb0 = b0 / tb0; Sb0 # Si falta Std. Error de (Intercept)
# b1 = tb1 * Sb1; b1 # Si falta Estimate de x
# b0 = tb0 * Sb0; b0 # Si falta Estimate de (Intercept)

# ¿Rechazaría la hipótesis de que la recta realmente es horizontal? 
# (0: no, 1: sí)
if ((b1 - 0) / Sb1 > tb1) "Si, rechazamos H0" else "No, aceptamos H0"

# ¿Rechazaría la hipótesis de que la recta pasa por el origen? (0: no, 1: sí)
if (pb0 < 0.05) "Si, rechazamos H0" else "No, aceptamos H0"

# ¿Podríamos rechazar con estos datos β0=-60? (use riesgo α=10%; 0: no, 1: sí).
B0 = -60; alpha = 0.1;
if ((b0 - B0) / Sb0 > qt(1 - alpha / 2, n)) "Si, rechazamos H0" else "No, aceptamos H0"

# ¿Podríamos rechazar con estos datos β1=-0.003? (use riesgo α=1%; 0: no, 1: sí).
B1 = -0.003; alpha = 0.01;
if ((b1 - B1) / Sb1 > qt(1 - alpha / 2, n)) "Si, rechazamos H0" else "No, aceptamos H0"

# Diga el valor que se ha estimado para la desviación típica del término 
# aleatorio del modelo.
e

# Extremo inferior de un intervalo de confianza 95% para la pendiente.
b1 - qt(0.975, n) * Sb1

# Extremo superior de un intervalo de confianza 95% para la pendiente.
b1 + qt(0.975, n) * Sb1

# P-valor de la prueba β0=0.
pb0

# P-valor de la prueba β1=0.
pb1

# ¿Cuánto vale la pendiente estimada?
b1

# ¿Cuánto vale la estimación del término independiente?
b0

# ¿Qué vale el error de estimación del término lineal?
Sb1

# ¿Qué vale el error de estimación del término independiente?
Sb0

# Responda con el estadístico de la prueba β1=0.
tb1

# Responda con el estadístico de la prueba β0=0.
tb0

# ¿Cuántos grados de libertad se asocian a la t-Student presente en este modelo?
n

# Halle el coeficiente de determinación asociado al modelo.
R


################################################################################

      # Inversions en hardware

"Hem examinat el registre dels darrers 18 ordinadors adquirits al departament i 
hem llistat el preu, la capacitat del disc dur i el proveïdor de cadascú. Podeu 
suposar que les compres són independents unes d'altres.

Analitzem la relació lineal entre preu i capacitat."

# Datos del problema: 

"Preu	Capacitat	Proveïdor
1	910	180	1
2	750	140	3
3	625	100	3
4	900	200	3
5	770	200	3
6	785	180	2
7	775	160	3
8	750	160	1
9	860	180	2
10	625	140	1
11	795	160	1
12	910	200	2
13	715	120	3
14	660	120	2
15	995	220	1
16	815	220	1
17	795	160	1
18	760	160	2"

datos <- read.table("clipboard", header=TRUE)
mod.lm <- lm(Preu ~ Capacitat, datos)
summary(mod.lm)

Y = datos$Preu; X = datos$Capacitat

n = length(Y);
SY2 = (sum(Y^2) - sum(Y)^2 / n) / (n - 1)
SX2 = (sum(X^2) - sum(X)^2 / n) / (n - 1)
SXY = (sum(X * Y) - sum(X) * sum(Y) / n) / (n - 1)
rXY = summary(mod.lm)$r.squared
b1 = SXY / SX2
b0 = mean(Y) - b1 * mean(X)
S2 = ((n - 1) * (SY2 - b1 * SXY)) / (n - 2)
Sb0 = sqrt(S2 * (1 / n + mean(X)^2 / ((n - 1) * SX2)))
Sb1 = sqrt(S2 / ((n - 1) * SX2))


# 1.  Quant val l'estimació del coeficient independent (terme constant)?
p1 = b0

# 2.  Quin és el valor de la variáncia residual? Arrodoniu a l'enter més pròxim.
p2 = S2

# 3.  Volem contrastar si podem acceptar que el preu de l'ordinador 
#     s'incrementa en 4 euros per cada Gigabyte de més que te el disc dur. 
#     Amb les dades de que disposeu, quin és el valor de l'estadístic 't' que 
#     està associat a aquesta prova?
B1 = 4
p3 = t = (b1 - B1) / Sb1

# 4.  Per a un risc α del 0.5% bilateral, quin seria el punt crític per rebutjar 
#     la hipòtesi anterior?
alpha = 0.005
p4 = PC = qt(1 - alpha / 2, n - 2)

# 5.  Feu els càlculs per trobar el valor del coeficient de determinació R2 del 
#     model "Preu" en funció de "Capacitat", amb quatre decimals exactes.
p5 = rXY

# 6.  Quant val el residu que proporciona el model per a l'ordinador que costa 
#     625 euros amb capacitat 100 GB?
y = 625; x = 100
p6 = y - x * b1 - b0

# 7.  Trobeu un interval de confiança al 95% per al preu mitjà d'un ordinador de 
#     100 GB de capacitat al disc dur.
prob = 0.95; x = 100
alpha = 1 - prob; y = b0 + b1 * x; t = qt(1 - alpha / 2, n - 2)
p7 = c(y - t * sqrt(S2) * sqrt(1 / n + (x - mean(X))^2 / sum((X - mean(X))^2)),
       y + t * sqrt(S2) * sqrt(1 / n + (x - mean(X))^2 / sum((X - mean(X))^2)))

# 8.  Suposeu que hem descobert un error contable, i que a tots els preus del 
#     proveïdor 2 els mancava l'IVA (21%). Refeu els càlculs, i digueu com queda 
#     ara el terme independent de la recta de regressió.
IVA = 0.21; prov = 2; datos2 = datos; prob = 1 + IVA
for (i in 1:n) {
  if (datos2$Proveïdor[i] == prov) {
    datos2$Preu[i] = prob * datos2$Preu[i]
  }
}
Y = datos2$Preu; X = datos2$Capacitat
n = length(Y);
SY2 = (sum(Y^2) - sum(Y)^2 / n) / (n - 1)
SX2 = (sum(X^2) - sum(X)^2 / n) / (n - 1)
SXY = (sum(X * Y) - sum(X) * sum(Y) / n) / (n - 1)
rXY = SXY / (sqrt(SX2) * sqrt(SY2))
b1 = SXY / SX2
p8 = b0 = mean(Y) - b1 * mean(X)

p1; p2; p3; p4; p5; p6; p7; p8


################################################################################
    
      # Rectas para todo

"En cierto estudio se pretende observar la asociación entre una variable X y una 
respuesta Y. Se dispone de 11 observaciones de la respuesta, con sus 
correspondientes valores de X, que han proporcionado los siguientes indicadores:"

n = 11
MediaX = -27.46791
MediaY = -19.87355
SX = 31.02892
SY = 1.491
SXY = 35.5455

rXY = SXY / (SX * SY)
b1 = SXY / SX^2
b0 = MediaY - b1 * MediaX
S2 = ((n - 1) * (SY^2 - b1 * SXY)) / (n - 2)
Sb0 = sqrt(S2 * (1 / n + MediaX^2 / ((n - 1) * SX^2)))
Sb1 = sqrt(S2 / ((n - 1) * SX^2))

# Halle el coeficiente de determinación asociado al modelo.
rXY^2

# ¿Cuánto vale la estimación del término independiente?
b0

# ¿Qué parte (en %) de la variabilidad total se atribuye a factores diferentes 
# de X?
100 - rXY^2*100

# ¿Se podría rechazar la hipótesis: β0=-59.5? (use riesgo α=5%; 0: no, 1: sí).
B0 = -59.5; alpha = 0.5;
if ((b0 - B0) / Sb0 > qt(1 - alpha / 2, n - 2)) "Si, rechazamos H0" else "No, aceptamos H0"

# ¿Se podría rechazar la hipótesis: β1=0.006? (use riesgo α=5%; 0: no, 1: sí).
B1 = 0.006; alpha = 0.05;
if ((b1 - B1) / Sb1 > qt(1 - alpha / 2, n - 2)) "Si, rechazamos H0" else "No, aceptamos H0"

# ¿Cuánto vale la pendiente estimada?
b1

# ¿Qué parte (en %) de la variabilidad total explica la variable X?
rXY^2 * 100

# De acuerdo con la hipótesis: β1=0.04, diga qué valor toma el estadístico de la 
# prueba.
B1 = 0.04
(b1 - B1) / Sb1

# De acuerdo con la hipótesis: β0=88.2, diga qué valor toma el estadístico de la 
# prueba.
B0 = 88.2
(b0 - B0) / Sb0

# La estimación del coeficiente lineal vale ...
b1

# Diga el valor que se ha estimado para la desviación típica del término 
# aleatorio del modelo.
sqrt(S2)

# Contrastando el término constante hemos obtenido un estadístico igual a 
# 0.33958. ¿Qué valor se estaba contrastando?
tb0 = 0.33958
b0 - tb0 * Sb0

# Obtenga una estimación de la variancia residual.
S2


################################################################################
  
         # Experimento químico

"Un equipo de investigadores que trabajan en seguridad en el trabajo está 
tratando de analizar cómo la piel absorbe un cierto componente químico 
peligroso. Para ello, colocan diferentes volúmenes del compuesto químico sobre 
diferentes segmentos de piel durante distintos intervalos de tiempo, midiendo al 
cabo de ese tiempo el porcentaje de volumen absorbido del compuesto. El diseño 
del experimento se ha realizado para que la interacción esperable entre el 
tiempo y el volumen no influya sobre los resultados.

Lo que los investigadores se cuestionan es si la cantidad o volumen de compuesto 
por un lado y el tiempo de exposición al que se somete por otro, influyen en el 
porcentaje absorbido. La Figura muestra una representación gráfica de los datos.

En primer lugar, vamos a considerar una recta de regresión para explicar el 
porcentaje de absorción en función del volumen de sustancia aplicado sobre la 
piel."

# Datos del problema:

"vol		 temp		 perc
0.5		2		51.86
0.5		10		48.63
0.5		24		84.24
2		2		53.98
2		10		70.5
2		24		85.64
5		2		47.37
5		10		64.35
5		24		89.74"

datos <- read.table("clipboard", header = TRUE)
mod.lm <- lm(perc ~ vol, datos)
summary(mod.lm)
Y = datos$perc; X = datos$vol
b0 = summary(mod.lm)$coefficients[1, 1]
b1 = summary(mod.lm)$coefficients[2, 1]
Sb0 = summary(mod.lm)$coefficients[1, 2]
Sb1 = summary(mod.lm)$coefficients[2, 2]
n = length(Y)
SY2 = (sum(Y^2) - sum(Y)^2 / n) / (n - 1)

# 1.  Estime la pendiente del modelo considerado.
p1 = b1

# 2.  Estime el error estándar de la pendiente del modelo.
p2 = Sb1

# 3.  En función del contraste de hipótesis adecuado, ¿se puede asumir un modelo 
#     lineal en el que el volumen del compuesto representa una variable 
#     explicativa? Asuma un riesgo α de 0.05 (responder con 1- sí, 0- no)
alpha = 0.05
p3 = if (b1 / Sb1 > qt(1 - alpha / 2, n - 2)) "Si, rechazamos H0" else "No, aceptamos H0"

# 4.  ¿Cuánto aumenta en promedio el porcentaje de absorción si incrementamos en 
#     0.5 unidades el volumen de compuesto utilizado?
x = 0.5
p4 = mean(Y) - mean(b0 + b1 * (X - x))

# 5.  Consideramos ahora la recta de regresión para explicar el porcentaje de 
#     absorción en función del tiempo de exposición.
#     
#     Estimar la pendiente del nuevo modelo.
remove(X)
X = datos$temp;
SX2 = (sum(X^2) - sum(X)^2 / n) / (n - 1)
SXY = (sum(X * Y) - sum(X) * sum(Y) / n) / (n - 1)
p5 = b1 = SXY / SX2

# 6.  ¿Cuál es el valor del estadístico t asociado a la prueba de hipótesis para 
#     determinar que este modelo lineal es válido?
mod.lm <- lm(perc ~ temp, datos)
Sb1 = summary(mod.lm)$coefficients[2, 2]
p6 = t = b1 / Sb1

# 7.  Obtén el P-valor asociado a la misma prueba.
p7 = 2 * (1 - pt(abs(t), n - 2))

# 8.  Calcular el coeficiente de correlación lineal entre porcentaje absorbido y 
#     tiempo de exposición.
p8 = rXY = SXY / sqrt(SX2 * SY2)

# 9.  ¿Cuál es el porcentaje de absorción que correspondería un individuo que se 
#     someta a un tiempo de exposición al compuesto de 12 horas?
x = 12
b0 = summary(mod.lm)$coefficients[1, 1]
b1 = summary(mod.lm)$coefficients[2, 1]
p9 = y = b0 + b1 * x

# 10. Calcule un intervalo de confianza al 95% para el porcentaje de absorción 
#     esperado, según la predicción anterior.
S = summary(mod.lm)$sigma
prob = 0.95; alpha = 1 - prob; t = qt(1 - alpha / 2, n - 2)
p10 = c(y - t * S * sqrt(1 / n + (x - mean(X))^2 / sum((X - mean(X))^2)),
       y + t * S * sqrt(1 / n + (x - mean(X))^2 / sum((X - mean(X))^2)))

p1; p2; p3; p4; p5; p6; p7; p8; p9; p10


################################################################################

         # Anuncios TV

"El objetivo de la publicidad en televisión es hacer que los televidentes 
obtengan una impresión favorable del producto y deseen comprarlo. Se realiza un 
experimento para determinar si la longitud de un anuncio publicitario afecta de 
algún modo a la apreciación que el espectador tiene del producto.

Para ello, se seleccionaron de forma aleatoria 50 personas que vieron un 
programa de una hora de duración. Durante el programa, se emitió un anuncio de 
dentífrico, de duración variable para cada persona en el experimento: una 
persona veía un anuncio de 20 segundos, otra de 30 segundos, …, hasta un máximo 
de 60 segundos. El contenido del anuncio era esencialmente el mismo. Al acabar 
el programa, se pasó una encuesta a los espectadores para medir cuánto 
recordaban del producto (en una escala de 0 a 30)."

# Datos del problema:

"seg		score
35		12
15		11
20		4
60		15
20		4
20		8
15		6
45		14
25		11
40		12
40		16
20		4
15		7
15		14
35		15
40		3
55		18
20		5
45		13
25		9
50		13
55		14
30		13
45		14
40		17
60		12
30		9
40		7
35		6
45		19
45		17
50		16
30		2
25		10
50		11
60		20
55		18
25		13
35		5
15		8
30		10
25		10
35		13
60		18
55		18
50		18
50		17
60		16
55		21
30		6"

datos <- read.table("clipboard", header = TRUE)
mod.lm <- lm(score ~ seg, datos)

Y = datos$score; X = datos$seg
b0 = summary(mod.lm)$coefficients[1, 1]
b1 = summary(mod.lm)$coefficients[2, 1]
Sb0 = summary(mod.lm)$coefficients[1, 2]
Sb1 = summary(mod.lm)$coefficients[2, 2]
n = length(Y)
SY2 = (sum(Y^2) - sum(Y)^2 / n) / (n - 1)
SX2 = (sum(X^2) - sum(X)^2 / n) / (n - 1)
SXY = (sum(X * Y) - sum(X) * sum(Y) / n) / (n - 1)
S = sqrt(((n - 1) * (SY2 - b1 * SXY)) / (n - 2))

# 1.  Dar la estimación del parámetro lineal (pendiente de la recta) del modelo 
#     de regresión lineal para la relación entre la longitud del anuncio y el 
#     resultado de la encuesta.
p1 = b1;

# 2.  Dar el error estándar del estimador anterior.
p2 = Sb1

# 3.  Con un riesgo alfa del 1%, ¿podemos concluir que la longitud del anuncio y 
#     la puntuación del test de memoria están linealmente relacionados? 
#     (1: si, 0: no).
alpha = 0.01; t = qt(1 - alpha / 2, n - 2)
p3 = if ((b1 - 0) / Sb1 > t) "Si, rechazamos H0" else "No, aceptamos H0"

# 4.  ¿Qué longitud tendría un anuncio visto por cierto individuo, el cual le 
#     hubiera asignado una puntuación de 2?
#     No se puede descartar que la respuesta tenga signo negativo.
y = 2
p4 = (y - b0) / b1 

# 5.  ¿Qué puntuación se obtendría de un individuo que ve un anuncio de 35 
#     segundos?
x = 35
p5 = y = b0 + b1 * x

# 6.  Dar un intervalo de confianza del 99% de la predicción anterior.
prob = 0.99; alpha = 1 - prob; t = qt(1 - alpha / 2, n - 2)
p6 = c(y - t * S * sqrt(1 + 1 / n + (x - mean(X))^2 / sum((X - mean(X))^2)),
       y + t * S * sqrt(1 + 1 / n + (x - mean(X))^2 / sum((X - mean(X))^2)))

# 7.  Estimar por IC del 99% la puntuación esperada de todos los anuncios que 
#     duran 35 segundos.
p7 = c(y - t * S * sqrt(1 / n + (x - mean(X))^2 / sum((X - mean(X))^2)),
       y + t * S * sqrt(1 / n + (x - mean(X))^2 / sum((X - mean(X))^2)))

# 8.  Obtenga una estimación del coeficiente de determinación R2.
p8 = (SXY / sqrt(SX2 * SY2))^2

# 9.  Obtenga el diagrama de residuos estandarizados vs la duración del anuncio, 
#     y diga cuál de las siguientes es la salida correcta:
Residus = c()
for (i in 1:n) {
   Residus[i] = Y[i]-b0-b1*X[i]
}
plot(X,Residus)

p1; p2; p3; p4; p5; p6; p7; p8


################################################################################

         # Análisis de residuos
"El crecimiento de un bebé en los primeros meses puede estar condicionado por 
varios factores. Uno de ellos puede ser el peso al nacer (X), pero no 
necesariamente la relación entre esta variable y, por ejemplo, el incremento de 
peso entre los días 70 y 100 de vida (Y)  puede ajustarse a través de un modelo 
de regresión lineal. Para comprobar que se cumplen las premisas se debe hacer un 
análisis exhaustivo de los residuos. En cada pregunta, observe los siguientes 
gráficos:

1. Y vs. X (Nótese que la pendiente negativa denota que los recién nacidos más 
pesados experimentan un menor crecimiento posterior)

2. Residuos vs. valores predichos

3. QQ-plot de Normalidad"


################################################################################