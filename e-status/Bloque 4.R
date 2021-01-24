# ECTS
hores = c(101, 106, 119, 115, 111, 119, 113, 88, 106)
# 1 Media
p1 = mean(hores)

# 2 Error de la media
p2 = sd(hores) / sqrt(length(hores))

# 3 Intervalo de confianza sabiendo la desviación
prob = 0.9; desv = 16
p3 = c(mean(hores) - qnorm(prob + (1 - prob)/2) * desv / sqrt(length(hores)), 
       mean(hores) + qnorm(prob + (1 - prob)/2) * desv / sqrt(length(hores)))

# 4 Intervalo de confianza sin saber la desviación
prob = 0.8
p4 = t.test(hores, conf.level=prob)$conf.int
p4 = c(p4[1], p4[2])

# 5 Averiguar n sabiendo la amplitud = límite superior - límite inferior
prob = 0.8; desv = 16; amplitud = 5
p5 = ceiling(((qnorm(prob + (1 - prob)/2) * desv) / (amplitud / 2))^2)

p1; p2; p3; p4; p5



# Modelos estadísticos
T = 40; S = 28; R = 12; Q = 37
X = 25; Y = 17; U = 44
# 1 Prob(T < 2.704)
p1 = pt(2.704, T)

# 2 Prob(S > ?) = 0.001
p2 = qt(1 - 0.001, S)

# 2 P(S < ?) = 0.99
p2 = qt(0.99, S)

# 3 P(|R| < ? ) = 0.998
p3 = qt(0.998, R)

# 3 P(|R| > 8.61) = ?
p3 = pt(-8.61, R) + (1 - pt(8.61, R))

# 4 P(|Q| > 1.06118) = ?
p4 = 1 - 2 * (1 - pt(1.06118, Q))

# 4 P(|Q| < 1.06118) = ?
p4 = 2 * (1 - pt(1.06118, Q))

# 5 
p5 = 1 - pchisq(12.401, X)

# 6
p6 = qchisq(0.99, Y)

# 7
p7 = pchisq(57.41, U) - pchisq(41.27, U)


# Huevos de gallina feliz
esp = 62.4; desv = 3.8
# 1 Intervalo
alpha = 1 - ((0.65 + 0.7) / 2) 
p1 = c(esp - desv * qnorm(1 - alpha/2), esp + desv * qnorm(1 - alpha/2))

# 2 Intervalo con diferente tamaño
desv2 = desv / sqrt(2)
p2 = c(esp - desv2 * qnorm(1 - alpha/2), esp + desv2 * qnorm(1 - alpha/2))

# 3 Intervalo con diferente tamaño 2
desv3 = desv / sqrt(9)
p3 = c(esp - desv3 * qnorm(1 - alpha/2), esp + desv3 * qnorm(1 - alpha/2))

# 4 Intervalo al revés
min = 61.2475; max = 63.5525; prob = 0.825
p4 = round(((qnorm(prob + (1 - prob)/2) * desv) / (max - esp))^2)

p1; p2; p3; p4



# New tires
life = c(40.6, 34.9, 39.6, 36.6, 35.7, 41.2, 33.6, 42.7, 43.8, 34.4, 34.7, 44, 39.8)
n = length(life)
# 1 H0
p1 = 40 # Dato que te lo da el enunciado

# 2 
# manufacturer's point of view -> > 
# users's point of view -> <

# 3 media
p3 = mean(life)

# 4 Calcular el valor del estadístico
p4 = (p3 - p1) / (sd(life) / sqrt(n))

# 5 P-valor
p5 = pt(-p4, n - 1)

# 6 Intervalo de confianza
prob = 0.99
p6 = t.test(life, conf.level=prob)$conf.int
p6 = c(p6[1], p6[2])

# 7 Desviación 
p7 = sd(life)

# 8 Intervalo de confianza
prob = 0.99; alpha = 1 - prob
p8 = c((sd(life)^2 * (n - 1)) / qchisq(1 - alpha / 2, n - 1),
       sd(life)^2 * (n - 1) / qchisq(alpha / 2, n - 1))

p1; p3; p4; p5; p6; p7; p8



# Autentication failed
tiempos = c(72, 97, 93, 89, 113, 100, 45, 141, 88, 88, 76)
n = length(tiempos)
# 1 Error  tipico de la media muestral
p1 = sd(tiempos) / sqrt(n)

# 2 Estimar la variancia del tiempo entre pulsaciones
p2 = p1^2 * n

# 3 Intervalo de confianza sin saber la desviación
p3 = (113.7 - mean(tiempos)) / (sqrt(p2) / sqrt(n))
p3 = (1 - (pt(-p3, n -1))*2) * 100

# 4 P-valor
z = (mean(tiempos) - 96) / p1
p4 =  (pt(z, n - 1)) # Opción de un asalto identificable por tiempos más rápidos
#p4 = 2 * p4 

# 5 Intervalo de confianza
prob = 0.85; alpha = 1 - prob
p5 = c(sqrt((n - 1) * sd(tiempos)^2 / qchisq(1 - alpha / 2, n - 1)),
       sqrt((n - 1) * sd(tiempos)^2 / qchisq(alpha / 2, n - 1)))
p5 = c(p5[1]^2, p5[2]^2) #Si pide para la variancia

# 6 Intervalo incorrecto
p = p2 * (n - 1) / 231.4
p = 1 - pchisq(p, n - 1)
p2 * (n - 1) / qchisq(p, n - 1)

p = p2 * (n - 1) / 251.1
p = 1 - pchisq(p, n - 1)
p2 * (n - 1) / qchisq(p, n - 1)

p = p2 * (n - 1) / 350.5
p = 1 - pchisq(p, n - 1)
p2 * (n - 1) / qchisq(p, n - 1)

p1; p2; p3; p4; p5



#Captchas
# 1 Calcular IC para una Binomial
n = 400; c = 49; prob = 0.98
alpha = 1 - prob; z = qnorm(1 - alpha / 2); P = c / n;
p1 = c(P - z * sqrt(P * (1 - P) / n), P + z * sqrt(P * (1 - P) / n))

# 2 Deducir el número de observaciones
confianza = 0.90; amplitud = 0.079; P = 0.2
alpha = 1- confianza; z = qnorm(1 - alpha / 2)
p2 = ceiling(P * (1 - P) / (amplitud / (2 * z))^2)

# 3
P = 0.5
p3 = ceiling(P * (1 - P) / (amplitud / (2 * z))^2)

# 4 
inf = 0.353; sup = 0.4796; confianza = 0.995
alpha = 1 - confianza; z = qnorm(1 - alpha / 2); amplitud = sup - inf; P = inf + amplitud / 2 
p4 = ceiling(P * (1 - P) / (amplitud / (2 * z))^2)

# 5 Calcular el estadistico suponiendo n0 con el primer ejercicio
n = 400; c = 49; prob = 0.98; n0 = 0.14
P = c / n;
p5 = z = (P - n0) / sqrt(n0 * (1 - n0) / n)

# 6 P valor apartado anterior
p6 = pnorm(z) * 2

p1; p2; p3; p4; p5; p6



# Las que más duran
media = 80
datos = c(99.5, 81, 91, 103.5, 59.5, 57, 95, 61, 57, 88, 92.5, 77.5, 57.5, 99.5, 76.5)
n = length(datos)
# 1 Calcular el estadístico z
var = 200
p1 = (mean(datos) - media) / sqrt(var / n)
p1 = 1 - pnorm(p1)

#2 
p2 = if (p1 > 0.05) 1 else 2 
# 1 - p1 > 0.05 => No tenemos evidencias para dudar de H (No se ha hallado diferencia estadísticamente significativa)
# 2 - p1 < 0.05 => Rechazo (H es poco verosimil)

# 3 desviación estándar de la muestra
p3 = sd(datos)

# 4 estadístico t según la desviación estándar de la muestra
p4 = (mean(datos) - media) / (p3 / sqrt(n))

# 5 dónde se ubica el punto crítico para rechazar la hipótesis H con riesgo de 0.01
alpha = 0.25
p5 = qt(1 - alpha, n - 1)

# 6 P valor asociado a la prueba
p6 = 1 - pt(p4, n - 1)

# 7 Valorar la conclusión
p7 = if (p6 < 0.05) 1 else 2 
# 1 - p5 < 0.05 => Con riesgo del 5%, podemos afirmar que las nuevas baterias duran más en media
# 2 - p5 > 0.05 => si existe una diferencia, no hemos sabido encontrarla

# 8 Intervalo de confianza
p8 = t.test(datos, conf.level=0.999)$conf.int
p8 = c(p8[1], p8[2])

# 9 Cuantas observaciones se necesitan
prob = 0.80; amplitud = 212; desv = 1075
p9 = ceiling(((qnorm(prob + (1 - prob)/2) * desv) / (amplitud / 2))^2)

p1; p2; p3; p4; p5; p6; p7; p8; p9

