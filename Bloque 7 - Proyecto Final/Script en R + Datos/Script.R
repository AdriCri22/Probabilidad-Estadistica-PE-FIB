## B7 PROYECTO

# ==============================================================================
# Leemos el archivo xlsx
library(readxl)
path <- 'C:/Users/adria/Google Drive/PE/B7/'
data <- data.frame(read_excel(paste0(path, 'MICRODATOS_CONSUMO_ALCOHOL_INE.CA.xlsx')))

# Ver la tabla en R
# View(data)

# Seleccionamos las variables que queremos
cols <- c("CCAA", "SEXOa", "EDADa", "NIVEST", "S109", "S110", 
          "T114_1", "T114_2", "T116_1", "T116_2", 
          paste0("V12", c(2:4)), paste0("W128Cer_", 1:7), "CLASE_PR")
data <- data[, cols]

# ==============================================================================
# Transformaciones de variables
# Variables numéricas
colsNum <- c("EDADa", "S109", "S110", "T114_1", "T114_2", "T116_1", "T116_2", 
             paste0("W128Cer_", 1:7), "V123", "V124")

# Variables categóricas
colsCat <- colnames(data)[which(!colnames(data) %in% colsNum)]

## Transformamos las variables categóricas que realmente son números
for (col in colsNum) {
  data[, col] <- as.numeric(data[, col])
} 


# ------------------------------------------------------------------------------
# Tratamiento información deporte semanal
colsDeporte <- c("T114_1", "T114_2", "T116_1", "T116_2")
for (cD in colsDeporte) {
  data[which(is.na(data[, cD])), cD] <- 0
  quien <- which(data[, cD] < 98)
  data <- data[quien, ]
} 

data$DHorasSem <- rowSums(data[, c('T114_1', 'T116_1')], na.rm = T)
data$DMinSem <- rowSums(data[, c('T114_2', 'T116_2')], na.rm = T)
data$TiempoDeporte <- (data$DHorasSem*60 + data$DMinSem)/60

data[, c("DHorasSem", "DMinSem", "T114_1", "T114_2", "T116_1", "T116_2")] <- NULL
data$RealizaDeporte <- ifelse(data$TiempoDeporte == 0, "No", "Si")

# -------------------------------------------------------------------------------
# Tratamiento variables altura (S109) y peso (S110)
colGeneral <- c("S109", "S110")

data <- data[which(data$S109 <= 220), ]
data <- data[which(data$S110 <= 180), ]
data$Altura <- data$S109
data$Peso <- data$S110
data$S109 <- NULL
data$S110 <- NULL

# -------------------------------------------------------------------------------# Tratamiento informaciÃ³n tabaco
# Tratamiento variables tabaco
## Tipo de tabaco a fumar (V122)
data[which(is.na(data$V122)), 'V122'] <- 'No fuma'
data[which(data$V122 == 1), 'V122'] <- 'cigarrillos'
data[which(data$V122 == 2), 'V122'] <- 'puros'
data[which(data$V122 == 3), 'V122'] <- 'tabaco pipa'
data[which(data$V122 == 4), 'V122'] <- 'otros'
data$TipoTabaco <- data$V122
data$V122 <- NULL

## Numero de cigarrillo (V123)
var <- 'V123'
data[which(is.na(data[, var])), var] <- 0
data <- data[which(data[, var] < 98), ] 
data$CigarrillosSemanales <- data[, var]*7
data[, var] <- NULL

## Edad de empezar a fumar (V124)
var <- 'V124'
data[which(is.na(data[, var])), var] <- 0
data <- data[which(data[, var] < 98), ] 
data$EdadFum <- data[, var]
data[, var] <- NULL

# ------------------------------------------------------------------------------
# Tratamiento variables genero, edad, estudios
## Sexo
data$Sexo <- ifelse(data$SEXOa == "1", "Hombre", "Mujer")
data$SEXOa <- NULL

## Edad
data$Edad <- data$EDADa
data$EDADa <- NULL

# Estudios
data[which(data$NIVEST == '02'), 'NIVEST'] <- 'Sin conocimientos'
data[which(data$NIVEST == '03'), 'NIVEST'] <- 'EP incompleta'
data[which(data$NIVEST == '04'), 'NIVEST'] <- 'EP completa'
data[which(data$NIVEST == '05'), 'NIVEST'] <- 'ESO'
data[which(data$NIVEST == '06'), 'NIVEST'] <- 'Bachillerato'
data[which(data$NIVEST == '07'), 'NIVEST'] <- 'Grado Medio'
data[which(data$NIVEST == '08'), 'NIVEST'] <- 'Grado Superior'
data[which(data$NIVEST == '09'), 'NIVEST'] <- 'Estudios Universitarios'

data$NivelEstudios <- data$NIVEST
data$NIVEST <- NULL

# ------------------------------------------------------------------------------
# Tratamiento CCAA
data[which(data$CCAA == '01'), 'CCAA'] <- 'Andalucia'
data[which(data$CCAA == '02'), 'CCAA'] <- 'Aragon'
data[which(data$CCAA == '03'), 'CCAA'] <- 'Asturias'
data[which(data$CCAA == '04'), 'CCAA'] <- 'Baleares'
data[which(data$CCAA == '05'), 'CCAA'] <- 'Canarias'
data[which(data$CCAA == '06'), 'CCAA'] <- 'Cantabria'
data[which(data$CCAA == '07'), 'CCAA'] <- 'Castilla Leon'
data[which(data$CCAA == '08'), 'CCAA'] <- 'Castilla La Mancha'
data[which(data$CCAA == '09'), 'CCAA'] <- 'Catalunya'
data[which(data$CCAA == '10'), 'CCAA'] <- 'Comunidad Valencia'
data[which(data$CCAA == '11'), 'CCAA'] <- 'Extremadura'
data[which(data$CCAA == '12'), 'CCAA'] <- 'Galicia'
data[which(data$CCAA == '13'), 'CCAA'] <- 'Madrid'
data[which(data$CCAA == '14'), 'CCAA'] <- 'Murcia'
data[which(data$CCAA == '15'), 'CCAA'] <- 'Navarra'
data[which(data$CCAA == '16'), 'CCAA'] <- 'Pais Vasco'
data[which(data$CCAA == '17'), 'CCAA'] <- 'La Rioja'
data[which(data$CCAA == '18'), 'CCAA'] <- 'Ceuta'
data[which(data$CCAA == '19'), 'CCAA'] <- 'Melilla'

data$ComunidadAutonoma <- data$CCAA
data$CCAA <- NULL

# ------------------------------------------------------------------------------
# Tratamiendo información clase social
data[which(data$CLASE_PR == '1'), 'CLASE_PR'] <- 'Jefes grandes comercios'
data[which(data$CLASE_PR == '2'), 'CLASE_PR'] <- 'Jefes pequenyos comercios'
data[which(data$CLASE_PR == '3'), 'CLASE_PR'] <- 'Puestos intermedios y autonomos'
data[which(data$CLASE_PR == '4'), 'CLASE_PR'] <- 'Supervisores y trabajadores tecnicos'
data[which(data$CLASE_PR == '5'), 'CLASE_PR'] <- 'Otros trabajadores cualificados'
data[which(data$CLASE_PR == '6'), 'CLASE_PR'] <- 'Trabajadores no cualificados'
data[which(data$CLASE_PR == '9'), 'CLASE_PR'] <- 'No contesta'

data$ClaseSocial <- data$CLASE_PR
data$CLASE_PR <- NULL

# ------------------------------------------------------------------------------
# La variable de cerveza esta establecida en gramos. Si se mira la metodología
# se ve que 10g son 1 botella de 333ml por lo tanto deberíamos sumar
# todos los gramos, dividir por 10 para obtener el número de botellines y 
# posteriormente multiplicar por la cantidad de botellines. 

## Ponemos a 0 todos aquellos que tengan NA's y eliminamos registros
colCerve <- paste0("W128Cer_", 1:7)
for (cC in colCerve) {
  # Convertimos Nas en 0
  data[which(is.na(data[, cC])), cC] <- 0
  # Eliminamos los registros que tengan en su columna 98 y 99
  data <- data[which(data[, cC] < 98), ]
}

### Calcular los gramos de cerveza consumidos a la semana
data[, 'ConsumoCerveza'] <- rowSums(data[, paste0("W128Cer_", 1:7)])
data[, 'ConsumoCerveza'] <- data$ConsumoCerveza * 0.333

data[, paste0("W128Cer_", 1:7)] <- NULL

## Exportar datos a csv
write.csv(data, "Datos.csv")

################################################################################

# Después de la primera ejecución ya no hara falta ejecutar todo el tratamiento
data <- read.csv("C:/Users/adria/Google Drive/PE/B7/Datos.csv")

################################################################################

##==============================================================================
##
##  Vemos que hariables siguen una distribución normal y cuales no
##
##==============================================================================

## El Tiempo que hacen deporte no sigue una distribución normal
hist(data$TiempoDeporte,
     main = "Deporte",
     xlab = "Tiempo",
     ylab = "Personas",
     col = "#FFB74D",
     border = "#E65100")

## La Altura sigue una distribuvió normal
hist(data$Altura,
     main = "Altura",
     xlab = "Altura",
     ylab = "Personas",
     col = "#E57373",
     border = "#B71C1C")

## El peso sigue una distribución normal
hist(data$Peso,
     main = "Peso",
     xlab = "Peso",
     ylab = "Personas",
     col = "#F06292",
     border = "#880E4F")

## La edad sigue una distribución normal
hist(data$Edad,
     main = "Edad",
     xlab = "Edad",
     ylab = "Personas",
     col = "#69F0AE",
     border = "#00C853")

## La cantidad de cigarrillos semanales no sigue una distribución normal
hist(data$CigarrillosSemanales,
     main = "Cigarrillos semanales",
     xlab = "Cigarrilos semanales",
     ylab = "Personas",
     col = "#9575CD",
     border = "#311B92")

## La edad con la gente empezó a fumar no sigue una distribución normal
hist(data$EdadFum,
     main = "Edad con la que empezó a fumar",
     xlab = "Edad",
     ylab = "Personas",
     col = "#FDD835",
     border = "#F9A825")

## El consumo de cerveza no sigue una distribución normal
hist(data$ConsumoCerveza,
     main = "Cantidad de cerveza",
     xlab = "Libros",
     ylab = "Personas",
     col = "#4DB6AC",
     border = "#00695C")

## =============================================================================
##
## COMPARACIÓN DE POBLACIONES
##
## =============================================================================

### Hacer esto la primera vez
#poblacionDeporte <- data[which(data$RealizaDeporte == 'Si'), ]
#poblacionDeporte <- poblacionDeporte[sample(1:nrow(poblacionDeporte), 5000), ]

#poblacionNoDeporte <- data[which(data$RealizaDeporte == 'No'), ]
#poblacionNoDeporte <- poblacionNoDeporte[sample(1:nrow(poblacionNoDeporte), 5000), ]

## Exportar datos a csv
#write.csv(poblacionDeporte, "poblacionDeporte.csv")
#write.csv(poblacionNoDeporte, "poblacionNoDeporte.csv")

## Para que no vuelva a coger los datos aleatorios y cambien los resultados
## cargamos el archivo que hemos creado
poblacionDeporte <- read.csv("C:/Users/adria/Google Drive/PE/B7/poblacionDeporte.csv")
poblacionNoDeporte <- read.csv("C:/Users/adria/Google Drive/PE/B7/poblacionNoDeporte.csv")

### Realizar pruebas de normalidad de las dos poblaciones
### Si son normales, hacer t - student sino hacer Normal con sigmas desconcidas

qqnorm(poblacionDeporte$ConsumoCerveza,
       main = "Normal Q-Q Plot:\n Consumo de cerveza de las\n personas que practican deporte",
       col = "#4DB6AC")
qqnorm(poblacionNoDeporte$ConsumoCerveza,
       main = "Normal Q-Q Plot:\n Consumo de cerveza de las\n personas que no practican deporte",
       col = "#FFB74D")

#### Visualizamos los datos
win.graph(width = 8, height = 7)
par(font = 2, font.lab = 4, font.axis = 2, las = 1)
dataSexo <- data.frame(Si = poblacionDeporte$ConsumoCerveza, No = poblacionNoDeporte$ConsumoCerveza)
colors = c("#4DB6AC", "#FFB74D")
colorsBorders = c("#00695C", "#E65100")
boxplot(dataSexo, 
        main = "Consumo de cerveza: Deportistas vs No deportistas", 
        xlab = "¿Hacen deporte?",
        ylab = "Litros de cerveza", 
        col = colors, 
        border = colors, 
        lwd = 1, 
        pch = 16)

shapiro.test(poblacionDeporte$ConsumoCerveza)
shapiro.test(poblacionNoDeporte$ConsumoCerveza)

### Queremos comparar si la gente que practica deporte consume más alcohol que 
### la gente que no practica deporte activamente. 
#### H0: mu_{si} - mu_{no} = 0 
#### H1: mu_{si} - mu_{no} != 0

#### 1. Calcular las medias por cada una de las poblaciones
mediaSi <- mean(poblacionDeporte$ConsumoCerveza)
mediaNo <- mean(poblacionNoDeporte$ConsumoCerveza)

#### 2. Calcular la n por población
nSi <- length(poblacionDeporte$ConsumoCerveza)
nNo <- length(poblacionNoDeporte$ConsumoCerveza)

#### 3. Calcular la desviación tipo por población
varSi <- var(poblacionDeporte$ConsumoCerveza)
varNo <- var(poblacionNoDeporte$ConsumoCerveza)

#### 4. Calcular el estadístico correspondiente
##### 4.1 Calcular estadístico própio
Z <- (mediaSi - mediaNo)/sqrt((varSi/nSi) + (varNo/nNo))

##### 4.2 Calcular el valor "oficial" de las tablas
alfa = 0.05


# Rechazaremos si :
abs(Z) > qnorm(1 - (alfa/2))

# Por lo tanto rechazamos la hipótesis nula (H0), no tenemos evidencias para
# asegurar que la media de cerveza consumida semanalmente sea igual para la 
# gente que practica deporte y los que no la practican

# Inferencia con el intervalo e confianza
s = sqrt(((nSi - 1) * varSi^2 + (nNo - 1) * varNo^2) / (nSi + nNo - 2))
z = qnorm(0.975)
IC = c((mediaSi - mediaNo) - z * sqrt(s^2 / nSi + s^2 / nNo), 
       (mediaSi - mediaNo) + z * sqrt(s^2 / nSi + s^2 / nNo))

# ------------------------------------------------------------------------------
### Contrastar los valores obtenidos anteriormente, por alguna función programada
### de R que te permita encontrarlos sin haber de calcular todo entero. 

t.test(poblacionDeporte$ConsumoCerveza, poblacionNoDeporte$ConsumoCerveza, alternative = "two.sided", var.equal = FALSE)

## =============================================================================
##
## PREDICCIONES
##
## =============================================================================

## Queremos saber cual es el impacto que tiene la gente que hace deporte en el 
## consumo de cerveza

datos <- rbind(poblacionDeporte, poblacionNoDeporte)
modelo <- lm(datos$ConsumoCerveza ~ datos$TiempoDeporte, datos)
modelo1 <- step(modelo)
summary(modelo1)
predict(modelo1, interval='confidence')

par(mfrow = c(2, 2), font.lab = 2, las = 1, font.axis = 2)
plot(modelo1, c(2, 1)) # QQ-Norm i Standard Residuals vs. Fitted
# Histograma de los residuos estandaritzados
hist(rstandard(modelo1)) 
plot (1 : nrow(datos), rstandard(modelo1), type="l") # Ordre dels residus

# Si intentamos hacer una transformación logaritmica:
log(datos$ConsumoCerveza)
# Vemos que no podemos porque salen valores infinitos con los que no podemos trabajar

### Obtenemos los parametros
Y = datos$ConsumoCerveza; X = datos$TiempoDeporte
b0 = summary(modelo1)$coefficients[1, 1]
b1 = summary(modelo1)$coefficients[2, 1]
Sb0 = summary(modelo1)$coefficients[1, 2]
Sb1 = summary(modelo1)$coefficients[2, 2]
tb0 = summary(modelo1)$coefficients[1, 3]
tb1 = summary(modelo1)$coefficients[2, 3]
pb0 = summary(modelo1)$coefficients[1, 4]
pb1 = summary(modelo1)$coefficients[2, 4]
df = summary(modelo1)$df[2]
R2 = summary(modelo1)$r.squared
n = df + 2;
rXY = sqrt(R2)
SY = sqrt((sum(Y^2) - sum(Y)^2 / n) / (n - 1))
SX = sqrt((sum(X^2) - sum(X)^2 / n) / (n - 1))
SXY = (sum(X * Y) - sum(X) * sum(Y) / n) / (n - 1)
S = summary(modelo1)$sigma

# Estimación puntual
x = 7
y = b0 + b1 * x

# Intervalo de confianza para el valor esperado
t = qt(0.975, df)
IC = c(y - t * S * sqrt(1 / n + (x - mean(X))^2 / sum((X - mean(X))^2)),
      y + t * S * sqrt(1 / n + (x - mean(X))^2 / sum((X - mean(X))^2)))

# Supongamos que B1 = 1
tb1 = (b1 - 1) / Sb1

## =============================================================================

## Exportar datos a csv
## write.csv(data, "Datos.csv")

###=============================================================================
###
###   Más comparaciones
###
###=============================================================================
##------------------------------------------------------------------------------
## Queremos saber si los hombres consumen la misma cantidad de cerveza que las 
## mujeres
##------------------------------------------------------------------------------

### Hacer esto la primera vez
#Hombre <- data[which(data$Sexo == 'Hombre'), ]
#Hombre <- Hombre[sample(1:nrow(Hombre), 5000), ]

#Mujer <- data[which(data$Sexo == 'Mujer'), ]
#Mujer <- Mujer[sample(1:nrow(Mujer), 5000), ]

## Exportar datos a csv
#write.csv(Hombre, "Hombre.csv")
#write.csv(Mujer, "Mujer.csv")

## Para que no vuelva a coger los datos aleatorios y cambien los resultados
## cargamos el archivo que hemos creado
Hombre <- read.csv("C:/Users/adria/Google Drive/PE/B7/Hombre.csv")
Mujer <- read.csv("C:/Users/adria/Google Drive/PE/B7/Mujer.csv")

### Hipótesis
#### H0: mu_{Hombres} = mu_{Mujeres}
#### H1: mu_{Hombres} > mu_{Mujeres}

### Vemos si sigue una distribución normal
qqnorm(Hombre$ConsumoCerveza, # No sigue una distribución normal
       main = "Normal Q-Q Plot:\n Consumo de cerveza: Hombres",
       col = "#FFB74D") 
qqnorm(Mujer$ConsumoCerveza, # No sigue una distribución normal
       main = "Normal Q-Q Plot:\n Consumo de cerveza: Mujeres",
       col = "#9575CD")

hist(Hombre$ConsumoCerveza)
hist(Mujer$ConsumoCerveza)

#### Visualizamos los datos
win.graph(width = 8, height = 7)
par(font = 2, font.lab = 4, font.axis = 2, las = 1)
dataSexo <- data.frame(Hombres = Hombre$ConsumoCerveza, Mujeres = Mujer$ConsumoCerveza)
colors = c("#FFB74D", "#9575CD")
boxplot(dataSexo, 
        main = "Consumo de cerveza: Hombre y mujeres", 
        xlab = "Sexos",
        ylab = "Litros de cerveza", 
        col = colors, 
        border = colors, 
        lwd = 1, 
        pch = 16)

### Calculamos los datos que necesitaremos para realizar los cálculos

##### Calculamos la media de las dos poblaciones
mediaH <- mean(Hombre$ConsumoCerveza)
mediaM <- mean(Mujer$ConsumoCerveza)

##### Calculamos la n por población
nH <- length(Hombre$ConsumoCerveza)
nM <- length(Mujer$ConsumoCerveza)

##### Calculamos la desviación tipo por población
varH <- var(Hombre$ConsumoCerveza)
varM <- var(Mujer$ConsumoCerveza)

### Calculamos el estadístico
Z <- (mediaM - mediaH) / sqrt((varM / nM) + (varH / nH))

### Calulamos el p-valor (Unilateral)
P <- (1 - pnorm(abs(Z)))

### Conclusiones
if (P < alfa) "Rechazamos H0" else "No rechazamos H0"

#### Rechazamos H0, por lo que no hay evidencias de que las mujeres consuman la
#### misma cantidad de alcohol que los hombres

#### Contrastamos nuestro resutado con una función de R que nos permite hacer 
#### todos los cálculos de manera automatizada
t.test(Hombre$ConsumoCerveza, Mujer$ConsumoCerveza, alternative = "two.sided", var.equal = FALSE)

##------------------------------------------------------------------------------
## Queremos saber si las personas que fuman consumen la misma cantidad de 
## cerveza que las que no fuman
##------------------------------------------------------------------------------

### Hacer esto la primera vez
#Fumadores <- data[which(data$TipoTabaco != 'No fuma'), ]
#Fumadores <- Fumadores[sample(1:nrow(Fumadores), 5000), ]

#noFumadores <- data[which(data$TipoTabaco == 'No fuma'), ]
#noFumadores <- noFumadores[sample(1:nrow(noFumadores), 5000), ]

## Exportar datos a csv
#write.csv(Fumadores, "Fumadores.csv")
#write.csv(noFumadores, "noFumadores.csv")

## Para que no vuelva a coger los datos aleatorios y cambien los resultados
## cargamos el archivo que hemos creado
Fumadores <- read.csv("C:/Users/adria/Google Drive/PE/B7/Fumadores.csv")
noFumadores <- read.csv("C:/Users/adria/Google Drive/PE/B7/noFumadores.csv")

### Hipótesis
#### H0: mu_{Fumadores} = mu_{noFumadores}
#### H1: mu_{Fumadores} > mu_{noFumadores}

### Vemos si sigue una distribución normal
qqnorm(Fumadores$ConsumoCerveza, # No sigue una distribución normal
       main = "Normal Q-Q Plot:\n Consumo de cerveza: Fumadores",
       col = "#E57373")
qqnorm(noFumadores$ConsumoCerveza, # No sigue una distribución normal
       main = "Normal Q-Q Plot:\n Consumo de cerveza: No Fumadores",
       col = "#F06292")

hist(Fumadores$ConsumoCerveza)
hist(noFumadores$ConsumoCerveza)

#### Visualizamos los datos
win.graph(width = 8, height = 7)
par(font = 2, font.lab = 4, font.axis = 2, las = 1)
dataSexo <- data.frame(Si = Fumadores$ConsumoCerveza, No = noFumadores$ConsumoCerveza)
colors = c("#E57373", "#F06292")
boxplot(dataSexo, 
        main = "Consumo de cerveza: Fumadores y No Fumadores", 
        xlab = "¿Fuman?",
        ylab = "Litros de cerveza", 
        col = colors, 
        border = colors, 
        lwd = 1, 
        pch = 16)

### Calculamos los datos que necesitaremos para realizar los cálculos

##### Calculamos la media de las dos poblaciones
mediaF <- mean(Fumadores$ConsumoCerveza)
mediaNF <- mean(noFumadores$ConsumoCerveza)

##### Calculamos la n por población
nF <- length(Fumadores$ConsumoCerveza)
nNF <- length(noFumadores$ConsumoCerveza)

##### Calculamos la desviación tipo por población
varF <- var(Fumadores$ConsumoCerveza)
varNF <- var(noFumadores$ConsumoCerveza)

### Calculamos el estadístico
Z <- (mediaNF - mediaF) / sqrt((varNF / nNF) + (varF / nF))

### Calulamos el p-valor
P <- (1 - pnorm(abs(Z)))

### Conclusiones
if (P < alfa) "Rechazamos H0" else "No rechazamos H0"

#### Rechazamos H0, por lo que no hay evidencias de que las mujeres consuman la
#### misma cantidad de alcohol que los hombres

#### Contrastamos nuestro resutado con una función de R que nos permite hacer 
#### todos los cálculos de manera automatizada
t.test(Fumadores$ConsumoCerveza, noFumadores$ConsumoCerveza, alternative = "two.sided", var.equal = FALSE)


##==============================================================================
##
##  Comparamos si los Andaluces beben más cerveza que los Catalanes
##
##==============================================================================

### Hacer esto la primera vez
#Catalanes <- data[which(data$ComunidadAutonoma != 'Catalunya'), ]
#Catalanes <- Catalanes[sample(1:nrow(Catalanes), 2500), ]

#Andaluces <- data[which(data$ComunidadAutonoma == 'Andalucia'), ]
#Andaluces <- Andaluces[sample(1:nrow(Andaluces), 2500), ]

## Exportar datos a csv
#write.csv(Catalanes, "Catalanes.csv")
#write.csv(Andaluces, "Andaluces.csv")

## Para que no vuelva a coger los datos aleatorios y cambien los resultados
## cargamos el archivo que hemos creado
Catalanes <- read.csv("C:/Users/adria/Google Drive/PE/B7/Catalanes.csv")
Andaluces <- read.csv("C:/Users/adria/Google Drive/PE/B7/Andaluces.csv")

### Vemos si sigue una distribución normal
qqnorm(Catalanes$ConsumoCerveza, # No sigue una distribución normal
       main = "Normal Q-Q Plot:\n Consumo de cerveza: Cataluña",
       col = "#B71C1C")
qqnorm(Andaluces$ConsumoCerveza, # No sigue una distribución normal
       main = "Normal Q-Q Plot:\n Consumo de cerveza: Andalucía",
       col = "#1B5E20")

hist(Catalanes$ConsumoCerveza)
hist(Andaluces$ConsumoCerveza)

#### Visualizamos los datos
win.graph(width = 8, height = 7)
par(font = 2, font.lab = 4, font.axis = 2, las = 1)
dataSexo <- data.frame(Cataluña = Catalanes$ConsumoCerveza, Andalucía = Andaluces$ConsumoCerveza)
colors = c("#B71C1C", "#1B5E20")
boxplot(dataSexo, 
        main = "Consumo de cerveza: Cataluña vs Andalucía", 
        xlab = "Demografía",
        ylab = "Litros de cerveza", 
        col = colors, 
        border = colors, 
        lwd = 1, 
        pch = 16)

### Calculamos los datos que necesitaremos para realizar los cálculos

##### Calculamos la media de las dos poblaciones
mediaC <- mean(Catalanes$ConsumoCerveza)
mediaA <- mean(Andaluces$ConsumoCerveza)

##### Calculamos la n por población
nC <- length(Catalanes$ConsumoCerveza)
nA <- length(Andaluces$ConsumoCerveza)

##### Calculamos la desviación tipo por población
varC <- var(Catalanes$ConsumoCerveza)
varA <- var(Andaluces$ConsumoCerveza)

### Calculamos el estadístico
Z <- (mediaC - mediaA) / sqrt((varC / nC) + (varA / nA))

### Calulamos el p-valor
P <- (1 - pnorm(abs(Z)))

### Conclusiones
if (P < alfa) "Rechazamos H0" else "No rechazamos H0"

################################################################################
##
##  Vamos a ver la relación entre el consumo de alcohol y la edad
##
################################################################################
modelo2 <- lm(datos$ConsumoCerveza ~ datos$Edad, data)
modelo2 <- step(modelo2)

## Para ver la relación edad Cerveza
plot(datos$ConsumoCerveza ~ datos$Edad, pch = 3,
     ylab = "Litros de cerveza",
     xlab = "Edad",
     col = "#546E7A")

################################################################################
##
##  Vamos a ver la relación entre el consumo de alcohol y el peso
##
################################################################################
plot(datos$ConsumoCerveza ~ datos$Peso, pch = 3,
     ylab = "Litros de cerveza",
     xlab = "Peso",
     col = "#FF3D00")

################################################################################
##
##  Vamos a ver la relación entre el consumo de alcohol y el altura
##
################################################################################
plot(datos$ConsumoCerveza ~ datos$Altura, pch = 3,
     ylab = "Litros de cerveza",
     xlab = "Altura",
     col = "#00695C")


