library(foreign)
library(dplyr)
library(tidyr)
library(ggplot2)
library(questionr)
library(descr)


# Se leen los archivos csv de la ENOE o en su caso en otro formato
coe1 <- read_csv("2018trim1_csv/COE1T118.CSV")
coe2 <- read_csv("2018trim1_csv/COE2T118.CSV")
hog <- read_csv("2018trim1_csv/HOGT118.CSV")
sdem <- read_csv("2018trim1_csv/SDEMT118.CSV")
viv <- read_csv("2018trim1_csv/VIVT118.CSV")


# Se chec el archivo coe1
head(coe1)

# Se checan los valores únicos de la variable cd_a
unique(coe1$cd_a)

# Se filtra para contener solo a Torreón, como ciudad autorepresentada

coe1.trc <- subset(coe1, coe1$cd_a == "06")
coe2.trc <- subset(coe2, coe2$cd_a == "06")

# Se checa que solo contenga Torreón
unique(hog$cd_a)
hog.trc <- subset(hog, hog$cd_a == 6)

sdem.trc <- subset(sdem, sdem$cd_a == "06")

unique(viv$cd_a)
viv.trc <- subset(viv, viv$cd_a == 6)

# Vivienda

# Se extrae el factor de expansión de vivienda de la encuesta
viv.fac <- viv.trc$fac
summary(viv.fac)

# P1 ¿Cuántas personas viven normalmente en esta vivienda
# contando a los niños chiquitos y a los ancianos?

# Si se utilizara solo las respuestas del ENOE se usan estos resultados
summary(as.factor(viv.trc$p1))

# Para poder conocer los valores estimados, se aplica el factor de expansión
wtd.table(viv.trc$p1, weights = viv.fac)

# P2 ¿Todas estas personas comparten un mismo gasto para comer?

# Se crean las varaibles levels y labels para poder conocer las respuestas de la pregunta,
# en este caso es una variable dicotómica que solo contiene las respuestas Si y No
viv.p2.lev <- c(1,2)
viv.p2.lab <- c("Si", "No")

# Se crea una variable llamada viv.p2 que contiene la pregunta p2, ya factorizada con las variables
# de levels y labels

viv.p2 <- factor(viv.trc$p2, levels = viv.p2.lev,
                 labels = viv.p2.lab)
# se aplica el factor de expansión
wtd.table(viv.p2, weights = viv.fac)

# P3 ¿Cuántos hogares o grupos de personas tienen gastos separados
# para comer contando el de usted?

summary(as.factor(viv.trc$p2))
wtd.table(viv.trc$p3, weights = viv.fac)

# Hogar
# se extrae el factor de expansión
hog.fac <- hog.trc$fac

# P4_1 ¿En este hogar se contrata a trabajos domésticos...?
hog.p4.lev <- c(1,2,3,4)
hog.p4.lab <- c("de entrada por salida",
                "de planta",
                "de entrada por salida y de planta",
                "No contrata")

hog.p4_1 <- factor(hog.trc$p4_1, levels = hog.p4.lev,
                   labels = hog.p4.lab)
summary(hog.p4_1)
wtd.table(hog.p4_1, weights = hog.fac)
# P4_2 ¿Cuántos?

summary(as.factor(hog.trc$p4_2))
wtd.table(hog.trc$p4_2, weights = hog.fac)

# Sociodemográfico

sdem.fac <- sdem.trc$fac

# Sexo
sex.lev <- c(1,2)
sex.lab <- c("Hombre", "Mujer")

sdem.sex <- factor(sdem.trc$sex, levels = sex.lev,
                   labels = sex.lab)
summary(sdem.sex)
wtd.table(sdem.sex, weights = sdem.fac)

# Edad

summary(as.factor(sdem.trc$eda))
wtd.table(sdem.trc$eda, weights = sdem.fac)

# P12 ¿Sabe leer y escribir un recado?

# P13 ¿Hasta qué grado aprobó en la escuela?

# P13_2 ¿Hasta qué año aprobó en la escuela?

# CS_P16 ¿Terminó los estudios o materias de esta carrera?

# CS_P17 ¿Asiste actualmente a la escuela?

# N_HIJ Cuantas hijas e hijos ha tenido

# E_CON Estado conyugal

# Clase1 Clasificación de la población en PEA y PNEA

# Clase2 Clasificación de la población ocupada y desocupada;
# disponible y no disponible

# Clase3 Clasificación de la población, en ocupada plena, sin pago,
# ausente con nexo laboral y retorno, desocupada, iniciadora con búsqueda y
# ausente sin ingreso y nexo laboral