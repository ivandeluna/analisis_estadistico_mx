
#--------------------------------------------------------------------------------------------
#
#                  Carga de archivos para la ENOE 3T 2018
#
#--------------------------------------------------------------------------------------------

library(readr)

#--------------------------------------------------------------------------------------------
#
#                  Leer archivos e importarlos a R, crear data frames
#
#--------------------------------------------------------------------------------------------


# Se leen los archivos csv de la ENOE o en su caso en otro formato
t318.coe1 <- read_csv("2018T3/coe1t318.CSV")
t318.coe2 <- read_csv("2018T3/coe2t318.CSV")
t318.hog <- read_csv("2018T3/hogt318.CSV")
t318.sdem <- read_csv("2018T3/sdemt318.CSV")
t318.viv <- read_csv("2018T3/vivt318.CSV")

# Se filtra para contener solo a Torreón, como ciudad autorepresentada

t318.coe1.trc <- subset(t318.coe1, t318.coe1$CD_A == "06")
t318.coe2.trc <- subset(t318.coe2, t318.coe2$CD_A == "06")
t318.hog.trc <- subset(t318.hog, t318.hog$CD_A == 6) # en esta tabla el valor es númerico y no caracter
t318.sdem.trc <- subset(t318.sdem, t318.sdem$CD_A == "06")

# De acuerdo al documento "Conociendo la base de datos de la ENOE"
# es posible unir tablas siguiendo los siguientes pasos:

# 1. Eliminar todos los registros de SDEM que en el campo R_DEF sean diferente a 00 o Entrevista completa
unique(t318.sdem.trc$R_DEF)
t318.sdem.trc <- subset(t318.sdem.trc, R_DEF == "00")

# 2. Eliminar todos los registros de la tabla SDEM que en el campo C_RES sean iguales a 2 o Ausente Definitivo
unique(t318.sdem.trc$C_RES)
t318.sdem.trc <- subset(t318.sdem.trc, C_RES != 2)

# 3. Eliminar todos los registros de la tabla SDEM que en el campo EDA contengan los cógidos de 00 a 11 a 99
unique(t318.sdem.trc$EDA)
t318.sdem.trc <- t318.sdem.trc[!(t318.sdem.trc$EDA %in%
                                   c("00","01","02","03",
                                     "04","05","06","07",
                                     "08","09","10","11","99")),]

# 4. Registros finales de la tabla SDEMT
length(t318.sdem.trc)

# 5. Eliminar todos los registros de la tabla COE1 y COE2 que en el campo R_DEF sean diferentes de 00.
unique(t318.coe1.trc$R_DEF)
t318.coe1.trc <- t318.coe1.trc[!(t318.coe1.trc$R_DEF == "00")]

# 6. Unir las tres tablas con base a la clave principal
t318.trc1 <- merge(t318.sdem.trc, t318.coe1.trc) # Tabla SDEM con COE1
t318.trc2 <- merge(t318.sdem.trc, t318.coe2.trc) # Tabla SDEM con COE2, no es necesario hacerla
t318.trc <- merge(t318.trc1, t318.coe2.trc)      # Tabla SDEM con COE1 y COE2

rm(t318.trc1)

