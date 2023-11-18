
#--------------------------------------------------------------------------------------------
#
#                  Carga de archivos para la ENOE 1T 2019
#
#--------------------------------------------------------------------------------------------

library(readr)

#--------------------------------------------------------------------------------------------
#
#                  Leer archivos e importarlos a R, crear data frames
#
#--------------------------------------------------------------------------------------------


# Se leen los archivos csv de la ENOE o en su caso en otro formato
t119.coe1 <- read_csv("2019T1/coe1t119.CSV")
t119.coe2 <- read_csv("2019T1/coe2t119.CSV")
t119.hog <- read_csv("2019T1/hogt119.CSV")
t119.sdem <- read_csv("2019T1/sdemt119.CSV")
t119.viv <- read_csv("2019T1/vivt119.CSV")

# Se filtra para contener solo a Torreón, como ciudad autorepresentada

t119.coe1.trc <- subset(t119.coe1, t119.coe1$CD_A == "06")
t119.coe2.trc <- subset(t119.coe2, t119.coe2$CD_A == "06")
t119.hog.trc <- subset(t119.hog, t119.hog$CD_A == 6) # en esta tabla el valor es númerico y no caracter
t119.sdem.trc <- subset(t119.sdem, t119.sdem$CD_A == "06")

t119.sdem.stl <- subset(t119.sdem, t119.sdem$CD_A == "17")

# De acuerdo al documento "Conociendo la base de datos de la ENOE"
# es posible unir tablas siguiendo los siguientes pasos:

# 1. Eliminar todos los registros de SDEM que en el campo R_DEF sean diferente a 00 o Entrevista completa
unique(t119.sdem.trc$R_DEF)
t119.sdem.trc <- subset(t119.sdem.trc, R_DEF == "00")

# 2. Eliminar todos los registros de la tabla SDEM que en el campo C_RES sean iguales a 2 o Ausente Definitivo
unique(t119.sdem.trc$C_RES)
t119.sdem.trc <- subset(t417.sdem.trc, C_RES != 2)

# 3. Eliminar todos los registros de la tabla SDEM que en el campo EDA contengan los códigos de 00 a 11 a 99
unique(t119.sdem.trc$EDA)
t119.sdem.trc <- t119.sdem.trc[!(t119.sdem.trc$EDA %in%
                                   c("00","01","02","03",
                                     "04","05","06","07",
                                     "08","09","10","11","99")),]

# 4. Registros finales de la tabla SDEMT
length(t119.sdem.trc)

# 5. Eliminar todos los registros de la tabla COE1 y COE2 que en el campo R_DEF sean diferentes de 00.
unique(t119.coe1.trc$R_DEF)
t119.coe1.trc <- t119.coe1.trc[!(t119.coe1.trc$R_DEF == "00")]

# 6. Unir las tres tablas con base a la clave principal
t119.trc1 <- merge(t119.sdem.trc, t119.coe1.trc) # Tabla SDEM con COE1
t119.trc2 <- merge(t119.sdem.trc, t119.coe2.trc) # Tabla SDEM con COE2, no es necesario hacerla
t119.trc <- merge(t119.trc1, t119.coe2.trc)      # Tabla SDEM con COE1 y COE2

