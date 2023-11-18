
#--------------------------------------------------------------------------------------------
#
#                  Carga de archivos para la ENOE 2T 2018
#
#--------------------------------------------------------------------------------------------

library(readr)

#--------------------------------------------------------------------------------------------
#
#                  Leer archivos e importarlos a R, crear data frames
#
#--------------------------------------------------------------------------------------------


# Se leen los archivos csv de la ENOE o en su caso en otro formato
t218.coe1 <- read_csv("2018T2/coe1t218.CSV")
t218.coe2 <- read_csv("2018T2/coe2t218.CSV")
t218.hog <- read_csv("2018T2/hogt218.CSV")
t218.sdem <- read_csv("2018T2/sdemt218.CSV")
t218.viv <- read_csv("2018T2/vivt218.CSV")

# Se filtra para contener solo a Torreón, como ciudad autorepresentada

t218.coe1.trc <- subset(t218.coe1, t218.coe1$CD_A == "06")
t218.coe2.trc <- subset(t218.coe2, t218.coe2$CD_A == "06")
t218.hog.trc <- subset(t218.hog, t218.hog$CD_A == 6) # en esta tabla el valor es númerico y no caracter
t218.sdem.trc <- subset(t218.sdem, t218.sdem$CD_A == "06")

# De acuerdo al documento "Conociendo la base de datos de la ENOE"
# es posible unir tablas siguiendo los siguientes pasos:

# 1. Eliminar todos los registros de SDEM que en el campo R_DEF sean diferente a 00 o Entrevista completa
unique(t218.sdem.trc$R_DEF)
t218.sdem.trc <- subset(t218.sdem.trc, R_DEF == "00")

# 2. Eliminar todos los registros de la tabla SDEM que en el campo C_RES sean iguales a 2 o Ausente Definitivo
unique(t218.sdem.trc$c_res)
t218.sdem.trc <- subset(t218.sdem.trc, C_RES != 2)

# 3. Eliminar todos los registros de la tabla SDEM que en el campo EDA contengan los cógidos de 00 a 11 a 99
unique(t218.sdem.trc$eda)
t218.sdem.trc <- t218.sdem.trc[!(t218.sdem.trc$EDA %in%
                                   c("00","01","02","03",
                                     "04","05","06","07",
                                     "08","09","10","11","99")),]

# 4. Registros finales de la tabla SDEMT
length(t218.sdem.trc)

# 5. Eliminar todos los registros de la tabla COE1 y COE2 que en el campo R_DEF sean diferentes de 00.
unique(t218.coe1.trc$r_def)
t218.coe1.trc <- t218.coe1.trc[!(t218.coe1.trc$R_DEF == "00")]

# 6. Unir las tres tablas con base a la clave principal
t218.trc1 <- merge(t218.sdem.trc, t218.coe1.trc) # Tabla SDEM con COE1
t218.trc2 <- merge(t218.sdem.trc, t218.coe2.trc) # Tabla SDEM con COE2, no es necesario hacerla
t218.trc <- merge(t218.trc1, t218.coe2.trc)      # Tabla SDEM con COE1 y COE2

rm(t218.trc1)

