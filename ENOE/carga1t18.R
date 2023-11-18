
#--------------------------------------------------------------------------------------------
#
#                  Carga de archivos para la ENOE 1T 2018
#
#--------------------------------------------------------------------------------------------

library(readr)

#--------------------------------------------------------------------------------------------
#
#                  Leer archivos e importarlos a R, crear data frames
#
#--------------------------------------------------------------------------------------------


# Se leen los archivos csv de la ENOE o en su caso en otro formato
t118.coe1 <- read_csv("2018T1/COE1T118.CSV")
t118.coe2 <- read_csv("2018T1/COE2T118.CSV")
t118.hog <- read_csv("2018T1/HOGT118.CSV")
t118.sdem <- read_csv("2018T1/SDEMT118.CSV")
t118.viv <- read_csv("2018T1/VIVT118.CSV")

# Se filtra para contener solo a Torreón, como ciudad autorepresentada

t118.coe1.trc <- subset(t118.coe1, t118.coe1$cd_a == "06")
t118.coe2.trc <- subset(t118.coe2, t118.coe2$cd_a == "06")
t118.hog.trc <- subset(t118.hog, t118.hog$cd_a == 6) # en esta tabla el valor es númerico y no caracter
t118.sdem.trc <- subset(t118.sdem, t118.sdem$cd_a == "06")

# De acuerdo al documento "Conociendo la base de datos de la ENOE"
# es posible unir tablas siguiendo los siguientes pasos:

# 1. Eliminar todos los registros de SDEM que en el campo R_DEF sean diferente a 00 o Entrevista completa
unique(t118.sdem.trc$r_def)
t118.sdem.trc <- subset(t118.sdem.trc, r_def == "00")

# 2. Eliminar todos los registros de la tabla SDEM que en el campo C_RES sean iguales a 2 o Ausente Definitivo
unique(t118.sdem.trc$c_res)
t118.sdem.trc <- subset(t118.sdem.trc, c_res != 2)

# 3. Eliminar todos los registros de la tabla SDEM que en el campo EDA contengan los cógidos de 00 a 11 a 99
unique(t118.sdem.trc$eda)
t118.sdem.trc <- t118.sdem.trc[!(t118.sdem.trc$eda %in%
                                   c("00","01","02","03",
                                     "04","05","06","07",
                                     "08","09","10","11","99")),]

# 4. Registros finales de la tabla SDEMT
length(t118.sdem.trc)

# 5. Elminar todos los registros de la tabla COE1 y COE2 que en el campo R_DEF sean diferentes de 00.
unique(t118.coe1.trc$r_def)
t118.coe1.trc <- t118.coe1.trc[!(t118.coe1.trc$r_def == "00")]

# 6. Unir las tres tablas con base a la clave principal
t118.trc1 <- merge(t118.sdem.trc, t118.coe1.trc) # Tabla SDEM con COE1
t118.trc2 <- merge(t118.sdem.trc, t118.coe2.trc) # Tabla SDEM con COE2, no es necesario hacerla
t118.trc <- merge(t118.trc1, t118.coe2.trc)      # Tabla SDEM con COE1 y COE2

rm(t118.trc1)
