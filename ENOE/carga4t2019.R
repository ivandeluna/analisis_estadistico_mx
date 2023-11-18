#--------------------------------------------------------------------------------------------
#
#                  Carga de archivos para la ENOE 4T 2019
#
#--------------------------------------------------------------------------------------------

library(readr)

#--------------------------------------------------------------------------------------------
#
#                  Leer archivos e importarlos a R, crear data frames
#
#--------------------------------------------------------------------------------------------


# Se leen los archivos csv de la ENOE o en su caso en otro formato
t419.coe1 <- read_csv("2019T4/COE1T419.CSV")
t419.coe2 <- read_csv("2019T4/COE2T419.CSV")
t419.hog <- read_csv("2019T4/HOGT419.CSV")
t419.sdem <- read_csv("2019T4/SDEMT419.CSV")
t419.viv <- read_csv("2019T4/VIVT419.CSV")

# Se filtra para contener solo a Torreón, como ciudad autorepresentada

t419.coe1.trc <- subset(t419.coe1, t419.coe1$cd_a == "06")
t419.coe2.trc <- subset(t419.coe2, t419.coe2$cd_a == "06")
t419.hog.trc <- subset(t419.hog, t419.hog$cd_a == 6) # en esta tabla el valor es númerico y no caracter
t419.sdem.trc <- subset(t419.sdem, t419.sdem$cd_a == "06")

t419.sdem.stl <- subset(t419.sdem, t419.sdem$cd_a == "17")

# De acuerdo al documento "Conociendo la base de datos de la ENOE"
# es posible unir tablas siguiendo los siguientes pasos:

# 1. Eliminar todos los registros de SDEM que en el campo R_DEF sean diferente a 00 o Entrevista completa
unique(t419.sdem.trc$r_def)
t419.sdem.trc <- subset(t419.sdem.trc, r_def == "00")

# 2. Eliminar todos los registros de la tabla SDEM que en el campo C_RES sean iguales a 2 o Ausente Definitivo
unique(t419.sdem.trc$c_res)
t419.sdem.trc <- subset(t419.sdem.trc, c_res != 2)

# 3. Eliminar todos los registros de la tabla SDEM que en el campo EDA contengan los códigos de 00 a 11 a 99
unique(t419.sdem.trc$eda)
t419.sdem.trc <- t419.sdem.trc[!(t419.sdem.trc$eda %in%
                                   c("00","01","02","03",
                                     "04","05","06","07",
                                     "08","09","10","11","99")),]

# 4. Registros finales de la tabla SDEMT
length(t419.sdem.trc)

# 5. Eliminar todos los registros de la tabla COE1 y COE2 que en el campo R_DEF sean diferentes de 00.
unique(t419.coe1.trc$r_def)
t419.coe1.trc <- t419.coe1.trc[!(t419.coe1.trc$r_def == "00"),]

# 6. Unir las tres tablas con base a la clave principal
t419.trc1 <- merge(t419.sdem.trc, t419.coe1.trc) # Tabla SDEM con COE1
t419.trc2 <- merge(t419.sdem.trc, t419.coe2.trc) # Tabla SDEM con COE2, no es necesario hacerla
t419.trc <- merge(t419.trc1, t419.coe2.trc)      # Tabla SDEM con COE1 y COE2
