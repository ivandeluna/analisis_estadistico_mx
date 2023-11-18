
#--------------------------------------------------------------------------------------------
#
#                  Carga de archivos para la ENOE 4T 2017
#
#--------------------------------------------------------------------------------------------

library(readr)

#--------------------------------------------------------------------------------------------
#
#                  Leer archivos e importarlos a R, crear data frames
#
#--------------------------------------------------------------------------------------------


# Se leen los archivos csv de la ENOE o en su caso en otro formato
t417.coe1 <- read_csv("2017T4/coe1t417.CSV")
t417.coe2 <- read_csv("2017T4/coe2t417.CSV")
t417.hog <- read_csv("2017T4/hogt417.CSV")
t417.sdem <- read_csv("2017T4/sdemt417.CSV")
t417.viv <- read_csv("2017T4/vivt417.CSV")

# Se filtra para contener solo a Torreón, como ciudad autorepresentada

t417.coe1.trc <- subset(t417.coe1, t417.coe1$cd_a == "06")
t417.coe2.trc <- subset(t417.coe2, t417.coe2$cd_a == "06")
t417.hog.trc <- subset(t417.hog, t417.hog$cd_a == 6) # en esta tabla el valor es númerico y no caracter
t417.sdem.trc <- subset(t417.sdem, t417.sdem$cd_a == "06")

# De acuerdo al documento "Conociendo la base de datos de la ENOE"
# es posible unir tablas siguiendo los siguientes pasos:

# 1. Eliminar todos los registros de SDEM que en el campo R_DEF sean diferente a 00 o Entrevista completa
unique(t417.sdem.trc$r_def)
t417.sdem.trc <- subset(t417.sdem.trc, r_def == "00")

# 2. Eliminar todos los registros de la tabla SDEM que en el campo C_RES sean iguales a 2 o Ausente Definitivo
unique(t417.sdem.trc$c_res)
t417.sdem.trc <- subset(t417.sdem.trc, c_res != 2)

# 3. Eliminar todos los registros de la tabla SDEM que en el campo EDA contengan los cógidos de 00 a 11 a 99
unique(t417.sdem.trc$eda)
t417.sdem.trc <- t417.sdem.trc[!(t417.sdem.trc$eda %in%
                                   c("00","01","02","03",
                                     "04","05","06","07",
                                     "08","09","10","11","99")),]

# 4. Registros finales de la tabla SDEMT
length(t417.sdem.trc)

# 5. Eliminar todos los registros de la tabla COE1 y COE2 que en el campo R_DEF sean diferentes de 00.
unique(t417.coe1.trc$r_def)
t417.coe1.trc <- t417.coe1.trc[!(t417.coe1.trc$r_def == "00")]

# 6. Unir las tres tablas con base a la clave principal
t417.trc1 <- merge(t417.sdem.trc, t417.coe1.trc) # Tabla SDEM con COE1
t417.trc2 <- merge(t417.sdem.trc, t417.coe2.trc) # Tabla SDEM con COE2, no es necesario hacerla
t417.trc <- merge(t417.trc1, t417.coe2.trc)      # Tabla SDEM con COE1 y COE2

