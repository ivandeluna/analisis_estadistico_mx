
#--------------------------------------------------------------------------------------------
#
#                  Carga de archivos para la ENOE 3T 2017
#
#--------------------------------------------------------------------------------------------

library(readr)

#--------------------------------------------------------------------------------------------
#
#                  Leer archivos e importarlos a R, crear data frames
#
#--------------------------------------------------------------------------------------------


# Se leen los archivos csv de la ENOE o en su caso en otro formato
t317.coe1 <- read_csv("2017T3/coe1t317.CSV")
t317.coe2 <- read_csv("2017T3/coe2t317.CSV")
t317.hog <- read_csv("2017T3/hogt317.CSV")
t317.sdem <- read_csv("2017T3/sdemt317.CSV")
t317.viv <- read_csv("2017T3/vivt317.CSV")

# Se filtra para contener solo a Torreón, como ciudad autorepresentada

t317.coe1.trc <- subset(t317.coe1, t317.coe1$cd_a == "06")
t317.coe2.trc <- subset(t317.coe2, t317.coe2$cd_a == "06")
t317.hog.trc <- subset(t317.hog, t317.hog$cd_a == 6) # en esta tabla el valor es númerico y no caracter
t317.sdem.trc <- subset(t317.sdem, t317.sdem$cd_a == "06")

t317.sdem.coah <- subset(t317.sdem, t317.sdem$ent == "05")

# De acuerdo al documento "Conociendo la base de datos de la ENOE"
# es posible unir tablas siguiendo los siguientes pasos:

# 1. Eliminar todos los registros de SDEM que en el campo R_DEF sean diferente a 00 o Entrevista completa
unique(t317.sdem.trc$r_def)
t317.sdem.trc <- subset(t317.sdem.trc, r_def == "00")

# 2. Eliminar todos los registros de la tabla SDEM que en el campo C_RES sean iguales a 2 o Ausente Definitivo
unique(t317.sdem.trc$c_res)
t317.sdem.trc <- subset(t317.sdem.trc, c_res != 2)

# 3. Eliminar todos los registros de la tabla SDEM que en el campo EDA contengan los cógidos de 00 a 11 a 99
unique(t317.sdem.trc$eda)
t317.sdem.trc <- t317.sdem.trc[!(t317.sdem.trc$eda %in%
                                   c("00","01","02","03",
                                     "04","05","06","07",
                                     "08","09","10","11","99")),]

# 4. Registros finales de la tabla SDEMT
length(t317.sdem.trc)

# 5. Elminar todos los registros de la tabla COE1 y COE2 que en el campo R_DEF sean diferentes de 00.
unique(t317.coe1.trc$r_def)
t317.coe1.trc <- t317.coe1.trc[!(t317.coe1.trc$r_def == "00")]

# 6. Unir las tres tablas con base a la clave principal
t317.trc1 <- merge(t317.sdem.trc, t317.coe1.trc) # Tabla SDEM con COE1
t317.trc2 <- merge(t317.sdem.trc, t317.coe2.trc) # Tabla SDEM con COE2, no es necesario hacerla
t317.trc <- merge(t317.trc1, t317.coe2.trc)      # Tabla SDEM con COE1 y COE2

rm(t317.trc1)
rm(t317.trc2)

